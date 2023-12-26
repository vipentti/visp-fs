// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.StringLifter

open System
open Visp.Compiler.Syntax
open Visp.Compiler.Text
open Visp.Common
open Visp.Compiler

type private Str = string * SynStringKind * range

let inline private toListAndReturn (lst: ResizeArray<'T>) =
    let mutable res = ([]: 'T list)

    for i = lst.Count - 1 downto 0 do
        res <- lst[i] :: res

    lst.ReturnToPool()

    res

let private handleInterpolatedString nest ((raw, kind, stringRange): Str) (constRange: range) =
    let normalized = Visp.Runtime.Library.StringMethods.normalizeIndent raw
    // TODO: Support non-multiline interpolated strings as printf argument
    // e.g. (printfn $"something {(+ 1 2)}") should be valid
    let tempSb = PooledStringBuilder.Get()

    for _ = 1 to nest do
        ignore <| tempSb.Append('{')

    let openBraces = tempSb.ToStringAndClear()

    for _ = 1 to nest do
        ignore <| tempSb.Append('}')

    let closeBraces = tempSb.ToStringAndClear()

    let mutable argIndex = 0
    let variables = PooledList.Get()
    let expressions = PooledList.Get()

    let mutable span = normalized.AsSpan()
    let mutable loop = true

    let resultSb = PooledStringBuilder.Get()
    resultSb.EnsureCapacity(raw.Length) |> ignore

    while loop && not span.IsEmpty do
        let openIndex = span.IndexOf(openBraces)

        if openIndex = -1 then
            resultSb.Append(span) |> ignore
            loop <- false
        else
            resultSb.Append(span.Slice(0, openIndex)) |> ignore

            let currentSlice = span.Slice(openIndex + openBraces.Length)
            let closeIndex = currentSlice.IndexOf(closeBraces)

            if closeIndex = -1 then
                resultSb.Append(span) |> ignore
                loop <- false
            else
                let varNameSlice = currentSlice.Slice(0, closeIndex)
                let colonIndex = varNameSlice.IndexOf(':')

                let (varName, fmt) =
                    if colonIndex = -1 then
                        let name = tempSb.Append(varNameSlice).ToStringAndClear()
                        (name, "")
                    else
                        let fmtSlice = varNameSlice.Slice(colonIndex)
                        let fmt = tempSb.Append(fmtSlice).ToStringAndClear()
                        let varNameSlice = varNameSlice.Slice(0, colonIndex)
                        let name = tempSb.Append(varNameSlice).ToStringAndClear()
                        (name, fmt)

                let expr = ParseUtils.parseStringToExpr constRange.FileName varName

                let needsArg =
                    match expr with
                    | SynExpr.Symbol it -> it.Text.Contains('.')
                    | _ -> true

                if needsArg then
                    let argName = tempSb.Append("exprArg").Append(argIndex).ToStringAndClear()

                    variables.Add(argName)
                    argIndex <- argIndex + 1

                    resultSb.Append(openBraces).Append(argName).Append(fmt).Append(closeBraces)
                    |> ignore

                    expressions.Add(expr)
                else
                    resultSb.Append(openBraces).Append(varName).Append(fmt).Append(closeBraces)
                    |> ignore

                    match Seq.tryFindIndex (fun it -> it = varName) variables with
                    | Some _ -> ()
                    | None ->
                        variables.Add(varName)
                        expressions.Add(expr)


                span <- currentSlice.Slice(closeIndex + closeBraces.Length)
                ()

            ()

        ()

    tempSb.ReturnToPool()

    let newStrConst =
        SynExpr.Const(SynConst.String(resultSb.ToStringAndReturn(), kind, stringRange), constRange)

    (newStrConst, toListAndReturn variables, toListAndReturn expressions)

let fmtTypeFactory (name: string) range =
    SynType.Ident(
        Ident(
            (if name.StartsWith("Printf.") then
                 name
             else
                 "Printf." + name),
            range
        )
    )

let private (|ArgMatch|_|) (index: int) (args: 'a list) =
    match index with
    | 0 ->
        match args with
        | it :: _ -> Some(it)
        | _ -> None
    | 1 ->
        match args with
        | _ :: it :: _ -> Some(it)
        | _ -> None
    | 2 ->
        match args with
        | _ :: _ :: it :: _ -> Some(it)
        | _ -> None
    | _ -> None

type private FmtMethod =
    { name: string
      typeName: string
      fmtArgPos: int }

let private mkFmtMethod n t a =
    { name = n
      typeName = t
      fmtArgPos = a }

// https://github.com/dotnet/fsharp/blob/99514c0fafa1f4a9ddf63e0439ec8804d87276eb/src/FSharp.Core/printf.fsi#L101-L443
let private fmtMethods =
    [ mkFmtMethod "bprintf" "BuilderFormat<_>" 1
      mkFmtMethod "fprintf" "TextWriterFormat<_>" 1
      mkFmtMethod "fprintfn" "TextWriterFormat<_>" 1
      mkFmtMethod "eprintf" "TextWriterFormat<_>" 0
      mkFmtMethod "eprintfn" "TextWriterFormat<_>" 0
      mkFmtMethod "printf" "TextWriterFormat<_>" 0
      mkFmtMethod "printfn" "TextWriterFormat<_>" 0
      mkFmtMethod "sprintf" "StringFormat<_>" 0
      mkFmtMethod "kbprintf" "BuilderFormat<_, _>" 2
      mkFmtMethod "kfprintf" "TextWriterFormat<_, _>" 2
      mkFmtMethod "kprintf" "StringFormat<_, _>" 1
      mkFmtMethod "ksprintf" "StringFormat<_, _>" 1
      mkFmtMethod "failwithf" "StringFormat<_, _>" 0 ]

/// match method name based on the last non-dotted part
let private matchesFmtMethod (str: string) (met: FmtMethod) =
    let mutable strSpan = str.AsSpan()
    let dotIndex = str.IndexOf('.')

    if dotIndex > -1 then
        strSpan <- strSpan.Slice(dotIndex + 1)

    strSpan.Equals(met.name, StringComparison.Ordinal)

let private isFmtMethod (str: string) =
    fmtMethods |> List.exists (matchesFmtMethod str)

let private getFmtMethod (str: string) =
    fmtMethods |> List.find (matchesFmtMethod str)

let private fmtMethodFactories =
    [ ("bprintf", fmtTypeFactory "BuilderFormat<_>")
      ("fprintf", fmtTypeFactory "TextWriterFormat<_>")
      ("fprintfn", fmtTypeFactory "TextWriterFormat<_>")
      ("eprintf", fmtTypeFactory "TextWriterFormat<_>")
      ("eprintfn", fmtTypeFactory "TextWriterFormat<_>")
      ("printf", fmtTypeFactory "TextWriterFormat<_>")
      ("printfn", fmtTypeFactory "TextWriterFormat<_>")
      ("sprintf", fmtTypeFactory "StringFormat<_>")
      ("kbprintf", fmtTypeFactory "BuilderFormat<_, _>")
      ("kfprintf", fmtTypeFactory "TextWriterFormat<_, _>")
      ("kprintf", fmtTypeFactory "StringFormat<_, _>")
      ("ksprintf", fmtTypeFactory "StringFormat<_, _>")
      ("failwithf", fmtTypeFactory "StringFormat<_, _>") ]
    |> Map.ofList

let private fmtArgMatches =
    [ ("bprintf", 1)
      ("fprintf", 1)
      ("fprintfn", 1)
      ("eprintf", 0)
      ("eprintfn", 0)
      ("printf", 0)
      ("printfn", 0)
      ("sprintf", 0)
      ("kbprintf", 2)
      ("kfprintf", 2)
      ("kprintf", 1)
      ("ksprintf", 1)
      ("failwithf", 0) ]
    |> Map.ofList

type private InterpolatedStringData =
    { text: string
      kind: SynStringKind
      stringRange: range
      constRange: range }

let private (|InterpolatedString|_|) (ex: SynExpr) =
    match ex with
    | SynExpr.Const(SynConst.String(raw, kind, stringRange), constRange) ->
        match kind with
        | SynStringKind.Interpolated _
        | SynStringKind.InterpolatedTripleQuote _ ->
            Some(
                { text = raw
                  kind = kind
                  stringRange = stringRange
                  constRange = constRange }
            )
        | _ -> None
    | _ -> None

let liftLiteralStrings (file: ParsedFile) =
    use liftableStringsPooled = PooledDictionary.GetPooled()
    let liftableStrings = liftableStringsPooled.Value
    let mutable strIndex = 0
    let mutable minRange = range.Zero
    let mutable maxRange = range.Zero
    let mutable maybeNextReturnType = None

    let (|InterpolatedStringCall|_|) (ex: SynExpr) =
        match ex with
        | SynExpr.FunctionCall(Patterns.SymbolWith maybeLifted, _, _) ->
            match liftableStrings.TryGetValue maybeLifted with
            | true, ex -> Some((maybeLifted, ex))
            | _ -> None
        | _ -> None

    let file =
        file
        |> Helpers.transformParsedFile (function
            | SynExpr.FunctionCall(Patterns.SymbolWith maybePrint, args, range) as it when
                isFmtMethod maybePrint
                ->

                let method = getFmtMethod maybePrint
                let retTy = fmtTypeFactory method.typeName range
                let argIndex = method.fmtArgPos

                match args with
                | ArgMatch argIndex (InterpolatedStringCall(name,
                                                            SynExpr.FunctionDef(fnname,
                                                                                flags,
                                                                                args,
                                                                                body,
                                                                                None,
                                                                                range))) ->

                    liftableStrings[name] <-
                        SynExpr.FunctionDef(fnname, flags, args, body, Some retTy, range)

                    ()
                | _ -> ()

                it
            | SynExpr.Const(SynConst.String(raw, kind, stringRange), constRange) as ex ->

                if strIndex = 0 then
                    minRange <- constRange

                maxRange <- constRange

                let fileName = System.IO.Path.GetFileNameWithoutExtension stringRange.FileName

                let name = $"LiftedString{strIndex}_{fileName}"
                strIndex <- strIndex + 1

                let nameExpr = Syntax.mkSynSymbolExpr name constRange

                match kind with
                | SynStringKind.Verbatim
                | SynStringKind.Regular
                | SynStringKind.TripleQuote ->
                    if Visp.Runtime.Library.StringMethods.isMultilineString raw then
                        let normalized = Visp.Runtime.Library.StringMethods.normalizeIndent raw

                        let bind =
                            SynExpr.LetOrUse(
                                Syntax.mkInferredNamePat name constRange,
                                SynExpr.Literal(
                                    SynConst.String(normalized, kind, stringRange),
                                    constRange
                                ),
                                LetFlags.None,
                                [],
                                constRange
                            )

                        liftableStrings[name] <- bind
                        nameExpr
                    else
                        ex

                | SynStringKind.Interpolated(plain = nest)
                | SynStringKind.InterpolatedTripleQuote(triple = nest) ->
                    let (newConst, variables, expressions) =
                        handleInterpolatedString nest (raw, kind, stringRange) constRange

                    let args =
                        variables |> List.map (fun it -> Syntax.mkInferredNamePat it constRange)

                    let retTy =
                        match maybeNextReturnType with
                        | Some(it) ->
                            maybeNextReturnType <- None
                            Some it
                        | None -> None

                    let func =
                        SynExpr.FunctionDef(
                            Syntax.mkSynSymbol name constRange,
                            FunctionFlags.Inline,
                            SynPat.Args(SynArgPats.List(args), constRange),
                            [ newConst ],
                            retTy,
                            constRange
                        )

                    liftableStrings[name] <- func
                    SynExpr.FunctionCall(nameExpr, expressions, constRange)

            | it -> it)

    let moduleDecls =
        liftableStrings.Values
        |> Seq.map (fun expr -> SynModuleDecl.Expr(expr, expr.Range))
        |> List.ofSeq
        |> (fun its -> ParsedFileFragment.AnonModule(its, Range.unionRanges minRange maxRange))

    let (ParsedFile frags) = file
    ParsedFile(moduleDecls :: frags)
