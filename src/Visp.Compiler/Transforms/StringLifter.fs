// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.StringLifter

open System
open Visp.Compiler.Syntax
open Visp.Compiler.Text
open Visp.Common
open Visp.Compiler

let liftLiteralStrings (file: ParsedFile) =
    let liftableStrings = new ResizeArray<_>()
    let mutable strIndex = 0
    let mutable minRange = range.Zero
    let mutable maxRange = range.Zero

    let file =
        file
        |> Helpers.transformParsedFile (function
            | SynExpr.Const(SynConst.String(raw, kind, stringRange), constRange) as ex ->

                if strIndex = 0 then
                    minRange <- constRange

                maxRange <- constRange

                let fileName = System.IO.Path.GetFileNameWithoutExtension stringRange.FileName

                let name = $"LiftedString{strIndex}_{fileName}"
                strIndex <- strIndex + 1

                let normalized = Visp.Runtime.Library.StringMethods.normalizeIndent raw
                let nameExpr = Syntax.mkSynSymbolExpr name constRange

                match kind with
                | SynStringKind.Verbatim
                | SynStringKind.Regular
                | SynStringKind.TripleQuote ->
                    if Visp.Runtime.Library.StringMethods.isMultilineString raw then
                        let bind =
                            SynExpr.SimpleLet(
                                Syntax.mkInferredName name constRange,
                                SynExpr.Literal(
                                    SynConst.String(normalized, kind, stringRange),
                                    constRange
                                ),
                                constRange
                            )

                        liftableStrings.Add(bind)
                        nameExpr
                    else
                        ex

                | SynStringKind.Interpolated(plain = nest)
                | SynStringKind.InterpolatedTripleQuote(triple = nest) ->
                    let sb = PooledStringBuilder.Get()

                    for _ = 1 to nest do
                        ignore <| sb.Append('{')

                    let op = sb.ToStringAndClear()

                    for _ = 1 to nest do
                        ignore <| sb.Append('}')

                    let cl = sb.ToStringAndClear()

                    let mutable argIndex = 0
                    let variables = ResizeArray<_>()
                    let expressions = ResizeArray<_>()

                    let mutable span = normalized.AsSpan()
                    let mutable loop = true

                    let resultSb = PooledStringBuilder.Get()

                    while loop && not span.IsEmpty do
                        let openIndex = span.IndexOf(op)

                        if openIndex = -1 then
                            resultSb.Append(span) |> ignore
                            loop <- false
                        else
                            resultSb.Append(span.Slice(0, openIndex)) |> ignore

                            let currentSlice = span.Slice(openIndex + op.Length)
                            let closeIndex = currentSlice.IndexOf(cl)

                            if closeIndex = -1 then
                                resultSb.Append(span) |> ignore
                                loop <- false
                            else
                                sb.Append(currentSlice.Slice(0, closeIndex)) |> ignore
                                let varName = sb.ToStringAndClear()

                                let expr = ParseUtils.parseStringToExpr constRange.FileName varName

                                let needsArg =
                                    match expr with
                                    | SynExpr.Symbol it -> it.Text.Contains('.')
                                    | _ -> true

                                if needsArg then
                                    sb.Append("exprArg").Append(argIndex) |> ignore
                                    let argName = sb.ToStringAndClear()
                                    variables.Add(argName)
                                    argIndex <- argIndex + 1
                                    resultSb.Append(op).Append(argName).Append(cl) |> ignore
                                    expressions.Add(expr)
                                else
                                    resultSb.Append(op).Append(varName).Append(cl) |> ignore

                                    match Seq.tryFindIndex (fun it -> it = varName) variables with
                                    | Some _ -> ()
                                    | None ->
                                        variables.Add(varName)
                                        expressions.Add(expr)


                                span <- currentSlice.Slice(closeIndex + cl.Length)
                                ()

                            ()

                        ()

                    sb.ReturnToPool()

                    let args =
                        variables
                        |> Seq.map (fun it -> Syntax.mkInferredArg it constRange)
                        |> List.ofSeq

                    let func =
                        SynExpr.FunctionDef(
                            Syntax.mkSynSymbol name constRange,
                            FunctionFlags.Inline,
                            args,
                            [ SynExpr.Const(
                                  SynConst.String(resultSb.ToStringAndReturn(), kind, stringRange),
                                  constRange
                              ) ],
                            constRange
                        )

                    liftableStrings.Add(func)
                    SynExpr.FunctionCall(nameExpr, expressions |> List.ofSeq, constRange)

            | it -> it)

    let moduleDecls =
        liftableStrings
        |> Seq.map (fun expr -> SynModuleDecl.Expr(expr, expr.Range))
        |> List.ofSeq
        |> (fun its -> ParsedFileFragment.AnonModule(its, Range.unionRanges minRange maxRange))

    let (ParsedFile frags) = file
    ParsedFile(moduleDecls :: frags)
