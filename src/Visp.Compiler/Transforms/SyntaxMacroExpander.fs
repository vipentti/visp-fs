// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.SyntaxMacros

open Visp.Common
open Visp.Compiler.Syntax
open Visp.Compiler.Transforms
open Visp.Compiler.SyntaxParser
open Visp.Compiler.Text
open Visp.Compiler
open FSharp.Text.Lexing
open Visp.Compiler.Syntax.Macros
open System.Collections.Generic

let rec private matchesPat (args: SynMacroBody list) (pats: SynMacroPat list) =
    // printfn "looking for\n%A\nin\n%A" args pats
    // TODO: Determine pattern matching
    match pats with
    | SynMacroPat.Symbol _ :: SynMacroPat.Ellipsis _ :: [] -> true
    | pt :: rest ->
        match args with
        | arg :: argRest ->
            // printfn "matching %A with %A" arg pt
            let temp =
                match (pt, arg) with
                // TODO: Constant matching
                | (SynMacroPat.Const _, SynMacroBody.Const _) -> true
                | (SynMacroPat.Symbol _, _) -> true
                | (SynMacroPat.Discard _, _) -> true
                // TODO: Nested matching
                | (SynMacroPat.List(lhs, _), SynMacroBody.List(exprs = rhs)) -> matchesPat rhs lhs
                // () gets parsed as UNIT but in some places we want to allow () to be treated as empty list
                | (SynMacroPat.List(lhs, _), SynMacroBody.Const(SynConst.Unit, _)) -> matchesPat [] lhs
                | _ -> false

            temp && matchesPat argRest rest
        | [] -> false
    | [] ->
        match args with
        | [] -> true
        | _ -> false

let private matchesCase (args: SynMacroBody list) (SynMacroCase(pats = pats)) = matchesPat args pats

[<RequireQualifiedAccess>]
type private BoundPatternBody =
    | List of body: SynMacroBody list
    | Item of body: SynMacroBody

let rec private bindPatterns
    (dict: Dictionary<string, BoundPatternBody>)
    (args: SynMacroBody list)
    (pats: SynMacroPat list)
    =
    match pats with
    // TODO support ellipsis in other positions
    | SynMacroPat.Symbol(sym, _) :: SynMacroPat.Ellipsis _ :: [] ->
        dict.Add(Syntax.textOfSymbol sym, BoundPatternBody.List(args))
        ()
    | pt :: patRest ->
        match args with
        | arg :: argRest ->
            match (pt, arg) with
            | (SynMacroPat.Discard _, _) -> ()
            | (SynMacroPat.Const _, _) -> ()
            | (SynMacroPat.Ellipsis _, it) -> failwithf "unsupported ellipsis for %A" it
            | (SynMacroPat.Symbol(sym, _), it) ->
                dict.Add(Syntax.textOfSymbol sym, BoundPatternBody.Item(it))
            | (SynMacroPat.List(ps, _), SynMacroBody.List(exprs = exprs)) ->
                bindPatterns dict exprs ps
                ()
            | (SynMacroPat.List(ps, _), SynMacroBody.Const (SynConst.Unit, _)) ->
                bindPatterns dict [] ps
                ()
            // TODO: valide if this is enough
            | (_, _) -> ()

            bindPatterns dict argRest patRest
        | [] -> failwithf "unmatched pattern %A" pt
    | [] ->
        match args with
        | [] -> ()
        | _ -> failwithf "unmatched arguments %A" args

let openToken =
    function
    | SynListKind.List -> LPAREN
    | SynListKind.Vector -> LBRACKET
    | SynListKind.HashMap -> LBRACE
    | SynListKind.HashSet -> HASH_BRACE
    | SynListKind.AttributeList -> HASH_BRACKET
    | SynListKind.HashParen -> HASH_PAREN

let closeToken =
    function
    | SynListKind.List -> RPAREN
    | SynListKind.HashParen -> RPAREN
    | SynListKind.Vector -> RBRACKET
    | SynListKind.HashMap -> RBRACE
    | SynListKind.HashSet -> RBRACE
    | SynListKind.AttributeList -> RBRACKET

let private evaluatePatterns
    (body: SynMacroBody)
    (pats: Dictionary<string, BoundPatternBody>)
    (range: range)
    : SynExpr =
    let findPattern bod (pats: Dictionary<string, BoundPatternBody>) =
        match bod with
        | SynMacroBody.Symbol sym ->
            match pats.TryGetValue(sym.Text) with
            | false, _ -> None
            | true, n -> Some(n)
        | _ -> None

    let rec tokenize
        (f: SynMacroBody)
        (pats: Dictionary<string, BoundPatternBody>)
        (res: ResizeArray<token>)
        =

        match findPattern f pats with
        | Some(pat) ->
            match pat with
            | BoundPatternBody.Item(it) -> tokenize it pats res
            | BoundPatternBody.List(lst) -> lst |> List.iter (fun ex -> tokenize ex pats res)

        | None ->
            match f with
            | SynMacroBody.List(kind, lst, _) ->
                res.Add(openToken kind)
                lst |> List.iter (fun ex -> tokenize ex pats res)
                res.Add(closeToken kind)

            | SynMacroBody.Trivia(kind, _) ->
                match kind with
                | SynMacroTriviaKind.Colon -> res.Add(COLON)
                | SynMacroTriviaKind.Dot -> res.Add(DOT)
                | SynMacroTriviaKind.Comma -> res.Add(COMMA)
                | SynMacroTriviaKind.Bar -> res.Add(BAR)

            | SynMacroBody.Keyword kw -> res.Add(KEYWORD(Syntax.textOfKeyword kw))
            | SynMacroBody.Ellipsis _ -> ()
            | SynMacroBody.Discard _ -> res.Add(SYMBOL "_")
            | SynMacroBody.Const(c, _) ->
                match c with
                | SynConst.Bool v -> res.Add(if v then TRUE else FALSE)
                | SynConst.Char ch -> res.Add(CHAR(ch.ToString()))
                | SynConst.Decimal dec -> res.Add(DECIMAL dec)
                | SynConst.Int32 dec -> res.Add(INT32 dec)
                | SynConst.Int64 dec -> res.Add(INT64 dec)
                | SynConst.Unit -> res.Add(UNIT)
                | SynConst.Nil -> res.Add(NIL)
                | SynConst.String(s, k, _) -> res.Add(STRING(s, k, ParseHelpers.LexCont.Token()))

                ()

            | SynMacroBody.Symbol sym -> res.Add(LexHelpers.symbolOrKeyword sym.Text)

    use pooled = PooledList.GetPooled<token>()
    let res = pooled.Value

    tokenize body pats res

    // Dummy lexbuffer
    let lexbuf = LexBuffer<_>.FromString ""
    let pos = Position.FirstLine range.FileName

    lexbuf.StartPos <-
        { pos with
            pos_lnum = range.StartLine
            pos_orig_lnum = range.StartLine
            pos_cnum = range.StartColumn }

    lexbuf.EndPos <-
        { pos with
            pos_lnum = range.EndLine
            pos_orig_lnum = range.EndLine
            pos_cnum = range.EndColumn }

    let mutable i = 0

    let getTokens _ =
        if i < res.Count then
            let r = res[i]
            i <- i + 1
            r
        else
            EOF

    // printfn "tokens %A" res
    // for tok in res do
    //     printf "%A " tok

    try
        let result = raw_expr getTokens lexbuf

        result
    with :? ParseHelpers.SyntaxError as syn ->
        LexHelpers.outputSyntaxError syn
        reraise ()

let private expandSynMacro (SynMacro(_, cases, _) as macro) (SynMacroCall(_, args, range) as call) =
    // printfn "todo %A -> %A" macro call

    let pat = cases |> List.tryFind (matchesCase args)

    match pat with
    | Some(SynMacroCase(pats, body, _)) ->
        use pooled = PooledDictionary.GetPooled<_, _>()
        let patterns = pooled.Value
        bindPatterns patterns args pats
        let result = evaluatePatterns body patterns range

        result
    | None -> failwith "no matching pattern"

let expand (expr: SynExpr) =

    let collect =
        function
        | SynExpr.SyntaxMacro(SynMacro(name = name; range = r) as macro) ->
            let text = (Syntax.textOfSymbol name)
            macroTable.AddMacro text macro

            SynExpr.SimpleLet(
                Syntax.mkInferredName ("macro_" + text) r,
                Syntax.mkSynString "__MACRO_INIT__" r,
                r
            )
        | it -> it

    let expandMacros =
        function
        | SynExpr.SyntaxMacroCall(SynMacroCall(name = name) as call) ->
            match macroTable.TryGetMacro(Syntax.textOfSymbol name) with
            | Some(syn) -> expandSynMacro syn call
            | None -> failwithf "macro: %A not found" name
        | it -> it

    // TODO: this should continue expanding until no more macro invocations are available
    [ collect; expandMacros ] |> Helpers.runTransforms1 expr
