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


let (|MatchingText|) str (pat: SynMacroPat) =
    match pat with
    | SynMacroPat.Symbol(it, _) -> if it.Text = str then true else false
    | _ -> false

let (|DiscardPredicate|Not|) (pat: SynMacroPat) =
    match pat with
    | SynMacroPat.List([ MatchingText "?discard" true ], _) -> DiscardPredicate
    | _ -> Not

let (|SymText|_|) (pat: SynMacroBody) =
    match pat with
    | SynMacroBody.Symbol it -> Some(it.Text)
    | _ -> None

let (|SpecialCall|_|) call (pat: SynMacroBody) =
    match pat with
    | SynMacroBody.List(k, (SynMacroBody.Symbol sym) :: rest, r) when sym.Text = call ->
        Some(k, rest, r)
    | _ -> None


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
                | (DiscardPredicate, SynMacroBody.Discard _) ->
                    // printfn "DISCAAARD pt: %A lhs: %A\nRESTPAT:\n%A\nARGREST:\n%A" pt arg rest argRest
                    true
                // TODO: Constant matching
                | (SynMacroPat.Const _, SynMacroBody.Const _) -> true
                | (SynMacroPat.Symbol _, _) -> true
                | (SynMacroPat.Discard _, _) -> true
                // TODO: Nested matching
                | (SynMacroPat.List(lhs, _), SynMacroBody.List(exprs = rhs)) -> matchesPat rhs lhs
                // () gets parsed as UNIT but in some places we want to allow () to be treated as empty list
                | (SynMacroPat.List(lhs, _), SynMacroBody.Const(SynConst.Unit, _)) ->
                    matchesPat [] lhs
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
            | (SynMacroPat.List(ps, _), SynMacroBody.Const(SynConst.Unit, _)) ->
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
    | SynListKind.BraceBar -> BRACE_BAR

let closeToken =
    function
    | SynListKind.List -> RPAREN
    | SynListKind.HashParen -> RPAREN
    | SynListKind.Vector -> RBRACKET
    | SynListKind.HashMap -> RBRACE
    | SynListKind.HashSet -> RBRACE
    | SynListKind.AttributeList -> RBRACKET
    | SynListKind.BraceBar -> BAR_BRACE

[<RequireQualifiedAccess>]
type private TokenizeMode =
    | Default
    | Macro

type private TokenizeArgs =
    { mutable depth: int32
      mutable mode: TokenizeMode }

    member t.TryNest() =
        if t.mode = TokenizeMode.Macro then
            t.depth <- t.depth + 1

    member t.StartMacro() =
        t.mode <- TokenizeMode.Macro
        t.TryNest()

    member t.TryUnnest() =
        if t.mode = TokenizeMode.Macro then
            t.depth <- t.depth - 1

            if t.depth <= 0 then
                t.mode <- TokenizeMode.Default
                t.depth <- 0

type private BoundPats = Dictionary<string, BoundPatternBody>

let private evaluatePatterns (body: SynMacroBody) (pats: BoundPats) (range: range) : SynExpr =


    let findPattern (pats: BoundPats) bod =
        match bod with
        | SynMacroBody.Symbol sym ->
            match pats.TryGetValue(sym.Text) with
            | false, _ -> None
            | true, n -> Some(n)
        | _ -> None

    let rec getBody (pats: BoundPats) bod =
        match findPattern pats bod with
        | Some(it) ->
            match it with
            | BoundPatternBody.Item it -> getBody pats it
            | BoundPatternBody.List lst -> lst |> List.map (getBody pats) |> List.concat
        | None -> [ bod ]

    let rec tokenize
        (pats: BoundPats)
        (res: ResizeArray<token>)
        (args: TokenizeArgs)
        (currentBody: SynMacroBody)
        =

        let handleSymbol args text =
            match args.mode with
            | TokenizeMode.Macro -> res.Add(SYMBOL text)
            | TokenizeMode.Default ->
                let tok = LexHelpers.symbolOrKeyword text

                match tok with
                | MACRO_NAME _
                | SYNTAX_MACRO -> args.StartMacro()
                | _ -> ()

                res.Add(tok)

        let bound_tokenize = tokenize pats res args

        match findPattern pats currentBody with
        | Some(pat) ->
            match pat with
            | BoundPatternBody.Item(it) -> bound_tokenize it
            | BoundPatternBody.List(lst) -> lst |> List.iter bound_tokenize

        | None ->
            match currentBody with
            | SpecialCall "m-concat-id" (_, call_args, _) ->
                match call_args with
                | arg1 :: arg2 :: [] ->
                    match ((getBody pats arg1), (getBody pats arg2)) with
                    | ([ SymText lhs ], [ SymText rhs ]) -> handleSymbol args (lhs + rhs)
                    | _ -> failwithf "todo concat id %A" call_args

                | _ -> failwithf "todo concat id %A" call_args

                ()
            | SpecialCall "m-map" (_, call_args, _) ->
                match call_args with
                | (SymText method) :: (SynMacroBody.List(_, list, _)) :: [] ->
                    let argz = list |> List.map (getBody pats) |> List.concat

                    match method with
                    | "m-name" ->
                        let names =
                            argz
                            |> List.choose (function
                                | SynMacroBody.Symbol it -> Some(it)
                                | SynMacroBody.List(_, SynMacroBody.Symbol it :: _, _) -> Some(it)
                                | SynMacroBody.Ellipsis _ -> None
                                | it -> failwithf "unsupported m-map %A" it)
                            |> List.map _.Text

                        names |> List.iter (handleSymbol args)

                    | _ -> failwithf "unsupported m-map method: %A %A" method call_args

                | _ -> failwithf "todo concat id %A" call_args

                ()
            | SynMacroBody.List(kind, lst, _) ->
                res.Add(openToken kind)

                args.TryNest()

                lst |> List.iter bound_tokenize

                args.TryUnnest()
                res.Add(closeToken kind)

            | SynMacroBody.Trivia(kind, _) ->
                match kind with
                | SynMacroTriviaKind.Colon -> res.Add(COLON)
                | SynMacroTriviaKind.ColonColon -> res.Add(COLON_COLON)
                | SynMacroTriviaKind.Dot -> res.Add(DOT)
                | SynMacroTriviaKind.Comma -> res.Add(COMMA)
                | SynMacroTriviaKind.Bar -> res.Add(BAR)

            | SynMacroBody.Keyword kw -> res.Add(KEYWORD(Syntax.textOfKeyword kw))
            | SynMacroBody.Ellipsis _ -> ()
            | SynMacroBody.Discard _ -> res.Add(SYMBOL "_")
            | SynMacroBody.Const(c, _) ->
                match c with
                | SynConst.Bool v -> res.Add(if v then TRUE else FALSE)
                | SynConst.Char ch -> res.Add(CHAR(ParseHelpers.charToParseable ch))
                | SynConst.Decimal it -> res.Add(DECIMAL it)
                | SynConst.SByte it -> res.Add(INT8(it, false))
                | SynConst.Int16 it -> res.Add(INT16(it, false))
                | SynConst.Int32 it -> res.Add(INT32(it, false))
                | SynConst.Int64 it -> res.Add(INT64(it, false))
                | SynConst.Byte it -> res.Add(UINT8 it)
                | SynConst.UInt16 it -> res.Add(UINT16 it)
                | SynConst.UInt32 it -> res.Add(UINT32 it)
                | SynConst.UInt64 it -> res.Add(UINT64 it)
                | SynConst.Single it -> res.Add(IEEE32 it)
                | SynConst.Double it -> res.Add(IEEE64 it)
                | SynConst.IntPtr it -> res.Add(NATIVEINT(it, false))
                | SynConst.UIntPtr it -> res.Add(UNATIVEINT it)
                | SynConst.Unit -> res.Add(UNIT)
                | SynConst.Nil -> res.Add(NIL)
                | SynConst.String(s, k, _) -> res.Add(STRING(s, k, ParseHelpers.LexCont.Token([])))

                ()

            | SynMacroBody.Symbol sym -> handleSymbol args sym.Text

    use pooled = PooledList.GetPooled<token>()
    let res = pooled.Value

    let args =
        { depth = 0
          mode = TokenizeMode.Default }

    tokenize pats res args body

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

        // printfn "result\n%A" result

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

let private hasMacroCall (expr: SynExpr) =
    expr
    |> Traversal.depthFirstExprs
    |> Seq.exists (function
        | SynExpr.SyntaxMacroCall _ -> true
        | _ -> false)

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

    let mutable didExpand = true

    let expandMacros =
        function
        | SynExpr.SyntaxMacroCall(SynMacroCall(name = name) as call) ->
            match macroTable.TryGetMacro(Syntax.textOfSymbol name) with
            | Some(syn) ->
                didExpand <- true
                expandSynMacro syn call
            | None -> failwithf "macro: %A not found" name
        | it -> it

    // TODO: this should continue expanding until no more macro invocations are available

    let mutable expr = expr |> Helpers.transform collect

    while hasMacroCall expr do
        expr <- Helpers.transform expandMacros expr

    expr
