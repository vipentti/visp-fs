// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module rec Visp.Compiler.Transforms.SyntaxMacros

open Visp.Compiler.SyntaxPrinter
open PrettyPrinter
open Visp.Common
open Visp.Compiler.Syntax
open Visp.Compiler.Transforms
open Visp.Compiler.SyntaxParser
open Visp.Compiler.Text
open Visp.Compiler
open FSharp.Text.Lexing
open Visp.Compiler.Syntax.Macros
open System.Collections.Generic
open Visp.Compiler.LexHelpers


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
    | SynMacroPat.List(list, _) :: SynMacroPat.Ellipsis _ :: [] ->
        // failwithf "list pattern matching %A vs %A" list args
        // printfn "ELLIPSIS MATCH"
        // matchesPat args list

        let mutable result = true

        for arg in args do
            match arg with
            | SynMacroBody.List(exprs = rhs) ->
                let r = matchesPat rhs list

                if not r then
                    result <- false
            | SynMacroBody.Trivia _ -> ()
            | it -> failwithf "unsupported ellipsis list %A" it
        // result <- matchesPat

        result

    // false
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
                // TODO: Trivia matching
                | (SynMacroPat.Trivia _, SynMacroBody.Trivia _) -> true
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
    | SynMacroPat.Symbol(sym, _) :: SynMacroPat.Ellipsis _ :: [] ->
        dict.Add(Syntax.textOfSymbol sym, BoundPatternBody.List(args))
        ()
    | SynMacroPat.List(lst, _) :: SynMacroPat.Ellipsis _ :: [] ->

        use tempPool = PooledDictionary.GetPooled<string, ResizeArray<_>>()
        let temp = tempPool.Value

        // eprintfn "args: %s" (args.Pretty())

        for arg in args do
            match arg with
            | SynMacroBody.List(_, exprs, _) ->
                // eprintfn "Binding: %s to %s" (arg.Pretty()) (lst.Pretty())
                for (lhs, rhs) in Seq.zip lst exprs do
                    match (lhs, rhs) with
                    | (SynMacroPat.Discard _, _) -> ()
                    | (SynMacroPat.Ellipsis _, _) -> failwithf "unsupported ellipsis"
                    | (SynMacroPat.Symbol(sym, _), it) ->
                        if not (temp.ContainsKey(sym.Text)) then
                            temp[sym.Text] <- PooledList.Get()

                        temp[sym.Text].Add(it)
                    | (SynMacroPat.Trivia _, SynMacroBody.Trivia _) -> ()

                    | (lhs, rhs) -> failwithf "unsupported ellipsis list %A, %A" lhs rhs

                    ()

                ()
            | _ -> failwithf "unsupported list pattern: %A" arg

        for kvp in temp do
            dict.Add(kvp.Key, BoundPatternBody.List(List.ofSeq kvp.Value))
            kvp.Value.ReturnToPool()

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
      mutable mode: TokenizeMode
      ctx: LexHelpers.LexContextStack }

    static member Default() =
        { depth = 0
          mode = TokenizeMode.Default
          ctx = LexHelpers.LexContextStack() }

    static member Macro() =
        { depth = 1
          mode = TokenizeMode.Macro
          ctx = LexHelpers.LexContextStack() }

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

[<NoEquality; NoComparison; RequireQualifiedAccess; StructuredFormatDisplay("{StructuredDisplay}")>]
type EvaluatedBody =
    | Item of SynMacroBody
    | List of kind: SynListKind * items: EvaluatedBody list
    | Splice of items: EvaluatedBody list

    member this.Items =
        match this with
        | List(items = it)
        | Splice(items = it) -> Some(it)
        | _ -> None

    member d.StructuredDisplay = evaluatedBodyToDoc d |> docToString

let rec evaluatedBodyToDoc =
    function
    | EvaluatedBody.Item it -> Print.parens <| Print.hsep [ Print.text "item"; macroBodyToDoc it ]
    | EvaluatedBody.List(kind, items) ->
        let nested = items |> List.map evaluatedBodyToDoc |> Print.hsep
        Print.parens <| Print.hsep [ text "coll"; text $"{kind}"; nested ]
    | EvaluatedBody.Splice(items) ->
        let nested = items |> List.map evaluatedBodyToDoc |> Print.hsep
        Print.parens <| Print.hsep [ Print.text "splice"; nested ]

let (|EvaluatedItems|_|) =
    function
    | EvaluatedBody.List(_, it) -> Some(it)
    | EvaluatedBody.Splice(it) -> Some(it)
    | _ -> None

let (|SpliceItems|_|) =
    function
    | EvaluatedBody.Splice(it) -> Some(it)
    | _ -> None


let (|EvaluatedSymbolText|_|) =
    function
    | EvaluatedBody.Item(SynMacroBody.Symbol(it)) -> Some(it.Text)
    | _ -> None

let (|BodySymbolText|_|) =
    function
    | (SynMacroBody.Symbol(it)) -> Some(it.Text)
    | _ -> None

let private mkItem v = EvaluatedBody.Item v

let private findPattern (pats: BoundPats) bod =
    match bod with
    | SynMacroBody.Symbol sym ->
        match pats.TryGetValue(sym.Text) with
        | false, _ -> None
        | true, n -> Some(n)
    | _ -> None

let rec private evaluateBody (pats: BoundPats) (currentBody: SynMacroBody) =
    let bound_evaluate = evaluateBody pats

    match findPattern pats currentBody with
    | Some(pat) ->
        match pat with
        | BoundPatternBody.Item it -> EvaluatedBody.Item it
        | BoundPatternBody.List it -> EvaluatedBody.Splice <| List.map EvaluatedBody.Item it
    | None ->
        match currentBody with
        | SpecialCall "m-concat-id" (_, call_args, r) ->
            let args = call_args |> List.map bound_evaluate

            let syms =
                args
                |> List.map (function
                    | EvaluatedBody.Item(SynMacroBody.Symbol it) -> it.Text
                    | _ -> failwith "unsupported m-concat-id")

            let id = String.concat "" syms

            EvaluatedBody.Item(SynMacroBody.Symbol(Syntax.mkSynSymbol id r))

        | SpecialCall "m-alternate-sep" (_, call_args, _) ->
            let args = call_args |> List.map bound_evaluate

            match args with
            | (sep :: EvaluatedItems lhs :: EvaluatedItems rhs :: []) ->
                EvaluatedBody.Splice(
                    List.zip lhs rhs |> List.map (fun (x, y) -> [ x; sep; y ]) |> List.concat
                )

            | _ -> failwithf "args: %A" args

        | SpecialCall "m-alternate" (_, call_args, _) ->
            let args = call_args |> List.map bound_evaluate

            match args with
            | (EvaluatedItems lhs :: EvaluatedItems rhs :: []) ->
                EvaluatedBody.Splice(
                    List.zip lhs rhs |> List.map (fun (x, y) -> [ x; y ]) |> List.concat
                )

            | _ -> failwithf "args: %A" args


        | SynMacroBody.List(kind, args, _) ->
            // let items = args |> List.map bound_evaluate
            // eprintfn "orig:%s\neval:\n%A" (args.Pretty()) items

            // EvaluatedBody.List(kind, items)
            // EvaluatedBody.List(kind,
            evaluateList pats kind args []
        //)
        //EvaluatedBody.L
        // evaluateList pats kind args
        | SynMacroBody.Call it -> evaluateMacroCall it
        | SynMacroBody.Trivia _
        | SynMacroBody.Symbol _
        | SynMacroBody.Keyword _
        | SynMacroBody.Ellipsis _
        | SynMacroBody.Const _
        | SynMacroBody.Discard _ -> EvaluatedBody.Item currentBody

and private evaluateList pats kind (args: SynMacroBody list) accum =
    // let rec loop pats kind args =
    //     match args with
    // |
    // | _ -> failwith "todo"

    match args with
    | (SynMacroBody.List(_, lst, _) :: SynMacroBody.Ellipsis _ :: rest) ->
        let evaled = lst |> List.map (evaluateBody pats)

        let splicable =
            match lst with
            | (SynMacroBody.Symbol _) :: (SynMacroBody.Trivia _) :: (SynMacroBody.Symbol _) :: [] ->
                let lhs = List.item 0 evaled
                let trivia = List.item 1 evaled
                let rhs = List.item 2 evaled

                match (lhs, rhs) with
                | (EvaluatedItems lhs), (EvaluatedItems rhs) ->
                    let items =
                        List.zip lhs rhs
                        |> List.map (fun (x, y) -> (EvaluatedBody.List(kind, [ x; trivia; y ])))

                    EvaluatedBody.Splice(items)
                | _ -> failwithf "Unsupported ellipsis list %s %A" (lst.Pretty()) evaled
            | (SynMacroBody.Symbol _) :: (SynMacroBody.Symbol _) :: [] ->
                let lhs = List.item 0 evaled
                let rhs = List.item 1 evaled

                match (lhs, rhs) with
                | (EvaluatedItems lhs), (EvaluatedItems rhs) ->
                    let items =
                        List.zip lhs rhs
                        |> List.map (fun (x, y) -> (EvaluatedBody.List(kind, [ x; y ])))

                    EvaluatedBody.Splice(items)
                | _ -> failwithf "Unsupported ellipsis list %s %A" (lst.Pretty()) evaled
            | lst -> failwithf "Unsupported ellipsis list %s %A" (lst.Pretty()) evaled

        (evaluateList pats kind rest (splicable :: accum))
    | a :: rest ->

        let item = evaluateBody pats a
        evaluateList pats kind rest (item :: accum)
    // item :: (evaluateList pats kind rest accum)
    | [] -> (EvaluatedBody.List(kind, List.rev accum))


// | _ ->
//     let items = args |> List.map (evaluateBody pats)
// EvaluatedBody.List(kind, items)
// | SynMacroBody.List _ -> failwith "todo"



and private evaluateMacroCall (SynMacroCall(name = name) as call) =
    match macroTable.TryGetMacro(name.Text) with
    | Some(syn) -> evaluateMacroToEvaluatedBody syn call
    | None -> failwithf "macro: %A not found" name

and private evaluateMacroToEvaluatedBody
    (SynMacro(_, cases, _) as _)
    (SynMacroCall(_, args, _) as _)
    =
    let pat = cases |> List.tryFind (matchesCase args)

    match pat with
    | Some(SynMacroCase(pats, body, _)) ->
        use pooled = PooledDictionary.GetPooled<_, _>()
        let patterns = pooled.Value
        bindPatterns patterns args pats
        evaluateBody patterns body
    | None -> failwith "no matching pattern"

let rec private tokenizeEvaluated
    (res: ResizeArray<token>)
    (args: TokenizeArgs)
    (currentBody: EvaluatedBody)
    =
    let handleSymbol args text =
        match args.mode with
        | TokenizeMode.Macro -> res.Add(SYMBOL text)
        | TokenizeMode.Default ->
            let tok = LexHelpers.symbolOrKeyword args.ctx.Current text

            match tok with
            | MACRO_NAME _
            | SYNTAX_MACRO -> args.StartMacro()
            | _ -> ()

            res.Add(tok)

    let bound_tokenize = tokenizeEvaluated res args

    match currentBody with
    | EvaluatedBody.Splice splice -> splice |> List.iter bound_tokenize

    | EvaluatedBody.List(kind, lst) ->
        res.Add(openToken kind)

        let mutable didPush = false

        let lst =
            match lst with
            | (EvaluatedSymbolText it) :: rest when macroTable.IsMacro it ->
                args.StartMacro()
                res.Add(MACRO_NAME it)
                rest
            | (EvaluatedSymbolText "member") :: rest when args.mode = TokenizeMode.Default ->
                // args.StartMacro()
                res.Add(MEMBER)
                args.ctx.Push LexContext.Member
                didPush <- true
                rest
            | it ->
                args.TryNest()
                it


        lst |> List.iter bound_tokenize

        args.TryUnnest()

        if didPush then
            args.ctx.Pop()

        res.Add(closeToken kind)

    | EvaluatedBody.Item it ->
        match it with
        | SynMacroBody.Call it -> failwithf "macro call should be evaluated: %A" it
        | SynMacroBody.Symbol sym -> handleSymbol args sym.Text
        | SynMacroBody.List(kind, lst, _) ->
            let evaled = EvaluatedBody.List(kind, List.map mkItem lst)
            bound_tokenize evaled
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


let tokensToFunc (tokens: ResizeArray<token>) (range: range) func =
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
        if i < tokens.Count then
            let r = tokens[i]
            i <- i + 1
            r
        else
            EOF

    // printfn "tokens:"

    // for tok in tokens do
    //     printf "%A " tok

    // printfn ""

    try
        func getTokens lexbuf
    with :? ParseHelpers.SyntaxError as syn ->
        LexHelpers.outputSyntaxError syn
        reraise ()


let tokensToMacroBody (tokens: ResizeArray<token>) (range: range) =
    tokensToFunc tokens range raw_macro_body

let evaluatedBodyToMacroBody range evaluated =
    use pooled = PooledList.GetPooled<token>()
    let res = pooled.Value
    let args = TokenizeArgs.Macro()
    tokenizeEvaluated res args evaluated
    tokensToMacroBody res range

let tokensToExpr (tokens: ResizeArray<token>) (range: range) = tokensToFunc tokens range raw_expr

let evaluatedBodyToExpr range evaluated =
    use pooled = PooledList.GetPooled<token>()
    let res = pooled.Value
    let args = TokenizeArgs.Default()
    tokenizeEvaluated res args evaluated

    // printfn "tokens:"

    // for tok in res do
    //     printf "%A " tok

    // printfn ""
    tokensToExpr res range

let private expandSynMacro (SynMacro(_, cases, _) as macro) (SynMacroCall(_, args, range) as call) =
    // printfn "todo %A -> %A" macro call
    let hasInteralMacroCalls bod =
        bod
        |> Traversal.depthFirstMacroBodyPred Traversal.alwaysTrue
        |> Seq.exists (function
            | SynMacroBody.Call _ -> true
            | _ -> false)

    let evalBody = evaluateBody (new BoundPats())


    let mutable evaluated =
        evaluateMacroToEvaluatedBody macro call |> evaluatedBodyToMacroBody range

    while hasInteralMacroCalls evaluated do
        // printfn "%s" (evaluated.Pretty())
        evaluated <- evalBody evaluated |> evaluatedBodyToMacroBody range

    // printfn "%s" (evaluated.Pretty())

    evaluatedBodyToExpr range <| EvaluatedBody.Item evaluated

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

    let expandMacros =
        function
        | SynExpr.SyntaxMacroCall(SynMacroCall(name = name) as call) as it ->
            match macroTable.TryGetMacro(Syntax.textOfSymbol name) with
            | Some(syn) -> expandSynMacro syn call
            | None -> failwithf "macro: %A not found" name
        | it -> it

    let mutable expr = expr |> Helpers.transform collect

    while hasMacroCall expr do
        expr <- Helpers.transform expandMacros expr

    expr
