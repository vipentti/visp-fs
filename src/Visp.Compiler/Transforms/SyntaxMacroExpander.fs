// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.SyntaxMacros

open System.Collections.Generic
open Visp.Compiler.Syntax
open Visp.Compiler.Transforms
open Visp.Compiler.SyntaxParser
open Visp.Compiler
open FSharp.Text.Lexing

// TODO: Store in some state somewhere
let macros = new Dictionary<string, SynMacro>()

let private matchesPat (args: SynMacroBody list) (pat: SynMacroPat list) =
    // TODO: Determine pattern matching
    true

let private matchesCase (args: SynMacroBody list) (SynMacroCase(pats = pats)) = matchesPat args pats

[<RequireQualifiedAccess>]
type private BoundPattern =
    | List of pat: SynMacroPat * body: SynMacroBody list
    | Item of pat: SynMacroPat * body: SynMacroBody

let rec private bindPatterns (args: SynMacroBody list) (pats: SynMacroPat list) =
    match pats with
    // TODO support ellipsis in other positions
    | SynMacroPat.Ellipsis _ as pt :: [] -> [ BoundPattern.List(pt, args) ]
    | pt :: rest ->
        match args with
        | arg :: argRest -> BoundPattern.Item(pt, arg) :: (bindPatterns argRest rest)
        | [] -> failwithf "unmatched pattern %A" pt
    | [] ->
        match args with
        | [] -> []
        | _ -> failwithf "unmatched arguments %A" args

let private evaluatePatterns (body: SynMacroBody) (pats: BoundPattern list) =

    let findPattern (f: SynMacroBody) (pats: BoundPattern list) =
        match f with
        | SynMacroBody.Symbol sym ->
            printfn "looking for %A in\n%A" sym pats

            let found =
                pats
                |> List.tryFind (function
                    | BoundPattern.Item(SynMacroPat.Symbol(it, _), _) -> sym.TextEquals it
                    | _ -> false)

            found

        | SynMacroBody.Ellipsis it ->
            let found =
                pats
                |> List.tryPick (function
                    | BoundPattern.List(SynMacroPat.Ellipsis _, _) as its -> Some(its)
                    | _ -> None)

            found

        | _ -> None


    let rec tokenize (f: SynMacroBody) (pats: BoundPattern list) (res: ResizeArray<token>) =
        match f with
        | SynMacroBody.List(lst, _) ->
            res.Add(LPAREN)
            lst |> List.iter (fun ex -> tokenize ex pats res)
            res.Add(RPAREN)
        | SynMacroBody.Vector(lst, _) ->
            res.Add(LBRACKET)
            lst |> List.iter (fun ex -> tokenize ex pats res)
            res.Add(RBRACKET)
        | SynMacroBody.HashMap(lst, _) ->
            res.Add(LBRACE)
            lst |> List.iter (fun ex -> tokenize ex pats res)
            res.Add(RBRACE)
        | SynMacroBody.HashSet(lst, _) ->
            res.Add(HASH_BRACE)
            lst |> List.iter (fun ex -> tokenize ex pats res)
            res.Add(RBRACE)

        | SynMacroBody.Keyword kw as tt ->
            match findPattern tt pats with
            | Some(pat) ->
                match pat with
                | BoundPattern.Item(_, it) -> tokenize it pats res
                | BoundPattern.List(_, lst) -> lst |> List.iter (fun ex -> tokenize ex pats res)

                ()
            | None -> res.Add(KEYWORD(Syntax.textOfKeyword kw))

        | SynMacroBody.Ellipsis _ as tt ->
            match findPattern tt pats with
            | Some(pat) ->
                match pat with
                | BoundPattern.Item(_, it) -> tokenize it pats res
                | BoundPattern.List(_, lst) -> lst |> List.iter (fun ex -> tokenize ex pats res)

                ()
            | None -> ()

        | SynMacroBody.Discard _ as tt ->
            match findPattern tt pats with
            | Some(pat) ->
                match pat with
                | BoundPattern.Item(_, it) -> tokenize it pats res
                | BoundPattern.List(_, lst) -> lst |> List.iter (fun ex -> tokenize ex pats res)

                ()
            | None -> res.Add(SYMBOL "_")


        | SynMacroBody.Const(c, _) as tt ->
            match findPattern tt pats with
            | Some(pat) ->
                match pat with
                | BoundPattern.Item(_, it) -> tokenize it pats res
                | BoundPattern.List(_, lst) -> lst |> List.iter (fun ex -> tokenize ex pats res)

                ()
            | None ->
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

        | SynMacroBody.Symbol sym as tt ->
            match findPattern tt pats with
            | Some(pat) ->
                match pat with
                | BoundPattern.Item(_, it) -> tokenize it pats res
                | BoundPattern.List(_, lst) -> lst |> List.iter (fun ex -> tokenize ex pats res)

                ()
            | None ->
                let text = (Syntax.textOfSymbol sym)
                res.Add(LexHelpers.symbolOrKeyword text)

    // | _ -> failwith "todo"


    let res = ResizeArray<token>()

    tokenize body pats res

    // let tokens = res.ToArray()

    // printfn "TOKENS: %A" tokens

    // Dummy lexbuffer
    let lexbuf = LexBuffer<_>.FromString ""
    lexbuf.EndPos <- Position.FirstLine ""

    let mutable i = 0

    let getTokens _ =
        if i < res.Count then
            let r = res[i]
            i <- i + 1
            r
        else
            EOF

    let result = raw_expr getTokens lexbuf

    printfn "RESULT %A" result

    result

let private expandSynMacro
    (SynMacro(_, cases, _) as macro)
    (SynMacroCall(_, args, _) as call)
    =
    printfn "todo %A -> %A" macro call

    let pat = cases |> List.tryFind (matchesCase args)

    match pat with
    | Some(SynMacroCase(pats, body, _)) ->
        // let bindings = Dictionary<string, SynMacroBody>()
        let patterns = bindPatterns args pats

        // printfn "found pat %A" patterns

        let result = evaluatePatterns body patterns

        // printfn "RESULT: %A" result

        result
    | None -> failwith "no matching pattern"

let expand (expr: SynExpr) =

    let collect =
        function
        | SynExpr.SyntaxMacro(SynMacro(name = name; range = r) as macro) ->
            let text = (Syntax.textOfSymbol name)
            macros[text] <- macro

            SynExpr.SimpleLet(
                Syntax.mkInferredName ("macro_" + text) r,
                Syntax.mkSynString "__MACRO_INIT__" r,
                r
            )
        | it -> it

    let expandMacros =
        function
        | SynExpr.SyntaxMacroCall(SynMacroCall(name = name) as call) ->
            match macros.TryGetValue(Syntax.textOfSymbol name) with
            | false, _ -> failwithf "macro: %A not found" name
            | true, syn -> expandSynMacro syn call
        | it -> it

    // TODO: this should continue expanding until no more macro invocations are available
    [ collect; expandMacros ] |> Helpers.runTransforms1 expr
