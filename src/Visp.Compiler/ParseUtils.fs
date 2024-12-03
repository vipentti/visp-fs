// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.ParseUtils

open FSharp.Text.Lexing
open Visp.Compiler
open Visp.Compiler.SyntaxParser
open Visp.Compiler.LexHelpers
open Visp.Compiler.Syntax.Macros
open System.Collections.Generic

type TokenWithPos =
    { token: token
      startPos: Position
      endPos: Position }

type NestedTokenizer =
    { mutable index: int
      tokens: IReadOnlyList<TokenWithPos>
      origStartPos: Position
      origEndPos: Position }

    member t.ReadNext() =
        if t.index < t.tokens.Count then
            let next = Some(t.tokens[t.index])
            t.index <- t.index + 1
            next
        else
            None

let rec mkTokenizerWithArgs args =
    let inline read_next args buf =
        match args.mode with
        | LexMode.Default -> Lexer.token args false buf
        | LexMode.TokenStream _ -> Lexer.tokenStream args false buf

    let handle_next (args: LexArgs) next =
        let mutable actual = next

        match next with
        | QUOTE_SYM -> args.mode <- LexMode.TokenStream TokenStreamMode.QuoteSym
        | QUOTE_KW -> // args.mode <- LexMode.TokenStream TokenStreamMode.Quote
            args.Nested <| LexMode.TokenStream TokenStreamMode.Quote
        | QUASIQUOTE_KW -> args.Nested <| LexMode.TokenStream TokenStreamMode.Quasiquote
        | SYNTAX_MACRO -> args.Nested <| LexMode.TokenStream TokenStreamMode.SyntaxMacroStart
        | SYMBOL s when args.mode.IsSyntaxMacroStart ->
            args.mode <- LexMode.TokenStream TokenStreamMode.Macro
            macroTable.AddMacroName s
            ()
        | MACRO_NAME _ -> args.Nested <| LexMode.TokenStream TokenStreamMode.Macro

        | MEMBER ->
            if args.CurrentContext = LexContext.LParen || args.CurrentContext = LexContext.Type then
                args.PopContext()
                args.PushContext LexContext.Member

        | MATCH ->
            if args.CurrentContext = LexContext.LParen then
                args.PopContext()
                args.PushContext LexContext.Match

        | UNION
        | RECORD
        | TYPE ->
            if args.CurrentContext = LexContext.LParen then
                args.PopContext()
                args.PushContext LexContext.Type

        | OP_LESS -> args.NestAngle()
        | OP_GREATER -> args.UnnestAngle()

        | HASH_PAREN
        | HASH_BRACKET
        | BRACE_BAR
        | BRACKET_BAR
        | PAREN_BAR
        | DOT_BRACKET
        | LPAREN
        | LBRACE
        | LBRACKET
        | HASH_BRACE ->
            if next = LPAREN || next = HASH_PAREN then
                let ctx =
                    if args.CurrentContext = LexContext.Default then
                        LexContext.LParen
                    else
                        args.CurrentContext

                args.PushContext ctx

            args.NestIfNotDefault()

        | BAR_PAREN
        | BAR_BRACKET
        | BAR_BRACE
        | RPAREN
        | RBRACE
        | RBRACKET ->
            if next = RPAREN then
                args.PopContext()

            args.UnnestIfNotDefault()
        | _ -> ()

        if args.debugTokens then
            if next <> actual then
                eprintfn
                    "%A -> (%A) %A %i %i %A"
                    next
                    actual
                    args.mode
                    args.depth
                    args.ContextCount
                    args.CurrentContext
            else
                eprintfn
                    "%A %A %i %i %A"
                    next
                    args.mode
                    args.depth
                    args.ContextCount
                    args.CurrentContext

        actual

    let next_token = read_next args
    let handle_token = handle_next args

    let mutable token_list = []
    let mutable include_tokenizers: NestedTokenizer list = []

    let read_next_nested_token (buf: LexBuffer<_>) =
        match include_tokenizers with
        | [] -> None
        | it :: rest ->
            match it.ReadNext() with
            | None ->
                buf.StartPos <- it.origStartPos
                buf.EndPos <- it.origEndPos
                include_tokenizers <- rest
                None
            | Some(tok) ->
                buf.StartPos <- tok.startPos
                buf.EndPos <- tok.endPos
                Some(tok.token)

    let read_next_token_list_token buf =
        match token_list with
        | [] ->
            match next_token buf with
            | TOKENLIST toks ->
                match toks with
                | tok :: rest ->
                    token_list <- rest
                    tok
                | [] -> (failwith "empty TOKENLIST is not supported")
            | it -> it
        | tok :: rest ->
            token_list <- rest
            tok

    // Nested TOKENLISTS are not supported.
    let tokenizer buf =
        let token =
            match read_next_nested_token buf with
            | Some(tok) -> tok
            | None -> read_next_token_list_token buf

        match token with
        | INCLUDE ->
            let mutable includes = []

            let mutable loop = true

            while loop do
                let next = next_token buf

                match next with
                | STRING(it, _, _) ->
                    includes <- it :: includes
                    ()
                | _ -> ()

                loop <- next <> RPAREN
                ()

            loop <- true

            while loop do
                match includes with
                | [] -> loop <- false
                | it :: rest ->
                    let tokz = getNestedTokens buf it false
                    include_tokenizers <- tokz :: include_tokenizers
                    includes <- rest

                ()

            // Includes will not be part of the final parse result
            // Close the paren which was before the INCLUDE
            // NOTE: This assumes correct formatting.
            RPAREN
        | it -> handle_token it

    tokenizer

and private getNestedTokens origBuf filePath dbg =
    let rootFile = origBuf.EndPos.FileName

    let actualPath =
        System.IO.Path.GetDirectoryName rootFile
        |> System.IO.Path.GetFullPath
        |> fun (r) -> System.IO.Path.Combine(r, filePath)
        |> System.IO.Path.GetFullPath

    let (stream, reader, lexbuf) = UnicodeFileAsLexbuf(actualPath, None)
    use _ = stream
    use _ = reader

    let tokenizer = mkTokenizer dbg
    let arr = ResizeArray<TokenWithPos>()

    while not lexbuf.IsPastEndOfStream do
        let next = tokenizer lexbuf
        let sp = lexbuf.StartPos
        let ep = lexbuf.EndPos

        if next <> EOF then
            arr.Add({ token = next; startPos = sp; endPos = ep; })

    { tokens = arr; index = 0; origStartPos = origBuf.StartPos; origEndPos = origBuf.EndPos; }

and mkTokenizer dbg =
    mkTokenizerWithArgs
    <| { mkDefaultLextArgs () with
           debugTokens = dbg }

let parseStringToExpr fileName str =
    let lexbuf = LexBuffer<_>.FromString str
    lexbuf.EndPos <- Position.FirstLine fileName
    let tokenizer = mkTokenizer false

    try
        raw_expr tokenizer lexbuf
    with :? ParseHelpers.SyntaxError as syn ->
        outputSyntaxError syn
        reraise ()

let debugTokenOutput args (lexbuf: LexBuffer<_>) =
    let debugToken =
        function
        | STRING(text, kind, cont) ->
            Syntax.StringWriterUtils.writeDebugStringType "STRING" text kind cont
        | KEYWORD_STRING(lhs, rhs) -> sprintf "KEYWORD_STRING (\"%s\", \"%s\")" lhs rhs
        | it -> sprintf "%A" it

    seq {
        let tokenizer = mkTokenizerWithArgs args

        while not lexbuf.IsPastEndOfStream do
            let next = tokenizer lexbuf

            yield
                sprintf
                    "%s %A %i %i %A"
                    (debugToken next)
                    args.mode
                    args.depth
                    args.ContextCount
                    args.CurrentContext
    }
