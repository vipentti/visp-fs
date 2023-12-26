// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.ParseUtils

open FSharp.Text.Lexing
open Visp.Compiler
open Visp.Compiler.SyntaxParser
open Visp.Compiler.LexHelpers
open Visp.Compiler.Syntax.Macros

let mkTokenizerWithArgs args =
    let inline read_next args buf =
        match args.mode with
        | LexMode.Default -> Lexer.token args false buf
        | LexMode.TokenStream _ -> Lexer.tokenStream args false buf

    let inline handle_next args next =
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
            eprintfn
                "%A %A %i %i %A"
                next
                args.mode
                args.depth
                args.ContextCount
                args.CurrentContext

        next

    let next_token = read_next args
    let handle_token = handle_next args

    let mutable token_list = []

    // Nested TOKENLISTS are not supported.
    let tokenizer buf =
        let token =
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

        handle_token token

    tokenizer

let mkTokenizer dbg =
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
