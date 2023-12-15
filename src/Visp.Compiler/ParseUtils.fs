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
    let tokens args buf =
        let next =
            match args.mode with
            | LexMode.Default -> Lexer.token args false buf
            | LexMode.TokenStream _ -> Lexer.tokenStream args false buf

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
            if args.CurrentContext = LexContext.LParen then
                args.PopContext()
                args.PushContext LexContext.Member

        | MATCH ->
            if args.CurrentContext = LexContext.LParen then
                args.PopContext()
                args.PushContext LexContext.Match

        | HASH_PAREN
        | HASH_BRACKET
        | BRACE_BAR
        | BRACKET_BAR
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

        | BAR_BRACKET
        | BAR_BRACE
        | RPAREN
        | RBRACE
        | RBRACKET ->
            if next = RPAREN then
                args.PopContext()

            args.UnnestIfNotDefault()
        | _ -> ()

        // eprintfn "%A %A %i %i %A" next args.mode args.depth args.ContextCount args.CurrentContext

        next

    // eprintfn ""

    tokens args

let mkTokenizer () =
    mkTokenizerWithArgs <| mkDefaultLextArgs ()

let parseStringToExpr fileName str =
    let lexbuf = LexBuffer<_>.FromString str
    lexbuf.EndPos <- Position.FirstLine fileName
    let tokenizer = mkTokenizer ()

    try
        raw_expr tokenizer lexbuf
    with :? ParseHelpers.SyntaxError as syn ->
        outputSyntaxError syn
        reraise ()
