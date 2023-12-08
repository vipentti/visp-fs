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

        // eprintfn "%A %A %i" next args.mode args.depth

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
        | HASH_PAREN
        | HASH_BRACKET
        | LPAREN
        | LBRACE
        | LBRACKET
        | HASH_BRACE -> args.NestIfNotDefault()
        | RPAREN
        | RBRACE
        | RBRACKET -> args.UnnestIfNotDefault()
        | _ -> ()

        next

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
