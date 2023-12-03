// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.LexHelpers

open Visp.Compiler.Text
open ParseHelpers
open SyntaxParser
open System

[<RequireQualifiedAccess>]
type TokenStreamMode =
    | Quote
    | QuoteSym
    | Quasiquote
    | QuasiquoteSym
    | Macro

    member this.IsQuoteMode =
        match this with
        | Quote
        | QuoteSym -> true
        | _ -> false

    member this.IsMacroMode =
        match this with
        | Macro -> true
        | _ -> false

    member this.IsQuasiquoteMode =
        match this with
        | Quasiquote
        | QuasiquoteSym -> true
        | _ -> false

[<RequireQualifiedAccess>]
type LexMode =
    | Default
    | TokenStream of TokenStreamMode

    member this.IsDefaultMode =
        match this with
        | Default -> true
        | _ -> false

    member this.IsQuasiquoteMode =
        match this with
        | TokenStream t -> t.IsQuasiquoteMode
        | _ -> false

    member this.IsMacroMode =
        match this with
        | TokenStream t -> t.IsMacroMode
        | _ -> false

    member this.IsQuoteMode =
        match this with
        | TokenStream t -> t.IsQuoteMode
        | _ -> false

type LexArgs =
    { mutable mode: LexMode
      mutable depth: int }

    member this.Nested m =
        this.mode <- m
        this.Nest()

    member this.Nest() = this.depth <- this.depth + 1

    member this.NestIfNotDefault() =
        if not this.mode.IsDefaultMode then
            this.Nest()

    member this.UnnestIfNotDefault() =
        if not this.mode.IsDefaultMode then
            if this.Unnest() <= 0 then
                this.Reset()

    member this.Unnest() =
        this.depth <- this.depth - 1
        this.depth

    member this.Reset() =
        this.mode <- LexMode.Default
        this.depth <- 0

let mkDefaultLextArgs () = { mode = LexMode.Default; depth = 0 }

type StringBuffer = Text.StringBuilder

[<Flags>]
type LexerStringFinisherContext =
    | InterpolatedPart = 1
    | Verbatim = 2
    | TripleQuote = 4

let addUnicodeString (buf: StringBuffer) (s: string) =
    buf.Append(s) |> ignore
    ()

type LexerStringFinisher =
    | LexerStringFinisher of
        (StringBuffer -> LexerStringKind -> LexerStringFinisherContext -> LexerContinuation -> token)

    member fin.Finish (buf: StringBuffer) kind context cont =
        let (LexerStringFinisher f) = fin
        f buf kind context cont

type LexerStringArgs = StringBuffer * LexerStringFinisher * range * LexerStringKind * LexArgs

let keywordTokenList =
    [ ("!arr", BANG_ARRAY)
      ("!array", BANG_ARRAY)
      ("!lst", BANG_LIST)
      ("!list", BANG_LIST)
      ("!map", BANG_MAP)
      ("!r", BANG_RANGE)
      ("!range", BANG_RANGE)
      ("!set", BANG_SET)
      ("!tuple", BANG_TUPLE)
      ("!vec", BANG_VEC)
      ("!vector", BANG_VEC)
      ("!t", BANG_TUPLE)
      ("atom", ATOM_KW)
      ("begin", BEGIN_KW)
      ("cons", CONS)
      ("concat", CONCAT)
      ("deref", DEREF_KW)
      ("do", DO_KW)
      ("fn", FN)
      ("fn*", FNSTAR)
      ("for/in", FOR_IN)
      ("->", THREAD_FIRST)
      ("->>", THREAD_LAST)
      ("if", IF_KW)
      ("inline", INLINE)
      ("let", LET)
      ("let*", LETSTAR)
      ("macro", MACRO)
      ("match", MATCH)
      ("member", MEMBER)
      ("memberfn", MEMBERFN)
      ("module", MODULE)
      ("mut", MUT)
      ("new", NEW)
      ("open", OPEN)
      ("override", OVERRIDE)
      ("quasiquote", QUASIQUOTE_KW)
      ("quote", QUOTE_KW)
      ("set!", SET)
      ("splice-unquote", SPLICE_UNQUOTE_KW)
      ("syntax-macro", SYNTAX_MACRO)
      ("type", TYPE)
      ("unquote", UNQUOTE_KW)
      ("when", WHEN)
      ("while", WHILE) ]

let keywordToTokenMap = keywordTokenList |> Map.ofList

let tryGetKeyword w = keywordToTokenMap.TryFind w
