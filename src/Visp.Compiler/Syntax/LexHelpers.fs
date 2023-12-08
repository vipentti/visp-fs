// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.LexHelpers

open Visp.Compiler.Text
open ParseHelpers
open SyntaxParser
open System
open Visp.Compiler.Syntax.Macros
open Visp.Common
open System.IO
open FSharp.Text.Lexing

[<RequireQualifiedAccess>]
type TokenStreamMode =
    | Quote
    | QuoteSym
    | Quasiquote
    | QuasiquoteSym
    | SyntaxMacroStart
    | Macro

    member this.IsQuoteMode =
        match this with
        | Quote
        | QuoteSym -> true
        | _ -> false

    member this.IsMacroMode =
        match this with
        | Macro -> true
        | SyntaxMacroStart -> true
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

    member this.IsSyntaxMacroStart =
        match this with
        | TokenStream t -> t = TokenStreamMode.SyntaxMacroStart
        | _ -> false

    member this.IsQuoteMode =
        match this with
        | TokenStream t -> t.IsQuoteMode
        | _ -> false

type LexArgs =
    { diagnosticsLogger: DiagnosticsLogger.DiagnosticsLogger
      mutable mode: LexMode
      mutable stringNest: LexerInterpolatedStringNesting
      mutable interpolationDelimiterLength: int
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

let mkDefaultLextArgs () =
    { diagnosticsLogger = DiagnosticsLogger.DiagnosticsThreadStatics.DiagnosticsLogger
      mode = LexMode.Default
      depth = 0
      interpolationDelimiterLength = 0
      stringNest = [] }

let mkTokenStreamArgs () =
    { mkDefaultLextArgs () with
        mode = LexMode.TokenStream(TokenStreamMode.Macro) }

type StringBuffer = Text.StringBuilder

[<Flags>]
type LexerStringFinisherContext =
    | InterpolatedPart = 1
    | Verbatim = 2
    | TripleQuote = 4

let addUnicodeString (buf: StringBuffer) (s: string) =
    buf.Append(s) |> ignore
    ()

let addUnicodeChar (buf: StringBuffer) (ch: char) =
    buf.Append(ch) |> ignore
    ()

let lastNewline (buf: StringBuffer) =
    let len = buf.Length
    0

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
      ("members", MEMBERS)
      ("memberfn", MEMBERFN)
      ("module", MODULE)
      ("mut", MUT)
      ("new", NEW)
      ("open", OPEN)
      ("override", OVERRIDE)
      ("quasiquote", QUASIQUOTE_KW)
      ("quote", QUOTE_KW)
      ("rec", REC)
      ("record", RECORD)
      ("rinit", RINIT)
      ("require", REQUIRE)
      ("set!", SET)
      ("seq", SEQ)
      ("splice-unquote", SPLICE_UNQUOTE_KW)
      ("syntax-macro", SYNTAX_MACRO)
      ("type", TYPE)
      ("union", UNION)
      ("unquote", UNQUOTE_KW)
      ("when", WHEN)
      ("while", WHILE)
      ("yield", YIELD false)
      ("yield!", YIELD true) ]

let keywordToTokenMap = keywordTokenList |> Map.ofList

let tryGetKeyword w = keywordToTokenMap.TryFind w

let alwaysSymbol (s: string) = SYMBOL(s)

let escape c =
    match c with
    | '\\' -> '\\'
    | '\'' -> '\''
    | 'a' -> char 7
    | 'f' -> char 12
    | 'v' -> char 11
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'b' -> '\b'
    | 'r' -> '\r'
    | c -> c

let unescape c =
    match c with
    | '\'' -> "\\'"
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    | '\b' -> "\\b"
    | '"' -> "\\\""
    | it when it = char 7 -> "\\a"
    | it when it = char 12 -> "\\f"
    | it when it = char 11 -> "\\v"
    | it -> string it

let isLetter (ch: char) = System.Char.IsLetter(ch)

let specialSymbol (s: string) =
    match s with
    | "." -> Some(DOT)
    | ".." -> Some(DOTDOT)
    | ".+" -> Some(DOT_PLUS)
    // TODO: Better conditions?
    | it when it.Length > 1 && it[0] = '+' && isLetter it[1] -> Some(PROP_PLUS s)
    | it when it.Length > 1 && it[0] = '.' && isLetter it[1] -> Some(DOT_METHOD s)
    | it when it.Length > 1 && it[0] = '-' && isLetter it[1] -> Some(APPLY_METHOD s)
    | _ -> None

let symbolOrKeyword (s: string) =
    match tryGetKeyword s with
    | Some(tok) -> tok
    | None ->
        if macroTable.IsMacro(s) then
            MACRO_NAME(s)
        else if s.EndsWith("!!") then
            MACRO_NAME(s.TrimEnd('!'))
        else
            match specialSymbol s with
            | Some(it) -> it
            | None -> SYMBOL s

let outputSyntaxError (syn: SyntaxError) =
    match syn.Data0 with
    | :? FSharp.Text.Parsing.ParseErrorContext<SyntaxParser.token> as ctx ->
        let (startPos, _) = ctx.ParseState.ResultRange
        eprintfn "ReduceTokens: %A" ctx.ReduceTokens
        eprintfn "ReducibleProductions: %A" ctx.ReducibleProductions
        eprintfn "ShiftTokens: %A" ctx.ShiftTokens
        eprintfn "StateStack: %A" ctx.StateStack
        eprintfn "%s(%i,%i)" (startPos.FileName) (startPos.Line) (startPos.Column)
        eprintfn "Token: %A" ctx.CurrentToken
        eprintfn "Message: %A" ctx.Message
    | _ -> ()

type ParseErrorState =
    { ReduceTokens: int list
      ReducibleProductions: list<list<int>>
      ShiftTokens: list<int>
      StateStack: list<int>
      FileName: string
      Line: int
      Column: int
      CurrentToken: token option
      Message: string }

    override ctx.ToString() =
        let sb = PooledStringBuilder.Get()
        use w = new StringWriter(sb)

        fprintfn w "ReduceTokens: %A" ctx.ReduceTokens
        fprintfn w "ReducibleProductions: %A" ctx.ReducibleProductions
        fprintfn w "ShiftTokens: %A" ctx.ShiftTokens
        fprintfn w "StateStack: %A" ctx.StateStack
        fprintfn w "%s(%i,%i)" (ctx.FileName) (ctx.Line) (ctx.Column)
        fprintfn w "Token: %A" ctx.CurrentToken
        fprintfn w "Message: %A" ctx.Message

        PooledStringBuilder.ToStringAndReturn(sb)

// exception ParseError of state: ParseErrorState * range: Text.range
type ParseError(state: ParseErrorState, syn: SyntaxError, range: Text.range) =
    inherit Exception(sprintf "%A" state, syn)

    member _.State = state
    member _.Syn = syn
    member _.Range = range

let syntaxErrorToParseError (syn: SyntaxError) =
    match syn.Data0 with
    | :? FSharp.Text.Parsing.ParseErrorContext<SyntaxParser.token> as ctx ->
        let (startPos, _) = ctx.ParseState.ResultRange

        let r =
            new ParseError(
                { ReduceTokens = ctx.ReduceTokens
                  ReducibleProductions = ctx.ReducibleProductions
                  ShiftTokens = ctx.ShiftTokens
                  StateStack = ctx.StateStack
                  CurrentToken = ctx.CurrentToken
                  Message = ctx.Message
                  FileName = startPos.FileName
                  Line = startPos.Line
                  Column = startPos.Column },
                syn,
                syn.range
            )

        r
    | _ -> failwith "not a valid syntax error"
