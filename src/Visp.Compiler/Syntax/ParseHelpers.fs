// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.ParseHelpers

open System
open FSharp.Text.Lexing
open Visp.Compiler.Text.Position
open Visp.Compiler.Text.Range
open Visp.Compiler.Text.FileIndex
open FSharp.Text.Parsing

//------------------------------------------------------------------------
// Parsing: Error recovery exception for fsyacc
//------------------------------------------------------------------------

/// The error raised by the parse_error_rich function, which is called by the parser engine
/// when a syntax error occurs. The first object is the ParseErrorContext which contains a dump of
/// information about the grammar at the point where the error occurred, e.g. what tokens
/// are valid to shift next at that point in the grammar. This information is processed in CompileOps.fs.
[<NoEquality; NoComparison>]
exception SyntaxError of obj (* ParseErrorContext<_> *) * range: Text.range

//------------------------------------------------------------------------
// Parsing: getting positions from the lexer
//------------------------------------------------------------------------

let posOfLexPosition (p: Position) = mkPos p.Line p.Column

let mkSynRange (p1: Position) (p2: Position) =
    let filename =
        match p1.FileName with
        | null -> "unknown"
        | it -> it

    mkFileIndexRange (fileIndexOfFile filename) (posOfLexPosition p1) (posOfLexPosition p2)

type LexBuffer<'Char> with

    member lexbuf.LexemeRange = mkSynRange lexbuf.StartPos lexbuf.EndPos

/// Get the range corresponding to the result of a grammar rule while it is being reduced
let lhs (parseState: IParseState) =
    let (lhs, rhs) = parseState.ResultRange
    mkSynRange lhs rhs

/// Get the range covering two of the r.h.s. symbols of a grammar rule while it is being reduced
let rhs2 (parseState: IParseState) i j =
    let p1 = parseState.InputStartPosition i
    let p2 = parseState.InputEndPosition j
    mkSynRange p1 p2

/// Get the range corresponding to one of the r.h.s. symbols of a grammar rule while it is being reduced
let rhs parseState i = rhs2 parseState i i

let private matchSpan (lhs: ReadOnlySpan<char>) (rhs: string) =
    let rhs = rhs.AsSpan()
    MemoryExtensions.Equals(lhs, rhs, StringComparison.Ordinal)

let private parseCharSpan (span: ReadOnlySpan<char>) =
    match span with
    | it when it.Length = 1 -> it[0]
    | it when it.Length > 0 && (it[0] = 'u' || it[0] = 'U') ->
        System.Int32.Parse(it.Slice(1), Globalization.NumberStyles.HexNumber) |> char
    | it when matchSpan it "newline" || matchSpan it "linefeed" || matchSpan it "lf" -> '\n'
    | it when matchSpan it "cr" || matchSpan it "return" -> '\r'
    | it when matchSpan it "nul" || matchSpan it "null" -> '\u0000'
    | it when matchSpan it "tab" -> '\t'
    | it when matchSpan it "space" -> ' '
    | it when matchSpan it "backspace" -> (char 8)
    | it when matchSpan it "\\\\" || matchSpan it "\\" -> '\\'
    | it -> failwithf "unsupported char '%s'" (it.ToString())

let parseChar (text: string) =
    // Remove #\ from the beginning
    let span = text.AsSpan().Slice(2)
    parseCharSpan span

let charToParseable (ch: char) =
    match ch with
    | '\n' -> "#\\lf"
    | '\r' -> "#\\cr"
    | '\t' -> "#\\tab"
    | ' ' -> "#\\space"
    | '\\' -> "#\\\\"
    | it when it = (char 8) -> "#\\backspace"
    | it when it = '\u0000' -> "#\\nul"
    | it -> $"#\\{it}"

//------------------------------------------------------------------------
// Parsing: continuations for whitespace tokens
//------------------------------------------------------------------------

[<RequireQualifiedAccess>]
type LexerStringStyle =
    | Verbatim
    | TripleQuote
    | SingleQuote
    | ExtendedInterpolated

[<RequireQualifiedAccess; Struct>]
type LexerStringKind =
    { IsByteString: bool
      IsInterpolated: bool
      IsInterpolatedFirst: bool }

    static member String =
        { IsByteString = false
          IsInterpolated = false
          IsInterpolatedFirst = false }

    static member ByteString =
        { IsByteString = true
          IsInterpolated = false
          IsInterpolatedFirst = false }

    static member InterpolatedStringFirst =
        { IsByteString = false
          IsInterpolated = true
          IsInterpolatedFirst = true }

    static member InterpolatedStringPart =
        { IsByteString = false
          IsInterpolated = true
          IsInterpolatedFirst = false }


/// Represents the degree of nesting of '{..}' and the style of the string to continue afterwards, in an interpolation fill.
/// Nesting counters and styles of outer interpolating strings are pushed on this stack.
type LexerInterpolatedStringNesting = (int * LexerStringStyle * int * Text.range) list

[<RequireQualifiedAccess>]
type LexerContinuation =
    | Token of nesting: LexerInterpolatedStringNesting
    | String of
        nesting: LexerInterpolatedStringNesting *
        style: LexerStringStyle *
        kind: LexerStringKind *
        delimLen: int *
        range: Text.range

    static member Default = Token([])

    member x.LexerInterpStringNesting =
        match x with
        | Token(nesting = nesting)
        | String(nesting = nesting) -> nesting

and LexCont = LexerContinuation
