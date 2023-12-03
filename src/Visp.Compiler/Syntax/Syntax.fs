// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Compiler.Syntax

open Visp.Compiler.Writer
open Visp.Compiler.Text
open System.Diagnostics

[<Struct; NoEquality; NoComparison; DebuggerDisplay("{idText}({idRange})")>]
type Ident(text: string, range: range) =
    member _.idText = text
    member _.idRange = range

    member this.trimStart(ch: char) =
        let newText = this.idText
        Ident(newText.TrimStart(ch), this.idRange)

    override _.ToString() = text

type SynSymbol =
    | SynSymbol of ident: Ident

    member this.trimStart(ch: char) =
        let (SynSymbol id) = this
        let newId = id.trimStart ch
        SynSymbol newId

    member this.Range =
        let (SynSymbol(id)) = this
        id.idRange

type SynKeyword = SynKeyword of ident: Ident

type SynKeyword with

    member this.Range =
        let (SynKeyword(id)) = this
        id.idRange

type LongIdent = Ident list

type SynLongIdent = SynLongIdent of id: LongIdent * dotRanges: range list * trivia: unit option list

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynType = Ident of Ident

[<Struct; RequireQualifiedAccess>]
type SynStringKind =
    | Regular
    | Verbatim
    | TripleQuote

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynConst =
    | Unit
    | Nil
    | Bool of bool
    //| SByte of sbyte
    //| Byte of byte
    //| Int16 of int16
    //| UInt16 of uint16
    //| UInt32 of uint32
    //| Int64 of int64
    //| UInt64 of uint64
    //| IntPtr of int64
    //| UIntPtr of uint64
    //| Single of single
    //| Double of double
    | Int64 of int64
    | Int32 of int32
    | Char of char
    | Decimal of System.Decimal
    | String of text: string * synStringKind: SynStringKind * range: range

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynName =
    // Argument with explicit type
    | Typed of name: SynSymbol * argtype: SynType * range: range
    // Argument with inferred type
    | Inferred of name: SynSymbol * range: range
//[<NoEquality; NoComparison; RequireQualifiedAccess>]

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynOpenDeclTarget =
    | ModuleOrNamespace of longId: SynLongIdent * range: range
    | Type of typeName: SynType * range: range

    member this.Range =
        match this with
        | ModuleOrNamespace(range = m) -> m
        | Type(range = m) -> m

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynExpr =
    // special operator application
    | Op of SynOp
    | MacroDef of name: SynSymbol * args: SynArg list * body: SynExpr list * range: range
    | MacroCall of name: SynSymbol * args: SynValue list * range: range
    | SyntaxMacroCall of name: SynSymbol * args: SynMacroBody list * range : range
    | SyntaxMacro of macro: SynMacro
    | FunctionDef of
        name: SynSymbol *
        isInline: bool *
        args: SynArg list *
        body: SynExpr list *
        range: range
    | FunctionCall of name: SynExpr * args: SynExpr list * range: range
    | LambdaShort of expr: SynExpr * range: range
    | LambdaDef of SynLambda
    | Symbol of SynSymbol
    | Keyword of SynKeyword
    | Cons of lhs: SynExpr * rhs: SynExpr * range: range
    | Concat of lhs: SynExpr * rhs: SynExpr * range: range
    | Const of constant: SynConst * range: range
    | Quote of shorthand: bool * expr: SynQuoted * range: range
    | Quasiquote of shorthand: bool * expr: SynQuasiquote * range: range
    | Begin of exprs: SynExpr list * range: range
    | New of typ: SynType * args: SynExpr list * range: range
    | SimpleLet of name: SynName * value: SynExpr * range: range
    | SimpleMut of name: SynName * value: SynExpr * range: range
    | LetStar of bindings: SynBinding list * body: SynExpr list * range: range
    | Set of name: SynExpr * value: SynExpr * range: range
    | If of cond: SynExpr * main: SynExpr * alt: SynExpr option * range: range
    | While of cond: SynExpr * body: SynExpr list * range: range
    | Pair of lhs: SynExpr * rhs: SynExpr * range: range
    | Tuple of exprs: SynExpr list * range: range
    | FsArray of exprs: SynExpr list * range: range
    | FsMap of exprs: SynExpr list * range: range
    | FsSet of exprs: SynExpr list * range: range
    | FsVec of exprs: SynExpr list * range: range
    | List of exprs: SynExpr list * range: range
    | Vector of exprs: SynExpr list * range: range
    | HashMap of exprs: SynExpr list * range: range
    | HashSet of exprs: SynExpr list * range: range
    | DotIndex of target: SynExpr * index: SynExpr * range: range
    | DotProperty of target: SynExpr * property: SynSymbol * range: range
    | DotMethod of target: SynExpr * method: SynSymbol * args: SynExpr list * range: range
    | Atom of expr: SynExpr * range: range
    | Deref of short: bool * expr: SynExpr * range: range
    | Type of name: SynSymbol * args: SynName list * members: SynTypeMember list * range: range
    | ThreadFirst of exprs: SynExpr list * range: range
    | ThreadLast of exprs: SynThreadable list * range: range
    | RangeExpr of first: SynExpr * step: SynExpr option * last: SynExpr * range: range
    | ForIn of name: SynName * binding: SynExpr * body: SynExpr list * range: range
    | Match of expr: SynExpr * pats: SynMatch list * range: range

    member this.writeTo(writer: CustomFileWriter) = ()

    member this.Range =
        match this with
        | Op op -> op.Range
        | Symbol(SynSymbol(id)) -> id.idRange
        | Keyword(SynKeyword(id)) -> id.idRange

        | Cons(range = r)
        | Concat(range = r)
        | Match(range = r)
        | Tuple(range = r)
        | Pair(range = r)
        | RangeExpr(range = r)
        | LambdaShort(range = r)
        | ForIn(range = r)
        | MacroDef(range = r)
        | MacroCall(range = r)
        | FunctionDef(range = r)
        | FunctionCall(range = r)
        | SyntaxMacroCall(range = r)
        | SyntaxMacro(SynMacro(range = r))
        | LambdaDef(SynLambda(range = r))
        | Const(range = r)
        | Quote(range = r)
        | Quasiquote(range = r)
        | Begin(range = r)
        | New(range = r)
        | SimpleLet(range = r)
        | SimpleMut(range = r)
        | LetStar(range = r)
        | Set(range = r)
        | If(range = r)
        | While(range = r)
        | FsMap(range = r)
        | FsArray(range = r)
        | FsSet(range = r)
        | FsVec(range = r)
        | List(range = r)
        | Vector(range = r)
        | HashMap(range = r)
        | HashSet(range = r)
        | DotIndex(range = r)
        | DotProperty(range = r)
        | DotMethod(range = r)
        | Atom(range = r)
        | Type(range = r)
        | ThreadFirst(range = r)
        | ThreadLast(range = r)
        | Deref(range = r) -> r


    member this.withRangeOf(r: range) =
        match this with
        | _ -> this

and SynBinding = SynBinding of name: SynName * expr: SynExpr * range: range

and SynMacro = SynMacro of name: SynSymbol * cases: SynMacroCase list * range: range

and SynMacroCase = SynMacroCase of pat: SynMacroPat * body: SynMacroBody * range: range

and [<RequireQualifiedAccess>] SynMacroPat =
    | Const of value: SynConst * range: range
    | List of pats: SynMacroPat list * range: range
    | Symbol of name: SynSymbol * range: range
    | Ellipsis of range: range
    | Discard of range: range

and [<NoEquality; NoComparison; RequireQualifiedAccess>] SynMacroBody =
    | List of exprs: SynMacroBody list * range: range
    | Vector of exprs: SynMacroBody list * range: range
    | HashMap of exprs: SynMacroBody list * range: range
    | HashSet of exprs: SynMacroBody list * range: range
    | Const of constant: SynConst * range: range
    | Symbol of value: SynSymbol
    | Keyword of value: SynKeyword
    | Ellipsis of range: range
    | Discard of range: range

and [<RequireQualifiedAccess>] SynMatch =
    | SynMatch of
        pattern: SynMatchPattern *
        cond: SynExpr option *
        body: SynExpr list *
        range: range

and [<RequireQualifiedAccess>] SynThreadable =
    | Expr of value: SynExpr * range: range
    | Index of expr: SynExpr * range: range
    | Property of symbol: SynSymbol * range: range
    | Method of symbol: SynSymbol * range: range

and [<RequireQualifiedAccess>] SynMatchPattern =
    | Const of value: SynConst * range: range
    | Tuple of pats: SynMatchPattern list * range: range
    | List of pats: SynMatchPattern list * range: range
    | Symbol of name: SynSymbol * range: range
    | Discard of range: range
    // Artificial only exists during parsing
    | CommaOrDot of range: range

and [<RequireQualifiedAccess>] SynTypeMember =
    | Let of name: SynName * value: SynExpr * range: range
    | Member of name: SynSymbol * value: SynExpr * range: range
    | MemberFn of name: SynSymbol * args: SynArg list * body: SynExpr list * range: range
    | OverrideMember of name: SynSymbol * value: SynExpr * range: range
    | OverrideFn of name: SynSymbol * args: SynArg list * body: SynExpr list * range: range

and [<RequireQualifiedAccess>] SynDirective = Open of path: SynSymbol * range: range

and [<NoEquality; NoComparison; RequireQualifiedAccess>] SynValue =
    | EmptyList of range: range
    | List of exprs: SynValue list * range: range
    | Vector of exprs: SynValue list * range: range
    | HashMap of exprs: SynValue list * range: range
    | HashSet of exprs: SynValue list * range: range
    | Const of constant: SynConst * range: range
    | Symbol of value: SynSymbol
    | Keyword of value: SynKeyword

and [<RequireQualifiedAccess>] SynQuoted =
    | EmptyList of range: range
    | List of exprs: SynQuoted list * range: range
    | Vector of exprs: SynQuoted list * range: range
    | HashMap of exprs: SynQuoted list * range: range
    | HashSet of exprs: SynQuoted list * range: range
    | Const of constant: SynConst * range: range
    | Symbol of value: SynSymbol
    | Keyword of value: SynKeyword

and [<RequireQualifiedAccess>] SynQuasiquote =
    | List of exprs: SynQuasiquote list * range: range
    | Vector of exprs: SynQuasiquote list * range: range
    | Const of constant: SynConst * range: range
    | Symbol of value: SynSymbol
    | Keyword of value: SynKeyword
    | Unquote of expr: SynExpr * range: range
    | SpliceUnquote of expr: SynExpr * range: range
    | Op of SynOp

and [<RequireQualifiedAccess>] SynOp =
    | Plus of args: SynExpr list * range: range
    | Mult of args: SynExpr list * range: range
    | Div of args: SynExpr list * range: range
    | Minus of args: SynExpr list * range: range

    member this.Range =
        match this with
        | Plus(range = r)
        | Mult(range = r)
        | Div(range = r)
        | Minus(range = r) -> r

and SynLambda =
    | SynLambda of args: SynArg list * body: SynExpr list * range: range

    member this.Range =
        let (SynLambda(_, _, rng)) = this
        rng

and SynArg =
    // Argument with explicit type
    | TypedArg of name: SynSymbol * argtype: SynType * range: range
    // Argument with inferred type
    | InferredArg of name: SynSymbol * range: range

and VispProgram = VispProgram of directives: SynDirective list * exprs: SynExpr list

module Syntax =
    [<Literal>]
    let parserRecovery = "__PARSER_RECOVERY__"

    let parserRecoveryConst r =
        SynConst.String(parserRecovery, SynStringKind.Regular, r)

    let parserRecoveryExpr r =
        SynExpr.Const(SynConst.String(parserRecovery, SynStringKind.Regular, r), r)

    let parserRecoverySymbol r = SynSymbol(Ident(parserRecovery, r))

    let parserRecoveryName r =
        SynName.Inferred(parserRecoverySymbol r, r)

    let parserRecoveryMatch r =
        SynMatch.SynMatch(SynMatchPattern.Const(parserRecoveryConst r, r), None, [], r)

    let textOfIdent (id: Ident) = id.idText
    let rangeOfIdent (id: Ident) = id.idRange

    let textOfSymbol (id: SynSymbol) =
        let (SynSymbol s) = id
        textOfIdent s

    let rangeOfSymbol (SynSymbol id) = id.idRange

    let textOfKeyword (id: SynKeyword) =
        let (SynKeyword s) = id
        textOfIdent s

    let textOfName =
        function
        | SynName.Inferred(it, _) -> textOfSymbol it
        | SynName.Typed(it, _, _) -> textOfSymbol it

    let rangeOfName =
        function
        | SynName.Inferred(it, _) -> rangeOfSymbol it
        | SynName.Typed(it, _, _) -> rangeOfSymbol it

    let textOfArg =
        function
        | SynArg.InferredArg(it, _) -> textOfSymbol it
        | SynArg.TypedArg(it, _, _) -> textOfSymbol it

    let rangeOfArg =
        function
        | SynArg.InferredArg(it, _) -> rangeOfSymbol it
        | SynArg.TypedArg(it, _, _) -> rangeOfSymbol it

    let mkFunctionCall sym ex range = SynExpr.FunctionCall(sym, ex, range)

    let mkCons lhs rhs range = SynExpr.Cons(lhs, rhs, range)

    let mkConcat lhs rhs range = SynExpr.Concat(lhs, rhs, range)

    let mkEmptyList range =
        SynExpr.Quote(false, SynQuoted.EmptyList range, range)

    let mkSynString s range =
        SynExpr.Const(SynConst.String(s, SynStringKind.Regular, range), range)

    let mkSynSymbol s range = (SynSymbol(Ident(s, range)))
    let mkSynExprSymbol s range = SynExpr.Symbol(mkSynSymbol s range)
    let mkSynSymbolExpr s range = SynExpr.Symbol(mkSynSymbol s range)

    let mkInferredName n range =
        SynName.Inferred(mkSynSymbol n range, range)

    let mkValue v r =
        mkFunctionCall (mkSynSymbolExpr "Value.from" r) [ v ] r

    let mkUnwrapList v r =
        mkFunctionCall (mkSynSymbolExpr "unwrapList" r) [ v ] r

    let mkInferredArg s range =
        SynArg.InferredArg(mkSynSymbol s range, range)

    let withoutCommaOrDots (l: SynMatchPattern list) =
        List.filter
            (function
            | SynMatchPattern.CommaOrDot _ -> false
            | _ -> true)
            l


[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynModuleDecl =
    | ModuleAbbrev of ident: Ident * longId: LongIdent * range: range
    | NestedModule of ident: SynSymbol * decls: SynModuleDecl list * range: range
    // isContinuing: bool *
    // moduleInfo: SynComponentInfo *
    // isRecursive: bool *
    // trivia: SynModuleDeclNestedModuleTrivia

    // | Let of isRecursive: bool * bindings: SynBinding list * range: range

    | Expr of expr: SynExpr * range: range

    // | Types of typeDefns: SynTypeDefn list * range: range

    // | Exception of exnDefn: SynExceptionDefn * range: range


    | Open of target: SynSymbol * range: range

    // | Attributes of attributes: SynAttributes * range: range

    // | HashDirective of hashDirective: ParsedHashDirective * range: range

    // | NamespaceFragment of fragment: SynModuleOrNamespace

    member d.Range =
        match d with
        | SynModuleDecl.ModuleAbbrev(range = m)
        | SynModuleDecl.NestedModule(range = m)
        | SynModuleDecl.Expr(range = m)
        | SynModuleDecl.Open(range = m) -> m

// | SynModuleDecl.Let (range = m)
// | SynModuleDecl.Expr (range = m)
// | SynModuleDecl.Types (range = m)
// | SynModuleDecl.Exception (range = m)
// | SynModuleDecl.Open (range = m)
// | SynModuleDecl.HashDirective (range = m)
// | SynModuleDecl.NamespaceFragment (SynModuleOrNamespace (range = m))
// | SynModuleDecl.Attributes (range = m) -> m
[<NoEquality; NoComparison; RequireQualifiedAccess>]
type ParsedFileFragment =
    | AnonModule of decls: SynModuleDecl list * range: range

    member d.Range =
        match d with
        | ParsedFileFragment.AnonModule(range = m) -> m

//| NamedModule of namedModule: SynModuleOrNamespace

[<NoEquality; NoComparison>]
type ParsedFile = ParsedFile of fragments: ParsedFileFragment list

[<NoEquality; NoComparison>]
type QualifiedNameOfFile =
    | QualifiedNameOfFile of Ident

    member x.Text = (let (QualifiedNameOfFile t) = x in t.idText)

    member x.Id = (let (QualifiedNameOfFile t) = x in t)

    member x.Range = (let (QualifiedNameOfFile t) = x in t.idRange)


module Patterns =
    let (|IdentWith|) (it: Ident) = (it.idText)

    let (|SymbolWith|) (it: SynExpr) =
        match it with
        | SynExpr.Symbol(SynSymbol(id)) -> id.idText
        | _ -> ""


module Keywords =
    let keywords =
        [| "atom"
           "begin"
           "do"
           "fn"
           "for/in"
           "if"
           "let"
           "match"
           "member"
           "memberfn"
           "module"
           "mut"
           "new"
           "open"
           "quasiquote"
           "quote"
           "set!"
           "splice-unquote"
           "type"
           "unquote"
           "while" |]

    let commonWords = [| "fn"; "let"; "mut"; "set!" |]

    let specialWords = [| "false"; "nil"; "true"; "unit" |]

    let interop =
        [| "!arr"
           "!array"
           "!list"
           "!lst"
           "!map"
           "!r"
           "!range"
           "!set"
           "!t"
           "!tuple"
           "!vec"
           "!vector" |]

    let interopCompletions =
        [| "!array"; "!list"; "!map"; "!range"; "!set"; "!tuple"; "!vector" |]
