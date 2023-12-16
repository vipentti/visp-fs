// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace rec Visp.Compiler.Syntax

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

    member this.TrimEnd(s: string) =
        let mutable newText = this.idText

        for i = (s.Length - 1) downto 0 do
            newText <- newText.TrimEnd(s[i])

        Ident(newText, this.idRange)

    override _.ToString() = text

type SynSymbol =
    | SynSymbol of ident: Ident

    member this.trimStart(ch: char) =
        let (SynSymbol id) = this
        let newId = id.trimStart ch
        SynSymbol newId

    member this.TrimEnd(ch: string) =
        let (SynSymbol id) = this
        let newId = id.TrimEnd ch
        SynSymbol newId

    member this.TextEquals(SynSymbol(other)) =
        System.String.Equals(this.Text, other.idText, System.StringComparison.Ordinal)

    member this.Text =
        let (SynSymbol(id)) = this
        id.idText

    member this.Range =
        let (SynSymbol(id)) = this
        id.idRange

type SynKeyword = SynKeyword of ident: Ident

type SynKeyword with

    member this.Text =
        let (SynKeyword(id)) = this
        id.idText

    member this.Range =
        let (SynKeyword(id)) = this
        id.idRange

type LongIdent = Ident list

type SynLongIdent = SynLongIdent of id: LongIdent * dotRanges: range list * trivia: unit option list

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynType =
    | Ident of Ident

    member this.Text =
        let (Ident id) = this
        id.idText

[<Struct; RequireQualifiedAccess>]
type SynStringKind =
    | Regular
    | Verbatim
    | TripleQuote
    | Interpolated of plain: int
    | InterpolatedTripleQuote of triple: int

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynConst =
    | Unit
    | Nil
    | Bool of bool
    | SByte of sbyte
    | Byte of byte
    | Int16 of int16
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    | IntPtr of int64
    | UIntPtr of uint64
    | Single of single
    | Double of double
    | Int64 of int64
    | Int32 of int32
    | Char of char
    | Decimal of System.Decimal
    | String of text: string * synStringKind: SynStringKind * range: range

type SynTyped = SynTyped of name: SynSymbol * argtype: SynType * range: range

[<NoComparison; RequireQualifiedAccess>]
type RecordLabelKind =
    | Default
    | Mutable

type RecordLabel =
    | RecordLabel of kind: RecordLabelKind * name: SynSymbol * argtype: SynType * range: range

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

[<System.Flags; RequireQualifiedAccess>]
type FunctionFlags =
    | None = 0
    | Inline = 1
    | Recursive = 2

[<System.Flags; RequireQualifiedAccess>]
type LetFlags =
    | None = 0
    | Use = 1
    | Mutable = 2
    | Bang = 4
    | And = 8

[<RequireQualifiedAccess>]
type DotMethodKind =
    | Tuple
    | Apply


[<RequireQualifiedAccess>]
type BeginKind =
    | Begin
    | Do

[<RequireQualifiedAccess>]
type CollectionKind =
    | Paren
    | Bracket
    | Brace
    | HashBrace
    | HashParen
    | HashBracket
    | BraceBar
    | FsList
    | FsArray
    | FsMap
    | FsSet
    | FsVec

[<NoEquality; NoComparison>]
type SynCollection<'T> =
    | SynCollection of kind: CollectionKind * exprs: 'T list * range: range

    member t.Range = let (SynCollection(range = it)) = t in it
    member t.Items = let (SynCollection(exprs = it)) = t in it

type SynExprs = SynCollection<SynExpr>

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynExpr =
    // special operator application
    | Op of SynOp
    | MacroDef of name: SynSymbol * args: SynArg list * body: SynExpr list * range: range
    | MacroCall of name: SynSymbol * args: SynValue list * range: range
    | SyntaxMacroCall of call: SynMacroCall
    | SyntaxMacro of macro: SynMacro
    | FunctionDef of
        name: SynSymbol *
        flags: FunctionFlags *
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
    | Literal of constant: SynConst * range: range
    | Quote of shorthand: bool * expr: SynQuoted * range: range
    | Quasiquote of shorthand: bool * expr: SynQuasiquote * range: range
    | Begin of exprs: SynExpr list * kind: BeginKind * range: range
    | New of typ: SynType * args: SynExpr list * range: range
    | LetOrUse of name: SynName * value: SynExpr * flags: LetFlags * range: range
    | LetStar of bindings: SynBinding list * body: SynExpr list * range: range
    | Set of name: SynExpr * value: SynExpr * range: range
    | If of cond: SynExpr * main: SynExpr * alt: SynExpr option * range: range
    | While of cond: SynExpr * body: SynExpr list * range: range
    | Pair of lhs: SynExpr * rhs: SynExpr * range: range
    | Tuple of exprs: SynExpr list * range: range
    | Collection of SynExprs
    | Computation of builder: SynSymbol * exprs: SynExpr list * range: range
    | FsSeq of exprs: SynExpr list * range: range
    | FsYield of expr: SynExpr * bang: bool * range: range
    | FsReturn of expr: SynExpr * bang: bool * range: range
    | FsDo of expr: SynExpr * bang: bool * range: range
    | DotIndex of target: SynExpr * index: SynExpr list * range: range
    | DotProperty of target: SynExpr * property: SynSymbol * range: range
    | DotMethod of
        target: SynExpr *
        method: SynSymbol *
        args: SynExpr list *
        kind: DotMethodKind *
        range: range
    | Atom of expr: SynExpr * range: range
    | Deref of short: bool * expr: SynExpr * range: range
    | Record of
        name: SynSymbol *
        labels: RecordLabel list *
        members: SynTypeMember list *
        attributes: SynAttributes *
        range: range
    | RecordInit of inits: SynInit list * range: range
    | Type of
        name: SynSymbol *
        args: SynName list *
        members: SynTypeMember list *
        attributes: SynAttributes *
        range: range
    | Union of
        name: SynSymbol *
        cases: UnionCase list *
        members: SynTypeMember list *
        attributes: SynAttributes *
        range: range
    | TypeAlias of name: SynSymbol * typ: SynType * range: range
    | ThreadFirst of exprs: SynExpr list * range: range
    | ThreadLast of exprs: SynThreadable list * range: range
    | RangeExpr of first: SynExpr * step: SynExpr option * last: SynExpr * range: range
    | ForIn of name: SynName * binding: SynExpr * body: SynExpr list * range: range
    | ForTo of
        name: SynName *
        start: SynExpr *
        finish: SynExpr *
        body: SynExpr list *
        down: bool *
        range: range
    | Match of expr: SynExpr * pats: SynMatch list * range: range

    member this.writeTo(writer: CustomFileWriter) = ()

    member this.Range =
        match this with
        | Op op -> op.Range
        | Collection op -> op.Range
        | Symbol(SynSymbol(id)) -> id.idRange
        | Keyword(SynKeyword(id)) -> id.idRange

        | Cons(range = r)
        | Concat(range = r)
        | Match(range = r)
        | Literal(range = r)
        | Tuple(range = r)
        | Pair(range = r)
        | RangeExpr(range = r)
        | LambdaShort(range = r)
        | ForIn(range = r)
        | ForTo(range = r)
        | MacroDef(range = r)
        | MacroCall(range = r)
        | Union(range = r)
        | FunctionDef(range = r)
        | Record(range = r)
        | RecordInit(range = r)
        | FunctionCall(range = r)
        | SyntaxMacroCall(SynMacroCall(range = r))
        | SyntaxMacro(SynMacro(range = r))
        | LambdaDef(SynLambda(range = r))
        | Const(range = r)
        | Quote(range = r)
        | Quasiquote(range = r)
        | Begin(range = r)
        | New(range = r)
        | Computation(range = r)
        | LetOrUse(range = r)
        | LetStar(range = r)
        | Set(range = r)
        | If(range = r)
        | While(range = r)
        | FsSeq(range = r)
        | FsYield(range = r)
        | FsDo(range = r)
        | FsReturn(range = r)
        | DotIndex(range = r)
        | DotProperty(range = r)
        | DotMethod(range = r)
        | Atom(range = r)
        | Type(range = r)
        | TypeAlias(range = r)
        | ThreadFirst(range = r)
        | ThreadLast(range = r)
        | Deref(range = r) -> r


    member this.withRangeOf(r: range) =
        match this with
        | _ -> this

and [<NoEquality; NoComparison; RequireQualifiedAccess>] SynInterpolatedStringPart =
    | String of value: string * range: range
    | FillExpr of fillExpr: SynExpr * qualifiers: Ident option

and [<NoEquality; NoComparison; RequireQualifiedAccess>] SynAttribute =
    { TypeName: SynType
      ArgExpr: SynExpr
      //   Target: Ident option
      //   AppliesToGetterAndSetter: bool
      Range: range }

and [<RequireQualifiedAccess>] SynAttributeList =
    { Attributes: SynAttribute list
      Range: range }

and SynAttributes = SynAttributeList list

and SynInit = SynInit of name: SynSymbol * expr: SynExpr * range: range

and SynBinding = SynBinding of name: SynName * expr: SynExpr * range: range

and SynMacro = SynMacro of name: SynSymbol * cases: SynMacroCase list * range: range

and SynMacroCall =
    | SynMacroCall of name: SynSymbol * args: SynMacroBody list * range: range

    member this.Name = let (SynMacroCall(name = n)) = this in n
    member this.NameText = let (SynMacroCall(name = n)) = this in n.Text

and SynMacroCase = SynMacroCase of pats: SynMacroPat list * body: SynMacroBody * range: range

and [<RequireQualifiedAccess>] UnionField =
    | Type of typ: SynType * range: range
    | Named of name: SynSymbol * typ: SynType * range: range

and UnionCase = UnionCase of name: SynSymbol * fields: UnionField list * range: range

and [<RequireQualifiedAccess>] RecordContent =
    | Label of RecordLabel
    | Member of SynTypeMember

and [<RequireQualifiedAccess>] SynMacroPat =
    | Const of value: SynConst * range: range
    | List of pats: SynMacroPat list * range: range
    | Symbol of name: SynSymbol * range: range
    | Ellipsis of range: range
    | Discard of range: range
    | Trivia of kind: SynMacroTriviaKind * range: range

and [<RequireQualifiedAccess>] SynMacroTriviaKind =
    | Dot
    | Comma
    | Colon
    | ColonColon
    | Bar

and [<RequireQualifiedAccess>] SynListKind =
    | List
    | Vector
    | HashMap
    | HashSet
    | HashParen
    | AttributeList
    | BraceBar
    | DotBracket
    | BracketBar

and [<NoEquality; NoComparison; RequireQualifiedAccess>] SynMacroBody =
    | List of kind: SynListKind * exprs: SynMacroBody list * range: range
    | Call of SynMacroCall
    | Const of constant: SynConst * range: range
    | Symbol of value: SynSymbol
    | Keyword of value: SynKeyword
    | Ellipsis of range: range
    | Discard of range: range
    | Trivia of kind: SynMacroTriviaKind * range: range

and [<RequireQualifiedAccess>] SynMatch =
    | SynMatch of
        pattern: SynMatchPattern *
        cond: SynExpr option *
        body: SynExpr list *
        range: range

and [<RequireQualifiedAccess>] SynThreadable =
    | Expr of value: SynExpr * range: range
    | Index of expr: SynExpr list * range: range
    | Property of symbol: SynSymbol * range: range
    | Method of symbol: SynSymbol * kind: DotMethodKind * range: range

and [<RequireQualifiedAccess>] SynPatternTriviaKind =
    | Dot
    | Comma
    | ColonColon
    | Brackets

and [<RequireQualifiedAccess>] SynMatchPattern =
    | Const of value: SynConst * range: range
    | Tuple of pats: SynMatchPattern list * range: range
    | List of pats: SynMatchPattern list * range: range
    | Symbol of name: SynSymbol * range: range
    | Discard of range: range
    | Trivia of kind: SynPatternTriviaKind * range: range

and SynMemberGet =
    | SynMemberGet of args: SynArg list * exprs: SynExpr list * range: range

    member t.Range = let (SynMemberGet(range = r)) = t in r

and SynMemberSet =
    | SynMemberSet of args: SynArg list * value: SynName * exprs: SynExpr list * range: range

    member t.Range = let (SynMemberSet(range = r)) = t in r

and [<RequireQualifiedAccess>] SynTypeMember =
    | Let of name: SynName * value: SynExpr * range: range
    | Mut of name: SynName * value: SynExpr * range: range
    | Member of name: SynSymbol * value: SynExpr * range: range
    | GetSet of name: SynSymbol * get: SynMemberGet option * set: SynMemberSet option * range: range
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

    member this.OperatorChar =
        match this with
        | Plus _ -> '+'
        | Mult _ -> '+'
        | Div _ -> '+'
        | Minus _ -> '+'

    member this.Exprs =
        match this with
        | Plus(args = r)
        | Mult(args = r)
        | Div(args = r)
        | Minus(args = r) -> r

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

and [<RequireQualifiedAccess>] SynArg =
    // Argument with explicit type
    | TypedArg of name: SynSymbol * argtype: SynType * range: range
    // Argument with inferred type
    | InferredArg of name: SynSymbol * range: range

    member d.NameText =
        match d with
        | TypedArg(name = name)
        | InferredArg(name = name) -> name.Text

and VispProgram = VispProgram of directives: SynDirective list * exprs: SynExpr list

module Coll =
    let mkList its r =
        (SynCollection(CollectionKind.Paren, its, r))

    let mkVector its r =
        (SynCollection(CollectionKind.Bracket, its, r))

    let mkHashParen its r =
        (SynCollection(CollectionKind.HashParen, its, r))

    let mkHashBracket its r =
        (SynCollection(CollectionKind.HashBracket, its, r))

    let mkBraceBar its r =
        (SynCollection(CollectionKind.BraceBar, its, r))

    let mkHashMap its r =
        (SynCollection(CollectionKind.Brace, its, r))

    let mkHashSet its r =
        (SynCollection(CollectionKind.HashBrace, its, r))

    let mkFsList its r =
        (SynCollection(CollectionKind.FsList, its, r))

    let mkFsArray its r =
        (SynCollection(CollectionKind.FsArray, its, r))

    let mkFsSet its r =
        (SynCollection(CollectionKind.FsSet, its, r))

    let mkFsVec its r =
        (SynCollection(CollectionKind.FsVec, its, r))

    let mkFsMap its r =
        (SynCollection(CollectionKind.FsMap, its, r))

module Patterns =
    let (|IdentWith|) (it: Ident) = (it.idText)

    let (|SymbolWith|) (it: SynExpr) =
        match it with
        | SynExpr.Symbol(SynSymbol(id)) -> id.idText
        | _ -> ""

    let (|SymbolText|) (it: SynExpr) =
        match it with
        | SynExpr.Symbol(sym) -> Some(sym.Text)
        | _ -> None

    let (|Text|) (it: SynSymbol) = it.Text


module CollExpr =
    let mkList its r = Coll.mkList its r |> SynExpr.Collection

    let mkVector its r =
        Coll.mkVector its r |> SynExpr.Collection

    let mkHashMap its r =
        Coll.mkHashMap its r |> SynExpr.Collection

    let mkHashSet its r =
        Coll.mkHashSet its r |> SynExpr.Collection

    let mkFsList its r =
        Coll.mkFsList its r |> SynExpr.Collection

    let mkFsArray its r =
        Coll.mkFsArray its r |> SynExpr.Collection

    let mkFsSet its r =
        Coll.mkFsSet its r |> SynExpr.Collection

    let mkFsVec its r =
        Coll.mkFsVec its r |> SynExpr.Collection

    let mkFsMap its r =
        Coll.mkFsMap its r |> SynExpr.Collection

    let mkHashParen its r =
        Coll.mkHashParen its r |> SynExpr.Collection

    let mkHashBracket its r =
        Coll.mkHashBracket its r |> SynExpr.Collection

    let mkBraceBar its r =
        Coll.mkBraceBar its r |> SynExpr.Collection


module Syntax =
    let UnitExpr r = SynExpr.Const(SynConst.Unit, r)

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

    let mkFunctionCallOrCexpr ex args r =
        match ex with
        | SynExpr.Symbol(SynSymbol(id) as sym) when id.idText.EndsWith("->") && id.idText.Length > 2 ->
            SynExpr.Computation(sym.TrimEnd("->"), args, r)
        | ex -> SynExpr.FunctionCall(ex, args, r)

    let symbolTextEquals (a: SynSymbol) b = a.TextEquals b

    let partitionMembers (r: RecordContent list) =
        let labels, members =
            r
            |> List.partition (function
                | RecordContent.Label _ -> true
                | _ -> false)

        (labels
         |> List.choose (function
             | RecordContent.Label it -> Some it
             | _ -> None),
         members
         |> List.choose (function
             | RecordContent.Member it -> Some it
             | _ -> None))

    let partitionChoices<'a, 'b> (r: Choice<'a, 'b> list) =
        (r
         |> List.choose (function
             | Choice1Of2 it -> Some(it)
             | _ -> None),
         r
         |> List.choose (function
             | Choice2Of2 it -> Some(it)
             | _ -> None))

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type ParsedHashDirectiveArgument =
    | String of value: string * stringKind: SynStringKind * range: range

    member this.Range =
        match this with
        | String(range = m) -> m

[<NoEquality; NoComparison>]
type ParsedHashDirective =
    | ParsedHashDirective of ident: string * args: ParsedHashDirectiveArgument list * range: range

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
    | Require of target: SynSymbol * version: string * range: range

    // | Attributes of attributes: SynAttributes * range: range

    | HashDirective of hashDirective: ParsedHashDirective * range: range

    // | NamespaceFragment of fragment: SynModuleOrNamespace

    member d.Range =
        match d with
        | SynModuleDecl.HashDirective(range = m)
        | SynModuleDecl.ModuleAbbrev(range = m)
        | SynModuleDecl.NestedModule(range = m)
        | SynModuleDecl.Expr(range = m)
        | SynModuleDecl.Require(range = m)
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
