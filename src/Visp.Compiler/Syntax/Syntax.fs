// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace rec Visp.Compiler.Syntax

open Visp.Compiler.Text
open System.Diagnostics
open Visp.Common

type SyntaxWriteUtilThreadStatics =
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private runningTests: bool

    static member RunningTests
        with get () = SyntaxWriteUtilThreadStatics.runningTests
        and set v = SyntaxWriteUtilThreadStatics.runningTests <- v

module StringWriterUtils =
    let inline writeStringWith (fn: char -> char option) (name: string) (text: string) range =
        let sb = PooledStringBuilder.Get()
        // Reserve capacity for the text + all the extra data being written
        sb.EnsureCapacity(text.Length + 60) |> ignore
        sb.Append name |> ignore
        sb.Append " (\"" |> ignore

        for ch in text do
            match fn ch with
            | Some(ch) -> ignore (sb.Append ch)
            | None -> ()

        sb.Append "\", " |> ignore
        sb.Append(sprintf "%A" range) |> ignore

        sb.Append ")" |> ignore

        sb.ToStringAndReturn()

    let inline writeDebugStringType (name: string) (text: string) kind range =
        let sb = PooledStringBuilder.Get()
        // Reserve capacity for the text + all the extra data being written
        sb.EnsureCapacity(text.Length + 60) |> ignore
        sb.Append name |> ignore
        sb.Append " (\"" |> ignore

        for ch in text do
            match ch with
            | '\r' when SyntaxWriteUtilThreadStatics.RunningTests -> ()
            | it -> ignore (sb.Append it)

        sb.Append "\", " |> ignore
        sb.Append(sprintf "%A" kind) |> ignore
        sb.Append(", ") |> ignore
        sb.Append(sprintf "%A" range) |> ignore

        sb.Append ")" |> ignore

        sb.ToStringAndReturn()

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

[<StructuredFormatDisplay("{Path}")>]
type NormalizedPath(text: string) =
    let original = text

    let originalPlus =
        let len = text.Length

        let sb = PooledStringBuilder.Get()
        sb.EnsureCapacity(len) |> ignore

        let mutable index = 0

        while index < len do
            let ch = text[index]
            index <- index + 1

            match ch with
            | '\\' ->
                if index + 1 < len && text[index + 1] = '\\' then
                    index <- index + 1

                sb.Append('\\').Append('\\') |> ignore
            | it -> sb.Append(it) |> ignore

        sb.ToStringAndReturn()

    let normalized =

        let len = text.Length

        let sb = PooledStringBuilder.Get()
        sb.EnsureCapacity(len) |> ignore

        let mutable index = 0

        while index < len do
            let ch = text[index]
            index <- index + 1

            let ch =
                match ch with
                | '\\' ->
                    if index + 1 < len && text[index + 1] = '\\' then
                        index <- index + 1

                    '/'
                | it -> it

            sb.Append(ch) |> ignore

        sb.ToStringAndReturn()

    member _.Original = original
    member _.OriginalDisplay = originalPlus
    member _.Path = normalized
    override _.ToString() = normalized

module FormatPath =
    let private isTruthy (value: string) =
        System.String.Equals(value, "true", System.StringComparison.OrdinalIgnoreCase)
        || System.String.Equals(value, "1", System.StringComparison.OrdinalIgnoreCase)

    let private isEnvVarSet (key: string) =
        isTruthy <| System.Environment.GetEnvironmentVariable(key)

    let normalizePath text =
        let normalized = NormalizedPath text

        if SyntaxWriteUtilThreadStatics.RunningTests || (isEnvVarSet "VISP_FS_RUNNING_TESTS") then
            normalized.Path
        else
            normalized.OriginalDisplay

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

module Symbol =
    let concat sep (syms: SynSymbol seq) =
        let rr = syms |> Seq.map _.Range |> Seq.reduce Range.unionRanges
        let text = syms |> Seq.map _.Text |> String.concat sep
        SynSymbol(Ident(text, rr))

type SynKeyword = SynKeyword of ident: Ident

type SynKeyword with

    member this.Text =
        let (SynKeyword(id)) = this
        id.idText

    member this.Range =
        let (SynKeyword(id)) = this
        id.idRange

type LongIdent = Ident list
// https://github.com/dotnet/fsharp/blob/fce0cf00585c12174fa3e51e4fc34afe784b9b4e/src/Compiler/pars.fsy#L6298

type SynLongIdent = SynLongIdent of id: LongIdent * dotRanges: range list * trivia: unit option list

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynType =
    | Ident of Ident
    | Generic of typeName: SynType * typeArgs: SynType list * range: range
    | Array of rank: int * elemType: SynType * range: range
    | Tuple of isStruct: bool * segments: SynTypeTupleSegment list * range: range
    | Discard of range: range
    | Fun of argType: SynType * returnType: SynType * range: range
    | Paren of innerType: SynType * range: range

    member t.Range =
        match t with
        | SynType.Ident it -> it.idRange
        | SynType.Generic(range = range)
        | SynType.Array(range = range)
        | SynType.Discard(range = range)
        | SynType.Fun(range = range)
        | SynType.Paren(range = range)
        | SynType.Tuple(range = range) -> range

and [<NoEquality; NoComparison; RequireQualifiedAccess>] SynTypeTupleSegment =
    | Type of typeName: SynType
    | Star of range: range

    member t.Range =
        match t with
        | SynTypeTupleSegment.Star it -> it
        | SynTypeTupleSegment.Type it -> it.Range

[<Struct; RequireQualifiedAccess>]
type SynStringKind =
    | Regular
    | Verbatim
    | TripleQuote
    | Interpolated of plain: int
    | InterpolatedTripleQuote of triple: int


[<StructuredFormatDisplay("{StructuredText}")>]
type SynString =
    | SynString of text: string * synStringKind: SynStringKind * range: range

    member t.Text = let (SynString(text = it)) = t in it

    member t.Range = let (SynString(range = it)) = t in it

    member t.Kind = let (SynString(synStringKind = it)) = t in it

    member t.StructuredText =
        let (SynString(text, k, r)) = t
        StringWriterUtils.writeDebugStringType "SynString" text k r

    override t.ToString() =
        let (SynString(text, k, r)) = t
        StringWriterUtils.writeDebugStringType "SynString" text k r

[<StructuredFormatDisplay("{StructuredText}")>]
type FilePath =
    | FilePath of text: NormalizedPath * range: range

    member t.Text = let (FilePath(text = it)) = t in it.Path

    member t.Range = let (FilePath(range = it)) = t in it

    member t.StructuredText = t.ToString()

    override t.ToString() =
        StringWriterUtils.writeStringWith
            (function
            | '\\' when SyntaxWriteUtilThreadStatics.RunningTests -> Some('/')
            | it -> Some(it))
            (nameof FilePath)
            t.Text
            t.Range

[<NoEquality; NoComparison; RequireQualifiedAccess; StructuredFormatDisplay("{StructuredText}")>]
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
    | UserNum of value: string * suffix: string
    | String of text: string * synStringKind: SynStringKind * range: range
    | SourceIdentifier of constant: string * value: string * range: range

    member t.StructuredText =
        match t with
        | Unit -> "Unit"
        | Nil -> "Nil"
        | Bool it -> sprintf "Bool %A" it
        | SByte it -> sprintf "SByte %A" it
        | Byte it -> sprintf "Byte %A" it
        | Int16 it -> sprintf "Int16 %A" it
        | UInt16 it -> sprintf "UInt16 %A" it
        | UInt32 it -> sprintf "UInt32 %A" it
        | UInt64 it -> sprintf "UInt64 %A" it
        | IntPtr it -> sprintf "IntPtr %A" it
        | UIntPtr it -> sprintf "UIntPtr %A" it
        | Single it -> sprintf "Single %A" it
        | Double it -> sprintf "Double %A" it
        | Int64 it -> sprintf "Int64 %A" it
        | Int32 it -> sprintf "Int32 %A" it
        | Char it -> sprintf "Char %A" it
        | Decimal it -> sprintf "Decimal %A" it
        | UserNum(va, su) -> sprintf "UserNum (%s, %s)" va su
        | SourceIdentifier(va, su, _) ->
            if SyntaxWriteUtilThreadStatics.RunningTests then
                match va with
                | "__SOURCE_DIRECTORY__" ->
                    sprintf "SourceIdentifier (\"%s\", \"%s\")" va (System.IO.Path.GetFileName su)
                | _ -> sprintf "SourceIdentifier (\"%s\", \"%s\")" va su
            else
                sprintf "SourceIdentifier (%s, %s)" va su
        | String(text, k, r) -> StringWriterUtils.writeDebugStringType "String" text k r

type SynTyped = SynTyped of name: SynSymbol * argtype: SynType * range: range

[<NoComparison; RequireQualifiedAccess>]
type RecordLabelKind =
    | Default
    | Mutable

type RecordLabel =
    | RecordLabel of kind: RecordLabelKind * name: SynSymbol * argtype: SynType * range: range

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
    | Static = 16

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
    | DotBracket
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
    | SyntaxMacroCall of call: SynMacroCall
    | SyntaxMacro of macro: SynMacro
    | FunctionDef of
        name: SynSymbol *
        flags: FunctionFlags *
        args: SynPat *
        body: SynExpr list *
        returnType: SynType option *
        range: range
    | FunctionCall of name: SynExpr * args: SynExpr list * range: range
    | LambdaShort of expr: SynExpr * range: range
    | LambdaDef of SynLambda
    | Symbol of SynSymbol
    | Keyword of SynKeyword
    | Const of constant: SynConst * range: range
    | Literal of constant: SynConst * range: range
    | Quote of shorthand: bool * expr: SynQuoted * range: range
    | Quasiquote of shorthand: bool * expr: SynQuasiquote * range: range
    | Begin of exprs: SynExpr list * kind: BeginKind * range: range
    | New of typ: SynType * args: SynExpr list * range: range
    | LetOrUse of
        pat: SynPat *
        value: SynExpr *
        flags: LetFlags *
        attributes: SynAttributes *
        range: range
    | LetStar of bindings: SynBinding list * body: SynExpr list * range: range
    | Set of name: SynExpr * value: SynExpr * range: range
    | If of cond: SynExpr * main: SynExpr * alt: SynExpr option * range: range
    | While of cond: SynExpr * body: SynExpr list * range: range
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
        args: SynPat *
        members: SynTypeMember list *
        attributes: SynAttributes *
        range: range
    | Union of
        name: SynSymbol *
        cases: UnionCase list *
        members: SynTypeMember list *
        attributes: SynAttributes *
        range: range
    | ObjectExpression of name: TypeOrCtor * members: SynTypeMember list * range: range
    | TypeAlias of name: SynSymbol * typ: SynType * range: range
    | ThreadFirst of exprs: SynExpr list * range: range
    | ThreadLast of exprs: SynThreadable list * range: range
    | RangeExpr of first: SynExpr * step: SynExpr option * last: SynExpr * range: range
    | ForIn of name: SynPat * binding: SynExpr * body: SynExpr list * range: range
    | ForTo of
        name: SynSymbol *
        start: SynExpr *
        finish: SynExpr *
        body: SynExpr list *
        down: bool *
        range: range
    | Match of expr: SynExpr * pats: SynMatch list * range: range


    member t.IsAtomic =
        match t with
        | SynExpr.Const _ -> true
        | SynExpr.Literal _ -> true
        | SynExpr.Symbol _ -> true
        | SynExpr.Keyword _ -> true
        | _ -> false

    member this.Range =
        match this with
        | Op op -> op.Range
        | Collection op -> op.Range
        | Symbol(SynSymbol(id)) -> id.idRange
        | Keyword(SynKeyword(id)) -> id.idRange

        | Match(range = r)
        | ObjectExpression(range = r)
        | Literal(range = r)
        | Tuple(range = r)
        | RangeExpr(range = r)
        | LambdaShort(range = r)
        | ForIn(range = r)
        | ForTo(range = r)
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

and SynBinding = SynBinding of name: SynPat * expr: SynExpr * range: range

and SynMacro = SynMacro of name: SynSymbol * cases: SynMacroCase list * range: range

and SynMacroCall =
    | SynMacroCall of name: SynSymbol * args: SynMacroBody list * range: range

    member this.Name = let (SynMacroCall(name = n)) = this in n
    member this.NameText = let (SynMacroCall(name = n)) = this in n.Text

and SynMacroCase = SynMacroCase of pats: SynMacroPat list * body: SynMacroBody * range: range

and [<NoEquality; NoComparison; RequireQualifiedAccess>] TypeOrCtor =
    | Type of typ: SynType * range: range
    | Ctor of typ: SynType * args: SynExpr list * range: range

and [<RequireQualifiedAccess>] UnionField =
    | Type of typ: SynType * range: range
    | Named of name: SynSymbol * typ: SynType * range: range

and UnionCase = UnionCase of name: SynSymbol * fields: UnionField list * range: range

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
    | Paren
    | Bracket
    | Brace
    | HashBrace
    | HashParen
    | HashBracket
    | BraceBar
    | DotBracket
    | BracketBar
    | ParenBar

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
    | SynMatch of pattern: SynPat * cond: SynExpr option * body: SynExpr list * range: range

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

and [<NoEquality; NoComparison; RequireQualifiedAccess>] SynArgPats =
    | Tuple of SynPat list
    | List of SynPat list

    member x.Patterns =
        match x with
        | Tuple pats -> pats
        | List pats -> pats


and [<NoEquality; NoComparison; RequireQualifiedAccess>] SynPat =
    | Const of constant: SynConst * range: range
    | Named of name: SynSymbol * range: range
    | Typed of pat: SynPat * targetType: SynType * range: range
    // | Tuple of isStruct: bool * pats: SynPat list * range: range
    | Args of argPats: SynArgPats * range: range
    // | Paren of pat: SynPat * range: range
    // | ListCons of lhs: SynPat * rhs: SynPat * range: range
    | Trivia of kind: SynPatternTriviaKind * range: range
    | Collection of SynCollection<SynPat>
    /// _
    | Discard of range: range
    /// :?
    | IsInst of pat: SynType * range: range
    | As of lhsPat: SynPat * rhsPat: SynPat * range: range
    | Ignore of range: range

    | Record of (SynSymbol * SynPat) list * range: range

and SynMemberGet =
    | SynMemberGet of args: SynPat * exprs: SynExpr list * range: range

    member t.Range = let (SynMemberGet(range = r)) = t in r

and SynMemberSet =
    | SynMemberSet of args: SynPat * value: SynPat * exprs: SynExpr list * range: range

    member t.Range = let (SynMemberSet(range = r)) = t in r

and [<System.FlagsAttribute; RequireQualifiedAccess>] MemberFlags =
    | Default = 0
    | Static = 1
    | Override = 2

and [<RequireQualifiedAccess>] SynTypeMember =
    | Let of
        pat: SynPat *
        value: SynExpr *
        flags: LetFlags *
        attributes: SynAttributes *
        range: range
    | Val of pat: SynPat * typ: SynType * flags: LetFlags * attributes: SynAttributes * range: range
    | Constructor of args: SynPat * body: SynExpr list * range: range

    | GetSet of
        name: SynSymbol *
        get: SynMemberGet option *
        set: SynMemberSet option *
        flags: MemberFlags *
        attributes: SynAttributes *
        range: range

    | Member of
        name: SynSymbol *
        body: SynExpr list *
        flags: MemberFlags *
        attributes: SynAttributes *
        range: range

    | MemberFn of
        name: SynSymbol *
        args: SynPat *
        body: SynExpr list *
        flags: MemberFlags *
        attributes: SynAttributes *
        range: range

    | Interface of name: SynSymbol * members: SynTypeMember list * range: range

and [<RequireQualifiedAccess>] SynDirective = Open of path: SynSymbol * range: range

and [<RequireQualifiedAccess>] SynQuoted =
    | EmptyList of range
    | Collection of SynCollection<SynQuoted>
    | Const of constant: SynConst * range: range
    | Symbol of value: SynSymbol
    | Keyword of value: SynKeyword

and [<RequireQualifiedAccess>] SynQuasiquote =
    | Collection of SynCollection<SynQuasiquote>
    | Const of constant: SynConst * range: range
    | Symbol of value: SynSymbol
    | Keyword of value: SynKeyword
    | Unquote of expr: SynExpr * range: range
    | SpliceUnquote of expr: SynExpr * range: range

and [<RequireQualifiedAccess>] SynOp =
    | Infix of op: SynSymbol * args: SynExpr list * range: range
    | Unary of op: SynSymbol * args: SynExpr list * range: range

    member this.OperatorChar =
        match this with
        | Infix(op = op)
        | Unary(op = op) -> op.Text

    member this.Exprs =
        match this with
        | Infix(args = op)
        | Unary(args = op) -> op

    member this.Range =
        match this with
        | Infix(range = op)
        | Unary(range = op) -> op

and SynLambda =
    | SynLambda of args: SynPat * body: SynExpr list * range: range

    member this.Range =
        let (SynLambda(_, _, rng)) = this
        rng

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

    let (|SymbolText|_|) (it: SynExpr) =
        match it with
        | SynExpr.Symbol(sym) -> Some(sym.Text)
        | _ -> None

    let (|Text|) (it: SynSymbol) = it.Text

    let (|ConstString|_|) =
        function
        | SynExpr.Const(SynConst.String(value, kind, r), _) -> Some((value, kind, r))
        | _ -> None


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

module SynPat =
    let mkParenCollection ls r =
        SynPat.Collection(SynCollection(CollectionKind.Paren, ls, r))

    let mkInParens ls r = mkParenCollection [ ls ] r

module Syntax =
    let UnitExpr r = SynExpr.Const(SynConst.Unit, r)

    [<Literal>]
    let parserRecovery = "__PARSER_RECOVERY__"

    let parserRecoveryConst r =
        SynConst.String(parserRecovery, SynStringKind.Regular, r)

    let parserRecoveryExpr r =
        SynExpr.Const(SynConst.String(parserRecovery, SynStringKind.Regular, r), r)

    let parserRecoverySymbol r = SynSymbol(Ident(parserRecovery, r))

    let parserRecoveryPat r = SynPat.Named(parserRecoverySymbol r, r)

    let parserRecoveryType r = SynType.Ident(Ident(parserRecovery, r))

    let parserRecoveryMatch r =
        SynMatch.SynMatch(SynPat.Const(parserRecoveryConst r, r), None, [], r)

    let textOfIdent (id: Ident) = id.idText
    let rangeOfIdent (id: Ident) = id.idRange

    let textOfSymbol (id: SynSymbol) =
        let (SynSymbol s) = id
        textOfIdent s

    let rangeOfSymbol (SynSymbol id) = id.idRange

    let textOfKeyword (id: SynKeyword) =
        let (SynKeyword s) = id
        textOfIdent s

    let mkFunctionCall sym ex range = SynExpr.FunctionCall(sym, ex, range)

    let mkEmptyList range =
        SynExpr.Quote(false, SynQuoted.EmptyList range, range)

    let mkSynString s range =
        SynExpr.Const(SynConst.String(s, SynStringKind.Regular, range), range)

    let mkSynSymbol s range = (SynSymbol(Ident(s, range)))
    let mkSynExprSymbol s range = SynExpr.Symbol(mkSynSymbol s range)
    let mkSynSymbolExpr s range = SynExpr.Symbol(mkSynSymbol s range)

    let mkSynTypeIdent s range = (SynType.Ident(Ident(s, range)))

    let mkCons lhs rhs range =
        mkFunctionCall (mkSynSymbolExpr "cons" range) [ lhs; rhs ] range

    let mkConcat lhs rhs range =
        mkFunctionCall (mkSynSymbolExpr "concat" range) [ lhs; rhs ] range

    let mkInferredNamePat n range =
        SynPat.Named(mkSynSymbol n range, range)

    let mkValue v r =
        mkFunctionCall (mkSynSymbolExpr "Value.from" r) [ v ] r

    let mkUnwrapList v r =
        mkFunctionCall (mkSynSymbolExpr "unwrapList" r) [ v ] r

    let mkFunctionCallOrCexpr ex args r =
        match ex with
        | SynExpr.Symbol(SynSymbol(id) as sym) when id.idText.EndsWith("->") && id.idText.Length > 2 ->
            SynExpr.Computation(sym.TrimEnd("->"), args, r)
        | ex -> SynExpr.FunctionCall(ex, args, r)

    let symbolTextEquals (a: SynSymbol) b = a.TextEquals b

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
    | IncludedModule of path: FilePath * decls: SynModuleDecl list * range: range
    | ModuleList of decls: SynModuleDecl list * range: range
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
    | Include of paths: FilePath list * range: range

    // | Attributes of attributes: SynAttributes * range: range

    | HashDirective of hashDirective: ParsedHashDirective * range: range

    // | NamespaceFragment of fragment: SynModuleOrNamespace

    member d.Range =
        match d with
        | SynModuleDecl.HashDirective(range = m)
        | SynModuleDecl.ModuleAbbrev(range = m)
        | SynModuleDecl.NestedModule(range = m)
        | SynModuleDecl.Expr(range = m)
        | SynModuleDecl.ModuleList(range = m)
        | SynModuleDecl.Require(range = m)
        | SynModuleDecl.Include(range = m)
        | SynModuleDecl.IncludedModule(range = m)
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

    member d.Decls = let (AnonModule(decls = it)) = d in it

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
