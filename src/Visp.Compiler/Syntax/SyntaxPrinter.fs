// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module rec Visp.Compiler.SyntaxPrinter

open System.Runtime.CompilerServices
open PrettyPrinter
open PrettyPrinter.Print
open Visp.Compiler.Syntax
open Visp.Common
open System.IO
open FSharp.Text.Parsing

// type SynMacroBody with
//     interface IToDoc with
//         member this.ToDoc () =

let charToParseable (ch: char) =
    match ch with
    | '\n' -> "#\\lf"
    | '\r' -> "#\\cr"
    | '\t' -> "#\\tab"
    | ' ' -> "#\\space"
    | '\\' -> "#\\\\"
    | it when it = (Operators.char 8) -> "#\\backspace"
    | it when it = '\u0000' -> "#\\nul"
    | it -> $"#\\{it}"

let text = Print.text

let constToDoc =
    function
    | SynConst.Unit -> Print.text "()"
    | SynConst.Nil -> Print.text "nil"
    | SynConst.Bool v -> if v then Print.text "true" else Print.text "false"
    | SynConst.Char v -> Print.text <| charToParseable v
    | SynConst.SByte(it) -> text $"{it}y"
    | SynConst.Int16(it) -> text $"{it}s"
    | SynConst.Int32(it) -> text $"{it}"
    | SynConst.Int64(it) -> text $"{it}L"
    | SynConst.Byte(it) -> text $"{it}uy"
    | SynConst.UInt16(it) -> text $"{it}us"
    | SynConst.UInt32(it) -> text $"{it}ul"
    | SynConst.UInt64(it) -> text $"{it}UL"
    | SynConst.IntPtr(it) -> text $"{it}n"
    | SynConst.UIntPtr(it) -> text $"{it}un"
    | SynConst.Decimal(it) -> text $"{it}M"
    | SynConst.Single(it) -> text $"{it}f"
    | SynConst.Double(it) -> text $"{it}"
    | SynConst.String(raw, kind, _) ->
        let quotes =
            match kind with
            | SynStringKind.Interpolated _
            | SynStringKind.Regular
            | SynStringKind.Verbatim -> "\""
            | SynStringKind.InterpolatedTripleQuote _
            | SynStringKind.TripleQuote
            | SynStringKind.Verbatim -> "\"\"\""

        let prefix =
            match kind with
            | SynStringKind.Interpolated nest -> String.replicate nest "$"
            | SynStringKind.Verbatim -> "@"
            | SynStringKind.InterpolatedTripleQuote nest -> String.replicate nest "$"
            | _ -> ""

        // TODO: This could be optimized to used PooledStringBuilder and only allocate once
        // but let's wait until there is a need
        Print.text (prefix + quotes + raw + quotes)

let rec macroPatToDoc =
    function
    | SynMacroPat.Const(it, _) -> constToDoc it
    | SynMacroPat.Ellipsis _ -> Print.text "..."
    | SynMacroPat.Discard _ -> Print.text "_"
    | SynMacroPat.Symbol(it, _) -> Print.text it.Text
    | SynMacroPat.Trivia(it, _) -> macroTriviaToDoc it
    | SynMacroPat.List(pats, _) ->
        let docs = pats |> List.map macroPatToDoc |> hsep

        parens docs

let macroTriviaToDoc =
    function
    | SynMacroTriviaKind.Dot -> Print.char '.'
    | SynMacroTriviaKind.Colon -> Print.char ':'
    | SynMacroTriviaKind.Comma -> Print.char ','
    | SynMacroTriviaKind.Bar -> Print.char '|'
    | SynMacroTriviaKind.ColonColon -> Print.text "::"

let rec macroBodyToDoc =
    function
    | SynMacroBody.Const(it, _) -> constToDoc it
    | SynMacroBody.Ellipsis _ -> Print.text "..."
    | SynMacroBody.Discard _ -> Print.text "_"
    | SynMacroBody.Trivia(it, _) -> macroTriviaToDoc it
    | SynMacroBody.Symbol it -> Print.text it.Text
    | SynMacroBody.Keyword it -> Print.text it.Text
    | SynMacroBody.Call(SynMacroCall(name, args, _)) ->
        let doc =
            (Print.(<+>))
                (Print.text name.Text)
                (args |> List.tail |> List.map macroBodyToDoc |> Print.hsep)

        Print.parens doc

    | SynMacroBody.List(kind, its, _) ->
        let body = its |> List.map macroBodyToDoc |> Print.hsep

        let surround =
            match kind with
            | SynListKind.Paren -> Print.parens
            | SynListKind.Brace -> Print.braces
            | SynListKind.Bracket -> Print.brackets
            | SynListKind.DotBracket -> Print.enclose (Print.text ".[") (Print.text "]")
            | SynListKind.BraceBar -> Print.enclose (Print.text "{|") (Print.text "|}")
            | SynListKind.ParenBar -> Print.enclose (Print.text "(|") (Print.text "|)")
            | SynListKind.BracketBar -> Print.enclose (Print.text "[|") (Print.text "|]")
            | SynListKind.HashParen -> Print.enclose (Print.text "#(") (Print.text ")")
            | SynListKind.HashBracket -> Print.enclose (Print.text "#[") (Print.text "]")
            | SynListKind.HashBrace -> Print.enclose (Print.text "#{") (Print.text "}")

        surround body

type SynMacroPat with

    member this.Pretty() =
        let doc = macroPatToDoc this
        let sb = PooledStringBuilder.Get()
        use sw = new StringWriter(sb)
        Print.writeSimpleDoc sw <| Print.renderPrettyDefault doc
        sb.ToStringAndReturn()

[<Extension>]
type Extensions =
    [<Extension>]
    static member inline Pretty(xs: list<SynMacroPat>) =
        let doc = parens <| (List.map macroPatToDoc xs |> hsep)
        let sb = PooledStringBuilder.Get()
        use sw = new StringWriter(sb)
        Print.writeSimpleDoc sw <| Print.renderPrettyDefault doc
        sb.ToStringAndReturn()

    [<Extension>]
    static member inline Pretty(xs: list<SynMacroBody>) =
        let doc = parens <| (List.map macroBodyToDoc xs |> hsep)
        let sb = PooledStringBuilder.Get()
        use sw = new StringWriter(sb)
        Print.writeSimpleDoc sw <| Print.renderPrettyDefault doc
        sb.ToStringAndReturn()

// type List<'T> with
//     member this.Pretty() =
//         let doc = macroPatToDoc this
//         let sb = PooledStringBuilder.Get()
//         use sw = new StringWriter(sb)
//         Print.writeSimpleDoc sw <| Print.renderPrettyDefault doc
//         sb.ToStringAndReturn()


type SynMacroBody with

    member this.Pretty() =
        let doc = macroBodyToDoc this
        let sb = PooledStringBuilder.Get()
        use sw = new StringWriter(sb)
        Print.writeSimpleDoc sw <| Print.renderPrettyDefault doc
        sb.ToStringAndReturn()

let nameToDoc =
    function
    | SynName.Inferred(it, _) -> text it.Text
    | SynName.Typed(name, typ, _) ->
        brackets <| (cat [ text name.Text; colon; space; text "todo types" ])

let patToDoc =
    function
    | SynPat.Named(it, _) -> text it.Text
    | it -> failwithf "todo pat: %A" it

let lambdaToDoc (SynLambda(args, body, _)) =
    //let args = List.map argToDoc args |> hsep |> parens
    let args = patToDoc args

    let body = List.map exprToDoc body |> vsep

    parens <| hcat [ text "fn"; space; args; line; indent 2 body ]

let isAtomic =
    function
    | SynExpr.Const _
    | SynExpr.Literal _
    | SynExpr.Symbol _
    | SynExpr.Keyword _ -> true
    | _ -> false

let inline sym (s: SynSymbol) = text s.Text

let opToDoc (op: SynOp) =
    let args = op.Exprs

    parens
    <| hcat
        [ text <| op.OperatorChar
          if not args.IsEmpty then space else mempty
          args |> List.map exprToDoc |> hsep ]


let rec exprToDoc =
    function
    | SynExpr.Const(it, _) -> constToDoc it
    | SynExpr.Literal(it, _) -> constToDoc it
    | SynExpr.Symbol it -> text it.Text
    | SynExpr.Keyword it -> text it.Text
    | SynExpr.Op it -> opToDoc it
    | SynExpr.Cons(lhs, rhs, _) -> parens <| text "cons" <+> (exprToDoc lhs) <+> (exprToDoc rhs)
    | SynExpr.Concat(lhs, rhs, _) -> parens <| text "concat" <+> (exprToDoc lhs) <+> (exprToDoc rhs)

    | SynExpr.FunctionCall(func, args, _) ->
        parens
        <| cat
            [ exprToDoc func
              if not args.IsEmpty then space else mempty
              args |> List.map exprToDoc |> hsep ]


    | SynExpr.LambdaDef(def) -> lambdaToDoc def

    | SynExpr.FunctionDef(name, flags, args, body, _) ->

        parens
        <| hcat
            [ text "fn"
              if flags.HasFlag(FunctionFlags.Inline) then
                  space
                  text "inline"
              else
                  mempty
              if flags.HasFlag(FunctionFlags.Recursive) then
                  space
                  text "rec"
              else
                  mempty
              space
              sym name
              space
              //List.map argToDoc args |> hsep |> parens
              patToDoc args
              line
              indent 2 (List.map exprToDoc body |> vsep) ]

    | SynExpr.Set(expr, value, _) ->
        let binding = exprToDoc expr
        let body = exprToDoc value

        parens <| cat [ text "set!"; space; binding; space; body ]

    | SynExpr.LetOrUse(name, value, flags, _) ->
        let binding = patToDoc name

        let body = exprToDoc value

        parens
        <| hcat
            [ text (
                  if flags.HasFlag(LetFlags.Use) then "use"
                  else if flags.HasFlag(LetFlags.Mutable) then "mut"
                  else "let"
              )
              if flags.HasFlag(LetFlags.Bang) then char '!' else mempty
              space
              binding
              if isAtomic value then
                  space
                  body
              else
                  line
                  indent 2 body ]

    | it -> failwithf "todo %A" it


type SynExpr with

    member this.Pretty() =
        let doc = exprToDoc this
        let sb = PooledStringBuilder.Get()
        use sw = new StringWriter(sb)
        Print.writeSimpleDoc sw <| Print.renderPrettyDefault doc
        sb.ToStringAndReturn()

let rec moduleDeclToDoc =
    function
    | SynModuleDecl.Open(it, _) -> parens <| cat [ text "open"; space; text it.Text ]
    | SynModuleDecl.Require(it, vers, _) ->
        parens <| cat [ text "require"; space; text it.Text; space; text vers ]
    | SynModuleDecl.Expr(it, _) -> exprToDoc it
    | SynModuleDecl.HashDirective _ -> failwithf "todo hash"
    | SynModuleDecl.NestedModule _ -> failwith "todo nested"
    | SynModuleDecl.ModuleAbbrev _ -> failwith "todo abbrev"

let doubleLine a b = hcat [ a; line; line; b ]

let fragmentToDoc (ParsedFileFragment.AnonModule(decls, _)) =
    decls |> List.map moduleDeclToDoc |> fold doubleLine


let parsedFileToDoc (ParsedFile(fragments)) =
    fragments |> List.map fragmentToDoc |> vsep

type ParsedFile with

    member this.Pretty() =
        let doc = parsedFileToDoc this
        let sb = PooledStringBuilder.Get()
        use sw = new StringWriter(sb)
        Print.writeSimpleDoc sw <| Print.renderPrettyDefault doc
        sb.ToStringAndReturn()


let docToString doc =
    let sb = PooledStringBuilder.Get()
    use sw = new StringWriter(sb)
    Print.writeSimpleDoc sw <| Print.renderPrettyDefault doc
    sb.ToStringAndReturn()
