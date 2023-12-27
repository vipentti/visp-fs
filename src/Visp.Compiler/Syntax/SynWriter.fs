// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Syntax.SynWriter

open SpanUtils.Extensions
open Visp.Compiler.Writer
open Visp.Common
open Visp.Compiler.Syntax
open Visp.Compiler.Text
open System.Collections.Generic

open Visp.Runtime.Library.CompileHelpers

type SynWriter(writer: CustomFileWriter) =
    let letStack = new Stack<bool>()

    member _.EnterLet() = letStack.Push(true)

    member _.LeaveLet() = letStack.Pop() |> ignore

    member _.InLet() =
        match letStack.TryPeek() with
        | false, _ -> false
        | true, it -> it

    member _.knownMethods = getMethods ()

    member _.writer = writer

    member d.IndentLevel = d.writer.IndentLevel

    member d.IncreaseIndent() = d.writer.IncreaseIndent()

    member d.DecreaseIndent() = d.writer.DecreaseIndent()

    member this.Write(text: string) = this.writer.Write(text)

    member this.Write(text: char) = this.writer.Write(text)

    member d.TW = d.writer.Inner

    member t.TryGetKnownMethod s = Map.tryFind s t.knownMethods
// member this.Write(it: int32) = this.writer.Write(it)
// member this.Write(it: int64) = this.writer.Write(it)
// member this.Write(it: int16) = this.writer.Write(it)
// member this.Write(it: byte) = this.writer.Write(it)
// member this.Write(it: decimal) =
//     this.writer.Write(it.ToString(CultureInfo.InvariantCulture))

let mkSynWriter w = new SynWriter(w)

[<RequireQualifiedAccess>]
type Text =
    | Char of char
    | String of string

[<Struct; RequireQualifiedAccess>]
type WriteState =
    | Body
    | Newline
    | Arg
    | Inline
    | InlineNoParens

    member this.EnsureNewline() =
        match this with
        | Body -> Body
        | _ -> Newline

    member this.IsInsideInline =
        match this with
        | Inline
        | Arg
        | InlineNoParens -> true
        | _ -> false

    member this.ShouldNewline =
        match this with
        | Body -> true
        | Newline -> true
        | _ -> false

    member this.Line =
        match this with
        | Body -> true
        | _ -> false

    member this.Indent =
        match this with
        | Body -> true
        | Newline -> true
        | _ -> false

    member this.parens =
        match this with
        | InlineNoParens -> false
        | _ -> true

let mkCh c = Text.Char c
let mkStr c = Text.String c

type WriteFun<'a> = (SynWriter -> WriteState -> 'a -> unit)

type CollectionWriters<'a> =
    { plain: WriteFun<'a>
      parens: WriteFun<'a>
      value: WriteFun<'a>
      filter: ('a -> bool) }

module Write =
    open Visp.Runtime.Library
    let flip f a b = f b a

    let chain fa fb : SynWriter -> unit =
        (fun (w: SynWriter) ->
            fa w
            fb w
            ())

    let chainN (lst: (SynWriter -> unit) list) : SynWriter -> unit =
        (fun (w: SynWriter) ->
            for it in lst do
                it w

            ())

    let space (w: SynWriter) = w.writer.WriteSpace()
    let newline (w: SynWriter) = w.writer.WriteLine()
    let fmtprintf (w: SynWriter) fmt = w.writer.WriteFormat fmt
    let fmtprintfn (w: SynWriter) fmt = w.writer.WriteFormatLine fmt
    let char (w: SynWriter) (ch: char) = w.writer.Write ch
    let string (w: SynWriter) (ch: string) = w.writer.Write ch
    let indent (w: SynWriter) = w.writer.WriteIndent()

    let newlineIndent: SynWriter -> unit = chain newline indent

    let lineof (w: SynWriter) (range: range) =
        fmtprintfn w "// line %i @\"%s\"" range.StartLine range.FileName

    let text (w: SynWriter) (txt: Text) =
        match txt with
        | Text.Char ch -> char w ch
        | Text.String ch -> string w ch

    let surroundWith (w: SynWriter) (lhs: Text) (rhs: Text) fn =
        text w lhs
        fn w
        text w rhs
        ()

    let surroundWithCh (w: SynWriter) (lhs: char) fn (rhs: char) =
        char w lhs
        fn w
        char w rhs
        ()

    let surroundWithString (w: SynWriter) (lhs: string) fn (rhs: string) =
        string w lhs
        fn w
        string w rhs
        ()


    let withIndent (w: SynWriter) space =
        let res = w.writer.Indent()

        if space then
            newlineIndent w

        res

    let withIndentIf (w: SynWriter) should =
        if should then w.writer.Indent() else w.writer.DoNotIndent()


    let reservedWords =
        [ "fun"
          "then"
          "done"
          "val"
          "end"
          "begin"
          "mod"
          "to"
          "with"
          "fixed"
          "base" ]
        |> Set.ofList

    let escapableChars = [ '?'; '-'; '+'; '*'; '/'; '!'; ':'; '<'; '>' ] |> Set.ofList

    let escapeString (w: SynWriter) (str: string) =
        let mutable finalIndentLevel = 0

        let mutable lines = str.EnumerateSplitLines()

        while lines.MoveNext() do
            let cur = lines.Current
            let mutable enu = cur.GetEnumerator()
            let mutable isDone = false

            let mutable level = 0

            while not isDone && enu.MoveNext() do
                let ch = enu.Current

                if not (System.Char.IsWhiteSpace ch) then
                    isDone <- true
                else
                    level <- level + 1

                ()

            finalIndentLevel <- level
            ()


        let mutable sb = PooledStringBuilder.Get()

        // if finalIndentLevel = 0 then
        let mutable lastNewline = false
        let mutable indent = 1
        let mutable enu = str.GetEnumerator()

        if w.InLet() && finalIndentLevel > 0 then
            finalIndentLevel <- finalIndentLevel - w.IndentLevel
            printfn "%i vs %i" finalIndentLevel w.IndentLevel

        while (enu.MoveNext()) do
            let mutable ch = enu.Current

            if ch = '\n' && finalIndentLevel > 0 then
                lastNewline <- true
                indent <- 1
                sb <- sb.Append ch
            else if lastNewline && ch = ' ' then
                while (indent < finalIndentLevel && ch = ' ' && enu.MoveNext()) do
                    indent <- indent + 1
                    ch <- enu.Current
                    ()

                if ch <> ' ' then
                    sb <- sb.Append ch

                while ch = ' ' && enu.MoveNext() do
                    ch <- enu.Current
                    sb <- sb.Append ch
                    ()

                lastNewline <- false
            else
                lastNewline <- false
                sb <- sb.Append ch

            ()

        sb.ToStringAndReturn()

    let normalizeString (name: string) =
        let mutable sb = PooledStringBuilder.Get()
        sb.EnsureCapacity(name.Length) |> ignore
        let mutable needs_escape = false

        if Set.contains name reservedWords then
            sb <- sb.Append('_')
            ()

        for ch in name do
            if Set.contains ch escapableChars then
                needs_escape <- true

            // TODO: Allow ^ also
            if ch = '^' then
                sb <- sb.Append('\'')
            else
                sb <- sb.Append(ch)

        (PooledStringBuilder.ToStringAndReturn(sb), needs_escape)

    let symbolText (SynSymbol _ as it) = normalizeString it.Text |> fst

    let name (w: SynWriter) (name: string) (escape: bool) =
        let (name, needs_escape) = normalizeString name

        if escape && needs_escape then
            w.writer.Write("``")

        w.Write(name)

        if escape && needs_escape then
            w.writer.Write("``")

    let symbol (w: SynWriter) (SynSymbol(id)) (escape: bool) = name w (id.idText) escape

    let writeConst (w: SynWriter) (toValue: bool) (cnst: SynConst) =
        if toValue then
            let fromName =
                match cnst with
                | SynConst.String _ -> "Value.string("
                | _ -> "Value.from("

            string w fromName


        let tw = w.TW

        match cnst with
        | SynConst.SourceIdentifier(_, str, _) ->
            surroundWithCh w '"' (flip string str) '"'
            ()
        | SynConst.String(str, kind, _) ->
            match kind with
            | SynStringKind.Regular -> surroundWithCh w '"' (flip string str) '"'
            | SynStringKind.Interpolated nest ->
                let prefix = new string ('$', nest)
                surroundWithString w (prefix + "\"") (flip string str) "\""
            | SynStringKind.InterpolatedTripleQuote nest ->
                let prefix = new string ('$', nest)

                surroundWithString w (prefix + "\"\"\"") (flip string str) "\"\"\""
            | SynStringKind.TripleQuote -> surroundWithString w "\"\"\"" (flip string str) "\"\"\""
            | SynStringKind.Verbatim -> failwith "unsupported"

        | SynConst.SByte(value) ->
            tw.Write(value)
            tw.Write('y')
        | SynConst.Int16(value) ->
            tw.Write(value)
            tw.Write('s')
        | SynConst.Int64(value) ->
            tw.Write(value)
            tw.Write('L')
        | SynConst.Byte(value) ->
            tw.Write(value)
            tw.Write('u')
            tw.Write('y')
        | SynConst.UInt16(value) ->
            tw.Write(value)
            tw.Write('u')
            tw.Write('s')
        | SynConst.UInt32(value) ->
            tw.Write(value)
            tw.Write('u')
            tw.Write('l')
        | SynConst.UInt64(value) ->
            tw.Write(value)
            tw.Write('U')
            tw.Write('L')
        | SynConst.Single(value) ->
            tw.Write(value)
            tw.Write('f')
        | SynConst.IntPtr(value) ->
            tw.Write(value)
            tw.Write('n')
        | SynConst.UIntPtr(value) ->
            tw.Write(value)
            tw.Write('u')
            tw.Write('n')
        | SynConst.Decimal(value) ->
            tw.Write(value)
            tw.Write('M')
        | SynConst.Int32(value) -> tw.Write(value)
        | SynConst.Double(value) -> fprintf tw "%A" value
        | SynConst.UserNum(va, su) ->
            tw.Write va
            tw.Write su
        | SynConst.Char(value) ->
            w.Write('\'')

            match value with
            | '\n' -> w.Write("\\n")
            | '\r' -> w.Write("\\r")
            | '\t' -> w.Write("\\t")
            | '\\' -> w.Write("\\\\")
            | it -> w.Write(it)

            w.Write('\'')
        | SynConst.Nil -> string w "Value.Nil"
        | SynConst.Unit -> if toValue then string w "Value.Unit" else string w "()"
        | SynConst.Bool(v) -> string w (if v then "true" else "false")


        if toValue then
            char w ')'


    let lineIf (w: SynWriter) (st: WriteState) (r: range) =
        if st.Line then
            lineof w r

    let indentIf (w: SynWriter) (st: WriteState) =
        if st.Indent then
            indent w

    let startExpr (w: SynWriter) (st: WriteState) (r: range) =
        if st.Indent then
            indent w

        if st.Line then
            lineof w r
            indent w

    let startNewlineExpr (w: SynWriter) (st: WriteState) (r: range) =
        let res = withIndentIf w (st.IsInsideInline)

        if st.IsInsideInline then
            newline w

        startExpr w (st.EnsureNewline()) r
        res

    let private writeSeqNext<'a>
        (w: SynWriter)
        (st: WriteState)
        (sep: SynWriter -> 'a -> unit)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        let mutable fst = true

        for it in items do
            if fst then fst <- false else sep w it
            wrt w st it

        ()

    let private writeSeq<'a>
        (w: SynWriter)
        (st: WriteState)
        (sep: SynWriter -> unit)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        let mutable fst = true

        for it in items do
            if fst then fst <- false else sep w
            wrt w st it

        ()

    let private writeBodyNoIndent<'a>
        (w: SynWriter)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        newline w
        writeSeq w WriteState.Body newline wrt items

    let private writeBody<'a>
        (w: SynWriter)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        use _ = withIndent w false
        newline w
        writeSeq w WriteState.Body newline wrt items

    let private writeInlineSeparated<'a>
        (w: SynWriter)
        (sep: string)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        writeSeq w WriteState.Inline (flip string sep) wrt items

    let private writeInlineCommaSeparated<'a>
        (w: SynWriter)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        writeInlineSeparated w ", " wrt items

    let private writeInlineSemicolon<'a>
        (w: SynWriter)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        writeInlineSeparated w ";" wrt items

    let private writeInlineSpaceSeparated<'a>
        (w: SynWriter)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        writeSeq w WriteState.Inline space wrt items

    let private writeArgComma<'a>
        (w: SynWriter)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        writeSeq w WriteState.Arg (flip string ", ") wrt items

    let private writeArgSpace<'a>
        (w: SynWriter)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        writeSeq w WriteState.Arg space wrt items

    let private writeSeqLeading<'a>
        (w: SynWriter)
        (st: WriteState)
        (sep: SynWriter -> unit)
        (wrt: SynWriter -> WriteState -> 'a -> unit)
        (items: seq<'a>)
        =
        for it in items do
            sep w
            wrt w st it

        ()

    let rec pairUp list =
        match list with
        | a :: b :: tail -> (a, b) :: pairUp tail
        | _ -> [] // Handles the case where there are fewer than 2 elements left

    let rec writeType w (typ: SynType) =
        match typ with
        | SynType.Ident(id) -> name w (id.idText) false
        | SynType.Paren(inner, _) ->
            char w '('
            writeType w inner
            char w ')'
        | SynType.Discard _ -> char w '_'
        | SynType.Generic(tn, targs, _) ->
            writeType w tn
            char w '<'
            writeInlineCommaSeparated w writeTypeHelp targs
            char w '>'
            ()
        | SynType.Array(rank, elem, _) ->
            writeType w elem
            char w '['

            for _ = 1 to (rank - 1) do
                char w ','

            char w ']'
        | SynType.Tuple(_, segs, _) ->
            let writeSeg w _ =
                function
                | SynTypeTupleSegment.Star _ -> string w "*"
                | SynTypeTupleSegment.Type it -> writeType w it

            writeInlineSpaceSeparated w writeSeg segs

            ()
        | SynType.Fun(arg, ret, _) ->
            writeType w arg
            string w " -> "
            writeType w ret
            ()

    and writeTypeHelp w _ = writeType w

    let rec synPat (w: SynWriter) (n: SynPat) =
        match n with
        | SynPat.Const(cnst, _) ->
            writeConst w false cnst
            ()
        | SynPat.Ignore _ -> ()
        | SynPat.Trivia(kind, _) ->
            match kind with
            | SynPatternTriviaKind.Brackets -> string w "[]"
            | SynPatternTriviaKind.ColonColon -> string w "::"
            | SynPatternTriviaKind.Comma
            | SynPatternTriviaKind.Dot -> string w ","
        | SynPat.Discard _ -> string w "_"
        | SynPat.Named(n, _) -> symbol w n true
        | SynPat.Typed(sub, typ, _) ->
            synPat w sub
            string w ": "
            writeType w typ
        | SynPat.As(lhs, rhs, _) ->
            synPat w lhs
            string w " as "
            synPat w rhs
        | SynPat.IsInst(typ, _) ->
            string w ":? "
            writeType w typ
        | SynPat.Record(fields, _) ->
            string w "{"

            writeInlineSemicolon
                w
                (fun w _ (name, pat) ->
                    symbol w name false
                    string w " = "
                    synPat w pat)
                fields

            string w "}"
        | SynPat.Args(args, _) ->
            match args with
            | SynArgPats.Tuple pats ->
                if pats.IsEmpty then
                    string w "()"
                else
                    string w "("
                    writeInlineSpaceSeparated w writeSynPat pats
                    string w ")"
            | SynArgPats.List pats ->
                if pats.IsEmpty then
                    string w "()"
                else
                    writeInlineSpaceSeparated
                        w
                        (fun w _ tp ->
                            match tp with
                            | SynPat.Typed _ as it ->
                                string w "("
                                synPat w it
                                string w ")"
                            | it -> synPat w it)
                        pats
        | SynPat.Collection(SynCollection(kind, its, _)) ->
            match kind with
            | CollectionKind.Paren ->
                string w "("
                writeInlineSpaceSeparated w writeSynPat its
                string w ")"
            | CollectionKind.Bracket ->
                string w "["
                writeInlineSpaceSeparated w writeSynPat its
                string w "]"
                ()
            | CollectionKind.FsList
            | CollectionKind.FsArray ->
                failwithf "unsupported pat collection: %A" its
                ()

            | it -> failwithf "unsupported pat collection: %A" it

    and writeSynPat (w: SynWriter) (_: WriteState) = synPat w

    let requiresNewline =
        function
        | SynExpr.If _
        | SynExpr.ForTo _
        | SynExpr.ForIn _
        | SynExpr.Match _ -> true

        | SynExpr.Collection(SynCollection(kind = k)) ->
            match k with
            | CollectionKind.Bracket
            | CollectionKind.FsVec -> true
            | _ -> false
        | _ -> false

    let usesParensByDefault =
        function
        | SynExpr.Tuple _
        | SynExpr.Op _ -> true
        | _ -> false

    let rec writeExpr (w: SynWriter) (st: WriteState) (expr: SynExpr) =

        match expr with
        | SynExpr.SyntaxMacroCall _ -> failwithf "unsupported %A" expr
        | SynExpr.SyntaxMacro _ -> failwithf "unsupported %A" expr
        | SynExpr.LambdaShort _ -> failwithf "unsupported %O" expr
        | SynExpr.Collection it -> writeCollectionExprs w st it

        | SynExpr.Match(exprs, pats, range) ->
            use _ = startNewlineExpr w st range

            string w "match "
            writeExpr w WriteState.Inline exprs
            string w " with"
            use _ = withIndent w false
            newlineIndent w

            writeSeq
                w
                WriteState.Body
                newlineIndent
                (fun w _ (SynMatch.SynMatch(pat, cond, body, _)) ->
                    string w "| "
                    synPat w pat
                    string w " "

                    match cond with
                    | Some(cond) ->
                        string w "when CoreMethods.isTruthy("
                        writeExprInParens w WriteState.Arg cond
                        string w ") "
                    | None -> ()

                    string w "->"
                    writeBody w writeExpr body

                    ())
                pats
        | SynExpr.ForTo(name, start, finish, body, down, range) ->
            use _ = startNewlineExpr w st range
            string w "for "
            symbol w name true
            string w " = "
            writeExpr w WriteState.Inline start
            if down then string w " downto " else string w " to "
            writeExpr w WriteState.Inline finish
            string w " do"
            writeBody w writeExpr body

        | SynExpr.ForIn(name, bind, body, range) ->
            use _ = startNewlineExpr w st range
            string w "for "
            synPat w name
            string w " in "
            writeExpr w WriteState.Inline bind
            string w " do"
            writeBody w writeExpr body
            ()

        | SynExpr.FsDo(expr, _, range) ->
            startExpr w st range
            string w "do! "
            writeExprInParens w WriteState.Inline expr

        | SynExpr.FsYield(expr, bang, range) ->
            startExpr w st range
            if bang then string w "yield! " else string w "yield "
            writeExprInParens w WriteState.Inline expr

        | SynExpr.FsReturn(expr, bang, range) ->
            startExpr w st range
            if bang then string w "return! " else string w "return "
            writeExprInParens w WriteState.Inline expr

        | SynExpr.Computation(builder, exprs, range) ->
            startExpr w st range
            symbol w builder false
            string w " {"
            writeBody w writeExpr exprs
            string w "}"

        | SynExpr.FsSeq(exprs, range) ->
            startExpr w st range
            string w "seq {"
            writeBody w writeExpr exprs
            string w "}"

        | SynExpr.RangeExpr(start, mid, last, range) ->
            startExpr w st range

            writeExprInParens w WriteState.Inline start
            string w " .. "

            match mid with
            | Some(mid) ->
                writeExprInParens w WriteState.Inline mid
                string w " .. "
            | None -> ()

            writeExprInParens w WriteState.Inline last

            ()

        | SynExpr.FunctionDef(name_, flags, args, body, retty, range) ->
            startExpr w st range

            string w "let "

            if flags.HasFlag(FunctionFlags.Recursive) then
                string w "rec "

            if flags.HasFlag(FunctionFlags.Inline) then
                string w "inline "

            symbol w name_ true
            space w
            //writeArgsOrEmpty w args
            synPat w args

            match retty with
            | None -> ()
            | Some(ty) ->
                string w " : "
                writeType w ty
                ()

            string w " ="

            use _ = withIndent w false
            newline w

            for expr in body do
                writeExpr w WriteState.Body expr
                newline w
        | SynExpr.Atom(expr, range) ->
            startExpr w st range
            string w "Value.atom(Value.from("
            writeExpr w WriteState.Arg expr
            string w "))"

        | SynExpr.Deref(_, expr, range) ->
            startExpr w st range
            string w "deref ("
            writeExpr w WriteState.Arg expr
            string w ")"

        | SynExpr.LetOrUse(name, body, flags, attributes, range) ->
            startExpr w st range
            writeLetFullNew w st name body flags attributes

        | SynExpr.Keyword kw -> writeQuoted w st (SynQuoted.Keyword kw)

        | SynExpr.Begin(expr, kind, range) ->
            startExpr w st range

            if kind = BeginKind.Do then
                char w '('

            if kind = BeginKind.Do then
                writeBody w writeExpr expr
            else
                writeBodyNoIndent w writeExpr expr

            if kind = BeginKind.Do then
                char w ')'

        | SynExpr.If(cond, thn, alt, range) ->
            use _ = startNewlineExpr w st range

            string w "if CoreMethods.isTruthy("
            use _ = withIndent w false
            newlineIndent w
            writeExprInParens w WriteState.Inline cond
            string w ")"
            newlineIndent w
            string w "then"

            (use _ = withIndent w false
             newline w
             writeExpr w WriteState.Body thn)

            match alt with
            | Some(alt) ->
                newlineIndent w
                string w "else"
                newline w

                (use _ = withIndent w false
                 writeExpr w WriteState.Body alt)
            | None -> ()


        | SynExpr.LetStar(bindings, body, _) ->
            indentIf w st
            string w "("

            use _ = withIndent w false

            for bind in bindings do
                let (SynBinding(name, body, _r)) = bind
                newline w
                startExpr w WriteState.Body _r
                writeLetFullNew w st name body LetFlags.None []
                ()

            writeSeqLeading w WriteState.Body newline writeExpr body

            string w ")"

        | SynExpr.Set(name, body, range) ->
            startExpr w st range

            writeExpr w WriteState.InlineNoParens name
            string w " <- "
            writeExpr w WriteState.Inline body
            ()

        | SynExpr.Tuple(exprs, range) ->
            startExpr w st range
            string w "("
            writeInlineCommaSeparated w writeExprWithNewlineAfter exprs
            string w ")"

        | SynExpr.Const(cnst, _) ->
            indentIf w st
            writeConst w false cnst

        | SynExpr.Literal(cnst, _) ->
            indentIf w st
            writeConst w false cnst

        | SynExpr.FunctionCall(expr, args, range) ->
            startExpr w st range

            match expr with
            | SynExpr.LambdaDef lam ->
                writeSynLambda w st lam
                writeCallArgs w args
            | SynExpr.Keyword _ as kw ->
                match args with
                | [ one ] ->
                    string w "HashMap.find"
                    writeCallArgs w [ kw; one ]
                | _ -> failwithf "unsupported call %O %A" expr args

            | Patterns.SymbolWith "with" ->
                match args with
                | [ expr; SynExpr.RecordInit(inits, _) ] -> writeRecordInit w st (Some expr) inits
                | _ ->
                    writeExpr w WriteState.Inline expr
                    writeCallArgs w args

            | Patterns.SymbolWith "cons" ->
                match args with
                | [ lhs; rhs ] ->
                    writeExprInParens w WriteState.Inline lhs
                    string w "::"
                    writeExprInParens w WriteState.Inline rhs
                | _ ->
                    writeExpr w WriteState.Inline expr
                    writeCallArgs w args

            | Patterns.SymbolWith "tuple" ->
                string w "("
                writeInlineCommaSeparated w writeExprWithNewlineAfter args
                string w ")"

            | Patterns.SymbolWith "concat" ->
                match args with
                | [ lhs; rhs ] ->
                    writeExprInParens w WriteState.Inline lhs
                    string w "@"
                    writeExprInParens w WriteState.Inline rhs
                | _ ->
                    writeExpr w WriteState.Inline expr
                    writeCallArgs w args

            | SynExpr.Symbol name ->
                match w.knownMethods.TryFind(Syntax.textOfSymbol name) with
                | Some method ->

                    let mutable parens = false

                    fmtprintf w "%s.``%s``" method.DeclaringType.Name method.Name

                    if isVariableArgMethod method then
                        parens <- true
                        string w "("


                        string w "state"

                        if not args.IsEmpty then
                            string w ", "

                        writeArgComma
                            w
                            (fun wt stt ex ->
                                string wt "Value.from("
                                writeExpr wt stt ex
                                string wt ")")
                            args
                    else if hasSingleValueArrayTypeArg method then
                        parens <- true
                        string w "("

                        writeArgComma
                            w
                            (fun wt stt ex ->
                                string wt "Value.from("
                                writeExpr wt stt ex
                                string wt ")")
                            args
                    else if hasParamArrayAttribute method then
                        parens <- true
                        string w "("

                        writeSeq w WriteState.Arg (flip string ", ") writeExpr args
                    else

                        // TODO: Support
                        parens <- true
                        string w "("
                        let parameters = method.GetParameters()
                        let zipped = Seq.zip args parameters

                        let mutable first = true

                        for (expr, arg) in zipped do
                            if first then first <- false else string w ", "

                            if arg.ParameterType = typeof<Value> then
                                string w "Value.from("
                                writeExpr w WriteState.Arg expr
                                string w ")"
                            else
                                writeExprInParens w WriteState.Arg expr

                            ()

                        ()

                    if parens then
                        char w ')'
                | None ->
                    symbol w name true
                    writeCallArgs w args
                    ()

            | SynExpr.ThreadLast _ as ex ->
                writeExpr w st ex
                writeCallArgs w args

            | it ->
                writeExpr w WriteState.Inline it
                writeCallArgs w args


        | SynExpr.Symbol sym ->
            indentIf w st
            symbol w sym true

        | SynExpr.LambdaDef lam -> writeSynLambda w st lam

        | SynExpr.Op op ->
            startExpr w st op.Range
            string w "("
            writeOp w st op
            string w ")"

        | SynExpr.New(typ, args, range) ->
            startExpr w st range
            string w "(new "
            writeType w typ
            string w "("
            writeArgComma w writeExpr args
            string w "))"

        | SynExpr.While(cond, body, range) ->
            startExpr w st range

            string w "while CoreMethods.isTruthy("
            writeExpr w WriteState.Arg cond
            string w ") do"
            writeBody w writeExpr body

        | SynExpr.DotMethod(inst, method, args, kind, range) ->
            startExpr w st range

            let parens = st <> WriteState.Body && st <> WriteState.Newline

            if parens then
                string w "("

            writeExpr w WriteState.Inline inst
            string w "."
            symbol w method true

            match kind with
            | DotMethodKind.Tuple ->
                string w "("
                writeArgComma w writeExpr args
                string w ")"
            | DotMethodKind.Apply ->
                string w " "

                if args.Length = 1 then
                    writeArgSpace w writeExpr args
                else
                    writeArgSpace w writeExprInParens args

                ()

            if parens then
                string w ")"

        | SynExpr.DotProperty(inst, prop, range) ->
            startExpr w st range

            if st.parens then
                string w "("

            writeExpr w WriteState.Inline inst
            string w "."
            symbol w prop true

            if st.parens then
                string w ")"

        | SynExpr.DotIndex(inst, prop, range) ->
            startExpr w st range

            if st.parens then
                string w "("

            writeExpr w WriteState.Inline inst
            string w ".["

            writeInlineCommaSeparated
                w
                (fun w st ex ->
                    match ex with
                    | Patterns.SymbolWith "*" -> string w "*"
                    | it -> writeExpr w st it)
                prop

            string w "]"

            if st.parens then
                string w ")"

            ()

        | SynExpr.Quote(_, expr, range) ->
            startExpr w st range
            writeQuoted w WriteState.Inline expr

        | SynExpr.Quasiquote _ -> failwithf "unsupported expr %O" expr
        // | SynExpr.ThreadLast _ -> failwithf "unsupported expr %O" expr

        | SynExpr.ThreadLast(args, range) ->
            // use _ = startNewlineExpr w st range
            startExpr w st range

            match args with
            | [] -> failwith "empty?"
            | [ arg ] ->
                match arg with
                | SynThreadable.Expr(arg, _) -> writeExpr w WriteState.Inline arg
                | _ -> failwithf "unsupported %A" arg
            | arg :: rest ->
                // writeExpr w wsNone arg
                match arg with
                | SynThreadable.Expr(arg, _) -> writeExpr w WriteState.Inline arg
                | _ -> failwithf "unsupported %A" arg

                use _ = withIndent w false

                for item in rest do
                    newlineIndent w

                    string w "|> "

                    match item with
                    | SynThreadable.Expr(item, _) ->
                        match item with
                        | SynExpr.FunctionCall(call, [], _) ->
                            writeExpr w WriteState.Inline call
                            ()
                        | _ -> writeExpr w WriteState.Inline item
                    | _ -> failwithf "unsupported %A" item

        | SynExpr.ThreadFirst(args, range) ->
            startExpr w st range

            match args with
            | [] -> failwith "unsupported empty ThreadFirst"
            | [ arg ] -> writeExpr w WriteState.Inline arg
            | arg :: rest ->
                writeExpr w WriteState.Inline arg
                use _ = withIndent w false

                for item in rest do
                    newlineIndent w

                    string w "|> "

                    match item with
                    | SynExpr.FunctionCall(call, [], _) ->
                        writeExpr w WriteState.Inline call
                        ()
                    | _ -> writeExpr w WriteState.Inline item

        | SynExpr.TypeAlias(name, typ, range) ->
            startExpr w st range
            fmtprintf w "type %s = " (symbolText name)
            writeType w typ

        | SynExpr.Union(name, cases, members, attributes, range) ->
            writeAttributesIfNotEmpty w st attributes

            startExpr w st range
            fmtprintf w "type %s =" (symbolText name)

            use _ = withIndent w false

            writeSeqLeading
                w
                WriteState.Body
                newline
                (fun w st (UnionCase(name, fields, range)) ->
                    startExpr w st range
                    string w "| "
                    symbol w name false

                    if not fields.IsEmpty then
                        string w " of"
                        let mutable fs = true

                        for field in fields do
                            if fs then
                                fs <- false
                                space w
                            else
                                string w " * "

                            match field with
                            | UnionField.Type(typ, _) -> writeType w typ
                            | UnionField.Named(name, typ, _) ->
                                symbol w name false
                                string w ": "
                                writeType w typ

                            ()

                    ())
                cases

            newline w

            if not members.IsEmpty then
                newline w
                writeSeq w WriteState.Body newline writeMember members

            ()

        | SynExpr.Type(name, args, members, attributes, range) ->
            writeAttributesIfNotEmpty w st attributes

            startExpr w st range
            fmtprintf w "type %s" (symbolText name)

            space w
            synPat w args

            // if args.IsEmpty then
            //     string w " ()"
            // else
            //     string w " ("
            //     writeSeq w WriteState.InlineNoParens (flip string ", ") writeName args
            //     string w ")"

            string w " ="
            writeBody w writeMember members

        | SynExpr.ObjectExpression(ctor, members, range) ->
            startExpr w st range
            string w "{"
            use _ = withIndent w true
            string w "new "

            match ctor with
            | TypeOrCtor.Type(ty, _) -> writeType w ty
            | TypeOrCtor.Ctor(ty, args, _) ->
                writeType w ty
                string w "("
                writeArgComma w writeExpr args
                string w ")"
                ()

            string w " with"

            writeBody w writeMember members

            string w "}"

            ()

        | SynExpr.Record(name, labels, members, attributes, range) ->
            writeAttributesIfNotEmpty w st attributes

            startExpr w st range
            fmtprintf w "type %s =" (Syntax.textOfSymbol name)

            use _ = withIndent w false
            newlineIndent w
            string w "{ "

            writeSeq
                w
                WriteState.InlineNoParens
                (flip string "; ")
                (fun w _ (RecordLabel(kind, name, typ, _)) ->
                    if kind = RecordLabelKind.Mutable then
                        string w "mutable "

                    string w name.Text
                    string w ": "
                    writeType w typ

                    ())
                labels

            string w " }"

            if not members.IsEmpty then
                writeBody w writeMember members


        | SynExpr.RecordInit(inits, range) ->
            startExpr w st range
            writeRecordInit w st None inits

    and private writeExprWithNewlineAfter (w: SynWriter) (st: WriteState) (expr: SynExpr) =
        writeExpr w st expr

        if requiresNewline expr then
            newlineIndent w

    and private writeRecordInit w _ (withExpr: SynExpr option) (inits: SynInit list) =

        string w "{"

        match withExpr with
        | Some(withExpr) ->
            w.IncreaseIndent()
            newlineIndent w
            writeExprInParens w WriteState.Inline withExpr
            string w " with"
        | None -> ()

        writeBody
            w
            (fun w st (SynInit(name, expr, range)) ->
                startExpr w st range
                symbol w name true
                string w " = "
                writeExpr w WriteState.Inline expr)
            inits

        newlineIndent w
        string w "}"

        if withExpr.IsSome then
            w.DecreaseIndent()

        ()

    and private writeCollection w st (writers: CollectionWriters<'a>) (cl: SynCollection<'a>) =
        let (SynCollection(kind, items, range)) = cl

        match kind with
        | CollectionKind.HashBracket
        | CollectionKind.BraceBar
        | CollectionKind.DotBracket
        | CollectionKind.HashParen -> failwithf "unsupported collection expr: %A" kind

        | CollectionKind.Brace ->
            startExpr w st range
            string w "["

            writeSeq
                w
                WriteState.Inline
                (flip char ';')
                (fun w st (key, value) ->
                    char w '('
                    writers.value w st key
                    char w ','
                    char w ' '
                    writers.value w st value
                    char w ')'
                    ())
                (pairUp items)

            string w "] |> HashMap.ofList"

        | CollectionKind.Bracket
        | CollectionKind.FsVec ->
            use _ = startNewlineExpr w st range

            fmtprintf
                w
                "let temp = %s(%i)"
                (if kind = CollectionKind.Bracket then
                     "Vector"
                 else
                     "ResizeArray<_>")
                items.Length

            writeSeqLeading
                w
                WriteState.Newline
                newlineIndent
                (fun ws _ a ->
                    string w "temp.Add("

                    if kind = CollectionKind.Bracket then
                        string w "Value.from("

                    writers.plain ws WriteState.Arg a

                    if kind = CollectionKind.Bracket then
                        string w ")"

                    string w ")")
                items

            newlineIndent w
            string w "temp"

        | CollectionKind.Paren
        | CollectionKind.HashBrace
        | CollectionKind.FsArray
        | CollectionKind.FsList
        | CollectionKind.FsMap
        | CollectionKind.FsSet ->
            startExpr w st range
            string w "["

            if kind = CollectionKind.FsArray then
                string w "|"

            let writer =
                if kind = CollectionKind.HashBrace then
                    writers.value
                else
                    writers.parens

            let items =
                if kind = CollectionKind.FsList || kind = CollectionKind.Paren then
                    items |> List.filter (writers.filter)
                else
                    items

            writeInlineSemicolon w writer items

            if kind = CollectionKind.FsArray then
                string w "|"

            string w "]"

            if kind = CollectionKind.FsMap then
                string w " |> Map.ofList"

            if kind = CollectionKind.FsSet then
                string w " |> Set.ofList"

            if kind = CollectionKind.HashBrace then
                string w " |> HashSet.ofList"

        ()

    and private writeCollectionExprs w st c =
        writeCollection
            w
            st
            { plain = writeExpr
              value = writeExprToValue
              parens = writeExprInParens
              filter =
                function
                | SynExpr.Const(SynConst.Unit, _) -> false
                | _ -> true }
            c

    and private writeAttributesIfNotEmpty w s (attributes: SynAttributes) =
        if not attributes.IsEmpty then
            writeAttributes w s attributes
            newline w
            indent w

    and private writeAttributes w _ (attributes: SynAttributes) =
        string w "[<"

        let mutable f = true

        for attr in attributes do
            for attr in attr.Attributes do
                if f then f <- false else string w "; "

                writeType w <| attr.TypeName
                writeExpr w WriteState.InlineNoParens attr.ArgExpr
                ()

        string w ">]"
        ()

    // and private writeName w st (name: SynName) =
    //     match name with
    //     | SynName.Inferred(n, _) -> symbol w n true
    //     | SynName.Typed(nm, typ, _) ->
    //         if st.parens then
    //             char w '('

    //         symbol w nm true
    //         string w ": "
    //         writeType w typ

    //         if st.parens then
    //             char w ')'

    // and private writeArgs w args =
    //     writeSeq w WriteState.Inline space writeArg args
    and private writeMemSet w (SynMemberSet(args, value, exprs, _)) =
        string w "set "
        synPat w args
        // writeSeq w WriteState.Inline (flip string ", ") writeArg args
        string w " "
        //writeName w WriteState.Inline value
        synPat w value
        string w " ="
        writeBody w writeExpr exprs

    and private writeMemGet w (SynMemberGet(args, exprs, _)) =
        string w "get "
        synPat w args
        string w " ="
        // string w "get ("
        // writeSeq w WriteState.Inline (flip string ", ") writeArg args
        // string w ") ="
        writeBody w writeExpr exprs

    and private writeMember w st (mem: SynTypeMember) =
        match mem with
        | SynTypeMember.Let(name, expr, flags, attributes, range) ->
            startExpr w st range
            writeLetFullNew w st name expr flags attributes
            ()
        | SynTypeMember.Constructor(args, exprs, range) ->
            startExpr w st range
            string w "new"
            synPat w args
            string w " ="
            writeBody w writeExpr exprs
        | SynTypeMember.Val(name, typ, flags, attributes, range) ->
            startExpr w st range

            writeAttributesIfNotEmpty w st attributes

            string w "val"

            if flags.HasFlag(LetFlags.Mutable) then
                string w " mutable"

            space w
            synPat w name
            string w " : "
            writeType w typ

        | SynTypeMember.GetSet(name, get, set, flags, attributes, range) ->
            startExpr w st range
            // fmtprintf w "member %s" (Syntax.textOfSymbol name)
            writeAttributesIfNotEmpty w st attributes

            if flags.HasFlag(MemberFlags.Static) then
                string w "static "

            if flags.HasFlag(MemberFlags.Override) then
                string w "override"
            else
                string w "member"

            space w
            symbol w name false

            use _ = withIndent w false
            newlineIndent w

            match (get, set) with
            | (Some get, Some set) ->
                startExpr w st get.Range
                string w "with "
                writeMemGet w get
                newlineIndent w
                startExpr w st set.Range
                string w "and "
                writeMemSet w set
                ()
            | (Some get, None) ->
                startExpr w st get.Range
                string w "with "
                writeMemGet w get

            | (None, Some set) ->
                startExpr w st set.Range
                string w "with "
                writeMemSet w set
            | (None, None) -> failwith "missing both getter and setter."

        | SynTypeMember.Member(name, exprs, flags, attributes, range) ->
            startExpr w st range

            writeAttributesIfNotEmpty w st attributes

            if flags.HasFlag(MemberFlags.Static) then
                string w "static "

            if flags.HasFlag(MemberFlags.Override) then
                string w "override"
            else
                string w "member"

            space w
            symbol w name false
            string w " ="
            writeBody w writeExpr exprs

        | SynTypeMember.MemberFn(name, args, body, flags, attributes, range) ->
            startExpr w st range
            writeAttributesIfNotEmpty w st attributes


            if flags.HasFlag(MemberFlags.Static) then
                string w "static "

            if flags.HasFlag(MemberFlags.Override) then
                string w "override"
            else
                string w "member"

            space w
            symbol w name false
            space w
            synPat w args
            string w " ="
            writeBody w writeExpr body

        | SynTypeMember.Interface(name, mems, range) ->
            startExpr w st range
            fmtprintf w "interface %s with" name.Text
            writeBody w writeMember mems

    and private writeLetFullNew
        (w: SynWriter)
        (st: WriteState)
        (name: SynPat)
        (body: SynExpr)
        (flags: LetFlags)
        (attributes: SynAttributes)
        =
        w.EnterLet()

        let isLiteral =
            not (flags.HasFlag(LetFlags.Mutable))
            && match body with
               | SynExpr.Literal _ -> true
               | _ -> false

        if isLiteral then
            string w "[<Literal>]"
            newline w
            indent w

        writeAttributesIfNotEmpty w st attributes

        if flags.HasFlag(LetFlags.Static) then
            string w "static "

        if flags.HasFlag(LetFlags.Use) then
            string w "use"
        else
            string w "let"

        if flags.HasFlag(LetFlags.Bang) then
            string w "!"

        if (flags.HasFlag(LetFlags.Mutable)) then
            string w " mutable"

        space w

        synPat w name
        string w " ="

        let should_indent =
            match body with
            | SynExpr.Const _
            | SynExpr.Literal _
            | SynExpr.Keyword _
            | SynExpr.Symbol _ -> false
            | _ -> true

        if should_indent then
            use _ = withIndent w false
            newline w
            writeExpr w WriteState.Body body
        else
            space w
            writeExpr w WriteState.Inline body

        w.LeaveLet()
        ()

    and private writeExprInParens w (st: WriteState) ex =
        let needsParens =
            match ex with
            | SynExpr.Const _
            | SynExpr.Tuple _
            | SynExpr.Literal _
            | SynExpr.Op _
            | SynExpr.Symbol _ -> false

            | _ -> true

        if needsParens then
            char w '('

        writeExpr w st ex

        if needsParens then
            char w ')'

    and private writeOp w (st: WriteState) (synOp: SynOp) =
        let mutable firstEx = true

        let writeOpEx w st ex =
            let newlineMandatory = requiresNewline ex

            if not firstEx then
                space w

            if firstEx then
                firstEx <- false

            writeExprInParens w st ex

            if newlineMandatory then
                newlineIndent w

        let opSym = synOp.OperatorChar

        let writeOpSeq args =
            writeSeq w WriteState.InlineNoParens (flip string $" {opSym}") (writeOpEx) args

        match synOp with
        | SynOp.Unary(_, args, _) ->
            string w opSym
            writeSeq w WriteState.InlineNoParens (flip string "") writeExprInParens args

        | SynOp.Infix(op, args, r) ->
            match opSym with
            | "+" ->
                match args with
                | [] -> string w "0"
                | [ one ] -> writeExpr w st one
                | rest -> writeOpSeq rest
            | "*" ->
                match args with
                | [] -> string w "1"
                | [ one ] -> writeExpr w st one
                | rest -> writeOpSeq rest

            | "/" ->
                match args with
                | [ one ] ->
                    string w "LanguagePrimitives.GenericOne / "
                    writeExprInParens w WriteState.Inline one
                | rest -> writeOpSeq rest
            | "-" ->
                match args with
                | [ one ] ->
                    char w '-'
                    writeExpr w WriteState.Inline one
                | rest -> writeOpSeq rest

            | ":>" ->
                match args with
                | [ lhs; rhs ] ->
                    writeExprInParens w WriteState.Inline lhs
                    string w " :> "

                    match rhs with
                    | SynExpr.Symbol it -> symbol w it false
                    | _ -> writeExpr w WriteState.Inline rhs
                | _ -> writeOpSeq args

            | "!=" -> writeOp w st (SynOp.Infix(SynSymbol(Ident("<>", op.Range)), args, r))

            | op ->
                // Comparison methods
                if comparisonMethods.ContainsKey op then
                    match w.TryGetKnownMethod op with
                    | Some(meth) ->
                        match args with
                        | [ _; _ ] -> writeOpSeq args
                        | args ->
                            fmtprintf w "%s.``%s``" meth.DeclaringType.Name meth.Name
                            string w "("
                            writeInlineCommaSeparated w writeExprInParens args
                            string w ")"

                    | None -> writeOpSeq args
                else
                    writeOpSeq args


        ()

    and private writeSynLambda w (st: WriteState) (SynLambda(args, body, range)) =
        startExpr w st range
        string w "(fun "
        synPat w args
        string w " ->"
        writeBody w writeExpr body
        string w ")"
        ()

    and private writeCallArgs w (args: SynExpr list) =
        if args.IsEmpty then
            string w " ()"
        else
            match args with
            | [ SynExpr.Tuple _ as arg ] ->
                space w
                writeExpr w WriteState.InlineNoParens arg
            | _ -> writeSeqLeading w WriteState.Inline space writeExprInParens args

    and private writeExprToValue w st (expr: SynExpr) =
        match expr with
        | SynExpr.Const(cnst, range) ->
            startExpr w st range
            writeConst w true cnst
        | SynExpr.Keyword(SynKeyword id) ->
            startExpr w st id.idRange
            fmtprintf w "Value.keyword(\"%s\")" id.idText
        | others ->
            startExpr w st others.Range
            string w "Value.from("
            writeExpr w WriteState.Inline others
            char w ')'

    and private writeQuotedInParens w (st: WriteState) ex =
        let needsParens =
            match ex with
            | SynQuoted.Const _
            | SynQuoted.Symbol _ -> false

            | _ -> true

        if needsParens then
            char w '('

        writeQuoted w st ex

        if needsParens then
            char w ')'

    and private writeQuoted w st (expr: SynQuoted) =
        match expr with
        | SynQuoted.Const(cnst, range) ->
            startExpr w st range
            writeConst w true cnst
        | SynQuoted.EmptyList range ->
            startExpr w st range
            string w "[]"
        | SynQuoted.Collection(SynCollection(_, _, range) as col) ->
            startExpr w st range
            string w "Value.from("
            use _ = withIndent w false
            newlineIndent w

            writeCollection
                w
                st
                { plain = writeQuoted
                  value = writeQuoted
                  parens = writeQuotedInParens
                  filter = (fun _ -> true) }
                col

            string w ")"
        | SynQuoted.Keyword(SynKeyword id) ->
            startExpr w st id.idRange
            fmtprintf w "Value.keyword(\"%s\")" id.idText

        | SynQuoted.Symbol(SynSymbol id) ->
            startExpr w st id.idRange
            fmtprintf w "Value.symbol(\"%s\")" id.idText

    let writeParsedFile w (ParsedFile(fragments)) =
        let rec writeModuleDecls w (decls: SynModuleDecl list) =
            match decls with
            | decl :: rest ->
                match decl with
                | SynModuleDecl.HashDirective(ParsedHashDirective(ident, args, r), _) ->
                    indent w
                    lineof w r
                    indent w
                    fmtprintf w "#%s" ident

                    for arg in args do
                        space w

                        match arg with
                        | ParsedHashDirectiveArgument.String(it, _, _) ->
                            char w '"'
                            string w it
                            char w '"'

                    ()
                | SynModuleDecl.ModuleAbbrev _ -> ()
                | SynModuleDecl.Require _ -> ()
                | SynModuleDecl.Include _ -> ()
                | SynModuleDecl.Expr(ex, _) -> writeExpr w WriteState.Body ex
                | SynModuleDecl.Open(target, range) ->
                    indent w
                    lineof w range
                    indent w
                    fmtprintfn w "open %s" (Syntax.textOfSymbol target)
                    ()
                | SynModuleDecl.ModuleList(decls, _) -> writeModuleDecls w decls

                | SynModuleDecl.IncludedModule(path, decls, range) ->
                    startExpr w WriteState.Body range
                    fmtprintfn w "let _ = \"%s\"" path.Text
                    writeModuleDecls w decls
                | SynModuleDecl.NestedModule(name, decls, range) ->
                    indent w
                    lineof w range
                    indent w
                    fmtprintfn w "module %s =" (Syntax.textOfSymbol name)

                    use _ = withIndent w false

                    writeModuleDecls w decls


                newline w

                writeModuleDecls w rest
            | _ -> ()

        let writeFragment w (ParsedFileFragment.AnonModule(decls, _)) = writeModuleDecls w decls

        fragments |> List.iter (writeFragment w)
