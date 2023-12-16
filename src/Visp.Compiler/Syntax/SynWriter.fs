// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Syntax.SynWriter

open SpanUtils.Extensions
open Visp.Compiler.Writer
open Visp.Common
open Visp.Compiler.Syntax
open Visp.Compiler.Text
open System.Globalization
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

    member this.Write(text: string) = this.writer.Write(text)

    member this.Write(text: char) = this.writer.Write(text)

    member d.TW = d.writer.Inner
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
        [ "fun"; "then"; "done"; "val"; "end"; "begin"; "mod"; "to"; "with" ]
        |> Set.ofList

    let escapableChars = [ '?'; '-'; '+'; '*'; '/'; '!'; ':' ] |> Set.ofList

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

    let normalizeString name =
        let mutable sb = PooledStringBuilder.Get()
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

    let writeType w (typ: SynType) =
        match typ with
        | SynType.Ident(id) -> name w (id.idText) false

    let synName (w: SynWriter) (n: SynName) =
        match n with
        | SynName.Inferred(n, _) -> symbol w n true
        | SynName.Typed(nm, typ, _) ->
            char w '('
            symbol w nm true
            string w ": "
            writeType w typ
            char w ')'

    let writeConst (w: SynWriter) (toValue: bool) (cnst: SynConst) =
        if toValue then
            let fromName =
                match cnst with
                | SynConst.String _ -> "Value.string("
                | _ -> "Value.from("

            string w fromName


        let tw = w.TW

        match cnst with
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
        | SynConst.Double(value) -> tw.Write(value)
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

    let rec writeExpr (w: SynWriter) (st: WriteState) (expr: SynExpr) =

        match expr with
        | SynExpr.MacroDef _ -> failwithf "unsupported %O" expr
        | SynExpr.MacroCall _ -> failwithf "unsupported %O" expr
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
                (fun w st (SynMatch.SynMatch(pat, cond, body, _)) ->
                    let rec writeMatchPattern w _ pat =
                        match pat with
                        // | SynMatchPattern.CommaOrDot _ -> failwith "not supported"
                        | SynMatchPattern.Trivia(kind, _) ->
                            match kind with
                            | SynPatternTriviaKind.Brackets -> string w "[]"
                            | SynPatternTriviaKind.ColonColon -> string w "::"
                            | SynPatternTriviaKind.Comma
                            | SynPatternTriviaKind.Dot -> string w ","

                            ()
                        | SynMatchPattern.Const(cnst, _) ->
                            writeConst w false cnst
                            ()
                        | SynMatchPattern.Discard _ -> string w "_"
                        | SynMatchPattern.Symbol(sym, _) -> symbol w sym true
                        | SynMatchPattern.Tuple(pats, _) ->
                            string w "("
                            writeInlineCommaSeparated w writeMatchPattern pats
                            string w ")"
                        | SynMatchPattern.List(pats, _) ->
                            string w "("
                            writeInlineSpaceSeparated w writeMatchPattern pats
                            string w ")"


                    string w "| "
                    writeMatchPattern w st pat
                    string w " "

                    match cond with
                    | Some(cond) ->
                        string w "when CoreMethods.isTruthy("
                        writeExpr w WriteState.Arg cond
                        string w ") "
                    | None -> ()

                    string w "->"
                    writeBody w writeExpr body

                    ())
                pats
        | SynExpr.ForTo(name, start, finish, body, down, range) ->
            use _ = startNewlineExpr w st range
            string w "for "
            writeName w WriteState.Inline name
            string w " = "
            writeExpr w WriteState.Inline start
            if down then string w " downto " else string w " to "
            writeExpr w WriteState.Inline finish
            string w " do"
            writeBody w writeExpr body

        | SynExpr.ForIn(name, bind, body, range) ->
            use _ = startNewlineExpr w st range
            string w "for "
            writeName w WriteState.Inline name
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

        | SynExpr.FunctionDef(name_, flags, args, body, range) ->
            startExpr w st range

            string w "let "

            if flags.HasFlag(FunctionFlags.Recursive) then
                string w "rec "

            if flags.HasFlag(FunctionFlags.Inline) then
                string w "inline "

            symbol w name_ true
            space w
            writeArgsOrEmpty w args
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

        | SynExpr.LetOrUse(name, body, flags, range) ->
            startExpr w st range
            writeLetFullNew w st name body flags

        | SynExpr.SimpleLet(name, body, range) ->
            startExpr w st range
            writeLet w st name body

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
            writeExpr w WriteState.Inline cond
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


        | SynExpr.LetStar(bindings, body, range) ->
            indentIf w st
            string w "("

            use _ = withIndent w false

            for bind in bindings do
                let (SynBinding(name, body, _r)) = bind
                newline w
                startExpr w WriteState.Body _r
                writeLet w st name body
                ()

            writeSeqLeading w WriteState.Body newline writeExpr body

            string w ")"

        | SynExpr.SimpleMut(name, body, range) ->
            startExpr w st range
            writeLetFull w st true name body
            ()

        | SynExpr.Set(name, body, range) ->
            startExpr w st range

            // symbol w name true
            writeExpr w WriteState.InlineNoParens name
            string w " <- "
            writeExpr w WriteState.Inline body
            ()

        | SynExpr.Pair(lhs, rhs, range) ->
            startExpr w st range
            string w "("
            writeExpr w WriteState.Inline lhs
            string w ", "
            writeExpr w WriteState.Inline rhs
            string w ")"

        | SynExpr.Tuple(exprs, range) ->
            startExpr w st range
            string w "("
            writeInlineCommaSeparated w writeExpr exprs
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
                                writeExpr w WriteState.Arg expr

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

            | _ -> failwithf "not callable %O" expr


        | SynExpr.Symbol sym ->
            indentIf w st
            symbol w sym true

        // | SynExpr.LambdaCall(lam, args, range) ->
        //     startExpr w st range
        //     writeSynLambda w st lam
        //     writeCallArgs w args
        | SynExpr.LambdaDef lam -> writeSynLambda w st lam

        | SynExpr.Op op -> writeOp w st op

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

        | SynExpr.Concat(lhs, rhs, range) ->
            startExpr w st range
            char w '('
            writeExpr w WriteState.Inline lhs
            string w ")@("
            writeExpr w WriteState.Inline rhs
            char w ')'

        | SynExpr.Cons(lhs, rhs, range) ->
            startExpr w st range
            char w '('
            writeExpr w WriteState.Inline lhs
            string w ")::("
            writeExpr w WriteState.Inline rhs
            char w ')'

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
            | [] -> failwith "empty?"
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
            if not attributes.IsEmpty then
                writeAttributes w st attributes
                newline w

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
            if not attributes.IsEmpty then
                writeAttributes w st attributes
                newline w

            startExpr w st range
            fmtprintf w "type %s" (symbolText name)

            if args.IsEmpty then
                string w " ()"
            else
                string w " ("
                writeSeq w WriteState.InlineNoParens (flip string ", ") writeName args
                string w ")"

            string w " ="
            writeBody w writeMember members

        | SynExpr.Record(name, labels, members, attributes, range) ->
            if not attributes.IsEmpty then
                writeAttributes w st attributes
                newline w

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

                    fmtprintf w "%s: %s" name.Text typ.Text

                    ())
                labels

            string w " }"

            if not members.IsEmpty then
                writeBody w writeMember members


        | SynExpr.RecordInit(inits, range) ->
            startExpr w st range
            string w "{ "

            writeSeq
                w
                WriteState.Inline
                (flip string "; ")
                (fun w st (SynInit(name, expr, _)) ->
                    symbol w name true
                    string w " = "
                    writeExpr w st expr
                    ())
                inits

            string w " }"

    and private writeCollectionExprs w st (SynCollection(kind, items, range) as c) =
        match kind with
        | CollectionKind.Paren
        | CollectionKind.HashBracket
        | CollectionKind.BraceBar
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
                    writeExprToValue w st key
                    char w ','
                    char w ' '
                    writeExprToValue w st value
                    char w ')'
                    ())
                (pairUp items)

            string w "] |> HashMap.ofList"

        | CollectionKind.Bracket
        | CollectionKind.FsVec ->
            startExpr w st range

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

                    writeExpr ws WriteState.Arg a

                    if kind = CollectionKind.Bracket then
                        string w ")"

                    string w ")")
                items

            newlineIndent w
            string w "temp"

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
                    writeExprToValue
                else
                    writeExpr

            let items =
                if kind = CollectionKind.FsList then
                    items
                    |> List.choose (function
                        | SynExpr.Const(SynConst.Unit, _) -> None
                        | it -> Some(it))
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

    and private writeName w st (name: SynName) =
        match name with
        | SynName.Inferred(n, _) -> symbol w n true
        | SynName.Typed(nm, typ, _) ->
            if st.parens then
                char w '('

            symbol w nm true
            string w ": "
            writeType w typ

            if st.parens then
                char w ')'

    // and private writeArgs w args =
    //     writeSeq w WriteState.Inline space writeArg args
    and private writeMemSet w (SynMemberSet(args, value, exprs, _)) =
        string w "set ("
        writeSeq w WriteState.Inline (flip string ", ") writeArg args
        string w ") "
        writeName w WriteState.Inline value
        string w " ="
        writeBody w writeExpr exprs

    and private writeMemGet w (SynMemberGet(args, exprs, _)) =
        string w "get ("
        writeSeq w WriteState.Inline (flip string ", ") writeArg args
        string w ") ="
        writeBody w writeExpr exprs

    and private writeMember w st (mem: SynTypeMember) =
        match mem with
        | SynTypeMember.Let(name, expr, range) ->
            startExpr w st range
            writeLet w st name expr
            ()
        | SynTypeMember.Mut(name, body, range) ->
            startExpr w st range
            string w "let mutable "
            synName w name
            string w " ="
            use _ = withIndent w false
            newline w
            writeExpr w WriteState.Body body
            ()
        | SynTypeMember.GetSet(name, get, set, range) ->
            startExpr w st range
            fmtprintf w "member %s" (Syntax.textOfSymbol name)
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

        | SynTypeMember.Member(name, expr, range) ->
            startExpr w st range
            fmtprintf w "member %s =" (Syntax.textOfSymbol name)
            use _ = withIndent w false
            newlineIndent w
            writeExpr w WriteState.Body expr
            ()
        | SynTypeMember.OverrideMember(name, expr, range) ->
            startExpr w st range
            fmtprintf w "override %s =" (Syntax.textOfSymbol name)
            use _ = withIndent w false
            newlineIndent w
            writeExpr w WriteState.Body expr
            ()
        | SynTypeMember.OverrideFn(name, args, body, range) ->
            startExpr w st range
            fmtprintf w "override %s" (Syntax.textOfSymbol name)
            space w
            writeArgsOrEmpty w args
            string w " ="
            use _ = withIndent w false
            newline w

            for expr in body do
                writeExpr w WriteState.Body expr
                newline w
        | SynTypeMember.MemberFn(name, args, body, range) ->
            startExpr w st range
            fmtprintf w "member %s" (Syntax.textOfSymbol name)
            space w
            writeArgsOrEmpty w args
            string w " ="
            use _ = withIndent w false
            newline w

            for expr in body do
                writeExpr w WriteState.Body expr
                newline w

    and private writeLetFullNew
        (w: SynWriter)
        (_: WriteState)
        (name: SynName)
        (body: SynExpr)
        (flags: LetFlags)
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

        if flags.HasFlag(LetFlags.Use) then
            string w "use"
        else
            string w "let"

        if flags.HasFlag(LetFlags.Bang) then
            string w "!"

        if (flags.HasFlag(LetFlags.Mutable)) then
            string w " mutable"

        space w

        synName w name
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

    and private writeLetFull (w: SynWriter) (st: WriteState) mut (name: SynName) (body: SynExpr) =
        w.EnterLet()

        let isLiteral =
            not mut
            && match body with
               | SynExpr.Literal _ -> true
               | _ -> false

        if isLiteral then
            string w "[<Literal>]"
            newline w
            indent w

        string w "let "

        if mut then
            string w "mutable "

        synName w name
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


    and private writeLet w (st: WriteState) (name: SynName) (body: SynExpr) =
        writeLetFull w st false name body
        ()

    and private writeExprInParens w (st: WriteState) ex =
        // TODO: Should we check if we have a tuple and then not add parens?
        char w '('
        writeExpr w st ex
        char w ')'

    and private writeOp w (st: WriteState) (op: SynOp) =
        let opState = WriteState.Inline

        match op with
        | SynOp.Plus(args, r) ->
            startExpr w st r

            match args with
            | [] -> string w "0"
            | [ one ] -> writeExpr w st one
            | rest ->
                writeSeq
                    w
                    WriteState.Inline
                    (fun w ->
                        string w " +"
                        newlineIndent w)
                    writeExprInParens
                    rest

        | SynOp.Mult(args, r) ->
            startExpr w st r

            match args with
            | [] -> string w "LanguagePrimitives.GenericOne"
            | [ one ] -> writeExpr w st one
            | rest -> writeSeq w WriteState.Inline (flip string " * ") writeExpr rest
        | SynOp.Div(args, r) ->
            startExpr w st r

            match args with
            | [ one ] ->
                string w "LanguagePrimitives.GenericOne / "
                writeExprInParens w WriteState.Inline one
            | rest -> writeSeq w WriteState.Inline (flip string " / ") writeExprInParens rest
        | SynOp.Minus(args, r) ->
            startExpr w st r

            match args with
            | [ one ] ->
                char w '-'
                writeExpr w opState one

            | rest -> writeSeq w WriteState.Inline (flip string " - ") writeExpr rest


        ()

    and private writeSynLambda w (st: WriteState) (SynLambda(args, body, range)) =
        startExpr w st range
        string w "(fun "
        writeArgsOrEmpty w args
        string w " ->"
        writeBody w writeExpr body
        string w ")"
        ()

    and private writeCallArgs w (args: SynExpr list) =
        if args.IsEmpty then
            string w " ()"
        else
            match args with
            | SynExpr.Tuple _ as arg :: [] ->
                space w
                writeExpr w WriteState.InlineNoParens arg
            | _ -> writeSeqLeading w WriteState.Inline space writeExprInParens args

    and writeArgsOrEmpty w (args: SynArg list) =
        let writeArgs w args =
            writeSeq w WriteState.Inline space writeArg args

        if args.IsEmpty then string w "()" else writeArgs w args

    and private writeArg w _ (arg: SynArg) =
        match arg with
        | SynArg.TypedArg(name, typ, _) ->
            char w '('
            symbol w name true
            string w ": "
            writeType w typ
            char w ')'
        | SynArg.InferredArg(name, _) -> symbol w name true

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


    and private writeQuoted w st (expr: SynQuoted) =
        match expr with
        | SynQuoted.Const(cnst, range) ->
            startExpr w st range
            writeConst w true cnst
        | SynQuoted.EmptyList range ->
            startExpr w st range
            string w "[]"
        | SynQuoted.List(items, range) ->
            startExpr w st range
            string w "Value.from(["
            writeSeq w WriteState.Inline (flip char ';') writeQuoted items
            string w "])"
        | SynQuoted.Keyword(SynKeyword id) ->
            startExpr w st id.idRange
            fmtprintf w "Value.keyword(\"%s\")" id.idText

        | SynQuoted.Symbol(SynSymbol id) ->
            startExpr w st id.idRange
            fmtprintf w "Value.symbol(\"%s\")" id.idText

        | SynQuoted.Vector(items, range) ->
            use _ = startNewlineExpr w st range
            string w "( "
            fmtprintf w "let temp = Vector(%i)" items.Length

            use _ = withIndent w false

            writeSeqLeading
                w
                WriteState.Newline
                newlineIndent
                (fun ws _ a ->
                    string w "temp.Add(Value.from("
                    writeQuoted ws WriteState.Arg a
                    string w "))")
                items

            newlineIndent w
            string w "Value.vector(temp))"

        | SynQuoted.HashSet(items, range) ->
            startExpr w st range
            string w "["
            writeInlineSemicolon w writeQuoted items
            string w "] |> HashSet.ofList"

        | SynQuoted.HashMap(items, range) ->
            startExpr w st range
            string w "["

            writeSeq
                w
                WriteState.Inline
                (flip char ';')
                (fun w st (key, value) ->
                    char w '('
                    writeQuoted w st key
                    char w ','
                    char w ' '
                    writeQuoted w st value
                    char w ')'
                    ())
                (pairUp items)

            string w "] |> HashMap.ofList"

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
                | SynModuleDecl.Expr(ex, _) -> writeExpr w WriteState.Body ex
                | SynModuleDecl.Open(target, range) ->
                    indent w
                    lineof w range
                    indent w
                    fmtprintfn w "open %s" (Syntax.textOfSymbol target)
                    ()
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
