// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Syntax.SynWriter

open Visp.Compiler.Writer
open Visp.Common
open Visp.Compiler.Syntax
open Visp.Compiler.Text
open System.Globalization

open Visp.Runtime.Library.CompileHelpers

type SynWriter(writer: CustomFileWriter) =
    member _.knownMethods = getMethods ()

    member _.writer = writer

    member this.Write(text: string) = this.writer.Write(text)

    member this.Write(text: char) = this.writer.Write(text)

    member this.Write(it: int32) = this.writer.Write(it)

    member this.Write(it: decimal) =
        this.writer.Write(it.ToString(CultureInfo.InvariantCulture))


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

// type WriteState =
//     { indent: bool
//       line: bool
//       parens: bool
//       newline: bool }

// let wsNoneNoParens =
//     { indent = false
//       line = false
//       parens = false
//       newline = true }

// let wsNone =
//     { indent = false
//       line = false
//       parens = true
//       newline = true }

// let WriteState.Body =
//     { indent = true
//       line = true
//       parens = true
//       newline = true }


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
        [ "then"; "done"; "val"; "end"; "begin"; "mod"; "to"; "with" ] |> Set.ofList

    let escapableChars = [ '?'; '-'; '+'; '*'; '/'; '!'; ':' ] |> Set.ofList

    let name (w: SynWriter) (name: string) (escape: bool) =
        let mutable sb = PooledStringBuilder.Get()
        let mutable needs_escape = false

        if Set.contains name reservedWords then
            sb <- sb.Append('_')
            ()

        for ch in name do
            if Set.contains ch escapableChars then
                needs_escape <- true

            sb <- sb.Append(ch)

        if escape && needs_escape then
            w.writer.Write("``")

        w.writer.Write(PooledStringBuilder.ToStringAndReturn(sb))

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

        match cnst with
        | SynConst.String(str, kind, _) ->
            match kind with
            | SynStringKind.Regular -> surroundWithCh w '"' (flip string str) '"'
            | SynStringKind.TripleQuote -> surroundWithString w "\"\"\"" (flip string str) "\"\"\""
            | SynStringKind.Verbatim -> failwith "unsupported"

        | SynConst.Int32(value) -> w.Write(value)
        | SynConst.Int64(value) ->
            w.Write(value)
            w.Write('L')
        | SynConst.Decimal(value) -> w.Write(value)
        | SynConst.Char(value) ->
            w.Write('\'')

            if (value = '\\') then
                w.Write(value)

            w.Write(value)
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
                        | SynMatchPattern.CommaOrDot _ -> failwith "not supported"
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

        | SynExpr.ForIn(name, bind, body, range) ->
            use _ = startNewlineExpr w st range
            string w "for "
            writeName w WriteState.Inline name
            string w " in "
            writeExpr w WriteState.Inline bind
            string w " do"
            writeBody w writeExpr body
            ()

        | SynExpr.FsYield(expr, range) ->
            startExpr w st range
            string w "yield "
            writeExprInParens w WriteState.Inline expr

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

        | SynExpr.SimpleLet(name, body, range) ->
            startExpr w st range
            writeLet w st name body

        | SynExpr.Keyword kw -> writeQuoted w st (SynQuoted.Keyword kw)

        | SynExpr.Begin(expr, range) ->
            startExpr w st range
            char w '('
            writeBody w writeExpr expr
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

            string w "let mutable "
            synName w name
            string w " ="
            use _ = withIndent w false
            newline w
            writeExpr w WriteState.Body body
            ()

        | SynExpr.Set(name, body, range) ->
            startExpr w st range

            // symbol w name true
            writeExpr w WriteState.InlineNoParens name
            string w " <- "
            writeExpr w WriteState.Inline body
            ()

        | SynExpr.FsVec(items, range) ->
            startExpr w st range
            fmtprintf w "let temp = ResizeArray<_>(%i)" items.Length

            writeSeqLeading
                w
                WriteState.Newline
                newlineIndent
                (fun ws _ a ->
                    string w "temp.Add("
                    writeExpr ws WriteState.Arg a
                    string w ")")
                items

            newlineIndent w
            string w "temp"


        | SynExpr.Vector(items, range) ->
            startExpr w st range
            fmtprintf w "let temp = Vector(%i)" items.Length

            writeSeqLeading
                w
                WriteState.Newline
                newlineIndent
                (fun ws _ a ->
                    string w "temp.Add(Value.from("
                    writeExpr ws WriteState.Arg a
                    string w "))")
                items

            newlineIndent w
            string w "temp"

        | SynExpr.List(items, range) ->
            startExpr w st range
            string w "["
            writeInlineSeparated w ";" writeExpr items
            string w "]"

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


        | SynExpr.FsArray(items, range) ->
            startExpr w st range
            string w "[|"
            writeInlineSemicolon w writeExpr items
            string w "|]"

        | SynExpr.FsSet(items, range) ->
            startExpr w st range
            string w "["
            writeInlineSemicolon w writeExpr items
            string w "] |> Set.ofList"

        | SynExpr.FsMap(items, range) ->
            startExpr w st range
            string w "["
            writeInlineSemicolon w writeExpr items
            string w "] |> Map.ofList"

        | SynExpr.HashSet(items, range) ->
            startExpr w st range
            string w "["
            writeInlineSemicolon w writeExprToValue items
            string w "] |> HashSet.ofList"

        | SynExpr.HashMap(items, range) ->
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

        | SynExpr.Const(cnst, _) ->
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
                    fmtprintf w "%s.``%s``(" method.DeclaringType.Name method.Name

                    if isVariableArgMethod method then
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
                        writeArgComma
                            w
                            (fun wt stt ex ->
                                string wt "Value.from("
                                writeExpr wt stt ex
                                string wt ")")
                            args
                    else if hasParamArrayAttribute method then
                        writeSeq w WriteState.Arg (flip string ", ") writeExpr args
                    else
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
                writeArgSpace w writeExprInParens args
                ()
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
            writeExpr w WriteState.Inline prop
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

        | SynExpr.Type(name, args, members, attributes, range) ->
            if not attributes.IsEmpty then
                writeAttributes w st attributes
                newline w

            startExpr w st range
            fmtprintf w "type %s" (Syntax.textOfSymbol name)

            if args.IsEmpty then
                string w " ()"
            else
                string w " ("
                writeSeq w WriteState.InlineNoParens (flip string ", ") writeName args
                string w ")"

            string w " ="
            writeBody w writeMember members

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


    and private writeLet w (st: WriteState) (name: SynName) (body: SynExpr) =
        string w "let "
        synName w name
        string w " ="

        let should_indent =
            match body with
            | SynExpr.Const _
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

        ()

    and private writeExprInParens w (st: WriteState) ex =
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
            | [] -> string w "1"
            | [ one ] -> writeExpr w st one
            | rest -> writeSeq w WriteState.Inline (flip string " * ") writeExpr rest
        | SynOp.Div(args, r) ->
            startExpr w st r

            match args with
            | [ one ] ->
                string w "1.0m / (decimal "
                writeExpr w WriteState.Inline one
                string w ")"
            | rest ->
                writeSeq
                    w
                    WriteState.Inline
                    (flip string " / ")
                    (fun w st a ->
                        string w "(decimal "
                        writeExpr w st a
                        string w ")")
                    rest
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

        // if body.Length = 1 then
        //     space w
        //     for ex in body do
        //         writeExpr w wsNone ex
        // else
        // use _ = withIndent w false
        writeBody w writeExpr body

        string w ")"
        ()

    and private writeCallArgs w (args: SynExpr list) =
        if args.IsEmpty then
            string w " ()"
        else
            writeSeqLeading
                w
                WriteState.Inline
                space
                (fun w st a ->
                    char w '('
                    writeExpr w st a
                    char w ')')
                args

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


    let private tfs =
        [| Visp.Compiler.Transforms.SyntaxMacros.expand
           Visp.Compiler.Transforms.QuasiquoteExpander.expand
           Visp.Compiler.Transforms.BuiltinMacros.expand
           Visp.Compiler.Transforms.Common.transformLambdaShortHands |]

    let expandExpr expr =
        Visp.Compiler.Transforms.Helpers.runTransforms tfs expr

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
                | SynModuleDecl.Expr(ex, _) -> writeExpr w WriteState.Body (expandExpr ex)
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
