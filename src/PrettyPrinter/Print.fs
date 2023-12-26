// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module PrettyPrinter.Print

open System.IO

// https://github.com/minad/wl-pprint-annotated/blob/master/src/Text/PrettyPrint/Annotated/WL.hs

[<NoEquality; NoComparison>]
type Doc<'a> =
    | Empty
    | Line
    | Char of char
    | Text of int * string
    | FlatAlt of lhs: Doc<'a> * rhs: Doc<'a>
    | Cat of lhs: Doc<'a> * rhs: Doc<'a>
    | Nest of indent: int * Doc<'a>
    | Union of lhs: Doc<'a> * rhs: Doc<'a>
    | Annotate of value: 'a * doc: Doc<'a>
    | Column of (int -> Doc<'a>)
    | Nesting of (int -> Doc<'a>)
    | Columns of (int option -> Doc<'a>)
    | Ribbon of (int option -> Doc<'a>)


[<NoEquality; NoComparison>]
type SimpleDoc<'a> =
    | SEmpty
    | SChar of char * SimpleDoc<'a>
    | SText of int * string * SimpleDoc<'a>
    | SLine of int * SimpleDoc<'a>
    | SPushAnn of 'a * SimpleDoc<'a>
    | SPopAnn of 'a * SimpleDoc<'a>

[<NoEquality; NoComparison>]
type Docs<'a, 'e> =
    | Nil
    | Cons of int * Doc<'a> * Docs<'a, 'e>

type IToDoc =
    abstract member ToDoc: unit -> Doc<'a>

let mkChar a = Char a
let mkText i a = Text(i, a)
let mkFlatAlt a b = FlatAlt(a, b)
let mkCat a b = Cat(a, b)
let mkNest a b = Nest(a, b)
let mkAnnotate a b = Annotate(a, b)
let mkUnion a b = Union(a, b)
let mkColumn a = Column a
let mkNesting a = Nesting a
let mkColumns a = Columns a
let mkRibbon a = Ribbon a
let annotate = mkAnnotate
let mempty<'a> : Doc<'a> = Empty
let mappend<'a> a b : Doc<'a> = Cat(a, b)

/// <summary>
/// The document <c>char c</c> contains the literal character <c>c</c>. The
/// character shouldn't be a newline (<c>'\n'</c>), the function 'line'
/// should be used for line breaks.
/// </summary>
let char<'a> (s: char) : Doc<'a> = if s = '\n' then Line else Char s

/// <summary>
/// The document <c>text s</c> contains the literal string <c>s</c>. The
/// string shouldn't contain any newline (<c>'\n'</c>) characters. If the
/// string contains newline characters, the function 'pretty' should be
/// used.
/// </summary>
let text<'a> (s: string) : Doc<'a> =
    if s = "" then Empty else Text(s.Length, s)

/// <summary>
/// The document <c>space</c> contains a single space, " ".
/// <para>
/// Example: <c>x <+> y = x &lt;&gt; space &lt;&gt; y</c>
/// </para>
/// </summary>
let space<'a> : Doc<'a> = char ' '

/// <summary>
/// The <c>line</c> document advances to the next line and indents to the
/// current nesting level. Document <c>line</c> behaves like <c>text " "</c>
/// if the line break is undone by 'group'.
/// </summary>
let line<'a> : Doc<'a> = FlatAlt(Line, space)

/// <summary>
/// The <c>linebreak</c> document advances to the next line and indents to
/// the current nesting level. Document <c>linebreak</c> behaves like
/// <c>mempty</c> if the line break is undone by 'group'.
/// </summary>
let linebreak<'a> : Doc<'a> = FlatAlt(Line, mempty)

/// <summary>
/// A linebreak that cannot be flattened; it is guaranteed to be
/// rendered as a newline.
/// </summary>
let hardline<'a> : Doc<'a> = Line

let flatAlt = mkFlatAlt

let nest = mkNest

let column = mkColumn
let nesting = mkColumn
let columns = mkColumns
let ribbon = mkRibbon

let rec flatten<'a> (a: Doc<'a>) : Doc<'a> =
    match a with
    | Empty -> a
    | Line -> a
    | Char _ -> a
    | Text _ -> a
    | FlatAlt(_, y) -> y
    | Cat(x, y) -> Cat(flatten x, flatten y)
    | Nest(i, x) -> Nest(i, flatten x)
    | Union(x, _) -> flatten x
    | Annotate(a, x) -> Annotate(a, (flatten x))
    | Column f -> mkColumn (f >> flatten)
    | Nesting f -> mkNesting (f >> flatten)
    | Columns f -> mkColumns (f >> flatten)
    | Ribbon f -> mkRibbon (f >> flatten)

/// <summary>
/// The <c>group</c> combinator is used to specify alternative layouts.
/// The document <c>group x</c> undoes all line breaks in document <c>x</c>.
/// The resulting line is added to the current line if that fits the page. Otherwise, the document <c>x</c>
/// is rendered without any changes.
/// </summary>
/// <param name="x">The document to potentially flatten.</param>
/// <returns>A document that represents either a flattened version of <paramref name="x"/> or the original <paramref name="x"/>.</returns>
let group<'a> (x: Doc<'a>) : Doc<'a> = Union((flatten x), x)

/// <summary>
/// The document <c>softline</c> behaves like <c>space</c> if the resulting output fits the page,
/// otherwise it behaves like <c>line</c>.
/// <para>
/// Example: <c>softline = group line</c>
/// </para>
/// </summary>
let softline<'a> : Doc<'a> = group line

/// <summary>
/// The document <c>softbreak</c> behaves like <c>mempty</c> if the resulting output fits the page,
/// otherwise it behaves like <c>line</c>.
/// <para>
/// Example: <c>softbreak = group linebreak</c>
/// </para>
/// </summary>
let softbreak<'a> : Doc<'a> = group linebreak

/// <summary>
/// The document contains a left parenthesis, "(".
/// </summary>
let lparen<'a> : Doc<'a> = char '('

/// <summary>
/// The document contains a right parenthesis, ")".
/// </summary>
let rparen<'a> : Doc<'a> = char ')'

/// <summary>
/// The document contains a left angle, "<".
/// </summary>
let langle<'a> : Doc<'a> = char '<'

/// <summary>
/// The document contains a right angle, ">".
/// </summary>
let rangle<'a> : Doc<'a> = char '>'

/// <summary>
/// The document contains a left brace, "{".
/// </summary>
let lbrace<'a> : Doc<'a> = char '{'

/// <summary>
/// The document contains a right brace, "}".
/// </summary>
let rbrace<'a> : Doc<'a> = char '}'

/// <summary>
/// The document contains a left square bracket, "[".
/// </summary>
let lbracket<'a> : Doc<'a> = char '['

/// <summary>
/// The document contains a right square bracket, "]".
/// </summary>
let rbracket<'a> : Doc<'a> = char ']'

/// <summary>
/// The document contains a single quote, "'".
/// </summary>
let squote<'a> : Doc<'a> = char '\''

/// <summary>
/// The document contains a double quote, "\"".
/// </summary>
let dquote<'a> : Doc<'a> = char '"'

/// <summary>
/// The document contains a semi colon, ";".
/// </summary>
let semi<'a> : Doc<'a> = char ';'

/// <summary>
/// The document contains a colon, ":".
/// </summary>
let colon<'a> : Doc<'a> = char ':'

/// <summary>
/// The document contains a comma, ",".
/// </summary>
let comma<'a> : Doc<'a> = char ','

/// <summary>
/// The document contains a single dot, ".".
/// </summary>
let dot<'a> : Doc<'a> = char '.'

/// <summary>
/// The document contains a back slash, "\".
/// </summary>
let backslash<'a> : Doc<'a> = char '\\'

/// <summary>
/// The document contains an equal sign, "=".
/// </summary>
let equals<'a> : Doc<'a> = char '='

/// <summary>
/// x  mkCat  y
/// </summary>
let (<->) = mkCat

/// <summary>
/// x  space  y
/// </summary>
let (<+>) x y = x <-> space <-> y

/// <summary>
/// x  softline  y
/// </summary>
let (</>) x y = x <-> softline <-> y

/// <summary>
/// x  softbreak  y
/// </summary>
let (<//>) x y = x <-> softbreak <-> y

/// <summary>
/// The document <c>(x &lt;~&gt; y)</c> concatenates document <c>x</c> and <c>y</c> with a
/// 'line' in between. (infixr 5)
/// </summary>
let (<~>) x y = x <-> line <-> y

/// <summary>
/// The document <c>(x &lt;~~&gt; y)</c> concatenates document <c>x</c> and <c>y</c> with
/// a <c>linebreak</c> in between. (infixr 5)
/// </summary>
let (<~~>) x y = x <-> linebreak <-> y

let fold f xs =
    if Seq.isEmpty xs then mempty else Seq.reduceBack f xs

/// <summary>
/// The document <c>hcat xs</c> concatenates all documents <c>xs</c>
/// horizontally with <c>(&lt;-&gt;)</c>.
/// </summary>
let hcat xs = fold (<->) xs

/// <summary>
/// The document <c>vcat xs</c> concatenates all documents <c>xs</c>
/// vertically with <c>(&lt;~~&gt;)</c>. If a 'group' undoes the line breaks
/// inserted by <c>vcat</c>, all documents are directly concatenated.
/// </summary>
let vcat xs = fold (<~~>) xs

/// <summary>
/// The document <c>fillCat xs</c> concatenates documents <c>xs</c>
/// horizontally with <c>(&lt;-&gt;)</c> as long as it fits the page, then inserts
/// a <c>linebreak</c> and continues doing that for all documents in <c>xs</c>.
/// Equivalent to <c>foldr (&lt;//&gt;) mempty xs</c>.
/// </summary>
let fillCat xs = fold (<//>) xs

/// <summary>
/// The document <c>vsep xs</c> concatenates all documents <c>xs</c>
/// vertically with <c>(&lt;#&gt;)</c>. If a 'group' undoes the line breaks
/// inserted by <c>vsep</c>, all documents are separated with a space.
/// </summary>
let vsep xs = fold (<~>) xs

/// <summary>
/// The document <c>hsep xs</c> concatenates all documents <c>xs</c>
/// horizontally with <c>(&lt;+&gt;)</c>.
/// </summary>
let hsep xs = fold (<+>) xs

/// <summary>
/// The document <c>cat xs</c> concatenates all documents <c>xs</c> either
/// horizontally with <c>(&lt;-&gt;)</c>, if it fits the page, or vertically with
/// <c>(&lt;##&gt;)</c>. Equivalent to <c>group (vcat xs)</c>.
/// </summary>
let cat xs = group (vcat xs)

/// <summary>
/// The document <c>sep xs</c> concatenates all documents <c>xs</c> either
/// horizontally with <c>(&lt;+&gt;)</c>, if it fits the page, or vertically with
/// <c>(&lt;#&gt;)</c>. Equivalent to <c>group (vsep xs)</c>.
/// </summary>
let sep xs = group (vsep xs)

/// <summary>
/// The document <c>fillSep xs</c> concatenates documents <c>xs</c>
/// horizontally with <c>(&lt;+&gt;)</c> as long as it fits the page, then
/// inserts a <c>line</c> and continues doing that for all documents in
/// <c>xs</c>. Equivalent to <c>foldr (&lt;/&gt;) mempty xs</c>.
/// </summary>
let fillSep xs = fold (</>) xs

/// <summary>
/// The document <c>enclose l r x</c> encloses document <c>x</c> between
/// documents <c>l</c> and <c>r</c> using <c>(&lt;-&gt;)</c>. Equivalent to
/// <c>l &lt;-&gt; x &lt;-&gt; r</c>.
/// </summary>
let enclose l r x = l <-> x <-> r

/// <summary>
/// Encloses document x with single quotes "'".
/// </summary>
let squotes<'a> (x: Doc<'a>) : Doc<'a> = enclose squote squote x

/// <summary>
/// Encloses document x with double quotes "\"".
/// </summary>
let dquotes<'a> (x: Doc<'a>) : Doc<'a> = enclose dquote dquote x

/// <summary>
/// Encloses document x in braces "{" and "}".
/// </summary>
let braces<'a> (x: Doc<'a>) : Doc<'a> = enclose lbrace rbrace x

/// <summary>
/// Encloses document x in parenthesis "(" and ")".
/// </summary>
let parens<'a> (x: Doc<'a>) : Doc<'a> = enclose lparen rparen x

/// <summary>
/// Encloses document x in angles "<" and ">".
/// </summary>
let angles<'a> (x: Doc<'a>) : Doc<'a> = enclose langle rangle x

/// <summary>
/// Encloses document x in square brackets "[" and "]".
/// </summary>
let brackets<'a> (x: Doc<'a>) : Doc<'a> = enclose lbracket rbracket x

/// <summary>
/// Generates a string consisting of a specified number of spaces.
/// </summary>
/// <param name="n">The number of spaces to generate.</param>
/// <returns>A string containing <paramref name="n"/> spaces.</returns>
let spaces n =
    match n with
    | n when n <= 0 -> ""
    | n when n = 1 -> " "
    | n when n = 2 -> "  "
    | n when n = 3 -> "   "
    | n when n = 4 -> "    "
    | _ -> new string (' ', n)

/// <summary>
/// Renders the document <c>x</c> with the nesting level set to the current column.
/// This function is used, for example, to implement 'hang'.
/// </summary>
/// <param name="d">The document to align.</param>
/// <returns>The aligned document.</returns>
let align d =
    column (fun k -> nesting (fun i -> nest (k - i) d))

/// <summary>
/// Implements hanging indentation. The document <c>hang i x</c> renders the document <c>x</c>
/// with a nesting level set to the current column plus <c>i</c>.
/// </summary>
/// <param name="i">The indentation level.</param>
/// <param name="d">The document to hang.</param>
/// <returns>The document with hanging indentation applied.</returns>
let hang i d = align (nest i d)

/// <summary>
/// Indents the document <c>x</c> with <c>i</c> spaces. For example:
/// <code>
/// let test = indent 4 (fillSep (map text (words "the indent combinator indents these words !")))
/// </code>
/// This will layout with a page width of 20 as:
/// <code>
///     the indent
///     combinator
///     indents these
///     words !
/// </code>
/// </summary>
/// <param name="i">The number of spaces to indent with.</param>
/// <param name="d">The document to indent.</param>
/// <returns>The indented document.</returns>
let indent i d = hang i (text (spaces i) <-> d)

let repeat x = Seq.initInfinite (fun _ -> x)

let encloseSep left right sep initialDocs =
    let docs = Seq.toList initialDocs

    match docs with
    | [] -> left <-> right
    | [ d ] -> left <-> d <-> right
    | ds ->
        let left' = left <-> flatAlt space mempty
        let right' = flatAlt space mempty <-> right
        let combineWithSep d acc = sep <-> space <-> d <-> acc

        let separatedDocs = Seq.map2 combineWithSep (Seq.append [ mempty ] (repeat sep)) ds

        group (align (left' <-> (vcat separatedDocs) <-> right'))

let docMapAnn<'a> (an: 'a -> Doc<'a> -> Doc<'a>) (doc: Doc<'a>) : Doc<'a> =
    let rec go doc =
        match doc with
        | Empty -> Empty
        | Char x -> Char x
        | Text(i, s) -> Text(i, s)
        | Line -> Line
        | FlatAlt(l, r) -> FlatAlt(go l, go r)
        | Cat(l, r) -> Cat(go l, go r)
        | Nest(i, d) -> Nest(i, go d)
        | Union(l, r) -> Union(go l, go r)
        | Annotate(a, d) -> an a (go d)
        | Column f -> Column(f >> go)
        | Nesting k -> Nesting(k >> go)
        | Columns k -> Columns(k >> go)
        | Ribbon k -> Ribbon(k >> go)

    go doc

let noAnnotate (doc: Doc<'a>) : Doc<'a> =
    let removeAnnotation _ d = d
    docMapAnn removeAnnotation doc

let renderFits
    (nicest: int -> int -> int -> int -> SimpleDoc<'a> -> SimpleDoc<'a> -> SimpleDoc<'a>)
    (rfrac: float)
    (w: int)
    (x: Doc<'a>)
    : SimpleDoc<'a> =
    let r = max 0 (min w (int (float w * rfrac)))

    let rec best
        (n: int)
        (k: int)
        (z: int -> int -> SimpleDoc<'a>)
        (docs: Docs<'a, 'e>)
        : SimpleDoc<'a> =
        match docs with
        | Nil -> z n k
        | Cons(i, d, ds) ->
            match d with
            | Empty -> best n k z ds
            | Char c -> let k' = k + 1 in SChar(c, best n k' z ds)
            | Text(l, s) -> let k' = k + l in SText(l, s, best n k' z ds)
            | Line -> SLine(i, best i i z ds)
            | FlatAlt(l, _) -> best n k z (Cons(i, l, ds))
            | Cat(x', y) -> best n k z (Cons(i, x', Cons(i, y, ds)))
            | Nest(j, x') -> let i' = i + j in best n k z (Cons(i', x', ds))
            | Annotate(a, d') ->
                let z' n' k' = SPopAnn(a, best n' k' z ds)
                SPushAnn(a, best n k z' (Cons(i, d', Nil)))
            | Union(p, q) ->
                nicest n k w r (best n k z (Cons(i, p, ds))) (best n k z (Cons(i, q, ds)))
            | Column f -> best n k z (Cons(i, f k, ds))
            | Nesting f -> best n k z (Cons(i, f i, ds))
            | Columns f -> best n k z (Cons(i, f (Some w), ds))
            | Ribbon f -> best n k z (Cons(i, f (Some r), ds))

    best 0 0 (fun _ _ -> SEmpty) (Cons(0, x, Nil))

/// <summary>
/// Compares the first lines of the two documents.
/// </summary>
/// <param name="n">Nesting level.</param>
/// <param name="k">Current column.</param>
/// <param name="p">Page width.</param>
/// <param name="r">Ribbon width.</param>
/// <param name="x'">First SimpleDoc to compare.</param>
/// <param name="y">Second SimpleDoc to compare.</param>
/// <returns>The SimpleDoc that fits better within the given widths.</returns>
let nicest1
    (n: int)
    (k: int)
    (p: int)
    (r: int)
    (x': SimpleDoc<'a>)
    (y: SimpleDoc<'a>)
    : SimpleDoc<'a> =
    let wid = min (p - k) (r - k + n)

    let rec fits (m: int) (w: int) (doc: SimpleDoc<'a>) : bool =
        match doc with
        | SEmpty -> true
        | SChar(_, x) -> fits m (w - 1) x
        | SText(l, _, x) -> fits m (w - l) x
        | SLine _ -> true
        | SPushAnn(_, x) -> fits m w x
        | SPopAnn(_, x) -> fits m w x

    if fits (min n k) wid x' then x' else y

/// <summary>
/// Compares the initial lines of the two documents that are nested at least as deep as the current nesting level.
/// If the initial lines of both documents fit within the page width, the document that takes fewer lines is preferred,
/// with a preference toward the first.
/// </summary>
/// <param name="n">Nesting level.</param>
/// <param name="k">Current column.</param>
/// <param name="p">Page width.</param>
/// <param name="r">Ribbon width.</param>
/// <param name="x'">First SimpleDoc to compare.</param>
/// <param name="y">Second SimpleDoc to compare.</param>
/// <returns>The SimpleDoc that fits better within the given widths.</returns>
let nicestR
    (n: int)
    (k: int)
    (p: int)
    (r: int)
    (x': SimpleDoc<'a>)
    (y: SimpleDoc<'a>)
    : SimpleDoc<'a> =
    let wid = min (p - k) (r - k + n)
    let inf = System.Double.PositiveInfinity

    /// <summary>
    /// Determines how well a document fits within a given width.
    /// </summary>
    /// <param name="m">Minimum nesting level to fit in.</param>
    /// <param name="w">The width in which to fit the first line.</param>
    /// <param name="doc">The document to fit.</param>
    /// <returns>The number of lines the document takes when fitting within the given width.</returns>
    let rec fits (m: int) (w: int) (doc: SimpleDoc<'a>) : float =
        match doc with
        | _ when w < 0 -> inf
        | SEmpty -> 0.0
        | SChar(_, x) -> fits m (w - 1) x
        | SText(l, _, x) -> fits m (w - l) x
        | SLine(i, x) when m < i -> 1.0 + fits m (p - i) x
        | SLine(_) -> 0.0
        | SPushAnn(_, x) -> fits m w x
        | SPopAnn(_, x) -> fits m w x

    if fits (min n k) wid x' <= fits (min n k) wid y then
        x'
    else
        y

/// <summary>
/// This is the default pretty printer which is used by 'show', 'putDoc', and 'hPutDoc'.
/// Renders document 'x' with a page width of 'width' and a ribbon width of
/// (ribbonfrac * width) characters. The ribbon width is the maximal amount of
/// non-indentation characters on a line. The parameter 'ribbonfrac' should be
/// between 0.0 and 1.0. If it is lower or higher, the ribbon width will be 0 or
/// 'width' respectively.
/// </summary>
/// <param name="ribbonfrac">Fraction of the page width used for the ribbon width.</param>
/// <param name="width">Page width in characters.</param>
/// <param name="x">Document to render.</param>
/// <returns>A SimpleDoc representing the rendered document.</returns>
let renderPretty (ribbonfrac: float) (width: int) (x: Doc<'a>) : SimpleDoc<'a> =
    renderFits nicest1 ribbonfrac width x

/// <summary>
/// This is the default pretty printer which is used by 'show', 'putDoc', and 'hPutDoc'.
/// This routine uses a page width of 100 characters and a ribbon width of 40 characters.
/// </summary>
/// <param name="x">Document to render.</param>
/// <returns>A SimpleDoc representing the rendered document with default settings.</returns>
let renderPrettyDefault (x: Doc<'a>) : SimpleDoc<'a> = renderPretty 0.4 100 x

/// <summary>
/// Maps annotations in a SimpleDoc, allowing for stateful processing of annotations.
/// </summary>
/// <param name="upPush">Function to update the state when encountering SPushAnn.</param>
/// <param name="upPop">Function to update the state when encountering SPopAnn.</param>
/// <param name="push">Function to process SPushAnn with the updated state.</param>
/// <param name="pop">Function to process SPopAnn with the updated state.</param>
/// <param name="initState">Initial state.</param>
/// <param name="doc">SimpleDoc to process.</param>
/// <returns>A new SimpleDoc with processed annotations.</returns>
let simpleDocMapAnn upPush upPop push pop initState (doc: SimpleDoc<'a>) : SimpleDoc<'a> =
    let rec go state simpleDoc =
        match simpleDoc with
        | SEmpty -> SEmpty
        | SChar(c, x) -> SChar(c, (go state x))
        | SText(l, s, x) -> SText(l, s, (go state x))
        | SLine(i, x) -> SLine(i, (go state x))
        | SPushAnn(a, x) ->
            let newState = upPush state a
            push newState (go newState x)
        | SPopAnn(a, x) ->
            let newState = upPop state a
            pop newState (go newState x)

    go initState doc

let mkPushAnn a r = SPushAnn(a, r)
let mkPopAnn a r = SPopAnn(a, r)

/// <summary>
/// Scans annotations in a SimpleDoc, merging state information into the annotations.
/// </summary>
/// <param name="mergeState">Function to merge state when encountering SPushAnn.</param>
/// <param name="initState">Initial state.</param>
/// <param name="doc">SimpleDoc to scan.</param>
/// <returns>A new SimpleDoc with merged state annotations.</returns>
let simpleDocScanAnn mergeState initState (doc: SimpleDoc<'a>) : SimpleDoc<'a> =
    // Helper functions to manage the state stack
    let merge stateStack a =
        match stateStack with
        | r :: _ -> (mergeState r a) :: stateStack
        | [] -> failwith "merge Stack underflow " // Error: Stack underflow

    let pop stateStack _ =
        match stateStack with
        | _ :: rs -> rs
        | [] -> failwith "pop Stack underflow" // Error: Stack underflow

    // Using simpleDocMapAnn with a state stack to scan annotations
    simpleDocMapAnn merge pop (List.head >> mkPushAnn) (List.head >> mkPopAnn) [ initState ] doc

type System.String with

    member this.pretty() = text this

let rec writeSimpleDocAnn
    (writer: TextWriter)
    (pushAnn: (TextWriter -> 'a -> unit) option)
    (popAnn: (TextWriter -> 'a -> unit) option)
    doc
    =

    match doc with
    | SEmpty -> ()
    | SChar(c, rest) ->
        writer.Write(c)
        writeSimpleDocAnn writer pushAnn popAnn rest
    | SText(_, s, rest) ->
        writer.Write(s)
        writeSimpleDocAnn writer pushAnn popAnn rest
    | SPushAnn(a, rest) ->
        match pushAnn with
        | None -> ()
        | Some(fn) -> fn writer a

        writeSimpleDocAnn writer pushAnn popAnn rest
    | SPopAnn(a, rest) ->
        match popAnn with
        | None -> ()
        | Some(fn) -> fn writer a

        writeSimpleDocAnn writer pushAnn popAnn rest
    | SLine(i, rest) ->
        // TODO: Should we use Environment.NewLine?
        writer.Write('\n')

        for _ in 1..i do
            writer.Write(' ')

        writeSimpleDocAnn writer pushAnn popAnn rest

/// <summary>
/// Writes the document into the given writer
/// </summary>
let writeSimpleDoc writer doc = writeSimpleDocAnn writer None None doc

/// <summary>
/// Writes the document into a string
/// </summary>
let displayString doc =
    use writer = new StringWriter()
    writeSimpleDoc writer doc
    writer.ToString()

let displayStringAnnotated pushAnn popAnn doc =
    use writer = new StringWriter()
    writeSimpleDocAnn writer (Some pushAnn) (Some popAnn) doc
    writer.ToString()

// -- | The document @(tupled xs)@ comma separates the documents @xs@ and
// -- encloses them in parenthesis. The documents are rendered
// -- horizontally if that fits the page. Otherwise they are aligned
// -- vertically. All comma separators are put in front of the elements.
// tupled :: Foldable f => f (Doc a) -> Doc a
let tupled xs = encloseSep lparen rparen comma xs


// -- | The document @(list xs)@ comma separates the documents @xs@ and
// -- encloses them in square brackets. The documents are rendered
// -- horizontally if that fits the page. Otherwise they are aligned
// -- vertically. All comma separators are put in front of the elements.
let list xs = encloseSep lbracket rbracket comma xs

// -- | The document @(semiBraces xs)@ separates the documents @xs@ with
// -- semi colons and encloses them in braces. The documents are rendered
// -- horizontally if that fits the page. Otherwise they are aligned
// -- vertically. All semi colons are put in front of the elements.
// semiBraces :: Foldable f => f (Doc a) -> Doc a
let semiBraces xs = encloseSep lbrace rbrace semi xs

type System.String with

    member t.ToDoc() = text t
