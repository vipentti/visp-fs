module Visp.LanguageServer

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading

open Microsoft.VisualStudio.LanguageServer.Protocol
open SpanUtils.Extensions
open StreamJsonRpc

open Visp.Common
open Visp.Compiler
open Visp.Compiler.Syntax

let NormalizePath (str: string) =
    let root = Path.GetPathRoot(str)

    let lowerRoot = root.ToLowerInvariant().Replace(":", "%3A")

    let file = str.Replace(root, lowerRoot).Replace('\\', '/')

    if file.StartsWith('/') then file else "/" + file

let ToFileUri (str: string) =
    str |> NormalizePath |> (+) "file://" |> Uri

type ICommonEvents =
    abstract member Nothing: bool

type WordRange = WordRange of text: string * range: Range

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#traceValue
[<RequireQualifiedAccess>]
type TraceValue =
    | Off
    | Messages
    | Verbose

let tryGetWordRange (text: string) (line: int) (ch: int) =
    let span = text.AsSpan()

    let mutable enu = span.EnumerateSplitLines()
    let mutable index = -1
    let mutable isDone = false
    let mutable result = None

    // eprintfn "Looking for line: %i ch: %i" line ch

    while not isDone && enu.MoveNext() do
        index <- index + 1

        if line = index then
            let current = enu.Current
            let sb = PooledStringBuilder.Get()
            sb.Append(current) |> ignore

            let lineText = PooledStringBuilder.ToStringAndReturn(sb)
            let tokens = Core.CoreParser.getTokens (lineText) "temp"

            // eprintfn "Found line: '%s'" lineText

            let found =
                Seq.tryFind
                    (fun (x, sp: FSharp.Text.Lexing.Position, ep: FSharp.Text.Lexing.Position) ->
                        // eprintfn "Found token %A %A %A" x sp.Column ep.Column

                        match x with
                        | SyntaxParser.token.SYMBOL _ -> ch >= sp.Column && ch <= ep.Column
                        | _ -> false)
                    tokens

            match found with
            | Some(SyntaxParser.token.SYMBOL(text), sp, ep) ->
                result <-
                    Some(
                        WordRange(
                            text,
                            new Range(
                                Start = new Position(line, sp.Column),
                                End = new Position(line, ep.Column)
                            )
                        )
                    )

                // eprintfn "Found result: %A" result

                ()
            | _ -> ()

            isDone <- true

        ()

    result

type CustomEvents() =

    let onInitializeCompletion = new Event<EventHandler, EventArgs>()
    let onShutdown = new Event<EventHandler, EventArgs>()
    let onInitialized = new Event<EventHandler, EventArgs>()

    let onTextDocumentOpened = new Event<DidOpenTextDocumentParams>()

    [<CLIEvent>]
    member this.OnTextDocumentOpened = onTextDocumentOpened.Publish

    [<CLIEvent>]
    member this.OnInitialized = onInitialized.Publish

    member this.TriggerOnInitialized(sender: obj) =
        onInitialized.Trigger(sender, EventArgs.Empty)

    [<CLIEvent>]
    member this.OnInitializeCompletion = onInitializeCompletion.Publish

    member this.TriggerInitializeCompletion(sender: obj) =
        onInitializeCompletion.Trigger(sender, EventArgs.Empty)

    [<CLIEvent>]
    member this.OnShutdown = onShutdown.Publish

    member this.TriggerShutdown(sender: obj) =
        onShutdown.Trigger(sender, EventArgs.Empty)

let textRangeToSyntaxRange (r: Visp.Compiler.Text.range) =
    let mutable result = new Range()
    result.Start <- new Position(r.StartLine, r.StartColumn)
    result.End <- new Position(r.EndLine, r.EndColumn)
    result

let fmtMemberName (s: string) =
    let i = s.IndexOf '.'
    if i >= 0 then s.Substring(i + 1) else s

[<RequireQualifiedAccess>]
type SymbolDetails =
    | FsharpMethod of text: string * details: string * range: Range
    | Interop of text: string * range: Range
    | Function of text: string * range: Range
    | Variable of text: string * mut: bool * range: Range
    | Type of text: string * range: Range
    | Member of text: string * fn: bool * range: Range
    | Parameter of text: string * range: Range
    | Symbol of text: string * range: Range
    | SyntaxMacro of text: string * range: Range
    | Union of text: string * range: Range
    | UnionMember of text: string * fn: bool * range: Range

    member this.Text =
        match this with
        | SyntaxMacro(text = it)
        | FsharpMethod(text = it)
        | Function(text = it)
        | Interop(text = it)
        | Variable(text = it)
        | Union(text = it)
        | UnionMember(text = it)
        | Type(text = it)
        | Member(text = it)
        | Variable(text = it)
        | Symbol(text = it)
        | Parameter(text = it) -> it

    member this.Range =
        match this with
        | SyntaxMacro(range = it)
        | FsharpMethod(range = it)
        | Function(range = it)
        | Interop(range = it)
        | Member(range = it)
        | Union(range = it)
        | UnionMember(range = it)
        | Type(range = it)
        | Symbol(range = it)
        | Variable(range = it)
        | Parameter(range = it) -> it

    member this.SymbolKind =
        match this with
        | FsharpMethod _ -> SymbolKind.Null
        | Function _ -> SymbolKind.Function
        | SyntaxMacro _ -> SymbolKind.Function
        | Interop _ -> SymbolKind.Operator
        | Symbol _ -> SymbolKind.Method
        | Variable _ -> SymbolKind.Variable
        | Parameter _ -> SymbolKind.Field
        | Union _ -> SymbolKind.Enum
        | UnionMember _ -> SymbolKind.EnumMember
        | Type _ -> SymbolKind.Class
        | Member(fn = fn) -> if fn then SymbolKind.Method else SymbolKind.Field

    member this.CompletionItemKind =
        match this with
        | FsharpMethod _ -> CompletionItemKind.Method
        | Function _ -> CompletionItemKind.Function
        | Interop _ -> CompletionItemKind.Operator
        | Symbol _ -> CompletionItemKind.Method
        | Variable _ -> CompletionItemKind.Variable
        | Parameter _ -> CompletionItemKind.Field
        | SyntaxMacro _ -> CompletionItemKind.Macro
        | Union _ -> CompletionItemKind.Union
        | UnionMember _ -> CompletionItemKind.EnumMember
        | Type _ -> CompletionItemKind.Class
        | Member(fn = fn) ->
            if fn then
                CompletionItemKind.Method
            else
                CompletionItemKind.Field

    member this.SortPrefix =
        match this with
        | FsharpMethod _ -> "44444"
        | Interop _ -> "44444"
        | Function _ -> "44444"
        | Variable _ -> "44444"
        | Type _ -> "44444"
        | Member _ -> "44444"
        | Parameter _ -> "44444"
        | Symbol _ -> "44444"
        | SyntaxMacro _ -> "44444"
        | Union _ -> "44444"
        | UnionMember _ -> "44444"

    member this.SortText = this.SortPrefix + this.Text

    member this.Label =
        match this with
        | Interop(it, _) -> it.TrimStart('!')
        | Member(text = text) ->
            let i = text.IndexOf('.')
            if i >= 0 then text.Substring(i + 1) else text
        | _ -> this.Text

    member this.InsertText =
        match this with
        | Interop(it, _) -> it
        | Member(text, fn, _) ->
            if fn then
                "." + (fmtMemberName text)
            else
                "+" + (fmtMemberName text)
        | _ -> null

    member this.Detail =
        match this with
        | FsharpMethod(_, sg, _) -> sg
        | SyntaxMacro _ -> "syntax-macro"
        | Interop _ -> "F# interop"
        | Symbol _ -> "symbol"
        | Function _ -> "function"
        | Union _ -> "union"
        | UnionMember _ -> "union-member"
        | Variable(mut = mut) -> if mut then "mutable" else "variable"
        | Parameter _ -> "parameter"
        | Type _ -> "type"
        | Member(fn = fn) -> if fn then "member function" else "member"

    member this.ToCompletionItem(index: int) =
        let item = new CompletionItem()
        item.SortText <- this.SortText
        item.Label <- this.Label
        item.InsertText <- this.InsertText
        item.Kind <- this.CompletionItemKind
        item.Data <- index
        item.Detail <- this.Detail
        item

let rec textRangeOfPat =
    function
    | SynPat.Named(it, r) -> [ (it.Text, r |> textRangeToSyntaxRange) ]
    | SynPat.Typed(pat, _, _) -> textRangeOfPat pat
    | SynPat.Args(args, _) ->
        match args with
        | SynArgPats.List(pats)
        | SynArgPats.Tuple(pats) -> List.concat (List.map textRangeOfPat pats)
    | SynPat.As(lhs, rhs, _) -> (textRangeOfPat lhs) @ (textRangeOfPat rhs)
    | SynPat.Collection(SynCollection(_, its, _)) -> its |> List.map textRangeOfPat |> List.concat
    | SynPat.Record(fields, _) ->
        fields
        |> List.map (fun (name, pat) ->
            [ (name.Text, name.Range |> textRangeToSyntaxRange) ] @ textRangeOfPat pat)
        |> List.concat
    | SynPat.Const _ -> []
    | SynPat.IsInst _ -> []
    | SynPat.Trivia _ -> []
    | SynPat.Ignore _ -> []
    | SynPat.Discard _ -> []

let memberToSymbolDetails _ memval var (mem: SynTypeMember) =
    match mem with
    | SynTypeMember.Let(pat = name; flags = flags) ->
        textRangeOfPat name
        |> List.map (fun (a, r) -> var (a, flags.HasFlag(LetFlags.Mutable), r))
    | SynTypeMember.Val(pat = name) ->
        textRangeOfPat name |> List.map (fun (a, r) -> var (a, false, r))
    | SynTypeMember.Interface _ -> []
    | SynTypeMember.Constructor _ -> []
    | SynTypeMember.GetSet(name = name)
    | SynTypeMember.Member(name = name) ->
        [ (memval (
              Syntax.textOfSymbol name,
              false,
              Syntax.rangeOfSymbol name |> textRangeToSyntaxRange
          ))
          (memval (
              "+" + (fmtMemberName name.Text),
              false,
              Syntax.rangeOfSymbol name |> textRangeToSyntaxRange
          )) ]

    | SynTypeMember.MemberFn(name = name) ->
        [ (memval (
              Syntax.textOfSymbol name,
              true,
              Syntax.rangeOfSymbol name |> textRangeToSyntaxRange
          ))
          (memval (
              "." + (fmtMemberName name.Text),
              true,
              Syntax.rangeOfSymbol name |> textRangeToSyntaxRange
          )) ]


let findAllSymbolDetails (syms: ResizeArray<_>) expr =

    match expr with
    | SynExpr.FunctionCall(SynExpr.Symbol sym, _, _) ->
        let r = Syntax.rangeOfSymbol sym
        syms.Add(SymbolDetails.Symbol(Syntax.textOfSymbol sym, textRangeToSyntaxRange r))
    | SynExpr.SyntaxMacro(SynMacro(Patterns.Text text, _, range)) ->
        syms.Add(SymbolDetails.SyntaxMacro(text, textRangeToSyntaxRange range))
        ()

    | SynExpr.FunctionDef(name, _, args, _, _, _) ->

        let r = Syntax.rangeOfSymbol name

        syms.Add(SymbolDetails.Function(Syntax.textOfSymbol name, textRangeToSyntaxRange r))

        syms.AddRange(textRangeOfPat args |> List.map SymbolDetails.Parameter)

    | SynExpr.TypeAlias(name, _, _) ->
        syms.Add(
            SymbolDetails.Type(
                Syntax.textOfSymbol name,
                Syntax.rangeOfSymbol name |> textRangeToSyntaxRange
            )
        )

    | SynExpr.Record(name, _, members, _, _)
    | SynExpr.Type(name, _, members, _, _) ->
        syms.Add(
            SymbolDetails.Type(
                Syntax.textOfSymbol name,
                Syntax.rangeOfSymbol name |> textRangeToSyntaxRange
            )
        )

        syms.AddRange(
            members
            |> Seq.map (memberToSymbolDetails name.Text SymbolDetails.Member SymbolDetails.Variable)
            |> Seq.concat
        )

    | SynExpr.Union(name, cases, members, _, _) ->
        syms.Add(
            SymbolDetails.Union(
                Syntax.textOfSymbol name,
                Syntax.rangeOfSymbol name |> textRangeToSyntaxRange
            )
        )

        let unionName = name.Text

        syms.AddRange(
            cases
            |> Seq.map (fun (UnionCase(name, _, _)) ->
                [ (SymbolDetails.UnionMember(
                      name.Text,
                      false,
                      name.Range |> textRangeToSyntaxRange
                  ))
                  (SymbolDetails.UnionMember(
                      unionName + "." + name.Text,
                      false,
                      name.Range |> textRangeToSyntaxRange
                  )) ])
            |> Seq.concat
        )

        syms.AddRange(
            members
            |> Seq.map (memberToSymbolDetails unionName SymbolDetails.Member SymbolDetails.Variable)
            |> Seq.concat
        )

    | SynExpr.LetOrUse(name, _, f, _, _) ->
        syms.AddRange(
            textRangeOfPat name
            |> List.map (fun (a, b) -> SymbolDetails.Variable(a, f.HasFlag(LetFlags.Mutable), b))
        )

    | SynExpr.Symbol sym ->
        syms.Add(SymbolDetails.Variable(sym.Text, false, sym.Range |> textRangeToSyntaxRange))
    | _ -> ()

    ()


let commonFsharpCollectionMethods =
    Runtime.Library.CompileHelpers.getFsharpListMethods ()

let fsharpCollectionMethodSymbolDetails =
    commonFsharpCollectionMethods
    |> Array.map (fun (m, n, sg) -> (m, SymbolDetails.FsharpMethod(m + "." + n, sg, new Range())))
    |> Array.groupBy fst
    |> Array.map (fun (m, items) -> (m, items |> Array.map snd))

let fsharpCollectionMethodCompletions word _ index =
    fsharpCollectionMethodSymbolDetails
    |> Array.map (fun (_, items) ->
        items
        |> Array.choose (fun it ->
            let item = it.ToCompletionItem(index ())

            if item.Label.StartsWith(word, StringComparison.Ordinal) then
                Some(item)
            else
                None))
    |> Array.concat

let interopSymbolDetails =
    Keywords.interopCompletions
    |> Array.map (fun kw -> SymbolDetails.Interop(kw, new Range()))

let completionItemFilterText (c: CompletionItem) =
    match c.InsertText with
    | null -> c.Label
    | it -> it

type VispDocumentItem =
    inherit TextDocumentItem

    val mutable symbols: Option<SymbolDetails array>

    new() =
        { inherit TextDocumentItem()
          symbols = None }

    new(item: TextDocumentItem) =
        { inherit
            TextDocumentItem(
                LanguageId = item.LanguageId,
                Uri = item.Uri,
                Version = item.Version,
                Text = item.Text
            )
          symbols = None }

    member this.Symbols =
        match this.symbols with
        | Some(it) -> it
        | None ->
            this.Parse()

            match this.symbols with
            | Some(it) -> it
            | None -> [||]

    member this.Reset() = ()

    member this.Parse() =
        try
            eprintfn "Parsing %s" (this.Uri.ToString())

            let file =
                Core.CoreParser.parseString (this.Text) (this.Uri.ToString())
                |> Transforms.Helpers.transformParsedFile Core.CoreParser.expandExpr

            let syms = ResizeArray<SymbolDetails>()

            file
            |> Transforms.Traversal.depthFirstExprsInFile
            |> Seq.iter (findAllSymbolDetails syms)

            this.symbols <- Some(syms.ToArray())
        // TODO: Resilient parsing
        with ex ->
            eprintfn "Failed to parse: %O" ex
            this.symbols <- Some([||])
            ()

        ()


let jsonOptions = new JsonSerializerOptions()
jsonOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
jsonOptions.PropertyNameCaseInsensitive <- true
jsonOptions.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
// jsonOptions.Converters.Add(new JsonStringEnumConverter())
jsonOptions.Converters.Add(new SumTypeConverterFactory())

let toJsonString (arg: obj) =
    let text =
        match arg with
        | null -> "(null)"
        | it -> JsonSerializer.Serialize(it, jsonOptions)

    text

let logJson message (arg: obj) =
    let text =
        match arg with
        | null -> "(null)"
        | it -> JsonSerializer.Serialize(it, jsonOptions)

    eprintfn "%s: %s" message text

type LanguageServerClient(sender: Stream, reader: Stream, jsonRpcTraceSource: TraceSource) as self =

    let (jsonRpc, target) =
        let target = new LanguageServerTarget(self, jsonRpcTraceSource)
        let formatter = new SystemTextJsonFormatter(JsonSerializerOptions = jsonOptions)
        let handler = new HeaderDelimitedMessageHandler(sender, reader, formatter)

        let jsonRpc = new JsonRpc(handler)

        let options =
            new JsonRpcTargetOptions(UseSingleObjectParameterDeserialization = true)

        jsonRpc.AddLocalRpcTarget(target, options)

        jsonRpc.TraceSource <- jsonRpcTraceSource

        jsonRpc.ActivityTracingStrategy <-
            new CorrelationManagerTracingStrategy(TraceSource = jsonRpcTraceSource)

        jsonRpc.Disconnected.Add(self.OnRpcDisconnected)

        let events = target.Events :> CustomEvents
        events.OnInitialized.AddHandler(EventHandler(self.OnInitialized))
        events.OnInitializeCompletion.AddHandler(EventHandler(self.OnInitializeCompletion))
        events.OnShutdown.AddHandler(EventHandler(self.OnShutdown))

        jsonRpc.StartListening()

        (jsonRpc, target)

    let disconnectEvent = new ManualResetEvent(false)

    let textDocuments = new Dictionary<TextDocumentIdentifier, VispDocumentItem>()

    let libFiles = new ResizeArray<TextDocumentIdentifier>()

    let mutable traceLevel = TraceValue.Off

    do self.ReadCoreLibFiles()

    member _.TraceSource = jsonRpcTraceSource

    member _.WaitForExit() =
        disconnectEvent.WaitOne() |> ignore
        ()

    member _.Exit() =
        disconnectEvent.Set() |> ignore

        ()

    member _.Target = target
    member _.JsonRpc = jsonRpc

    member _.ReadCoreLibFiles() =
        let root = ProjectGenerator.CoreLibRoot()

        for file in Directory.GetFiles(root, "*.visp", SearchOption.AllDirectories) do
            let id = new TextDocumentIdentifier(Uri = ToFileUri(file))

            let contents = File.ReadAllText(file)

            let doc =
                new VispDocumentItem(
                    Uri = id.Uri,
                    Text = contents,
                    Version = 1,
                    LanguageId = "visp-fs"
                )

            textDocuments[id] <- doc
            doc.Parse()
            libFiles.Add(id)

        ()

    member this.OnRpcDisconnected(ev: JsonRpcDisconnectedEventArgs) =
        this.LogInfo "Disconnecting"
        this.Exit()

    member this.OnInitialized (sender: obj) (ev: EventArgs) = this.LogInfo "OnInitialized"

    member this.OnInitializeCompletion (sender: obj) (ev: EventArgs) = this.LogInfo "init complete"

    member this.OnShutdown (sender: obj) (ev: EventArgs) = this.LogInfo "shutdown"

    member this.OnTextDocumentOpened(message: DidOpenTextDocumentParams) =
        this.LogInfo(sprintf "Received %O" message.TextDocument.Uri)

        this.OpenTextDocument message.TextDocument

        ()

    member this.UpdateTextDocument(doc: DidChangeTextDocumentParams) =
        this.LogInfo("Updating document")

        // logJson "UpdateTextDocument" doc.TextDocument
        let id = new TextDocumentIdentifier(Uri = doc.TextDocument.Uri)

        let text = doc.ContentChanges.[0].Text

        match textDocuments.TryGetValue(id) with
        | false, _ -> ()
        | true, textDoc ->
            textDoc.Version <- doc.TextDocument.Version
            textDoc.Text <- text
            textDoc.Parse()

    member _.DocumentSymbolDetails(id: TextDocumentIdentifier) =
        match textDocuments.TryGetValue(id) with
        | false, _ -> [||]
        | true, textDoc -> textDoc.Symbols

    member _.DocumentSymbols(id: TextDocumentIdentifier) =
        match textDocuments.TryGetValue(id) with
        | false, _ -> [||]
        | true, textDoc ->
            let symbols = textDoc.Symbols

            Array.map
                (fun (it: SymbolDetails) ->
                    new SymbolInformation(
                        Name = it.Text,
                        Kind = it.SymbolKind,
                        Location = new Location(Uri = id.Uri, Range = it.Range)
                    ))
                symbols

    member d.GetLibSymbols() =
        libFiles |> Seq.map d.DocumentSymbols |> Array.concat

    member d.GetLibSymbolDetails() =
        libFiles |> Seq.map d.DocumentSymbolDetails |> Array.concat

    member d.GetDocumentSymbols(args: DocumentSymbolParams) = d.DocumentSymbols(args.TextDocument)

    member this.GetCompletionsAt(args: TextDocumentPositionParams) =
        match textDocuments.TryGetValue(args.TextDocument) with
        | false, _ -> (true, [||])
        | true, textDoc ->
            this.LogInfo(sprintf "Looking for %A in %O" args.Position args.TextDocument.Uri)

            // logJson "GetCompletionsAt" args

            let mutable i = -1

            let index () =
                i <- i + 1
                i

            let libSymbols = this.GetLibSymbolDetails()

            let mutable symbols =
                Array.concat [| textDoc.Symbols; libSymbols |]
                |> Array.distinctBy _.Text
                |> Array.map (fun it -> it.ToCompletionItem(index ()))

            match tryGetWordRange (textDoc.Text) (args.Position.Line) (args.Position.Character) with
            | Some(WordRange(wordAtCursor, replaceRange)) ->
                // logJson "WordAtCursor" (box (wordAtCursor, replaceRange))
                let withoutExlamation = wordAtCursor.TrimStart('!')

                if wordAtCursor.StartsWith('!') && wordAtCursor.Length >= 2 then
                    let possible =
                        interopSymbolDetails |> Array.map (fun it -> it.ToCompletionItem(index ()))

                    symbols <- Array.concat [| possible; symbols |]

                if wordAtCursor.Length >= 3 then
                    let dotIndex = wordAtCursor.IndexOf '.'
                    let found = fsharpCollectionMethodCompletions wordAtCursor dotIndex index

                    symbols <- Array.concat [| found; symbols |]
                    ()

                let searchMembers =
                    wordAtCursor.Length > 1
                    && (wordAtCursor[0] = '.' || wordAtCursor[0] = '+' || wordAtCursor[0] = '-')
                    && System.Char.IsLetter(wordAtCursor[1])

                let memberSearch =
                    if searchMembers then
                        wordAtCursor.Substring(1)
                    else
                        wordAtCursor

                symbols
                |> Array.distinctBy completionItemFilterText
                |> Array.choose (fun it ->
                    let mutable temp = false

                    if it.InsertText <> null then
                        temp <- it.InsertText.Contains(withoutExlamation, StringComparison.Ordinal)

                    if
                        temp
                        || it.Label.Contains(wordAtCursor, StringComparison.Ordinal)
                        || (searchMembers
                            && it.Label.EndsWith(memberSearch, StringComparison.Ordinal))
                    then
                        let text =
                            match it.InsertText with
                            | null -> it.Label
                            | it -> it

                        it.TextEdit <- new TextEdit(NewText = text, Range = replaceRange)
                        Some(it)
                    else
                        None)
                |> (fun it ->
                    let isIncomplete = wordAtCursor.Length < 3 && it.Length > 1
                    (isIncomplete, it))
            | None -> (true, symbols)

    member this.OpenTextDocument(item: TextDocumentItem) =
        let ident = new TextDocumentIdentifier(Uri = item.Uri)

        this.LogInfo($"Opened {ident}")

        let item = new VispDocumentItem(item)

        textDocuments.[ident] <- item

        item.Parse()

        ()

    member _.SetTraceLevel v = traceLevel <- v

    member _.LogEnabled =
        match traceLevel with
        | TraceValue.Verbose -> true
        | _ -> false

    member this.LogInfo
        (
            message: string,
            [<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string
        ) =

        if this.LogEnabled then
            // eprintfn "%s: %s" memberName message

            this.TraceSource.TraceEvent(
                TraceEventType.Information,
                0,
                sprintf "%s: %s" memberName message
            )


and LanguageServerTarget(server: LanguageServerClient, traceSource: TraceSource) =
    let events = CustomEvents()
    let mutable traceLevel = TraceValue.Off

    member _.Server = server

    member _.Events = events

    member _.TraceSource = traceSource

    member _.LogEnabled =
        match traceLevel with
        | TraceValue.Verbose -> true
        | _ -> false

    [<JsonRpcMethod(Methods.InitializeName, UseSingleObjectParameterDeserialization = true)>]
    member this.Initialize(arg: JsonDocument) =
        this.LogEnter arg

        let result =
            new InitializeResult(
                Capabilities =
                    new VSServerCapabilities(
                        TextDocumentSync =
                            new TextDocumentSyncOptions(
                                OpenClose = true,
                                Save = true,
                                Change = TextDocumentSyncKind.Full
                            ),
                        CompletionProvider =
                            new CompletionOptions(
                                ResolveProvider = false
                            // TriggerCharacters = [| "$"; "_" |]
                            ),
                        DocumentSymbolProvider = true
                    // TODO: Rest of capabilities
                    )
            )

        if arg <> null then
            let root = arg.RootElement

            match root.TryGetProperty("trace") with
            | false, _ -> ()
            | true, el ->
                if el.ValueKind = JsonValueKind.String then
                    match el.GetString() with
                    | "messages" -> traceLevel <- TraceValue.Messages
                    | "verbose" -> traceLevel <- TraceValue.Verbose
                    | "off"
                    | _ -> traceLevel <- TraceValue.Off

                    this.Server.SetTraceLevel traceLevel
                    ()

                ()

            ()

        this.Events.TriggerInitializeCompletion this

        this.LogExit result

        result

    [<JsonRpcMethod(Methods.ShutdownName)>]
    member this.Shutdown() =
        this.LogEnter(null :> obj)

        this.Events.TriggerShutdown this

        this.LogExit null
        null :> obj

    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#setTrace
    [<JsonRpcMethod("$/setTrace", UseSingleObjectParameterDeserialization = true)>]
    member this.UpdateTraceLevel(arg: JsonDocument) =
        this.LogEnter(arg)

        if arg <> null then
            let root = arg.RootElement

            match root.TryGetProperty("value") with
            | false, _ -> ()
            | true, el ->
                if el.ValueKind = JsonValueKind.String then
                    match el.GetString() with
                    | "messages" -> traceLevel <- TraceValue.Messages
                    | "verbose" -> traceLevel <- TraceValue.Verbose
                    | "off"
                    | _ -> traceLevel <- TraceValue.Off

                    this.Server.SetTraceLevel traceLevel
                    ()

                ()

            ()

        ()

        this.LogExit(null :> obj)

    [<JsonRpcMethod(Methods.InitializedName, UseSingleObjectParameterDeserialization = true)>]
    member this.Initialized(init: JsonDocument) =
        this.LogEnter init
        this.Events.TriggerOnInitialized this
        this.LogExit null
        ()

    [<JsonRpcMethod(Methods.TextDocumentDidOpenName, UseSingleObjectParameterDeserialization = true)>]
    member this.OnTextDocumentOpened(arg: DidOpenTextDocumentParams) =
        this.LogEnter arg

        this.Server.OnTextDocumentOpened arg

        this.LogExit null

        ()

    [<JsonRpcMethod(Methods.TextDocumentDidChangeName,
                    UseSingleObjectParameterDeserialization = true)>]
    member this.TextDocumentDidChange(arg: DidChangeTextDocumentParams) =
        this.LogEnter arg

        server.UpdateTextDocument arg

        this.LogExit null

        ()

    [<JsonRpcMethod(Methods.TextDocumentDocumentSymbolName,
                    UseSingleObjectParameterDeserialization = true)>]
    member this.GetDocumentSymbols(arg: DocumentSymbolParams) =
        this.LogEnter arg

        let result = this.Server.GetDocumentSymbols arg

        this.LogExit result

        result

    [<JsonRpcMethod(Methods.TextDocumentCompletionName,
                    UseSingleObjectParameterDeserialization = true)>]
    member this.GetCompletionsAt(arg: TextDocumentPositionParams) =
        this.LogEnter arg

        let (incomplete, items) = this.Server.GetCompletionsAt arg
        let result = new CompletionList(IsIncomplete = incomplete, Items = items)

        this.LogExit result

        result

    [<JsonRpcMethod(Methods.TextDocumentCompletionResolveName,
                    UseSingleObjectParameterDeserialization = true)>]
    member this.CompletionResolve(arg: CompletionItem) =
        this.LogEnter arg

        // TODO: Support documentation / details
        let result = arg

        this.LogExit result

        result

    member this.LogEnter
        (
            arg: obj,
            [<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string
        ) =
        if this.LogEnabled then

            let text =
                match arg with
                | null -> "(null)"
                | it -> JsonSerializer.Serialize(it, jsonOptions)

            // eprintfn "Enter %s: %s" memberName text

            this.TraceSource.TraceEvent(
                TraceEventType.Information,
                0,
                sprintf "Enter %s: %s" memberName text
            )

        ()


    member this.LogExit
        (
            result: obj,
            [<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string
        ) =
        if this.LogEnabled then

            let text =
                match result with
                | null -> "(null)"
                | it -> JsonSerializer.Serialize(it, jsonOptions)

            // eprintfn "Exit %s: %s" memberName text

            this.TraceSource.TraceEvent(
                TraceEventType.Information,
                0,
                sprintf "Exit %s: %s" memberName text
            )

        ()
