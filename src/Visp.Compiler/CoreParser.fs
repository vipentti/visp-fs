// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Compiler.Core

open Visp.Compiler.SyntaxPrinter
open System.IO
open FSharp.Text.Lexing
open Visp.Compiler
open Visp.Compiler.SyntaxParser
open Visp.Compiler.LexHelpers
open Visp.Compiler.Syntax.Macros

type ParserOptions =
    { DebugTokens: bool
      ReturnLast: bool
      DebugParse: bool }

    static member Default =
        { DebugTokens = false
          ReturnLast = true
          DebugParse = false }

    static member Library =
        { ParserOptions.Default with
            ReturnLast = false }



module CoreParser =
    open Visp.Compiler.Syntax
    open System.Collections.Generic

    let getLibFilePath name =
        let src_dir = __SOURCE_DIRECTORY__
        Path.Combine(src_dir, "..", "..", "visp", "lib", name) |> Path.GetFullPath

    let parseFile filePath (options: ParserOptions) =
        let (stream, reader, lexbuf) = UnicodeFileAsLexbuf(filePath, None)

        use _ = stream
        use _ = reader

        let tokenizer = ParseUtils.mkTokenizer options.DebugTokens

        try
            let mutable res = start tokenizer lexbuf

            if options.DebugParse then
                eprintfn "%A" res

            if options.ReturnLast then
                res <- Transforms.LastExpressionUpdater.update res

            res
        with :? ParseHelpers.SyntaxError as syn ->
            outputSyntaxError syn

            reraise ()

    let pathsToFile = Dictionary<string, ParsedFile>()

    let includeFiles = Dictionary<string, SynModuleDecl list>()

    let rec readIncludeFiles (ParsedFile _ as file) =
        let rec readFilePath rootPath (FilePath(path, range) as fp) =
            let path = System.IO.Path.Combine(rootPath, path.Path) |> System.IO.Path.GetFullPath

            let nestedDecls =
                match dictTryFind path includeFiles with
                | Some(it) -> it
                | None ->
                    let parsed = parseFile path ParserOptions.Library |> readIncludeFiles
                    pathsToFile.Add(path, parsed)
                    let (ParsedFile fragments) = parsed

                    let nestedDecls =
                        fragments
                        |> List.collect (fun (ParsedFileFragment.AnonModule(decls, _)) -> decls)

                    includeFiles.Add(path, nestedDecls)

                    nestedDecls

            SynModuleDecl.IncludedModule(fp, nestedDecls, range)

        and readInclude =
            function
            | SynModuleDecl.Include(paths, range) as _ ->

                let filePath =
                    System.IO.Path.GetDirectoryName range.FileName |> System.IO.Path.GetFullPath

                let includedModules = paths |> List.map (readFilePath filePath)

                SynModuleDecl.ModuleList(includedModules, range)

            | it -> it

        Transforms.Helpers.transformSynModuleDecls readInclude file

    let private tfs =
        [| Transforms.SyntaxMacros.expand
           Transforms.QuasiquoteExpander.expand
           Transforms.BuiltinMacros.expand
           Transforms.Common.transformLambdaShortHands |]

    let expandExpr expr =
        Transforms.Helpers.runTransforms tfs expr

    let transformFile file =
        file
        |> readIncludeFiles
        |> Transforms.StringLifter.liftLiteralStrings
        |> Transforms.Helpers.transformParsedFile expandExpr

    let writeParsedFile file outputStream (template: string) =
        let fileWriter = Writer.CustomFileWriter(outputStream, 2, "//")
        fileWriter.Write(template.Trim())
        fileWriter.WriteLine()
        let writer = Visp.Syntax.SynWriter.mkSynWriter fileWriter
        let file = transformFile file
        Visp.Syntax.SynWriter.Write.writeParsedFile writer file
        fileWriter.WriteLine()

    let writeToStreamNew file outputStream _ =
        let fileWriter = Writer.CustomFileWriter(outputStream, 2, "//")

        let mainProgram =
            """
// This file is auto-generated

#nowarn "0020"  // unused results from functions

open Visp.Runtime.Library

let state = { Todo = () }

"""

        fileWriter.Write(mainProgram.Trim())
        fileWriter.WriteLine()

        let writer = Visp.Syntax.SynWriter.mkSynWriter fileWriter

        let file = transformFile file
        Visp.Syntax.SynWriter.Write.writeParsedFile writer file

        fileWriter.WriteLine()

    let outputSyntaxError (syn: ParseHelpers.SyntaxError) =
        match syn.Data0 with
        | :? FSharp.Text.Parsing.ParseErrorContext<SyntaxParser.token> as ctx ->
            let (startPos, _) = ctx.ParseState.ResultRange
            eprintfn "ReduceTokens: %A" ctx.ReduceTokens
            eprintfn "ReducibleProductions: %A" ctx.ReducibleProductions
            eprintfn "ShiftTokens: %A" ctx.ShiftTokens
            eprintfn "StateStack: %A" ctx.StateStack
            eprintfn "%s(%i,%i)" (startPos.FileName) (startPos.Line) (startPos.Column)
            eprintfn "Token: %A" ctx.CurrentToken
            eprintfn "Message: %A" ctx.Message
        | _ -> ()

    let debugLexFile filePath =
        let (stream, reader, lexbuf) = UnicodeFileAsLexbuf(filePath, None)
        use _ = stream
        use _ = reader
        let args = mkDefaultLextArgs ()
        ParseUtils.debugTokenOutput args lexbuf |> List.ofSeq

    let getTokens str fileName =
        let lexbuf = LexBuffer<_>.FromString str
        lexbuf.EndPos <- Position.FirstLine fileName

        let tokenizer = ParseUtils.mkTokenizerWithArgs <| mkTokenStreamArgs ()

        seq {
            while not lexbuf.IsPastEndOfStream do
                let next = tokenizer lexbuf
                let sp = lexbuf.StartPos
                let ep = lexbuf.EndPos
                yield (next, sp, ep)
        }

    let parseString str fileName =
        let lexbuf = LexBuffer<_>.FromString str
        lexbuf.EndPos <- Position.FirstLine fileName

        let tokenizer = ParseUtils.mkTokenizer false

        try
            let res = SyntaxParser.start tokenizer lexbuf
            res
        with :? ParseHelpers.SyntaxError as syn ->
            outputSyntaxError syn

            reraise ()
