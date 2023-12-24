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


module CoreParser =
    let getLibFilePath name =
        let src_dir = __SOURCE_DIRECTORY__

        let my2DArray: int array2d = array2D [ [ 1; 0 ]; [ 0; 1 ] ]

        let xx = Array2D.length1 my2DArray

        let yy = my2DArray[0, 0]

        Path.Combine(src_dir, "..", "..", "visp", "lib", name) |> Path.GetFullPath

    let private tfs =
        [| Transforms.SyntaxMacros.expand
           Transforms.QuasiquoteExpander.expand
           Transforms.BuiltinMacros.expand
           Transforms.Common.transformLambdaShortHands |]

    let expandExpr expr =
        Transforms.Helpers.runTransforms tfs expr

    let transformFile file =
        file
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

    let parseFile filePath returnLast =
        let (stream, reader, lexbuf) = UnicodeFileAsLexbuf(filePath, None)

        use _ = stream
        use _ = reader

        let tokenizer = ParseUtils.mkTokenizer ()

        try
            let mutable res = start tokenizer lexbuf

            // eprintfn "%A" res
            // eprintfn "%s" (res.Pretty())

            if returnLast then
                res <- Transforms.LastExpressionUpdater.update res
            // eprintfn "%A" res

            // use outputStream = new StringWriter()
            // writeToStreamNew res outputStream filePath
            // printfn "%s" (outputStream.ToString())

            res
        with :? ParseHelpers.SyntaxError as syn ->
            outputSyntaxError syn

            reraise ()

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

        let tokenizer = ParseUtils.mkTokenizer ()

        try
            let res = SyntaxParser.start tokenizer lexbuf
            res
        with :? ParseHelpers.SyntaxError as syn ->
            outputSyntaxError syn

            reraise ()
