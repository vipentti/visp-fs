// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module TestUtils

open System.IO
open Visp.Compiler.Core
open VerifyXunit
open Visp.Compiler

let getVispFilePath name =
    let src_dir = __SOURCE_DIRECTORY__
    Path.Combine(src_dir, "..", "..", "visp", name) |> Path.GetFullPath

let parseCoreLibs () =
    try
        let libs = ProjectGenerator.CoreLibs

        for lib in libs do
            let path = lib.Path

            CoreParser.parseFile path ParserOptions.Library
            // TODO: We only really want macros here?
            |> CoreParser.transformFile
            |> ignore

        ()

    with :? ParseHelpers.SyntaxError as syn ->
        raise (LexHelpers.syntaxErrorToParseError syn)


let inline private verify (out: string) dir (param: string) =
    async {
        let task =
            Verifier
                .Verify(out)
                .UseDirectory(dir)
                .DisableDiff()
                .UseParameters(param)
                .ToTask()

        return! (task |> Async.AwaitTask)
    }

let runTokenTest (name: string) =
    async {
        parseCoreLibs ()
        let path = getVispFilePath name

        try
            let parsed = CoreParser.debugLexFile path
            let nameParam = name.Replace('/', '_').Replace('\\', '_')
            Syntax.SyntaxWriteUtilThreadStatics.NormalizeLineEndings <- true
            let output = parsed |> String.concat "\n"
            return! verify output "token-snapshots" nameParam
        with :? ParseHelpers.SyntaxError as syn ->
            return raise <| (LexHelpers.syntaxErrorToParseError syn)
    }

let runStructuredOutputTest (name: string) =
    async {
        parseCoreLibs ()
        let path = getVispFilePath name

        try
            let parsed = CoreParser.parseFile path ParserOptions.Default

            let nameParam = name.Replace('/', '_').Replace('\\', '_')

            Syntax.SyntaxWriteUtilThreadStatics.NormalizeLineEndings <- true

            let output = (sprintf "%120A" parsed).Replace("\\r\\n", "\\n").Replace("\r\n", "\n")

            return! verify output "parsing-snapshots" nameParam
        with :? ParseHelpers.SyntaxError as syn ->
            return raise <| (LexHelpers.syntaxErrorToParseError syn)
    }

let runWriteTest (name: string) =
    async {
        parseCoreLibs ()

        let path = getVispFilePath name
        let fileName = Path.GetFileName path

        let filePathToReplace =
            Path.GetDirectoryName path |> Path.TrimEndingDirectorySeparator

        try
            let parsed = CoreParser.parseFile path ParserOptions.Default

            use stream = new StringWriter()
            CoreParser.writeToStreamNew parsed stream fileName

            let output =
                stream
                    .ToString()
                    .Replace(filePathToReplace + "/", "")
                    .Replace(filePathToReplace + "\\", "")

            let nameParam = name.Replace('/', '_').Replace('\\', '_')

            return! verify output "snapshots" nameParam

        with :? ParseHelpers.SyntaxError as syn ->
            return raise <| (LexHelpers.syntaxErrorToParseError syn)
    }

let runTest (name: string) = runWriteTest name
