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

            CoreParser.parseFile path lib.ReturnLast
            // TODO: We only really want macros here?
            |> CoreParser.transformFile
            |> ignore

        ()

    with :? ParseHelpers.SyntaxError as syn ->
        raise (LexHelpers.syntaxErrorToParseError syn)

let runTest (name: string) =
    parseCoreLibs ()

    let path = getVispFilePath name
    let fileName = Path.GetFileName path

    let filePathToReplace =
        Path.GetDirectoryName path |> Path.TrimEndingDirectorySeparator

    try
        let parsed = CoreParser.parseFile path true

        use stream = new StringWriter()
        CoreParser.writeToStreamNew parsed stream fileName

        let output =
            stream
                .ToString()
                .Replace(filePathToReplace + "/", "")
                .Replace(filePathToReplace + "\\", "")

        Verifier
            .Verify(output)
            .UseDirectory("snapshots")
            .DisableDiff()
            .UseParameters(name.Replace('/', '_').Replace('\\', '_'))
            .ToTask()
        |> Async.AwaitTask
    with :? ParseHelpers.SyntaxError as syn ->
        raise (LexHelpers.syntaxErrorToParseError syn)
