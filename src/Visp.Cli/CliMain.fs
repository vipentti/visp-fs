// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

open System.IO.Abstractions
open Visp.Compiler.ProjectGenerator
open CliWrap

[<EntryPoint>]
let main args =
    let fs = new FileSystem()

    let projectPath = tempDirPath "visp-fs-temp-project"
    let filePath = fs.Path.GetFullPath args.[0]
    let cwd = fs.Path.GetDirectoryName filePath

    let mutable files = [ VispFile.Main filePath ]

    if not (Array.exists (fun it -> it = "--no-lib") args) then
        files <- VispFile.CoreLib "core.visp" :: files

    let generator = new FsharpGenerator(fs, projectPath)

    generator.WriteVispFiles files

    let dotnet =
        Cli
            .Wrap("dotnet")
            .WithArguments(
                Array.concat [| [| "run"; "--project"; projectPath; "--" |]; args[1..] |]
            )
            .WithWorkingDirectory(cwd)
            .WithStandardOutputPipe(PipeTarget.ToDelegate(fun x -> printfn "%s" x))
            .WithStandardErrorPipe(PipeTarget.ToDelegate(fun x -> printfn "%s" x))
            .WithValidation(CommandResultValidation.None)

    let result = dotnet.ExecuteAsync().Task |> Async.AwaitTask |> Async.RunSynchronously

    result.ExitCode
