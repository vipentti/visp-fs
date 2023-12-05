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

    let mainFile = [ VispFile.Main filePath ]

    let knownArguments = [ "--no-lib"; "--release"; "--package" ] |> Set.ofList

    let files =
        if (Array.contains "--no-lib" args) then
            mainFile
        else
            CoreLibs @ mainFile

    let release =
        if Array.contains "--release" args then
            [| "--configuration"; "Release" |]
        else
            [||]

    let pkg =
        if Array.contains "--package" args then
            RuntimeLibraryReference.Package
        else
            RuntimeLibraryReference.Project

    let cmdArguments =
        args[1..] |> Array.filter (fun it -> not (Set.contains it knownArguments))

    let generator = new FsharpGenerator(fs, projectPath)

    generator.WriteVispFiles pkg files None

    let dotnet =
        Cli
            .Wrap("dotnet")
            .WithArguments(
                Array.concat
                    [| [| "run"; "--project"; projectPath |]; release; [| "--" |]; cmdArguments |]
            )
            .WithWorkingDirectory(cwd)
            .WithStandardOutputPipe(PipeTarget.ToDelegate(fun x -> printfn "%s" x))
            .WithStandardErrorPipe(PipeTarget.ToDelegate(fun x -> printfn "%s" x))
            .WithValidation(CommandResultValidation.None)

    let result = dotnet.ExecuteAsync().Task |> Async.AwaitTask |> Async.RunSynchronously

    result.ExitCode
