// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

open System.IO.Abstractions
open Visp.Compiler.ProjectGenerator
open CliWrap

let tryNextArg v args =
    match Array.tryFindIndex v args with
    | None -> None
    | Some(id) -> if id + 1 < args.Length then Some(args[id + 1]) else None

[<EntryPoint>]
let main args =
    let fs = new FileSystem()

    let projectPath = tempDirPath "visp-fs-temp-project"
    let filePath = fs.Path.GetFullPath args.[0]
    let cwd = fs.Path.GetDirectoryName filePath

    let mainFile = [ VispFile.Main filePath ]

    let knownArguments =
        [ "--no-lib"; "--release"; "--package"; "--debug-tokens"; "--debug-parse" ]
        |> Set.ofList

    let hasArg arg = Array.contains arg args

    let files =
        if (Array.contains "--no-lib" args) then
            mainFile
        else
            CoreLibs @ mainFile

    let buildConfig =
        if Array.contains "--release" args then
            DotnetCompiler.BuildConfiguration.Release
        else
            DotnetCompiler.BuildConfiguration.Debug

    let pkg =
        if Array.contains "--package" args then
            RuntimeLibraryReference.Package
        else
            RuntimeLibraryReference.Project

    let options =
        { WriteOptions.Default with
            DebugTokens = hasArg "--debug-tokens"
            DebugParse = hasArg "--debug-parse" }


    let cmdArguments =
        args[1..] |> Array.filter (fun it -> not (Set.contains it knownArguments))

    let generator = new FsharpGenerator(fs, projectPath)

    generator.WriteVispFiles pkg files options

    let result = DotnetCompiler.buildAndRun projectPath cwd buildConfig cmdArguments |> Async.RunSynchronously

    printfn "%s" result.Output

    result.ExitCode

