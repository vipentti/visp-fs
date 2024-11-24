module TestUtils

open FsUnit.Xunit
open CliWrap
open System.IO
open System.IO.Abstractions
open System.Text
open VerifyXunit
open Visp.Compiler.ProjectGenerator
open Visp.Compiler

let getVispFilePath name =
    let src_dir = __SOURCE_DIRECTORY__
    Path.Combine(src_dir, "..", "..", "visp", name) |> Path.GetFullPath

let CreateAndRunProject filePath =
    ((if runtimePackageExists () then
          "runtime package found"
      else
          $"runtime package not found in {runtimePackagePath}"),
     if commonPackageExists () then
         "common package found"
     else
         $"common package not found in {commonPackagePath}")
    |> should equal ("runtime package found", "common package found")

    let filePath = Path.GetFullPath filePath
    let fileNameWithoutExtension = Path.GetFileNameWithoutExtension filePath
    let cwd = Path.GetDirectoryName filePath
    let rnd = Path.GetRandomFileName()

    let projectPath =
        Path.Combine(
            Path.GetTempPath(),
            "visp-fs-execution",
            (sprintf "%s-%s-project" fileNameWithoutExtension rnd)
        )

    let outputPath =
        Path.Combine(
            projectPath,
            "output"
        )

    let dllPath =
        Path.Combine(
            outputPath,
            "project.dll"
        )

    let files = CoreLibs @ [ VispFile.Main filePath ]

    let generator = new FsharpGenerator(new FileSystem(), projectPath)

    let sb = new StringBuilder()
    let buildSb = new StringBuilder()

    Syntax.SyntaxWriteUtilThreadStatics.RunningTests <- true

    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/compiler-options#compiler-options-listed-alphabetically
    generator.WriteVispFiles
        RuntimeLibraryReference.Package
        files
        { WriteOptions.Default with
            Flags = (Some "--debug- --nooptimizationdata --optimize-") }


    //let dotnetBuild =
    //    Cli
    //        .Wrap("dotnet")
    //        .WithArguments(Array.concat [| [| "build"; projectPath; "--output"; outputPath |] |])
    //        .WithWorkingDirectory(cwd)
    //        .WithStandardOutputPipe(PipeTarget.ToStringBuilder(buildSb))
    //        .WithStandardErrorPipe(PipeTarget.ToStringBuilder(buildSb))
    //        .WithValidation(CommandResultValidation.None)

    //let dotnet =
    //    Cli
    //        .Wrap("dotnet")
    //        .WithArguments(Array.concat [| [| dllPath |] |])
    //        // .WithArguments(Array.concat [| [| "run"; "--project"; projectPath |] |])
    //        .WithWorkingDirectory(cwd)
    //        .WithStandardOutputPipe(PipeTarget.ToStringBuilder(sb))
    //        .WithStandardErrorPipe(PipeTarget.ToStringBuilder(sb))
    //        .WithValidation(CommandResultValidation.None)

    async {
        let mutable succeed = false

        try
            //let! buildResult = dotnetBuild.ExecuteAsync().Task |> Async.AwaitTask

            //if buildResult.ExitCode <> 0 then
            //    failwithf "Build failed: %s" (buildSb.ToString())

            //let! result = dotnet.ExecuteAsync().Task |> Async.AwaitTask


            let! result = DotnetCompiler.buildAndRun projectPath cwd DotnetCompiler.BuildConfiguration.Debug [||]

            sb.Append(result.Output).AppendLine().Append("ExitCode: ").Append(result.ExitCode).AppendLine()
            |> ignore

            succeed <- result.ExitCode = 0

            // Remove once https://github.com/dotnet/msbuild/issues/10998 is fixed
            // https://github.com/dotnet/runtime/issues/109815
            return (result.ExitCode, sb.ToString().Replace("\x1b]9;4;3;\x1b\\", "").Replace("\x1b]9;4;0;\x1b\\", ""))
        finally
            try
                if succeed && Directory.Exists(projectPath) then
                    Directory.Delete(projectPath, true)
                    ()
            with _ ->
                ()
    }

let runTest (name: string) =
    async {
        let path = getVispFilePath name
        let! (_, output) = CreateAndRunProject path

        return!
            Verifier
                .Verify(output)
                .UseDirectory("snapshots")
                .UseParameters(name.Replace('/', '_').Replace('\\', '_'))
                .DisableDiff()
                .ToTask()
            |> Async.AwaitTask
    }
