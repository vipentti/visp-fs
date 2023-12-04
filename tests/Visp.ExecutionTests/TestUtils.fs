module TestUtils

open FsUnit.Xunit
open CliWrap
open System.IO
open System.IO.Abstractions
open System.Text
open VerifyXunit
open Visp.Compiler.ProjectGenerator

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

    let files = [ VispFile.CoreLib "core.visp"; VispFile.Main filePath ]

    let generator = new FsharpGenerator(new FileSystem(), projectPath)

    let sb = new StringBuilder()

    generator.WriteVispFiles RuntimeLibraryReference.Package files

    let dotnet =
        Cli
            .Wrap("dotnet")
            .WithArguments(Array.concat [| [| "run"; "--project"; projectPath |] |])
            .WithWorkingDirectory(cwd)
            .WithStandardOutputPipe(PipeTarget.ToStringBuilder(sb))
            .WithStandardErrorPipe(PipeTarget.ToStringBuilder(sb))
            .WithValidation(CommandResultValidation.None)

    async {
        let mutable succeed = false
        try
            let! result = dotnet.ExecuteAsync().Task |> Async.AwaitTask

            sb.AppendLine().Append("ExitCode: ").Append(result.ExitCode).AppendLine()
            |> ignore

            succeed <- result.ExitCode = 0

            return (result.ExitCode, sb.ToString())
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
