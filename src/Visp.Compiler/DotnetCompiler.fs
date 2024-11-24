module DotnetCompiler

open CliWrap
open System.IO
open System.Text

type BuildConfiguration =
    | Debug
    | Release

type BuildResult (exitCode: int, output: string, dllPath: string) =
    member _.ExitCode = exitCode
    member _.Output = output
    member _.DllPath = dllPath

type RunResult (exitCode: int, output: string) =
    member _.ExitCode = exitCode
    member _.Output = output

let buildProject projectPath cwd (config: BuildConfiguration) =

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

    let buildSb = new StringBuilder()

    let buildConfig =
        [|
            "--configuration";
            match config with
            | Debug -> "Debug"
            | Release -> "Release"
        |]

    let dotnetBuild =
        Cli
            .Wrap("dotnet")
            .WithArguments(Array.concat [| [| "build"; projectPath; "--output"; outputPath; "--tl:off" |]; buildConfig |])
            .WithEnvironmentVariables(fun it -> it.Set("MSBUILDTERMINALLOGGER", "off") |> ignore)
            .WithWorkingDirectory(cwd)
            .WithStandardOutputPipe(PipeTarget.ToStringBuilder(buildSb))
            .WithStandardErrorPipe(PipeTarget.ToStringBuilder(buildSb))
            .WithValidation(CommandResultValidation.None)

    async {
        let! buildResult = dotnetBuild.ExecuteAsync().Task |> Async.AwaitTask

        return BuildResult(buildResult.ExitCode, buildSb.ToString(), dllPath)
    }

let runBuildResult (result: BuildResult) cwd cmdArgs =

    let sb = new StringBuilder()

    let dotnet =
        Cli
            .Wrap("dotnet")
            .WithArguments(Array.concat [| [| result.DllPath |]; [| "--" |]; cmdArgs |])
            .WithWorkingDirectory(cwd)
            .WithStandardOutputPipe(PipeTarget.ToStringBuilder(sb))
            .WithStandardErrorPipe(PipeTarget.ToStringBuilder(sb))
            .WithValidation(CommandResultValidation.None)

    async {
        let! result = dotnet.ExecuteAsync().Task |> Async.AwaitTask

        return RunResult(result.ExitCode, sb.ToString())
    }

let buildAndRun projectPath cwd (config: BuildConfiguration) cmdArgs  =
    async {
        let! buildResult = buildProject projectPath cwd config

        if buildResult.ExitCode <> 0 then
            failwithf "Build failed: %s" (buildResult.Output)

        return! runBuildResult buildResult cwd cmdArgs
    }
