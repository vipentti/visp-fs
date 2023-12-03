open System.Diagnostics
open System.IO
open System
open Visp.LanguageServer

let getEnv v def =
    match Environment.GetEnvironmentVariable(v) with
    | null -> def
    | it -> it

let CreateTraceSource debug =
    let src =
        new TraceSource(
            "VispFs.LanguageServer",
            if debug then
                SourceLevels.Verbose ||| SourceLevels.ActivityTracing ||| SourceLevels.All
            else
                SourceLevels.Warning
        )

    let localAppData =
        Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)

    let traceRoot = getEnv "VISP_FS_TRACE_DIR" localAppData

    let traceDir = Path.Combine(traceRoot, "visp-fs", "traces") |> Path.GetFullPath

    let dir = Directory.CreateDirectory(traceDir)

    let logFileName = $"visp-fs-log-{DateTime.Now:``yyyy-MM-dd``}.svclog"

    let logPath = Path.Combine(dir.FullName, logFileName)

    let listener = new XmlWriterTraceListener(logPath)

    src.Listeners.Add(listener) |> ignore

    Trace.AutoFlush <- true

    src


[<EntryPoint>]
let main args =
    Console.OutputEncoding <- Text.Encoding.UTF8

    let shouldDebug =
        Array.contains "--debug" args || (getEnv "VISP_FS_LSP_DEBUG" "" = "true")

    let source = CreateTraceSource shouldDebug

    source.TraceEvent(TraceEventType.Information, 0, "visp-fs LanguageServer Starting...")

    try
        try
            use output = Console.OpenStandardOutput()
            use input = Console.OpenStandardInput()

            let server = LanguageServerClient(output, input, source)

            server.WaitForExit()
        with ex ->
            source.TraceEvent(TraceEventType.Critical, 0, ex.ToString())
    finally
        source.TraceEvent(TraceEventType.Information, 0, "visp-fs Stopping...")

    0
