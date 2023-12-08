// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.DiagnosticsLogger

open Visp.Compiler.Text.Range
open Visp.Compiler.Text
open System
open System.Diagnostics
open System.Reflection
open System.Threading

/// Thrown when we want to add some range information to a .NET exception
exception WrappedError of exn * range with
    override this.Message =
        match this :> exn with
        | WrappedError(exn, _) -> "WrappedError(" + exn.Message + ")"
        | _ -> "WrappedError"

/// Thrown when immediate, local error recovery is not possible. This indicates
/// we've reported an error but need to make a non-local transfer of control.
/// Error recovery may catch this and continue (see 'errorRecovery')
///
/// The exception that caused the report is carried as data because in some
/// situations (LazyWithContext) we may need to re-report the original error
/// when a lazy thunk is re-evaluated.
exception ReportedError of exn option with
    override this.Message =
        let msg =
            "The exception has been reported. This internal exception should now be caught at an error recovery point on the stack."

        match this :> exn with
        | ReportedError(Some exn) -> msg + " Original message: " + exn.Message + ")"
        | _ -> msg

/// Thrown when we stop processing the F# Interactive entry or #load.
exception StopProcessingExn of exn option with
    override _.Message =
        "Processing of a script fragment has stopped because an exception has been raised"

    override this.ToString() =
        match this :> exn with
        | StopProcessingExn(Some exn) -> "StopProcessingExn, originally (" + exn.ToString() + ")"
        | _ -> "StopProcessingExn"

let (|StopProcessing|_|) exn =
    match exn with
    | StopProcessingExn _ -> Some()
    | _ -> None

let StopProcessing<'T> = StopProcessingExn None


exception DiagnosticWithText of number: int * message: string * range: range with
    override this.Message =
        match this :> exn with
        | DiagnosticWithText(_, msg, _) -> msg
        | _ -> "impossible"

exception InternalError of message: string * range: range with
    override this.Message =
        match this :> exn with
        | InternalError(msg, m) -> msg + m.ToString()
        | _ -> "impossible"

exception InternalException of exn: Exception * msg: string * range: range with
    override this.Message =
        match this :> exn with
        | InternalException(_, msg, _) -> msg
        | _ -> "impossible"

    override this.ToString() =
        match this :> exn with
        | InternalException(exn, _, _) -> exn.ToString()
        | _ -> "impossible"

let mkDiagnosticWithText ((n, text), m) = DiagnosticWithText(n, text, m)

// Attach a range if this is a range dual exception.
let rec AttachRange m (exn: exn) =
    if equals m range0 then
        exn
    else
        match exn with
        // Strip TargetInvocationException wrappers
        | :? TargetInvocationException -> AttachRange m exn.InnerException
        | :? NotSupportedException -> exn
        | :? SystemException -> InternalException(exn, exn.Message, m)
        | _ -> exn

/// Closed enumeration of build phases.
[<RequireQualifiedAccess>]
type DiagnosticSeverity =
    | Info
    | Warning
    | Error

/// Closed enumeration of build phases.
[<RequireQualifiedAccess>]
type BuildPhase =
    | DefaultPhase
    | Compile
    | Parse
    | Output
    | Internal

/// Literal build phase subcategory strings.
module BuildPhaseSubcategory =
    [<Literal>]
    let DefaultPhase = ""

    [<Literal>]
    let Compile = "compile"

    [<Literal>]
    let Parse = "parse"

    [<Literal>]
    let Output = "output"

    [<Literal>]
    let Internal = "internal" // Compiler ICE

[<DebuggerDisplay("{DebugDisplay()}")>]
type PhasedDiagnostic =
    { Exception: exn
      Phase: BuildPhase }

    /// Construct a phased error
    static member Create(exn: exn, phase: BuildPhase) : PhasedDiagnostic =
        { Exception = exn; Phase = phase }

    member this.DebugDisplay() =
        sprintf "%s: %s" (this.Subcategory()) this.Exception.Message

    /// This is the textual subcategory to display in error and warning messages (shows only under --vserrors):
    ///
    ///     file1.fs(72): subcategory warning FS0072: This is a warning message
    ///
    member pe.Subcategory() =
        match pe.Phase with
        | BuildPhase.DefaultPhase -> BuildPhaseSubcategory.DefaultPhase
        | BuildPhase.Compile -> BuildPhaseSubcategory.Compile
        | BuildPhase.Parse -> BuildPhaseSubcategory.Parse
        | BuildPhase.Output -> BuildPhaseSubcategory.Output
        | BuildPhase.Internal -> BuildPhaseSubcategory.Internal

[<AbstractClass>]
[<DebuggerDisplay("{DebugDisplay()}")>]
type DiagnosticsLogger(nameForDebugging: string) =
    abstract ErrorCount: int

    // The 'Impl' factoring enables a developer to place a breakpoint at the non-Impl
    // code just below and get a breakpoint for all error logger implementations.
    abstract DiagnosticSink: diagnostic: PhasedDiagnostic * severity: DiagnosticSeverity -> unit

    member x.CheckForErrors() = (x.ErrorCount > 0)

    member _.DebugDisplay() =
        sprintf "DiagnosticsLogger(%s)" nameForDebugging

let DiscardErrorsLogger =
    { new DiagnosticsLogger("DiscardErrorsLogger") with
        member _.DiagnosticSink(diagnostic, severity) = ()
        member _.ErrorCount = 0 }

let AssertFalseDiagnosticsLogger =
    { new DiagnosticsLogger("AssertFalseDiagnosticsLogger") with
        member _.DiagnosticSink(diagnostic, severity) = (* assert false; *) ()
        member _.ErrorCount = (* assert false; *) 0 }

type CapturingDiagnosticsLogger(nm, ?eagerFormat) =
    inherit DiagnosticsLogger(nm)
    let mutable errorCount = 0
    let diagnostics = ResizeArray()

    override _.DiagnosticSink(diagnostic, severity) =
        let diagnostic =
            match eagerFormat with
            | None -> diagnostic
            | Some f -> f diagnostic

        if severity = DiagnosticSeverity.Error then
            errorCount <- errorCount + 1

        diagnostics.Add(diagnostic, severity)

    override _.ErrorCount = errorCount

    member _.Diagnostics = diagnostics |> Seq.toList

    member _.CommitDelayedDiagnostics(diagnosticsLogger: DiagnosticsLogger) =
        // Eagerly grab all the errors and warnings from the mutable collection
        let errors = diagnostics.ToArray()
        errors |> Array.iter diagnosticsLogger.DiagnosticSink

/// Type holds thread-static globals for use by the compile.
type internal DiagnosticsThreadStatics =
    [<ThreadStatic; DefaultValue>]
    static val mutable private buildPhase: BuildPhase

    [<ThreadStatic; DefaultValue>]
    static val mutable private diagnosticsLogger: DiagnosticsLogger

    static member BuildPhaseUnchecked = DiagnosticsThreadStatics.buildPhase

    static member BuildPhase
        with get () =
            match box DiagnosticsThreadStatics.buildPhase with
            | null -> BuildPhase.DefaultPhase
            | _ -> DiagnosticsThreadStatics.buildPhase
        and set v = DiagnosticsThreadStatics.buildPhase <- v

    static member DiagnosticsLogger
        with get () =
            match box DiagnosticsThreadStatics.diagnosticsLogger with
            | null -> AssertFalseDiagnosticsLogger
            | _ -> DiagnosticsThreadStatics.diagnosticsLogger
        and set v = DiagnosticsThreadStatics.diagnosticsLogger <- v

[<AutoOpen>]
module DiagnosticsLoggerExtensions =
    /// Instruct the exception not to reset itself when thrown again.
    let PreserveStackTrace exn =
        try
            let preserveStackTrace =
                typeof<Exception>
                    .GetMethod(
                        "InternalPreserveStackTrace",
                        BindingFlags.Instance ||| BindingFlags.NonPublic
                    )

            preserveStackTrace.Invoke(exn, null) |> ignore
        with _ ->
            // This is probably only the mono case.
            Debug.Assert(false, "Could not preserve stack trace for watson exception.")
            ()

type DiagnosticsLogger with

    member x.EmitDiagnostic(exn, severity) =
        match exn with
        | InternalError(s, _)
        | InternalException(_, s, _)
        | Failure s as exn ->
            Debug.Assert(
                false,
                sprintf "Unexpected exception raised in compiler: %s\n%s" s (exn.ToString())
            )
        | _ -> ()

        match exn with
        | StopProcessing
        | ReportedError _ ->
            PreserveStackTrace exn
            raise exn
        | _ ->
            x.DiagnosticSink(
                PhasedDiagnostic.Create(exn, DiagnosticsThreadStatics.BuildPhase),
                severity
            )

    member x.ErrorR exn =
        x.EmitDiagnostic(exn, DiagnosticSeverity.Error)

    member x.Warning exn =
        x.EmitDiagnostic(exn, DiagnosticSeverity.Warning)

    member x.InformationalWarning exn =
        x.EmitDiagnostic(exn, DiagnosticSeverity.Info)

    member x.Error exn =
        x.ErrorR exn
        raise (ReportedError(Some exn))

    member x.SimulateError diagnostic =
        x.DiagnosticSink(diagnostic, DiagnosticSeverity.Error)
        raise (ReportedError(Some diagnostic.Exception))

    member x.ErrorRecovery (exn: exn) (m: range) =
        // Never throws ReportedError.
        // Throws StopProcessing and exceptions raised by the DiagnosticSink(exn) handler.
        match exn with
        // Don't send ThreadAbortException down the error channel
        | :? System.Threading.ThreadAbortException
        | WrappedError(:? System.Threading.ThreadAbortException, _) -> ()
        | ReportedError _
        | WrappedError(ReportedError _, _) -> ()
        | StopProcessing
        | WrappedError(StopProcessing, _) ->
            PreserveStackTrace exn
            raise exn
        | _ ->
            try
                x.ErrorR(AttachRange m exn) // may raise exceptions, e.g. an fsi error sink raises StopProcessing.
            with
            | ReportedError _
            | WrappedError(ReportedError _, _) -> ()

    member x.StopProcessingRecovery (exn: exn) (m: range) =
        // Do standard error recovery.
        // Additionally ignore/catch StopProcessing. [This is the only catch handler for StopProcessing].
        // Additionally ignore/catch ReportedError.
        // Can throw other exceptions raised by the DiagnosticSink(exn) handler.
        match exn with
        | StopProcessing
        | WrappedError(StopProcessing, _) -> () // suppress, so skip error recovery.
        | _ ->
            try
                x.ErrorRecovery exn m
            with
            | StopProcessing
            | WrappedError(StopProcessing, _) -> () // catch, e.g. raised by DiagnosticSink.
            | ReportedError _
            | WrappedError(ReportedError _, _) -> () // catch, but not expected unless ErrorRecovery is changed.

    member x.ErrorRecoveryNoRange(exn: exn) = x.ErrorRecovery exn range0

/// Raises an exception with error recovery and returns unit.
let errorR exn =
    DiagnosticsThreadStatics.DiagnosticsLogger.ErrorR exn

/// Raises a special exception and returns 'T - can be caught later at an errorRecovery point.
let error exn =
    DiagnosticsThreadStatics.DiagnosticsLogger.Error exn

let errorRecovery exn m =
    DiagnosticsThreadStatics.DiagnosticsLogger.ErrorRecovery exn m
