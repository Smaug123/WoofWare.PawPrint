namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// Comparing one guest's behaviour under PawPrint against the same guest under the
/// real .NET runtime.
///
/// Which cases get compared, and on which hosts, is `OraclePolicy`'s question; this
/// module only knows what "agreeing" means once both runtimes have answered.
[<RequireQualifiedAccess>]
module DifferentialOracle =

    /// Assert that the two runtimes agreed about how the guest terminated, and that
    /// they agreed on the exit code the case declares.
    ///
    /// `fileName` names the guest in every failure message; `expectsUnhandledException`
    /// is the case's own declaration that an escaping exception is the point of the
    /// test rather than a surprise.
    let compareOutcomes
        (fileName : string)
        (expectedReturnCode : int)
        (expectsUnhandledException : bool)
        (realResult : RealRuntimeResult)
        (pawPrintResult : RunOutcome)
        : unit
        =
        // NormalExit and ProcessExit both represent a clean process termination with
        // an exit code on the terminating thread's eval stack; the only difference is
        // whether the guest returned from Main or called Environment.Exit. The real
        // runtime surfaces both as RealRuntimeResult.NormalExit, so normalise here.
        let normalisedPawPrint =
            match pawPrintResult with
            | RunOutcome.ProcessExit (s, t) -> RunOutcome.NormalExit (s, t)
            | other -> other

        match realResult, normalisedPawPrint with
        | RealRuntimeResult.NormalExit exitCode, RunOutcome.NormalExit (terminalState, terminatingThread) ->
            if exitCode <> expectedReturnCode then
                failwith
                    $"Real runtime exited with code %d{exitCode} for %s{fileName}, but the case declares ExpectedReturnCode = %d{expectedReturnCode}."

            let pawPrintExitCode =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> failwith "expected program to return a value, but it returned void"
                | head :: _ ->
                    match head with
                    | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                    | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

            if pawPrintExitCode <> exitCode then
                failwith
                    $"PawPrint exited with code %d{pawPrintExitCode} for %s{fileName}, but the real runtime exited with %d{exitCode}."
        | RealRuntimeResult.UnhandledException _, RunOutcome.GuestUnhandledException _ ->
            if not expectsUnhandledException then
                failwith
                    $"Both runtimes threw unhandled exceptions for %s{fileName}, but this test was not expected to throw. Add to expectsUnhandledException if intentional."
        | RealRuntimeResult.NormalExit exitCode, RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith
                $"Real runtime exited normally with code %d{exitCode}, but PawPrint threw unhandled exception: %O{exn.ExceptionObject}"
        | RealRuntimeResult.Aborted (_code, report), _ ->
            failwith
                $"Real runtime called Environment.FailFast for %s{fileName}; this fixture does not exercise FailFast:\n%s{report}"
        | RealRuntimeResult.UnhandledException realExn, RunOutcome.NormalExit (terminalState, terminatingThread) ->
            let pawPrintExitCode =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> None
                | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> Some i
                | _ -> None

            failwith
                $"Real runtime terminated with an unhandled exception, but PawPrint exited normally (code: %O{pawPrintExitCode}):\n%s{realExn}"
        | _, RunOutcome.Aborted (_, _, fatal) ->
            let m = fatal.Message |> Option.defaultValue "<no message>"

            failwith $"PawPrint guest aborted (%O{fatal.Code}) for %s{fileName}: %s{m}"
        | _, RunOutcome.SignalTerminated (_, signal) ->
            failwith
                $"PawPrint guest was terminated by POSIX signal %O{signal} for %s{fileName}; this test does not exercise signal-driven termination"
        | _, RunOutcome.ProcessExit _ -> failwith "unreachable: normalised away above"

    /// The oracle loads the guest under a fixed `runtimeconfig.json`
    /// (`RealRuntime.runtimeConfig`) that carries no `configProperties`, so a case's
    /// AppContext properties never reach the real runtime. Comparing a seeded PawPrint
    /// against an unseeded oracle would dress a PawPrint-only fact up as a
    /// cross-runtime one, so a case that seeds properties must not be compared.
    let assertComparable (case : EndToEndTestCase) : unit =
        if not (AppContextProperties.isEmpty case.AppContext) then
            failwith
                $"%s{case.FileName} sets AppContext properties (%O{case.AppContext}), but its OraclePolicy asks for a differential comparison (%O{case.Oracle}). Drop the properties, or -- if the case exists to assert what they do -- register it in sourcesImpure with Oracle = OraclePolicy.Never."
