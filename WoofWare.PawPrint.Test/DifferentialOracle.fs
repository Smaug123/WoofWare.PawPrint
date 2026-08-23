namespace WoofWare.PawPrint.Test

open System.Threading.Tasks
open WoofWare.PawPrint

/// Comparing one guest's behaviour under PawPrint against the same guest under the
/// real .NET runtime.
///
/// Which cases get compared, and on which hosts, is `OraclePolicy`'s question; this
/// module only knows what "agreeing" means once both runtimes have answered.
[<RequireQualifiedAccess>]
module DifferentialOracle =

    /// Run the real-runtime oracle at the same time as the interpreted run, instead of
    /// after it, and return both answers once both have finished.
    ///
    /// A guest's two runs do not interact: the oracle is a separate process, and the only
    /// thing that crosses between them is the PE image, which both merely read. So the
    /// overlap is invisible to each side, and in particular the interpreted run is as
    /// deterministic as it was — the interpreter still sees one thread driving it, and
    /// PawPrint's own scheduler, not the host's, decides what the guest observes.
    ///
    /// The oracle gets a dedicated thread rather than a pool one. It spends nearly all of
    /// its time blocked on a child process, and every NUnit worker running one of these
    /// blocks until it finishes; on a pool thread that is a queue of blocked work items
    /// waiting for the pool's thread-injection heuristic to notice, which reintroduces
    /// exactly the serialisation this exists to remove.
    ///
    /// Both runs are awaited before this returns, *including* when the interpreted run
    /// throws. The oracle owns a child process and a scratch directory it deletes on its
    /// way out, so abandoning it mid-flight would leak both into the rest of the suite.
    let alongsideInterpreted (oracle : unit -> 'oracle) (interpreted : unit -> 'interpreted) : 'oracle * 'interpreted =
        let oracleRun =
            Task.Factory.StartNew ((fun () -> oracle ()), TaskCreationOptions.LongRunning)

        let interpretedResult =
            try
                interpreted ()
            with _ ->
                // Deliberately swallowed: the interpreted run's failure is the one worth
                // reporting, and this wait is only here to be sure the child process and
                // its scratch directory are gone before the test ends.
                (try
                    oracleRun.Wait ()
                 with _ ->
                     ())

                reraise ()

        // Not `.Result`, which would wrap a failure in an AggregateException and bury the
        // oracle's own message.
        oracleRun.GetAwaiter().GetResult (), interpretedResult

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
