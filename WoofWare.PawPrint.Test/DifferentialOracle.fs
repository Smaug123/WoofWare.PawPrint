namespace WoofWare.PawPrint.Test

open System.Threading.Tasks
open WoofWare.PawPrint
open WoofWare.PosixKernel

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
        (pawPrintResult : RunEnd)
        : unit
        =
        // A guest the real runtime runs to an answer has no undefined value in its control flow
        // or its result, so a stop is never an agreement.
        let pawPrintResult =
            match pawPrintResult with
            | RunEnd.Ended outcome -> outcome
            | RunEnd.StoppedAtUndefinedValue (_, _, observation) ->
                failwith $"PawPrint: guest used an undefined value: %O{observation}"

        // NormalExit and ProcessExit both represent a clean process termination with
        // the latched exit code; the only difference is whether the guest returned from
        // Main or called Environment.Exit. The real runtime surfaces both as
        // RealRuntimeResult.NormalExit, so normalise here.
        let normalisedPawPrint =
            match pawPrintResult with
            | RunOutcome.ProcessExit (s, t, termination) -> RunOutcome.NormalExit (s, t, termination)
            | other -> other

        match realResult, normalisedPawPrint with
        | RealRuntimeResult.NormalExit exitCode, RunOutcome.NormalExit (terminalState, _, termination) ->
            if exitCode <> expectedReturnCode then
                failwith
                    $"Real runtime exited with code %d{exitCode} for %s{fileName}, but the case declares ExpectedReturnCode = %d{expectedReturnCode}."

            let pawPrintExitCode = terminalState.LatchedExitCode

            if pawPrintExitCode <> exitCode then
                failwith
                    $"PawPrint exited with code %d{pawPrintExitCode} for %s{fileName}, but the real runtime exited with %d{exitCode}."

            // `Process.ExitCode` is how .NET renders the real process's wait status, which
            // is how a shell renders it too.
            let rendered =
                ProcessTermination.shellStatus
                    (SimulatedUnixPlatform.signalNumbering terminalState.Kernel.UnixPlatform)
                    termination

            if rendered <> exitCode then
                failwith
                    $"PawPrint's kernel says %s{fileName} ended by %O{termination}, which a parent reads as %d{rendered}, but the real runtime exited with %d{exitCode}."
        | RealRuntimeResult.UnhandledException realExn,
          RunOutcome.GuestUnhandledException (finalState, _, exn, termination) ->
            if not expectsUnhandledException then
                failwith
                    $"Both runtimes threw unhandled exceptions for %s{fileName}, but this test was not expected to throw. Add to expectsUnhandledException if intentional.\nReal runtime:\n%s{realExn}\nPawPrint:\n%s{UnhandledExceptionReport.describe finalState exn}"

            // The real runtime aborts, which is what the oracle's classification of it
            // as an unhandled exception rests on.
            match termination with
            | ProcessTermination.Signaled (Signal.SIGABRT, _) -> ()
            | other ->
                failwith
                    $"PawPrint's kernel says %s{fileName} ended by %O{other} after an unhandled exception, but the runtime ends such a process with SIGABRT."
        | RealRuntimeResult.NormalExit exitCode, RunOutcome.GuestUnhandledException (finalState, _, exn, _) ->
            failwith
                $"Real runtime exited normally with code %d{exitCode}, but PawPrint threw unhandled exception:\n%s{UnhandledExceptionReport.describe finalState exn}"
        | RealRuntimeResult.Aborted (_code, report), _ ->
            failwith
                $"Real runtime called Environment.FailFast for %s{fileName}; this fixture does not exercise FailFast:\n%s{report}"
        | RealRuntimeResult.UnhandledException realExn, RunOutcome.NormalExit (terminalState, _, _) ->
            failwith
                $"Real runtime terminated with an unhandled exception, but PawPrint exited normally (code: %d{terminalState.LatchedExitCode}):\n%s{realExn}"
        | _, RunOutcome.Aborted (_, _, fatal, _) ->
            let m = fatal.Message |> Option.defaultValue "<no message>"

            failwith $"PawPrint guest aborted (%O{fatal.Code}) for %s{fileName}: %s{m}"
        | _, RunOutcome.SignalTerminated (_, signal, _) ->
            failwith
                $"PawPrint guest was terminated by POSIX signal %O{signal} for %s{fileName}; this test does not exercise signal-driven termination"
        | _, RunOutcome.ProcessExit _ -> failwith "unreachable: normalised away above"

    /// Refuse a case whose configuration the oracle cannot reproduce, so that
    /// comparing it would dress a PawPrint-only fact up as a cross-runtime one.
    ///
    /// The oracle loads the guest under a fixed `runtimeconfig.json`
    /// (`RealRuntime.runtimeConfig`) that carries no `configProperties`, so a case's
    /// AppContext properties never reach the real runtime. It materialises the seed
    /// in a scratch directory that belongs to whoever runs the tests, so it cannot
    /// give that directory another owner either. (A seed *entry* that states an
    /// owner is refused where the oracle materialises it.)
    let assertComparable (case : EndToEndTestCase) : unit =
        if not (AppContextProperties.isEmpty case.AppContext) then
            failwith
                $"%s{case.FileName} sets AppContext properties (%O{case.AppContext}), but its OraclePolicy asks for a differential comparison (%O{case.Oracle}). Drop the properties, or -- if the case exists to assert what they do -- register it in sourcesImpure with Oracle = OraclePolicy.Never."

        match case.KernelConfig.FileSystemRootOwner with
        | None -> ()
        | Some owner ->
            failwith
                $"%s{case.FileName} gives its seed's root directory the owner %O{owner} (KernelConfig.FileSystemRootOwner), but its OraclePolicy asks for a differential comparison (%O{case.Oracle}), and the oracle's root is a scratch directory owned by whoever runs the tests. Leave the root owner as None, or register the case in sourcesImpure with Oracle = OraclePolicy.Never."
