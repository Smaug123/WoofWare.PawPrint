namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// A `[UnmanagedCallersOnly]` method may be entered only from native code. CoreCLR compiles one
/// with `CORJIT_FLAG_REVERSE_PINVOKE` (jitinterface.cpp), whose prologue performs a reverse
/// P/Invoke transition asserting *preemptive* GC mode; managed code runs cooperative, so any
/// managed entry reaches `ReversePInvokeBadTransition` (dllimportcallback.cpp) and takes the
/// process down with `COR_E_EXECUTIONENGINE` and the message
/// "Invalid Program: attempted to call a UnmanagedCallersOnly method from managed code."
///
/// That is a fatal error, not an exception: no `catch` sees it and no handler search runs. So these
/// guests, though they live in `sourcesImpure/`, cannot be *registered* impure cases —
/// `TestImpureCases.runTest` treats a fatal outcome as a test failure — and are driven here against
/// the real runtime as oracle instead.
///
/// The four are four *arrivals* at one refusal, not four refusals: a delegate, plain reflection,
/// reflection through CoreLib's emitted invoke stub, and a managed `calli` over the method's
/// address. They are separate guests because they reach the callee through different interpreter
/// code, and a gate covering only some of them would pass a narrower test. A fifth,
/// `UnmanagedCallersOnlyCctorNotRun.cs`, pins *when* the refusal happens rather than that it does.
///
/// The routes that must keep working are the control, `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs`:
/// the legal `delegate* unmanaged<...>` call site, and binding a delegate without invoking it.
[<TestFixture>]
module TestUnmanagedCallersOnlyEntry =

    let private assy = typeof<RunResult>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

    /// The guests, each named by the interpreter path it arrives through.
    let private cases : string list =
        [
            "UnmanagedCallersOnlyDelegateInvoke.cs"
            "UnmanagedCallersOnlyReflectionInvoke.cs"
            "UnmanagedCallersOnlyForceEmitInvoke.cs"
            "UnmanagedCallersOnlyManagedCalli.cs"
        ]

    /// The distinguishing half of CoreCLR's message. Asserted on both runtimes: an abort for some
    /// *other* reason would otherwise pass this test, and under PawPrint in particular a
    /// `FailFast` from somewhere inside CoreLib would look identical at the outcome level.
    let private messageMarker = "UnmanagedCallersOnly"

    let private runUnderPawPrint (sourceName : string) (argv : string list) (image : byte[]) : RunOutcome =
        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        use peImage = new MemoryStream (image)

        let hostConfig = HostConfig.Default dotnetRuntimes

        let hostConfig =
            { hostConfig with
                Guest =
                    { hostConfig.Guest with
                        Argv = argv
                    }
            }

        try
            Program.run loggerFactory (Some sourceName) peImage hostConfig
        with _ ->
            for message in messages () do
                Console.Error.WriteLine $"{message}"

            reraise ()

    /// What the guest wrote to stderr, as PawPrint's emulated kernel recorded it.
    let private guestStandardError (state : IlMachineState) : string =
        OutputLogEntry.bytesFor FileDescriptorRole.StandardError state.Kernel.OutputLog
        |> Seq.toArray
        |> Text.Encoding.UTF8.GetString

    [<TestCaseSource(nameof cases)>]
    let ``entering an UnmanagedCallersOnly method from managed code is fatal`` (sourceName : string) : unit =
        let image = Roslyn.compile [ Assembly.getEmbeddedResourceAsString sourceName assy ]

        // The oracle first, so that a guest which stopped reaching the refusal on real .NET is
        // reported as such rather than as a PawPrint divergence.
        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.Aborted (ObservedFatalError.Other, report) ->
            if not (report.Contains messageMarker) then
                failwith
                    $"%s{sourceName}: the real runtime aborted, but for some other reason than the UnmanagedCallersOnly transition:\n%s{report}"
        | RealRuntimeResult.Aborted (ObservedFatalError.FailFast, report) ->
            failwith
                $"%s{sourceName}: the real runtime printed the `Process terminated.` banner, so it aborted with COR_E_FAILFAST rather than the execution-engine error a bad reverse-P/Invoke transition raises:\n%s{report}"
        | RealRuntimeResult.NormalExit exitCode ->
            failwith
                $"%s{sourceName}: the real runtime returned %d{exitCode} instead of aborting, so this guest no longer reaches the UnmanagedCallersOnly transition and no longer tests anything"
        | RealRuntimeResult.UnhandledException report ->
            failwith
                $"%s{sourceName}: the real runtime terminated with an unhandled exception, so the refusal has become catchable:\n%s{report}"

        match runUnderPawPrint sourceName [] image with
        | RunOutcome.Aborted (_, _, fatal) ->
            fatal.Code |> shouldEqual FatalErrorCode.ExecutionEngine

            match fatal.Message with
            | None -> failwith $"%s{sourceName}: PawPrint aborted with no message; expected one naming the refusal"
            | Some message ->
                if not (message.Contains messageMarker) then
                    failwith $"%s{sourceName}: PawPrint aborted with the wrong message: %s{message}"
        | RunOutcome.NormalExit (terminalState, terminatingThread)
        | RunOutcome.ProcessExit (terminalState, terminatingThread) ->
            let returned =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> "void"
                | ret :: _ -> $"%O{ret}"

            failwith
                $"%s{sourceName}: PawPrint ran the UnmanagedCallersOnly method and returned %s{returned}; the real runtime refuses this entry uncatchably"
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"%s{sourceName}: PawPrint let the refusal become a catchable exception: %O{exn.ExceptionObject}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"%s{sourceName}: PawPrint terminated the guest by POSIX signal %O{signal}"

    /// The refusal precedes the callee's class initialiser, which is what says the gate sits ahead
    /// of frame commitment rather than anywhere inside the call that happens to abort.
    ///
    /// Measured on real .NET, and asserted on both runtimes here: a printing static constructor on
    /// the declaring type never prints. The `run` half is the non-vacuity guard — the same guest,
    /// entering the same type by an ordinary route, must print — because "the marker is absent" is
    /// otherwise satisfied by a guest that could not have printed it at all.
    [<Test>]
    let ``the refusal precedes the declaring type's static constructor`` () : unit =
        let sourceName = "UnmanagedCallersOnlyCctorNotRun.cs"
        let marker = "HOLDER CCTOR RAN"

        let image = Roslyn.compile [ Assembly.getEmbeddedResourceAsString sourceName assy ]

        // Non-vacuity, on the oracle: reading the type runs the constructor and the marker reaches
        // stderr, so its absence below is a fact about *when* the refusal happens.
        match RealRuntime.executeWithRealRuntime [| "run" |] image with
        | RealRuntimeResult.NormalExit 0 -> ()
        | other ->
            failwith
                $"the real runtime did not complete the ordinary read of the declaring type, so the absence assertion below would be vacuous: %O{other}"

        match RealRuntime.executeWithRealRuntime [| "call" |] image with
        | RealRuntimeResult.Aborted (ObservedFatalError.Other, report) ->
            if not (report.Contains messageMarker) then
                failwith $"the real runtime aborted for some other reason:\n%s{report}"

            if report.Contains marker then
                failwith
                    $"the real runtime ran the declaring type's static constructor before refusing the entry; this test's premise is wrong:\n%s{report}"
        | other -> failwith $"the real runtime did not refuse the managed entry: %O{other}"

        // Non-vacuity again, on PawPrint: the same read must print through PawPrint's own console
        // path, or its silence below would say nothing.
        match runUnderPawPrint sourceName [ "run" ] image with
        | RunOutcome.NormalExit (terminalState, _)
        | RunOutcome.ProcessExit (terminalState, _) ->
            let written = guestStandardError terminalState

            if not (written.Contains marker) then
                failwith
                    $"PawPrint completed the ordinary read of the declaring type without the static constructor reaching stderr, so the absence assertion below would be vacuous; stderr was: %s{written}"
        | other -> failwith $"PawPrint did not complete the ordinary read of the declaring type: %O{other}"

        match runUnderPawPrint sourceName [ "call" ] image with
        | RunOutcome.Aborted (terminalState, _, fatal) ->
            fatal.Code |> shouldEqual FatalErrorCode.ExecutionEngine

            let written = guestStandardError terminalState

            if written.Contains marker then
                failwith
                    $"PawPrint ran the declaring type's static constructor before refusing the entry, so the gate sits after class initialisation; stderr was: %s{written}"
        | other -> failwith $"PawPrint did not refuse the managed entry: %O{other}"
