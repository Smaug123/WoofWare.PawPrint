namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `EmulatedKernel.abort`: how CoreCLR's `PROCAbort` ends the process, answered by the
/// kernel.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmulatedKernelAbort =

    let private thread : ThreadId = ThreadId 0

    /// A process as the runtime starts it, with one task: `thread`, its leader.
    let private kernelOn (platform : SimulatedUnixPlatform) (coreDumps : CoreDumps) : EmulatedKernel =
        EmulatedKernel.image platform StandardStreamsConfig.piped
        |> KernelImage.mapProcess (ProcessLaunch.withCoreDumps coreDumps)
        |> EmulatedKernel.boot

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    /// `kernel` with `thread` blocking SIGABRT through `pthread_sigmask`.
    let private masked (kernel : EmulatedKernel) : EmulatedKernel =
        let numbering = SimulatedUnixPlatform.signalNumbering kernel.UnixPlatform

        let block =
            match numbering with
            | SignalNumbering.Linux -> 0
            | SignalNumbering.Darwin -> 1

        match
            UnixSignal.pthreadSigmask
                (kernel.Caller thread)
                block
                (Some (SignalMask.ofSignals numbering (Set.singleton Signal.SIGABRT)))
                kernel.System
        with
        | SignalMaskAnswer.Completed _, system -> EmulatedKernel.withUnix system kernel
        | SignalMaskAnswer.Failed errno, _ -> failwith $"pthread_sigmask failed with %O{errno}"

    [<Test>]
    let ``the runtime's own SIGABRT handler does not save the process`` () : unit =
        for platform in platforms do
            let kernel = kernelOn platform CoreDumps.Suppressed

            // The runtime starts with SIGABRT caught by its own handler, which
            // `PROCAbort` takes away before it aborts.
            match KernelSignals.disposition Signal.SIGABRT kernel.System with
            | SignalDisposition.Catch action ->
                action.Handler
                |> shouldEqual (NativeSignalHandler.CoreClrPalFault PalReplacedDisposition.Default)
            | other -> failwith $"expected the runtime's handler, got %A{other}"

            EndedProcess.termination (EmulatedKernel.abort thread kernel).Ended
            |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, false))

    [<Test>]
    let ``an abort unblocks SIGABRT first, so a thread that blocks it still dies of it`` () : unit =
        // `abort(3)` unblocks SIGABRT before raising it, whether the thread
        // blocked it with a mask call or is inside a handler whose mask holds
        // it.
        for platform in platforms do
            for blocked in [ masked ; SignalFrames.enter thread (Set.singleton Signal.SIGABRT) ] do
                let kernel = kernelOn platform CoreDumps.Suppressed |> blocked

                SignalMask.contains Signal.SIGABRT (SignalState.maskOf thread kernel.Signals)
                |> shouldEqual true

                EndedProcess.termination (EmulatedKernel.abort thread kernel).Ended
                |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, false))

    [<Test>]
    let ``an abort dumps core exactly when the process writes dumps`` () : unit =
        for platform in platforms do
            for coreDumps in [ CoreDumps.Suppressed ; CoreDumps.Written ] do
                EndedProcess.termination (EmulatedKernel.abort thread (kernelOn platform coreDumps)).Ended
                |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, coreDumps = CoreDumps.Written))

    [<Test>]
    let ``an abort ends a process whose SIGABRT is already pending, blocked`` () : unit =
        // A `raise(SIGABRT)` while the thread blocks it leaves the signal pending. The unblock
        // `abort(3)` makes lets it through, and the thread dies of it as it returns from that
        // call, before `abort` raises it again.
        for platform in platforms do
            let kernel = kernelOn platform CoreDumps.Suppressed |> masked
            let numbering = SimulatedUnixPlatform.signalNumbering platform

            let kernel =
                match
                    UnixSignal.pthreadKill
                        (kernel.Caller thread)
                        thread
                        (Signal.toRawSignoUnder numbering Signal.SIGABRT)
                        kernel.System
                with
                | ProcessFate.Continues (KillAnswer.Completed KillOutcome.ProcessRuns, system) ->
                    EmulatedKernel.withUnix system kernel
                | other -> failwith $"expected SIGABRT to be left pending, got %A{other}"

            SignalState.pending kernel.Signals
            |> List.map (fun entry -> entry.Signal)
            |> shouldEqual [ Signal.SIGABRT ]

            EndedProcess.termination (EmulatedKernel.abort thread kernel).Ended
            |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, false))
