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
        |> UnixBootImage.withCoreDumps coreDumps
        |> EmulatedKernel.boot

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    [<Test>]
    let ``the runtime's own SIGABRT handler does not save the process`` () : unit =
        for platform in platforms do
            let kernel = kernelOn platform CoreDumps.Suppressed

            // The runtime starts with SIGABRT caught by its own handler, which
            // `PROCAbort` takes away before it aborts.
            match KernelSignals.disposition Signal.SIGABRT (EmulatedKernel.unix kernel) with
            | SignalDisposition.Catch action ->
                action.Handler
                |> shouldEqual (NativeSignalHandler.CoreClrPalFault PalReplacedDisposition.Default)
            | other -> failwith $"expected the runtime's handler, got %A{other}"

            EmulatedKernel.abort thread kernel
            |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, false))

    [<Test>]
    let ``an abort from inside a handler that blocks SIGABRT is refused`` () : unit =
        // `abort(3)` unblocks SIGABRT before raising it, which would change the
        // mask of the handler it is called from. The model holds a mask only as
        // handler frames, and PawPrint never leaves one pushed between
        // instructions, so this is a state only a test builds; the abort is
        // refused rather than answered as if the signal were unblocked.
        for platform in platforms do
            let kernel =
                kernelOn platform CoreDumps.Suppressed
                |> SignalFrames.enter thread (Set.singleton Signal.SIGABRT)

            Assert.Throws (fun () -> EmulatedKernel.abort thread kernel |> ignore<ProcessTermination>)
            |> ignore<exn>

    [<Test>]
    let ``an abort dumps core exactly when the process writes dumps`` () : unit =
        for platform in platforms do
            for coreDumps in [ CoreDumps.Suppressed ; CoreDumps.Written ] do
                EmulatedKernel.abort thread (kernelOn platform coreDumps)
                |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, coreDumps = CoreDumps.Written))
