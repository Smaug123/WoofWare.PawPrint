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

    /// A process as the runtime starts it, with one task.
    let private kernelOn (platform : SimulatedUnixPlatform) (coreDumps : CoreDumps) : EmulatedKernel =
        let kernel = EmulatedKernel.create platform

        kernel
        |> EmulatedKernel.mapTasks (
            UnixTaskTable.register thread (CpuId 0) (EmulatedKernel.osThreadId kernel.Process.ProcessId thread)
        )
        |> EmulatedKernel.mapProcess (UnixProcessState.withCoreDumps coreDumps)

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    [<Test>]
    let ``the runtime's own SIGABRT handler does not save the process`` () : unit =
        for platform in platforms do
            let kernel = kernelOn platform CoreDumps.Suppressed

            // The runtime starts with SIGABRT caught by its own handler, which
            // `PROCAbort` takes away before it aborts.
            SignalState.disposition Signal.SIGABRT kernel.Process.Signals
            |> shouldEqual (SignalDisposition.Catch NativeSignalHandler.CoreClrPal)

            EmulatedKernel.abort thread (ImmutableArray.Create thread) kernel
            |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, false))

    [<Test>]
    let ``a thread that blocks SIGABRT still dies of its own abort`` () : unit =
        for platform in platforms do
            let kernel =
                kernelOn platform CoreDumps.Suppressed
                |> EmulatedKernel.mapProcess (fun proc ->
                    { proc with
                        Signals = SignalState.block thread Signal.SIGABRT proc.Signals
                    }
                )

            EmulatedKernel.abort thread (ImmutableArray.Create thread) kernel
            |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, false))

    [<Test>]
    let ``an abort dumps core exactly when the process writes dumps`` () : unit =
        for platform in platforms do
            for coreDumps in [ CoreDumps.Suppressed ; CoreDumps.Written ] do
                EmulatedKernel.abort thread (ImmutableArray.Create thread) (kernelOn platform coreDumps)
                |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, coreDumps = CoreDumps.Written))
