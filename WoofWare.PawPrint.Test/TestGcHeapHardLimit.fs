namespace WoofWare.PawPrint.Test

open System
open FsUnitTyped
open NUnit.Framework

/// The test host deliberately runs with a bounded GC heap, configured as a
/// RuntimeHostConfigurationOption in WoofWare.PawPrint.Test.fsproj.
///
/// These tests exist because that setting is invisible in a normal passing run, and the failure it
/// prevents is silent: unbounded, this suite's test host peaks around 21.8 GB RSS on a big machine,
/// which on a 16 GB CI runner gets it SIGKILLed by the OOM killer mid-run. That surfaces only as
/// "Test host process crashed" with no stderr and no indication of which test or that memory was
/// even involved. If the wiring ever breaks, nothing else in the suite would notice.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGcHeapHardLimit =

    /// Must match the value of System.GC.HeapHardLimitPercent in WoofWare.PawPrint.Test.fsproj.
    [<Literal>]
    let private ExpectedPercent : int = 50

    [<Test>]
    let ``the heap hard limit percentage reaches the test host`` () : unit =
        match AppContext.GetData "System.GC.HeapHardLimitPercent" with
        | null ->
            failwith
                "System.GC.HeapHardLimitPercent is not set in the test host. It is configured as a RuntimeHostConfigurationOption in WoofWare.PawPrint.Test.fsproj; if that wiring has broken, the test host can be OOM-killed on CI with no diagnostics."
        | value ->
            // Decimal, not hex: runtimeconfig properties are parsed with base 0 by
            // coreclr/utilcode/configuration.cpp, unlike the DOTNET_GCHeapHardLimitPercent
            // environment variable, which coreclr/vm/gcenv.ee.cpp parses as base 16.
            string value |> shouldEqual (string ExpectedPercent)

    [<Test>]
    let ``the GC applies the heap hard limit`` () : unit =
        // That the property reaches the process does not mean the GC acted on it. Both figures below
        // are computed in gc.cpp's GetMemoryInfo from its single notion of total memory, and only one
        // of them is affected by the limit:
        //
        //     highMemLoadThresholdBytes = highMemPercent% of total
        //     totalAvailableMemoryBytes = the heap hard limit, or total if there is none
        //
        // So their ratio answers the question without anyone needing to know what "total" is. That
        // matters: asking the host for its physical memory would need a platform-specific route (and
        // a sandbox can refuse it), and would be the wrong number anyway under a container memory
        // limit, where the GC's "total" is the cgroup limit. Here the total simply cancels.
        //
        // highMemPercent is 90..97 (gc.cpp derives it as 100 - available_mem_th, with available_mem_th
        // in 3..10), so the ratio is ~0.52 when the 50% limit applies, ~0.85 were the value ever
        // parsed as hex (0x50 = 80), and ~1.05 if the limit did not apply at all.
        //
        // Neither figure comes from the last GC's record, so both are valid even if no GC has run.
        let info = GC.GetGCMemoryInfo ()

        if info.HighMemoryLoadThresholdBytes <= 0L then
            Assert.Ignore "The runtime reported no high-memory-load threshold to compare against"

        let ratioPercent =
            info.TotalAvailableMemoryBytes * 100L / info.HighMemoryLoadThresholdBytes

        // 0.7 separates the three cases above with room to spare. The one way to provoke a false
        // failure is to drive highMemPercent below 71 with DOTNET_GCHighMemPercent, which nothing
        // here does.
        if ratioPercent > 70L then
            failwith
                $"Expected the GC to cap its heap at %d{ExpectedPercent}%% of memory, but it reports %d{info.TotalAvailableMemoryBytes} bytes available against a high-memory-load threshold of %d{info.HighMemoryLoadThresholdBytes} bytes, a ratio of %d{ratioPercent}%% where at most 70%% was expected. The heap hard limit is not being applied (or DOTNET_GCHighMemPercent is set below 71)."
