namespace WoofWare.PawPrint.Test

open System
open FsUnitTyped
open NUnit.Framework

/// The test host deliberately runs with a bounded GC heap, configured as a
/// RuntimeHostConfigurationOption in WoofWare.PawPrint.Test.fsproj.
///
/// These tests exist because that setting is invisible in a normal passing run, and the failures it
/// prevents are silent or far away: unbounded, this suite's test host peaks around 21.8 GB RSS on a
/// big machine, which on a 16 GB CI runner gets it SIGKILLed by the OOM killer mid-run, and a
/// runaway test on a developer machine takes the whole machine down with it. If the wiring ever
/// breaks, nothing else in the suite would notice.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGcHeapHardLimit =

    /// Must match the value of System.GC.HeapHardLimit in WoofWare.PawPrint.Test.fsproj: 8 GiB.
    [<Literal>]
    let private ExpectedBytes : int64 = 8589934592L

    [<Test>]
    let ``the heap hard limit reaches the test host`` () : unit =
        match AppContext.GetData "System.GC.HeapHardLimit" with
        | null ->
            failwith
                "System.GC.HeapHardLimit is not set in the test host. It is configured as a RuntimeHostConfigurationOption in WoofWare.PawPrint.Test.fsproj; if that wiring has broken, the test host can take all of a machine's memory."
        | value ->
            // Decimal, not hex: runtimeconfig properties are parsed with base 0 by
            // coreclr/utilcode/configuration.cpp, unlike the DOTNET_GCHeapHardLimit
            // environment variable, which coreclr/vm/gcenv.ee.cpp parses as base 16.
            string value |> shouldEqual (string ExpectedBytes)

    [<Test>]
    let ``the GC applies the heap hard limit`` () : unit =
        // That the property reaches the process does not mean the GC acted on it. gc.cpp's
        // GetMemoryInfo reports the heap hard limit as the total available memory when there is
        // one, and the machine's physical memory (or a container's limit) otherwise.
        //
        // A DOTNET_GCHeapHardLimit environment variable overrides the runtimeconfig property
        // (gcenv.ee.cpp consults the environment first), so a developer who sets one to a different
        // value will see this fail. That is the intended reading: the suite is then not running
        // under the ceiling CI runs it under.
        GC.GetGCMemoryInfo().TotalAvailableMemoryBytes |> shouldEqual ExpectedBytes
