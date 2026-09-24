namespace WoofWare.PosixKernel.Test

open System
open FsUnitTyped
open NUnit.Framework

/// The test host deliberately runs with a bounded GC heap, configured as a
/// RuntimeHostConfigurationOption in WoofWare.PosixKernel.Test.fsproj, so that a runaway property
/// test fails with an OutOfMemoryException instead of taking all of the machine's memory. If the
/// wiring ever breaks, nothing else in the suite would notice.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGcHeapHardLimit =

    /// Must match the value of System.GC.HeapHardLimit in WoofWare.PosixKernel.Test.fsproj: 8 GiB.
    [<Literal>]
    let private ExpectedBytes : int64 = 8589934592L

    [<Test>]
    let ``the GC applies the heap hard limit`` () : unit =
        // gc.cpp's GetMemoryInfo reports the heap hard limit as the total available memory when
        // there is one, and the machine's physical memory (or a container's limit) otherwise. A
        // DOTNET_GCHeapHardLimit environment variable overrides the runtimeconfig property, and a
        // different value makes this fail, as it should.
        GC.GetGCMemoryInfo().TotalAvailableMemoryBytes |> shouldEqual ExpectedBytes
