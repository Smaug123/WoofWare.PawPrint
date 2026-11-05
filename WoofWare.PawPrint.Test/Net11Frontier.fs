namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// Why a test is parked on `EmulatedRuntime.Net11`, for each cause that blocks more than a handful
/// of tests there. Each was measured on 11.0.0-rc.1.26425.128, with the test failing
/// under PawPrint and not on account of the real runtime; a park whose cause is specific to one
/// test says so at the park instead.
[<RequireQualifiedAccess>]
module Net11Frontier =

    /// .NET 11's contract table has no row for the cast helpers' fallback.
    let castFallbackThroughUnmanagedCallersOnly : string =
        "On .NET 11, the QCall `IsInstanceOf_NoCacheLookup`, which the cast helpers fall back to, asks an object implementing `IDynamicInterfaceCastable` through an `[UnmanagedCallersOnly]` overload of `DynamicInterfaceCastableHelpers.IsInterfaceImplemented` (`UnmanagedCallersOnlyCaller::InvokeThrowing`, callhelpers.h): the overload catches what the method raises and hands it back, and the VM throws it again (`COMPlusThrow`), first running `Exception.InternalPreserveStackTrace` on it. A row naming the overload would hide what the method raises, since the escape analysis does not know the VM throws it again, so `NativeContractTable.Net11.tsv` has no row for the QCall and the analysis cannot see through the cast helpers. Un-park when a native's row can name a call the VM makes through `[UnmanagedCallersOnly]`."

    /// .NET 11's `SpanHelpers` copies and clears memory through QCalls the table has no row for.
    let spanHelpersMemoryQCalls : string =
        "On .NET 11, `SpanHelpers.Memmove` and `SpanHelpers.ClearWithoutReferences` hand a long copy or clear to QCalls whose entry points are `memmove` and `memset` (SpanHelpers.ByteMemOps.cs), where .NET 10 copies through the `Buffer_MemMove` QCall. `NativeContractTable.Net11.tsv` has no row for either, and the escape analysis cannot see through `CastHelpers.Box`, `Box_Nullable` or `Unbox_Nullable`, which copy a struct that way, nor so through any struct `box`, any `unbox` of a `Nullable<T>`, or `Unsafe.Unbox`. Un-park when .NET 11's table has rows for them."

    /// Park the calling test on .NET 11 alone, for `reason`: `ParkedOn.check` on that runtime.
    let park (reason : string) : unit =
        ParkedOn.check [ EmulatedRuntime.Net11 ] reason
