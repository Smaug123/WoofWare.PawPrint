namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// Why a test is parked on `EmulatedRuntime.Net11`, for each cause that blocks more than a handful
/// of tests there. Each was measured on 11.0.0-rc.1.26425.128 on 2026-10-14, with the test failing
/// under PawPrint and not on account of the real runtime; a park whose cause is specific to one
/// test says so at the park instead.
[<RequireQualifiedAccess>]
module Net11Frontier =

    /// .NET 11's `RuntimeMethodInfo` asks whether its method is async.
    let isAsyncMethod : string =
        "On .NET 11, `RuntimeMethodInfo` asks whether its method is an async method before invoking it or reading much of its metadata (`IsDisallowedAsyncHelper`, RuntimeMethodInfo.CoreCLR.cs:80), through the `RuntimeMethodHandle::IsAsyncMethod` FCall, which PawPrint does not implement: \"Unimplemented native method (InternalCall): System.Private.CoreLib System.RuntimeMethodHandle::IsAsyncMethod\". `MethodBase.Invoke`, F#'s printf and much of reflection over methods reach it. Un-park when it is answered."

    /// The checked-in native contract table is .NET 10's.
    let nativeContractTable : string =
        "`NativeContractTable.tsv` was read from .NET 10's source, and has rows for natives .NET 11 does not have: the `Environment.get_CurrentManagedThreadId` FCall, which .NET 11's CoreLib does not declare (it reads a thread-static instead), citing `JIT_GetCurrentManagedThreadId`, which .NET 11's VM does not define; and `Monitor`'s `TryEnter_FastPath` and `Exit_FastPath` FCalls and `Monitor_Enter_Slowpath` and `Monitor_Exit_Slowpath` QCalls, which .NET 11 replaces with its managed thin lock, the third of which runs `SynchronizationContext.InvokeWaitMethodHelper`, which .NET 11's binder does not name and its CoreLib does not define. Un-park when the table is kept per runtime."

    /// .NET 11's `SpanHelpers` copies and clears memory through QCalls the table has no row for.
    let spanHelpersMemoryQCalls : string =
        "On .NET 11, `SpanHelpers.Memmove` and `SpanHelpers.ClearWithoutReferences` hand a long copy or clear to QCalls whose entry points are `memmove` and `memset` (SpanHelpers.ByteMemOps.cs), where .NET 10 copies through the `Buffer_MemMove` QCall. `NativeContractTable.tsv` is .NET 10's (`nativeContractTable`), so it has no row for either, and the escape analysis cannot see through `CastHelpers.Box`, `Box_Nullable` or `Unbox_Nullable`, which copy a struct that way, nor so through any struct `box`, any `unbox` of a `Nullable<T>`, or `Unsafe.Unbox`. Un-park when the table is kept per runtime."

    /// Park the calling test on .NET 11 alone, for `reason`: `ParkedOn.check` on that runtime.
    let park (reason : string) : unit =
        ParkedOn.check [ EmulatedRuntime.Net11 ] reason
