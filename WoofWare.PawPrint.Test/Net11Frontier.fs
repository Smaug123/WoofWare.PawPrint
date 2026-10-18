namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// Why a test is parked on `EmulatedRuntime.Net11`, for each cause that blocks more than a handful
/// of tests there. Each was measured on 11.0.0-rc.1.26425.128 on 2026-10-14, with the test failing
/// under PawPrint and not on account of the real runtime; a park whose cause is specific to one
/// test says so at the park instead.
[<RequireQualifiedAccess>]
module Net11Frontier =

    /// .NET 11 reads a method's handle from any `IRuntimeMethodInfo` at one field offset.
    let runtimeMethodInfoValue : string =
        "On .NET 11, `IRuntimeMethodInfo.GetValue` reads the method handle of any `IRuntimeMethodInfo` as `Unsafe.As<RuntimeMethodInfoStub>(method).m_value` (RuntimeHandles.cs:980), relying on every implementation keeping its handle at that field's offset. PawPrint resolves the field by identity, so on a `RuntimeMethodInfo`, whose handle is `m_handle`, the read stops: \"CliValueType.FindFieldById: field '...::m_value' not found\". Reflection over methods, `MethodBase.Invoke` and F#'s printf all reach it. Un-park when that read is answered."

    /// .NET 11's `Delegate_BindToMethodInfo` has a new signature.
    let delegateBindToMethodInfo : string =
        "On .NET 11, `Delegate.BindToMethodInfo` calls the `Delegate_BindToMethodInfo` QCall with a new signature, `(MethodTable*, MethodTable*, RuntimeMethodHandleInternal, QCallTypeHandle, DelegateBindingFlags, ObjectHandleOnStack, BindToMethodDetails*) -> int`, which PawPrint's handler, matching .NET 10's, does not recognise: \"Unimplemented native method (PInvokeImpl QCall!Delegate_BindToMethodInfo)\". `DynamicMethod.CreateDelegate` reaches it. Un-park when the handler takes .NET 11's signature."

    /// .NET 11's `FileStream` asks whether the filesystem supports locking.
    let fileSystemSupportsLocking : string =
        "On .NET 11, opening a file through `SafeFileHandle` asks `Interop.Sys.FileSystemSupportsLocking` (SafeFileHandle.Unix.cs:528), the `SystemNative_FileSystemSupportsLocking` P/Invoke, which PawPrint does not implement: \"Unimplemented native method (PInvokeImpl libSystem.Native!SystemNative_FileSystemSupportsLocking)\". Un-park when the emulated kernel answers it."

    /// Roslyn's inline-array helpers over references, which .NET 11's CoreLib uses.
    let inlineArrayOfReferences : string =
        "On .NET 11, CoreLib and System.Linq index inline arrays of object references through Roslyn's `<PrivateImplementationDetails>.InlineArrayElementRef` and `InlineArrayAsSpan`, which reinterpret the inline-array local. PawPrint refuses that view: \"refusing byte view over value type containing object references in plain byref\". Un-park when such an inline array can be indexed."

    /// .NET 11's Linux CoreLib blocks on a futex.
    let linuxFutex : string =
        "On .NET 11's Linux CoreLib, `LowLevelLock` and `LowLevelLifoSemaphore` block through `LowLevelThreadBlocker`, which calls the `SystemNative_LowLevelFutex_WaitOnAddress` P/Invoke that PawPrint does not implement: \"Unimplemented native method (PInvokeImpl libSystem.Native!SystemNative_LowLevelFutex_WaitOnAddress)\". The Darwin CoreLib blocks on `LowLevelMonitor` there instead, so these pass on a macOS host; measured on CI's Linux leg on 2026-10-17. Un-park when the futex wait runs."

    /// `UnicodeEncoding.GetBytes` orders two byte views of one stackalloc block.
    let stackallocByteOrder : string =
        "On .NET 11, `UnicodeEncoding.GetBytes` (IL offset 797) compares two byte views of one stackalloc block with `clt.un`, and PawPrint refuses: \"refusing clt.un: no structural order between byrefs <<byte 17 of <stack memory block #0> ...>> and <<byte 92 of <stack memory block #0> ...>>\". Sockets, pipes and `EventSource` reach it. Un-park when those byrefs are ordered by their byte coordinates."

    /// `MemoryExtensions.MinMaxInteger` bit-casts a value type holding runtime pointers.
    let bitCast : string =
        "On .NET 11, `MemoryExtensions.MinMaxInteger` (reached from span searches such as `string.StartsWith`) calls `Unsafe.BitCast` over a value type holding runtime pointers, which PawPrint's byte model refuses: \"Unsafe.BitCast ... input is not byte-addressable: rejected: value type containing runtime pointers\". Un-park when such a value can be bit-cast."

    /// The `MarshalNative_HasLayout` QCall, which .NET 11's `Marshal` asks before laying out a type.
    let marshalHasLayout : string =
        "On .NET 11, `Marshal.StructureToPtr`, `PtrToStructure` and `SizeOf` ask the `MarshalNative_HasLayout` QCall, which PawPrint does not implement: \"Unimplemented native method (PInvokeImpl QCall!MarshalNative_HasLayout)\". Un-park when it is answered."

    /// `System.GCMemoryInfoData`'s layout changed.
    let gcMemoryInfo : string =
        "On .NET 11, `System.GCMemoryInfoData` has no `_generationInfo0` field, which `GC.GetMemoryInfo`'s native filler writes: \"System.GCMemoryInfoData does not declare expected instance field '_generationInfo0'\". Un-park when the native filler writes .NET 11's layout."

    /// The checked-in hardware-intrinsic table is .NET 10's.
    let hardwareIntrinsicTable : string =
        "`HardwareIntrinsicTable.tsv` reproduces .NET 10's JIT tables. .NET 11's `hwintrinsiclistarm64.h` has rows of a shape the reader does not parse, and .NET 11 adds instruction sets no row covers (Arm `Sha3`, x64 `Avx512Bmm`, `AvxVnni.V512`, `X86Base.X64.BigMul`). Un-park when the table is kept per runtime."

    /// The checked-in native contract table is .NET 10's.
    let nativeContractTable : string =
        "`NativeContractTable.tsv` was read from .NET 10's source, and has rows for natives .NET 11 does not have: the `Environment.get_CurrentManagedThreadId` FCall, which .NET 11's CoreLib does not declare (it reads a thread-static instead), citing `JIT_GetCurrentManagedThreadId`, which .NET 11's VM does not define; and `Monitor`'s `TryEnter_FastPath` and `Exit_FastPath` FCalls and `Monitor_Enter_Slowpath` and `Monitor_Exit_Slowpath` QCalls, which .NET 11 replaces with its managed thin lock, the third of which runs `SynchronizationContext.InvokeWaitMethodHelper`, which .NET 11's binder does not name and its CoreLib does not define. Un-park when the table is kept per runtime."

    /// The escape analysis models .NET 10's helpers around a synchronized method.
    let synchronizedMethodHelpers : string =
        "On .NET 11, the JIT brackets a synchronized method with `Monitor.SynchronizedMethodEnter` and `Monitor.SynchronizedMethodExit` (corelib.h's `SYNCHRONIZED_METHOD_ENTER` and `_EXIT`), where .NET 10 calls `Monitor.Enter(object, ref bool)` and `Monitor.ExitIfLockTaken`, which RC1's CoreLib does not define. The escape analysis runs .NET 10's pair, so over RC1's CoreLib it refuses every synchronized method it reaches: \"expected System.Private.CoreLib, Version=11.0.0.0 ... to define one static System.Threading.Monitor.ExitIfLockTaken of 2 arguments, found 0\". Un-park when the analysis runs each runtime's helpers."

    /// Real .NET 11 ends the process when making an exception raises another.
    let exceptionMaking : string =
        "Measured on real .NET 11: when the parameterless constructor the runtime runs to make a fault's or a failed bind's exception raises an exception of its own (here, from the culture's resource lookup), the process aborts (SIGABRT, exit 134), where .NET 10 delivers the constructor's exception in place of the one being made, as `ExceptionMaking` describes. `ExceptionMaking` describes .NET 10's making. Un-park when it describes each runtime's."

    /// Real .NET 11 ends the process when wrapping a thrown non-exception raises.
    let nonExceptionWrapping : string =
        "Measured on real .NET 11: when `RuntimeWrappedException(object)`, which the runtime runs to wrap a thrown object that is not an exception, raises an exception of its own (here, from the culture's resource lookup), the process overflows its stack through `Exception.InternalPreserveStackTrace` and aborts (exit 134), where .NET 10 throws the constructor's exception in the wrapper's place, as the escape analysis describes. The analysis describes .NET 10's wrapping. Un-park when it describes each runtime's."

    /// Park the calling test on .NET 11 alone, for `reason`: `ParkedOn.check` on that runtime.
    let park (reason : string) : unit =
        ParkedOn.check [ EmulatedRuntime.Net11 ] reason
