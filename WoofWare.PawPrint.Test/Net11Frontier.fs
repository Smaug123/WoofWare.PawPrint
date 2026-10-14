namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// Why a test is parked on `EmulatedRuntime.Net11`, for each cause that blocks more than a handful
/// of tests there. Each was measured on 11.0.0-rc.1.26425.128 on 2026-10-14, with the test failing
/// under PawPrint and not on account of the real runtime; a park whose cause is specific to one
/// test says so at the park instead.
[<RequireQualifiedAccess>]
module Net11Frontier =

    /// .NET 11's `Monitor.Enter` takes CoreLib's managed thin lock.
    let thinLock : string =
        "On .NET 11, `Monitor.Enter` (and so every `lock`, including the one `AppContext.GetData` takes over its store) runs CoreLib's managed thin lock, `ObjectHeader.AcquireThinLock`, which reads the object header in front of the locked object's `RawData.Data`. PawPrint's heap objects have no header, so the read stops at a byte-view refusal that names whichever type is locked. .NET 10 enters through the `Monitor.TryEnter_FastPath` InternalCall instead. Un-park when an object's header can be addressed."

    /// `MemoryExtensions.MinMaxInteger` bit-casts a value type holding runtime pointers.
    let bitCast : string =
        "On .NET 11, `MemoryExtensions.MinMaxInteger` (reached from span searches such as `string.StartsWith`) calls `Unsafe.BitCast` over a value type holding runtime pointers, which PawPrint's byte model refuses: \"Unsafe.BitCast ... input is not byte-addressable: rejected: value type containing runtime pointers\". Un-park when such a value can be bit-cast."

    /// The `MarshalNative_HasLayout` QCall, which .NET 11's `Marshal` asks before laying out a type.
    let marshalHasLayout : string =
        "On .NET 11, `Marshal.StructureToPtr`, `PtrToStructure` and `SizeOf` ask the `MarshalNative_HasLayout` QCall, which PawPrint does not implement: \"Unimplemented native method (PInvokeImpl QCall!MarshalNative_HasLayout)\". Un-park when it is answered."

    /// The `ThreadNative_SetWaitSleepJoinState` QCall, which .NET 11's managed wait subsystem calls.
    let waitSleepJoin : string =
        "On .NET 11, a thread that sleeps, joins or waits goes through the managed `WaitSubsystem`, which calls the `ThreadNative_SetWaitSleepJoinState` QCall that PawPrint does not implement: \"Unimplemented native method (PInvokeImpl QCall!ThreadNative_SetWaitSleepJoinState)\". Un-park when RC1's managed wait subsystem runs."

    /// `System.GCMemoryInfoData`'s layout changed.
    let gcMemoryInfo : string =
        "On .NET 11, `System.GCMemoryInfoData` has no `_generationInfo0` field, which `GC.GetMemoryInfo`'s native filler writes: \"System.GCMemoryInfoData does not declare expected instance field '_generationInfo0'\". Un-park when the native filler writes .NET 11's layout."

    /// The thread-static managed thread id, which .NET 11's CoreLib reads where .NET 10 asks the VM.
    let managedThreadId : string =
        "On .NET 11, `Environment.CurrentManagedThreadId` and `System.Threading.Lock`'s owner check read the thread-static `ManagedThreadId.t_currentManagedThreadId` (ManagedThreadId.CoreCLR.cs:14), which CoreCLR fills when a thread starts. PawPrint does not seed it, so every thread reads 0, which `Lock` takes to mean no thread: a thread holding a `Lock` is told it does not. Un-park when thread creation seeds it."

    /// `IntrinsicBody` does not classify .NET 11's new intrinsics.
    let newIntrinsics : string =
        "On .NET 11, `Interlocked.CompareExchange(int*, int, int)` is a new `[Intrinsic]` whose body calls itself, which `IntrinsicBody` does not recognise as a JIT expansion, and `RuntimeHelpers.SetNextCallGenericContext` and `SetNextCallAsyncContinuation` are new placeholders that `IntrinsicBody` classifies as VM substitutions although the pinned VM source substitutes neither. Un-park when `IntrinsicBody` classifies .NET 11's new intrinsics."

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

    /// Park the calling test on .NET 11 alone, for `reason`: `ParkedOn.check` on that runtime.
    let park (reason : string) : unit =
        ParkedOn.check [ EmulatedRuntime.Net11 ] reason
