namespace WoofWare.PawPrint

open System.Reflection

[<RequireQualifiedAccess>]
module NativeDispatch =
    // The native/extern boundary is the only runtime-specific surface PawPrint supplies (the managed
    // BCL is loaded from the guest's own assemblies), so this handler list IS "the native code for a
    // runtime". It is the net10 set: PawPrint currently emulates only `EmulatedRuntime.net10`. When a
    // second runtime is added, give it its own list and have `tryExecute` select between them on the
    // active `EmulatedRuntime` (threaded through the machine config).
    let private net10NativeHandlers : (NativeCallContext -> NativeHandlerResult option) list =
        [
            NativeGc.tryExecute
            NativeGcFrameRegistration.tryExecute
            NativeEnvironment.tryExecute
            NativeMonitor.tryExecute
            NativeMarshal.tryExecute
            NativeBuffer.tryExecute
            NativeQCall.tryExecute
            // QCall migration note: some name-based native handlers below still model
            // CoreCLR QCalls on newer runtimes. Move each to NativeQCall as its import
            // metadata is needed, then delete the corresponding name-based fallback.
            NativeMetadataImport.tryExecute
            NativeGcHandle.tryExecute
            NativeDependentHandle.tryExecute
            NativeRuntimeFieldHandle.tryExecute
            NativeRuntimeMethodHandle.tryExecute
            NativeRuntimeHelpers.tryExecute
            NativeSignature.tryExecute
            NativeRuntimeType.tryExecute
            NativeRuntimeAssembly.tryExecute
            NativeThreading.tryExecute
            NativeType.tryExecute
            NativeString.tryExecute
            NativeSystemNative.tryExecute
            NativeLowLevelMonitor.tryExecute
            NativeDebugger.tryExecute
            NativeException.tryExecute
            NativeDelegate.tryExecute
        ]

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        net10NativeHandlers |> List.tryPick (fun handler -> handler ctx)

    let failUnimplemented (ctx : NativeCallContext) : NativeHandlerResult = NativeCall.failUnimplemented ctx

    let private declaresSetLastError (import : NativeMethodImport option) : bool =
        match import with
        | None -> false
        | Some import -> import.Attributes.HasFlag MethodImportAttributes.SetLastError

    /// The first of the three steps CoreCLR's forward P/Invoke stub performs around a call whose
    /// import declares `SetLastError = true`: the calling thread's system error is zeroed, so that
    /// what the call leaves behind is the call's own and not a predecessor's. A no-op for every
    /// other import, and for an `InternalCall`, which has no import to declare anything.
    ///
    /// Must run before the handler, not after it: `SystemNative_SetErrNo` writes the very slot this
    /// clears, and running the two in the wrong order would erase what the guest asked for.
    let clearLastError
        (import : NativeMethodImport option)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        if declaresSetLastError import then
            state.MapKernel (EmulatedKernel.withLastSystemError thread 0)
        else
            state

    /// The last of those three steps: the calling thread's system error is copied into its
    /// last-P/Invoke-error slot, which is the one `Marshal.GetLastPInvokeError` and CoreLib's
    /// `Interop.Sys.GetLastErrorInfo` read. A no-op for every other import.
    ///
    /// Belongs only on the outcomes that return to managed code. A handler that raised instead of
    /// performing the call has no error to report, and a handler that is to be re-entered has not
    /// finished producing one.
    let captureLastError
        (import : NativeMethodImport option)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        if declaresSetLastError import then
            // Reads the slot rather than being handed a value, exactly as `StubHelpers::SetLastError`
            // (marshalnative.cpp:311-319) does: on Unix the PAL's last error *is* errno, so whatever
            // the handler wrote there is what the real stub would have copied — including the Win32
            // codes some handlers legitimately put in it.
            let systemError = EmulatedKernel.lastSystemErrorFor thread state.Kernel
            state.MapKernel (EmulatedKernel.withLastPInvokeError thread systemError)
        else
            state
