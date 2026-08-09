namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeDispatch =
    // The native/extern boundary is the only runtime-specific surface PawPrint supplies (the managed
    // BCL is loaded from the guest's own assemblies), so this handler list IS "the native code for a
    // runtime". It is the net10 set: PawPrint currently emulates only `EmulatedRuntime.net10`. When a
    // second runtime is added, give it its own list and have `tryExecute` select between them on the
    // active `EmulatedRuntime` (threaded through the machine config); nothing here forecloses that.
    let private net10NativeHandlers : (NativeCallContext -> NativeHandlerResult option) list =
        [
            NativeGc.tryExecute
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
        ]

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        net10NativeHandlers |> List.tryPick (fun handler -> handler ctx)

    let failUnimplemented (ctx : NativeCallContext) : NativeHandlerResult = NativeCall.failUnimplemented ctx
