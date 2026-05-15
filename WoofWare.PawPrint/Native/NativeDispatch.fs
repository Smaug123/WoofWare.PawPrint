namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeDispatch =
    let private nativeHandlers : (NativeCallContext -> ExecutionResult option) list =
        [
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
        ]

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        nativeHandlers |> List.tryPick (fun handler -> handler ctx)

    let failUnimplemented (ctx : NativeCallContext) : ExecutionResult = NativeCall.failUnimplemented ctx
