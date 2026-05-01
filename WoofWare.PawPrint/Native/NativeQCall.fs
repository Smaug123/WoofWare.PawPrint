namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeQCall =
    let private handlers : Map<string, NativeCallContext -> ExecutionResult option> =
        [
            "ReflectionInvocation_RunClassConstructor",
            NativeRuntimeHelpers.tryExecuteQCall "ReflectionInvocation_RunClassConstructor"
            "RuntimeFieldHandle_GetRVAFieldInfo",
            NativeRuntimeFieldHandle.tryExecuteQCall "RuntimeFieldHandle_GetRVAFieldInfo"
            "QCall_GetGCHandleForTypeHandle", NativeGcHandle.tryExecuteQCall "QCall_GetGCHandleForTypeHandle"
            "QCall_FreeGCHandleForTypeHandle", NativeGcHandle.tryExecuteQCall "QCall_FreeGCHandleForTypeHandle"
            "MarshalNative_SizeOfHelper", NativeMarshal.tryExecuteQCall "MarshalNative_SizeOfHelper"
            "Buffer_MemMove", NativeBuffer.tryExecuteQCall "Buffer_MemMove"
            "RuntimeTypeHandle_ConstructName", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_ConstructName"
            "MethodTable_CanCompareBitsOrUseFastGetHashCode",
            NativeRuntimeType.tryExecuteQCall "MethodTable_CanCompareBitsOrUseFastGetHashCode"
            "Array_CreateInstance", NativeArray.tryExecuteQCall "Array_CreateInstance"
            // The CoreLib source is a Kernel32 LibraryImport, but the runtime
            // assembly we execute presents this PAL entry point through QCall
            // import metadata.
            "GetEnvironmentVariableW", NativeKernel32.tryExecuteQCall "GetEnvironmentVariableW"
        ]
        |> Map.ofList

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        match NativeCall.tryQCallEntryPoint ctx with
        | None -> None
        | Some entryPoint -> handlers |> Map.tryFind entryPoint |> Option.bind (fun handler -> handler ctx)
