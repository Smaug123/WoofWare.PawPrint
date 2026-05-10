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
            "ExceptionNative_GetMessageFromNativeResources",
            NativeException.tryExecuteQCall "ExceptionNative_GetMessageFromNativeResources"
            "RuntimeTypeHandle_ConstructName", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_ConstructName"
            "RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter"
            "RuntimeTypeHandle_GetConstraints", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetConstraints"
            "RuntimeTypeHandle_GetDeclaringTypeHandle",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetDeclaringTypeHandle"
            "RuntimeTypeHandle_GetFields", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetFields"
            "RuntimeTypeHandle_GetInstantiation", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetInstantiation"
            "RuntimeTypeHandle_Instantiate", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_Instantiate"
            "ModuleHandle_ResolveType", NativeRuntimeType.tryExecuteQCall "ModuleHandle_ResolveType"
            "MethodTable_CanCompareBitsOrUseFastGetHashCode",
            NativeRuntimeType.tryExecuteQCall "MethodTable_CanCompareBitsOrUseFastGetHashCode"
            "TypeHandle_GetCorElementType", NativeRuntimeType.tryExecuteQCall "TypeHandle_GetCorElementType"
            "ThreadNative_GetCurrentThread", NativeThreading.tryExecuteQCall "ThreadNative_GetCurrentThread"
            "ThreadNative_Initialize", NativeThreading.tryExecuteQCall "ThreadNative_Initialize"
            "ThreadNative_Join", NativeThreading.tryExecuteQCall "ThreadNative_Join"
            "DebugDebugger_IsManagedDebuggerAttached",
            NativeDebugger.tryExecuteQCall "DebugDebugger_IsManagedDebuggerAttached"
            "Signature_Init", NativeSignature.tryExecuteQCall "Signature_Init"
            "Array_CreateInstance", NativeArray.tryExecuteQCall "Array_CreateInstance"
            "Enum_GetValuesAndNames", NativeEnum.tryExecuteQCall "Enum_GetValuesAndNames"
            "AssemblyNative_GetResource", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetResource"
            "AssemblyNative_GetTypeCore", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetTypeCore"
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
