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
            "RuntimeTypeHandle_GetDeclaringTypeHandleForGenericVariable",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetDeclaringTypeHandleForGenericVariable"
            "RuntimeTypeHandle_GetFields", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetFields"
            "RuntimeTypeHandle_GetInstantiation", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetInstantiation"
            "RuntimeTypeHandle_GetInterfaces", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetInterfaces"
            "RuntimeTypeHandle_Instantiate", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_Instantiate"
            "ModuleHandle_ResolveType", NativeRuntimeType.tryExecuteQCall "ModuleHandle_ResolveType"
            "MethodTable_CanCompareBitsOrUseFastGetHashCode",
            NativeRuntimeType.tryExecuteQCall "MethodTable_CanCompareBitsOrUseFastGetHashCode"
            "TypeHandle_GetCorElementType", NativeRuntimeType.tryExecuteQCall "TypeHandle_GetCorElementType"
            "TypeHandle_CanCastTo_NoCacheLookup", NativeRuntimeType.tryExecuteQCall "TypeHandle_CanCastTo_NoCacheLookup"
            "ThreadNative_GetCurrentThread", NativeThreading.tryExecuteQCall "ThreadNative_GetCurrentThread"
            "ThreadNative_Initialize", NativeThreading.tryExecuteQCall "ThreadNative_Initialize"
            "ThreadNative_Join", NativeThreading.tryExecuteQCall "ThreadNative_Join"
            "DebugDebugger_IsManagedDebuggerAttached",
            NativeDebugger.tryExecuteQCall "DebugDebugger_IsManagedDebuggerAttached"
            "EventPipeInternal_Enable", NativeEventPipe.tryExecuteQCall "EventPipeInternal_Enable"
            "EventPipeInternal_Disable", NativeEventPipe.tryExecuteQCall "EventPipeInternal_Disable"
            "EventPipeInternal_CreateProvider", NativeEventPipe.tryExecuteQCall "EventPipeInternal_CreateProvider"
            "EventPipeInternal_DefineEvent", NativeEventPipe.tryExecuteQCall "EventPipeInternal_DefineEvent"
            "EventPipeInternal_GetProvider", NativeEventPipe.tryExecuteQCall "EventPipeInternal_GetProvider"
            "EventPipeInternal_DeleteProvider", NativeEventPipe.tryExecuteQCall "EventPipeInternal_DeleteProvider"
            "EventPipeInternal_EventActivityIdControl",
            NativeEventPipe.tryExecuteQCall "EventPipeInternal_EventActivityIdControl"
            "EventPipeInternal_WriteEventData", NativeEventPipe.tryExecuteQCall "EventPipeInternal_WriteEventData"
            "EventPipeInternal_GetSessionInfo", NativeEventPipe.tryExecuteQCall "EventPipeInternal_GetSessionInfo"
            "EventPipeInternal_GetNextEvent", NativeEventPipe.tryExecuteQCall "EventPipeInternal_GetNextEvent"
            "EventPipeInternal_SignalSession", NativeEventPipe.tryExecuteQCall "EventPipeInternal_SignalSession"
            "EventPipeInternal_WaitForSessionSignal",
            NativeEventPipe.tryExecuteQCall "EventPipeInternal_WaitForSessionSignal"
            "Signature_Init", NativeSignature.tryExecuteQCall "Signature_Init"
            "Array_CreateInstance", NativeArray.tryExecuteQCall "Array_CreateInstance"
            "Enum_GetValuesAndNames", NativeEnum.tryExecuteQCall "Enum_GetValuesAndNames"
            "AssemblyNative_GetResource", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetResource"
            "AssemblyNative_GetTypeCore", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetTypeCore"
            "AssemblyNative_IsApplyUpdateSupported",
            NativeMetadataUpdater.tryExecuteQCall "AssemblyNative_IsApplyUpdateSupported"
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
