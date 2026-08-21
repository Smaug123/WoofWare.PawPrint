namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeQCall =
    let private handlers : Map<string, NativeCallContext -> NativeHandlerResult option> =
        [
            "ReflectionInvocation_RunClassConstructor",
            NativeRuntimeHelpers.tryExecuteQCall "ReflectionInvocation_RunClassConstructor"
            "RuntimeFieldHandle_GetRVAFieldInfo",
            NativeRuntimeFieldHandle.tryExecuteQCall "RuntimeFieldHandle_GetRVAFieldInfo"
            "RuntimeFieldHandle_SetValue", NativeRuntimeFieldHandle.tryExecuteQCall "RuntimeFieldHandle_SetValue"
            "RuntimeMethodHandle_IsCAVisibleFromDecoratedType",
            NativeRuntimeMethodHandle.tryExecuteQCall "RuntimeMethodHandle_IsCAVisibleFromDecoratedType"
            "RuntimeMethodHandle_GetMethodInstantiation",
            NativeRuntimeMethodHandle.tryExecuteQCall "RuntimeMethodHandle_GetMethodInstantiation"
            "RuntimeMethodHandle_GetStubIfNeededSlow",
            NativeRuntimeMethodHandle.tryExecuteQCall "RuntimeMethodHandle_GetStubIfNeededSlow"
            "RuntimeMethodHandle_InvokeMethod",
            NativeReflectionInvocation.tryExecuteQCall "RuntimeMethodHandle_InvokeMethod"
            "ReflectionInvocation_GetBoxInfo",
            NativeReflectionInvocation.tryExecuteQCall "ReflectionInvocation_GetBoxInfo"
            "QCall_GetGCHandleForTypeHandle", NativeGcHandle.tryExecuteQCall "QCall_GetGCHandleForTypeHandle"
            "QCall_FreeGCHandleForTypeHandle", NativeGcHandle.tryExecuteQCall "QCall_FreeGCHandleForTypeHandle"
            "MarshalNative_SizeOfHelper", NativeMarshal.tryExecuteQCall "MarshalNative_SizeOfHelper"
            "MarshalNative_TryGetStructMarshalStub",
            NativeMarshal.tryExecuteQCall "MarshalNative_TryGetStructMarshalStub"
            "Buffer_MemMove", NativeBuffer.tryExecuteQCall "Buffer_MemMove"
            "ExceptionNative_GetMessageFromNativeResources",
            NativeException.tryExecuteQCall "ExceptionNative_GetMessageFromNativeResources"
            "ExceptionNative_GetFrozenStackTrace", NativeException.tryExecuteQCall "ExceptionNative_GetFrozenStackTrace"
            "RuntimeTypeHandle_ConstructName", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_ConstructName"
            "RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter"
            "RuntimeTypeHandle_GetActivationInfo",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetActivationInfo"
            "RuntimeTypeHandle_InternalAlloc", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_InternalAlloc"
            "RuntimeTypeHandle_InternalAllocNoChecks",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_InternalAllocNoChecks"
            "RuntimeTypeHandle_GetConstraints", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetConstraints"
            "RuntimeTypeHandle_GetDeclaringMethodForGenericParameter",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetDeclaringMethodForGenericParameter"
            "RuntimeTypeHandle_GetDeclaringTypeHandle",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetDeclaringTypeHandle"
            "RuntimeTypeHandle_GetDeclaringTypeHandleForGenericVariable",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetDeclaringTypeHandleForGenericVariable"
            "RuntimeTypeHandle_GetFields", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetFields"
            "RuntimeTypeHandle_GetGenericTypeDefinition",
            NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetGenericTypeDefinition"
            "RuntimeTypeHandle_GetInstantiation", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetInstantiation"
            "RuntimeTypeHandle_GetInterfaces", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetInterfaces"
            "RuntimeTypeHandle_Instantiate", NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_Instantiate"
            "ModuleHandle_GetMDStreamVersion", NativeModuleHandle.tryExecuteQCall "ModuleHandle_GetMDStreamVersion"
            "ModuleHandle_GetPEKind", NativeModuleHandle.tryExecuteQCall "ModuleHandle_GetPEKind"
            "ModuleHandle_GetDynamicMethod", NativeModuleHandle.tryExecuteQCall "ModuleHandle_GetDynamicMethod"
            "RuntimeModule_GetTypes", NativeRuntimeModule.tryExecuteQCall "RuntimeModule_GetTypes"
            "Delegate_BindToMethodInfo", NativeDelegate.tryExecuteQCall "Delegate_BindToMethodInfo"
            "Delegate_FindMethodHandle", NativeDelegate.tryExecuteQCall "Delegate_FindMethodHandle"
            "ModuleHandle_ResolveType", NativeRuntimeType.tryExecuteQCall "ModuleHandle_ResolveType"
            "ModuleHandle_ResolveMethod", NativeRuntimeType.tryExecuteQCall "ModuleHandle_ResolveMethod"
            "MethodTable_CanCompareBitsOrUseFastGetHashCode",
            NativeRuntimeType.tryExecuteQCall "MethodTable_CanCompareBitsOrUseFastGetHashCode"
            "TypeHandle_GetCorElementType", NativeRuntimeType.tryExecuteQCall "TypeHandle_GetCorElementType"
            "TypeHandle_CanCastTo_NoCacheLookup", NativeRuntimeType.tryExecuteQCall "TypeHandle_CanCastTo_NoCacheLookup"
            // The *object*-castability sibling of the above. Reachable from ordinary guest code
            // via `Type.IsInstanceOfType`, `Array.SetValue` and the casting arm of `Array.Copy`,
            // all of which call `CastHelpers` from managed BCL source; PawPrint's cast cache is
            // a permanently-empty sentinel, so every such call misses and lands here.
            "IsInstanceOf_NoCacheLookup", NativeCastHelpers.tryExecuteQCall "IsInstanceOf_NoCacheLookup"
            "ThreadNative_GetCurrentThread", NativeThreading.tryExecuteQCall "ThreadNative_GetCurrentThread"
            "ThreadNative_Initialize", NativeThreading.tryExecuteQCall "ThreadNative_Initialize"
            "ThreadNative_Join", NativeThreading.tryExecuteQCall "ThreadNative_Join"
            "ThreadNative_SetIsBackground", NativeThreading.tryExecuteQCall "ThreadNative_SetIsBackground"
            "ThreadNative_GetIsBackground", NativeThreading.tryExecuteQCall "ThreadNative_GetIsBackground"
            "ThreadNative_InformThreadNameChange", NativeThreading.tryExecuteQCall "ThreadNative_InformThreadNameChange"
            "ThreadNative_YieldThread", NativeThreading.tryExecuteQCall "ThreadNative_YieldThread"
            "ThreadNative_Sleep", NativeThreading.tryExecuteQCall "ThreadNative_Sleep"
            "ThreadNative_SpinWait", NativeThreading.tryExecuteQCall "ThreadNative_SpinWait"
            "Monitor_Wait", NativeMonitor.tryExecuteQCall "Monitor_Wait"
            "Monitor_Pulse", NativeMonitor.tryExecuteQCall "Monitor_Pulse"
            "Monitor_PulseAll", NativeMonitor.tryExecuteQCall "Monitor_PulseAll"
            "Monitor_TryEnter_Slowpath", NativeMonitor.tryExecuteQCall "Monitor_TryEnter_Slowpath"
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
            // `XplatEventLogger` QCalls, present only in Linux-built CoreLibs
            // (FEATURE_EVENTSOURCE_XPLAT). PawPrint never connects to an
            // external tracing consumer, so the deterministic answers are
            // "no knob set" / "logging disabled" / "no-op".
            "EventSource_GetClrConfig", NativeEventSource.tryExecuteQCall "EventSource_GetClrConfig"
            "IsEventSourceLoggingEnabled", NativeEventSource.tryExecuteQCall "IsEventSourceLoggingEnabled"
            "LogEventSource", NativeEventSource.tryExecuteQCall "LogEventSource"
            "Signature_Init", NativeSignature.tryExecuteQCall "Signature_Init"
            "Signature_GetCustomModifiersAtOffset",
            NativeSignature.tryExecuteQCall "Signature_GetCustomModifiersAtOffset"
            "Array_CreateInstance", NativeArray.tryExecuteQCall "Array_CreateInstance"
            "GCInterface_AllocateNewArray", NativeGc.tryExecuteQCall "GCInterface_AllocateNewArray"
            "Enum_GetValuesAndNames", NativeEnum.tryExecuteQCall "Enum_GetValuesAndNames"
            "MetadataImport_Enum", NativeMetadataImport.tryExecuteQCall "MetadataImport_Enum"
            "AssemblyNative_GetCodeBase", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetCodeBase"
            "AssemblyNative_GetEntryAssembly", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetEntryAssembly"
            "AssemblyNative_GetFlags", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetFlags"
            "AssemblyNative_GetHashAlgorithm", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetHashAlgorithm"
            "AssemblyNative_GetLocale", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetLocale"
            "AssemblyNative_GetLocation", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetLocation"
            "AssemblyNative_GetPublicKey", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetPublicKey"
            "AssemblyNative_GetResource", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetResource"
            "AssemblyNative_GetSimpleName", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetSimpleName"
            "AssemblyNative_GetFullName", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetFullName"
            "AssemblyNative_GetTypeCore", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetTypeCore"
            "AssemblyNative_GetVersion", NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetVersion"
            "AssemblyNative_IsApplyUpdateSupported",
            NativeMetadataUpdater.tryExecuteQCall "AssemblyNative_IsApplyUpdateSupported"
            "CustomAttribute_CreateCustomAttributeInstance",
            NativeCustomAttribute.tryExecuteQCall "CustomAttribute_CreateCustomAttributeInstance"
            "CustomAttribute_CreatePropertyOrFieldData",
            NativeCustomAttribute.tryExecuteQCall "CustomAttribute_CreatePropertyOrFieldData"
            // The primitive underneath every `BindingFlags.IgnoreCase` reflection
            // member lookup: `RuntimeType.RuntimeTypeCache.Filter.Match` routes a
            // case-insensitive list type through `MdUtf8String.EqualsCaseInsensitive`.
            "MdUtf8String_EqualsCaseInsensitive",
            NativeMdUtf8String.tryExecuteQCall "MdUtf8String_EqualsCaseInsensitive"
            // The CoreLib source is a Kernel32 LibraryImport, but the runtime
            // assembly we execute presents this PAL entry point through QCall
            // import metadata.
            "GetEnvironmentVariableW", NativeKernel32.tryExecuteQCall "GetEnvironmentVariableW"
            // `Environment.GetEnvironmentVariables` is the only caller, and it
            // takes the block and releases it in a `finally`, so the pair is
            // always reached together.
            "GetEnvironmentStringsW", NativeKernel32.tryExecuteQCall "GetEnvironmentStringsW"
            "FreeEnvironmentStringsW", NativeKernel32.tryExecuteQCall "FreeEnvironmentStringsW"
            // `CLRConfig.GetConfigBoolValue` is a QCall on CoreCLR for internal
            // knob lookups (e.g. `AutoreleasePool.EnableAutoreleasePool`). PawPrint
            // answers "knob not set" deterministically; see #609 for deferred work.
            "ClrConfig_GetConfigBoolValue", NativeClrConfig.tryExecuteQCall "ClrConfig_GetConfigBoolValue"
            // CoreCLR-on-Unix rebinds `Libraries.Kernel32` to `RuntimeHelpers.QCall`,
            // so the Win32-shaped Semaphore P/Invokes plus `CloseHandle` reach
            // the runtime as QCalls under their Win32 wide-string names.
            // `WaitHandle_WaitOneCore` is a separate QCall declared on
            // `WaitHandle` itself. All four are dispatched into the
            // deterministic state machine in `WaitHandle.fs`.
            "CreateSemaphoreExW", NativeWaitHandle.tryExecuteQCall "CreateSemaphoreExW"
            "ReleaseSemaphore", NativeWaitHandle.tryExecuteQCall "ReleaseSemaphore"
            "CloseHandle", NativeWaitHandle.tryExecuteQCall "CloseHandle"
            "WaitHandle_WaitOneCore", NativeWaitHandle.tryExecuteQCall "WaitHandle_WaitOneCore"
            "WaitHandle_WaitMultipleIgnoringSyncContext",
            NativeWaitHandle.tryExecuteQCall "WaitHandle_WaitMultipleIgnoringSyncContext"
            // `LowLevelLifoSemaphore.Unix.cs` independently imports the
            // prioritized 2-arg waiter; routes to the same deterministic
            // state machine as `WaitOneCore`.
            "WaitHandle_WaitOnePrioritized", NativeWaitHandle.tryExecuteQCall "WaitHandle_WaitOnePrioritized"
            // `Mutex.CoreCLR.Unix.cs` imports `PAL_CreateMutexW` directly
            // (separate from the semaphore's `CreateSemaphoreExW`);
            // `ReleaseMutex` is the Win32 wide-string name that
            // `Interop.Mutex.cs` uses, routed to QCall on Unix via the
            // `Libraries.Kernel32 = RuntimeHelpers.QCall` rebinding.
            // Both dispatch into the deterministic mutex state machine
            // in `WaitHandle.fs`.
            "PAL_CreateMutexW", NativeWaitHandle.tryExecuteQCall "PAL_CreateMutexW"
            "ReleaseMutex", NativeWaitHandle.tryExecuteQCall "ReleaseMutex"
            // `EventWaitHandle.Windows.cs` runs on .NET 10 CoreCLR
            // regardless of host; `Libraries.Kernel32 = RuntimeHelpers
            // .QCall` routes the three Kernel32 LibraryImports
            // (`CreateEventEx` / `SetEvent` / `ResetEvent`) to the
            // runtime as QCalls under their Win32 wide-string entry
            // points. Both `Manual` and `Auto` reset modes dispatch into
            // the deterministic state machine in `WaitHandle.fs`.
            "CreateEventExW", NativeWaitHandle.tryExecuteQCall "CreateEventExW"
            "SetEvent", NativeWaitHandle.tryExecuteQCall "SetEvent"
            "ResetEvent", NativeWaitHandle.tryExecuteQCall "ResetEvent"
        ]
        |> Map.ofList

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        match NativeCall.tryQCallEntryPoint ctx with
        | None -> None
        | Some entryPoint -> handlers |> Map.tryFind entryPoint |> Option.bind (fun handler -> handler ctx)
