namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeEventPipe =
    /// Treat IntPtr.Zero as "no provider"; non-zero values must be EventPipe provider
    /// handles minted by `EventPipeInternal_CreateProvider`. Real EventPipe also rejects
    /// foreign IntPtr values; this preserves that property by failing loudly on tags from
    /// other registries (GCHandlePtr, TypeHandlePtr, ...).
    let private providerHandleOfArgument
        (operation : string)
        (argName : string)
        (arg : CliType)
        : EventPipeProviderHandle option
        =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.EventPipeProviderPtr handle)) -> Some handle
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | other -> failwith $"%s{operation}: expected %s{argName} to be EventPipe provider handle, got %O{other}"

    let private eventHandleOfArgument
        (operation : string)
        (argName : string)
        (arg : CliType)
        : EventPipeEventHandle option
        =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.EventPipeEventPtr handle)) -> Some handle
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | other -> failwith $"%s{operation}: expected %s{argName} to be EventPipe event handle, got %O{other}"

    let private uint32Argument (operation : string) (argName : string) (arg : CliType) : uint32 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) -> uint32 i
        | other -> failwith $"%s{operation}: expected %s{argName} to be UInt32 argument, got %O{other}"

    let private int64Argument (operation : string) (argName : string) (arg : CliType) : int64 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim i)) -> i
        | other -> failwith $"%s{operation}: expected %s{argName} to be Int64 argument, got %O{other}"

    let private pushIntPtrZero (thread : ThreadId) (state : IlMachineState) : ExecutionResult =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)) thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let private pushProviderHandle
        (handle : EventPipeProviderHandle)
        (thread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        state
        |> IlMachineState.pushToEvalStack'
            (EvalStackValue.NativeInt (NativeIntSource.EventPipeProviderPtr handle))
            thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let private pushEventHandle
        (handle : EventPipeEventHandle)
        (thread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.EventPipeEventPtr handle)) thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let private pushUInt64Zero (thread : ThreadId) (state : IlMachineState) : ExecutionResult =
        // The CLI evaluation stack stores UInt64 in the same Int64 cell, two's-complement.
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 (Int64Source.Verbatim 0L)) thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let private pushInt32 (value : int32) (thread : ThreadId) (state : IlMachineState) : ExecutionResult =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let private justStep (state : IlMachineState) : ExecutionResult =
        (state, WhatWeDid.Executed) |> ExecutionResult.Stepped

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "EventPipeInternal_Enable",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Diagnostics.Tracing",
                                              "EventPipeSerializationFormat",
                                              formatGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64) when
            formatGenerics.IsEmpty
            ->
            // No tracing session is ever opened in PawPrint, so we always report a zero
            // session ID. EventPipeEventDispatcher treats sessionID == 0 as "enable failed"
            // and stops trying to drive the dispatcher loop, which is exactly the behaviour
            // we want.
            state |> pushUInt64Zero ctx.Thread |> Some

        | "EventPipeInternal_Disable",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64 ],
          MethodReturnType.Void ->
            // No session was ever enabled; ignore the supplied sessionID.
            state |> justStep |> Some

        | "EventPipeInternal_CreateProvider",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePointer (ConcreteVoid state.ConcreteTypes) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "EventPipeInternal_CreateProvider"

            let providerNamePtr =
                NativeCall.managedPointerOfPointerArgument operation "providerName" instruction.Arguments.[0]

            // We intentionally drop the callback function pointer and the callbackContext.
            // PawPrint never invokes the callback, and storing them would silently swallow
            // any future regression that tried to call into them — by discarding here, an
            // attempted invocation becomes a compile error against `EventPipeProviderInfo`.

            let providerName =
                NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state providerNamePtr

            let handle, registry =
                state.EventPipeProviders
                |> EventPipeProviderRegistry.allocateProvider providerName

            let state =
                { state with
                    EventPipeProviders = registry
                }

            state |> pushProviderHandle handle ctx.Thread |> Some

        | "EventPipeInternal_DefineEvent",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcretePointer (ConcreteVoid state.ConcreteTypes)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "EventPipeInternal_DefineEvent"

            let provider =
                providerHandleOfArgument operation "provHandle" instruction.Arguments.[0]
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: provHandle was IntPtr.Zero, but DefineEvent requires a registered provider"
                )

            let eventID = uint32Argument operation "eventID" instruction.Arguments.[1]
            let keywords = int64Argument operation "keywords" instruction.Arguments.[2]
            let eventVersion = uint32Argument operation "eventVersion" instruction.Arguments.[3]
            let level = uint32Argument operation "level" instruction.Arguments.[4]

            // metadata pointer and metadataLength are accepted but not retained: the metadata
            // blob is the schema EventPipe would use to format payloads, and PawPrint does not
            // emit payloads anywhere.

            // Validate that the provider is actually registered. This catches callers that hand
            // us a forged IntPtr (e.g. from a different registry tagged as 0L by accident).
            match state.EventPipeProviders |> EventPipeProviderRegistry.lookupProvider provider with
            | None -> failwith $"%s{operation}: provHandle %O{provider} is not a registered EventPipe provider"
            | Some _ -> ()

            let info : EventPipeEventInfo =
                {
                    Provider = provider
                    EventID = eventID
                    Keywords = keywords
                    EventVersion = eventVersion
                    Level = level
                }

            let handle, registry =
                state.EventPipeProviders |> EventPipeProviderRegistry.allocateEvent info

            let state =
                { state with
                    EventPipeProviders = registry
                }

            state |> pushEventHandle handle ctx.Thread |> Some

        | "EventPipeInternal_GetProvider",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "EventPipeInternal_GetProvider"

            let providerNamePtr =
                NativeCall.managedPointerOfPointerArgument operation "providerName" instruction.Arguments.[0]

            let providerName =
                NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state providerNamePtr

            match
                state.EventPipeProviders
                |> EventPipeProviderRegistry.findFirstByName providerName
            with
            | Some handle -> state |> pushProviderHandle handle ctx.Thread |> Some
            | None -> state |> pushIntPtrZero ctx.Thread |> Some

        | "EventPipeInternal_DeleteProvider",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let operation = "EventPipeInternal_DeleteProvider"

            // EventPipeEventProvider.Dispose calls DeleteProvider unconditionally, including
            // when registration failed and `_provHandle` is still zero. Match the native
            // contract by treating IntPtr.Zero as a no-op rather than a failure.
            match providerHandleOfArgument operation "provHandle" instruction.Arguments.[0] with
            | None -> state |> justStep |> Some
            | Some handle ->
                let registry =
                    state.EventPipeProviders |> EventPipeProviderRegistry.freeProvider handle

                let state =
                    { state with
                        EventPipeProviders = registry
                    }

                state |> justStep |> Some

        | "EventPipeInternal_EventActivityIdControl",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ; ConcretePointer guidPtrHandle ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            (match guidPtrHandle with
             | ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "Guid", guidGenerics) ->
                 guidGenerics.IsEmpty
             | _ -> false)
            ->
            // The C# wrapper takes `ref Guid`, but the LibraryImport source generator marshals
            // ref-to-value-type to a raw `Guid*` for the QCall, and `[return: MarshalAs(...)]`
            // is absent so the raw int32 BOOL flows through unchanged: nonzero is success.
            //
            // CoreCLR keeps a per-thread activity ID independently of any tracing session, so
            // EventSource calls this entry point during normal `WriteEvent` flow even when no
            // EventPipe session is enabled. We honour the read-only GET_ID code by writing
            // Guid.Empty into *activityId — the value real CoreCLR would also produce when no
            // SetCurrentThreadActivityId has run on this thread — and reject mutating control
            // codes loudly until per-thread activity ID storage is added.
            let operation = "EventPipeInternal_EventActivityIdControl"
            let controlCode = uint32Argument operation "controlCode" instruction.Arguments.[0]

            let activityIdPtr =
                NativeCall.managedPointerOfPointerArgument operation "activityId" instruction.Arguments.[1]

            match controlCode with
            | 1u ->
                // EP_ACTIVITY_CONTROL_GET_ID
                let zeroGuid, state =
                    IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes guidPtrHandle

                let state = IlMachineState.writeManagedByref state activityIdPtr zeroGuid
                state |> pushInt32 1 ctx.Thread |> Some
            | 2u ->
                failwith
                    $"%s{operation}: TODO: EP_ACTIVITY_CONTROL_SET_ID requires per-thread activity ID tracking, which PawPrint does not yet implement"
            | 3u ->
                failwith
                    $"%s{operation}: TODO: EP_ACTIVITY_CONTROL_CREATE_ID requires deterministic GUID generation, which PawPrint does not yet implement"
            | 4u ->
                failwith
                    $"%s{operation}: TODO: EP_ACTIVITY_CONTROL_GET_SET_ID requires per-thread activity ID tracking, which PawPrint does not yet implement"
            | 5u ->
                failwith
                    $"%s{operation}: TODO: EP_ACTIVITY_CONTROL_CREATE_SET_ID requires deterministic GUID generation and per-thread tracking, which PawPrint does not yet implement"
            | unknown -> failwith $"%s{operation}: unknown EP_ACTIVITY_CONTROL_* code %u{unknown}"

        | "EventPipeInternal_WriteEventData",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System",
                                                               "Guid",
                                                               activityGuidGenerics))
            ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System",
                                                               "Guid",
                                                               relatedGuidGenerics)) ],
          MethodReturnType.Void when activityGuidGenerics.IsEmpty && relatedGuidGenerics.IsEmpty ->
            let operation = "EventPipeInternal_WriteEventData"

            // Validate that the event handle is one we issued. EventSource itself only ever
            // calls this with a handle DefineEvent returned, so a foreign IntPtr here would
            // indicate a bug worth catching loudly.
            match eventHandleOfArgument operation "eventHandle" instruction.Arguments.[0] with
            | None ->
                // EventSource.WriteEventCore can call WriteEventData with eventHandle == 0
                // when an event is disabled before the call; treat that as a no-op.
                ()
            | Some handle ->
                match state.EventPipeProviders |> EventPipeProviderRegistry.lookupEvent handle with
                | Some _ -> ()
                | None -> failwith $"%s{operation}: eventHandle %O{handle} is not a registered EventPipe event"

            // No event delivery; data and activity IDs are intentionally ignored.
            state |> justStep |> Some

        // The four entry points below all carry `[return: MarshalAs(UnmanagedType.Bool)]` on
        // their managed wrappers. The LibraryImport source generator emits the underlying
        // QCall as returning int32 and the wrapper applies `cgt.un` to convert; matching on
        // PrimitiveType.Boolean here would never fire. Push 0 (FALSE) for "no session" — that
        // is the truthful answer because PawPrint never opens a tracing session.
        | "EventPipeInternal_GetSessionInfo",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64 ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // No session was ever opened, so reporting "no session info available" is correct.
            // The pSessionInfo out-pointer is left untouched.
            state |> pushInt32 0 ctx.Thread |> Some

        | "EventPipeInternal_GetNextEvent",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64 ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // No session is enabled, so there is never a next event to drain.
            state |> pushInt32 0 ctx.Thread |> Some

        | "EventPipeInternal_SignalSession",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // No session to signal; report failure.
            state |> pushInt32 0 ctx.Thread |> Some

        | "EventPipeInternal_WaitForSessionSignal",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // No session to wait on; report failure immediately rather than blocking.
            state |> pushInt32 0 ctx.Thread |> Some

        | _ -> None
