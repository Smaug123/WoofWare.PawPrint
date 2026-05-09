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

    let private pushBool (value : bool) (thread : ThreadId) (state : IlMachineState) : ExecutionResult =
        state
        |> IlMachineState.pushToEvalStack (CliType.ofBool value) thread
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
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "Guid", guidGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when guidGenerics.IsEmpty ->
            // We never produce activity IDs because no session is ever enabled. The activityId
            // byref is left untouched: callers that observe it will see whatever they passed
            // in, which matches the native semantics for "no current activity".
            state |> pushInt32 0 ctx.Thread |> Some

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

        | "EventPipeInternal_GetSessionInfo",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64 ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            // No session was ever opened, so reporting "no session info available" is correct.
            // The pSessionInfo out-pointer is left untouched.
            state |> pushBool false ctx.Thread |> Some

        | "EventPipeInternal_GetNextEvent",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64 ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            // No session is enabled, so there is never a next event to drain.
            state |> pushBool false ctx.Thread |> Some

        | "EventPipeInternal_SignalSession",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            // No session to signal; report failure.
            state |> pushBool false ctx.Thread |> Some

        | "EventPipeInternal_WaitForSessionSignal",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            // No session to wait on; report failure immediately rather than blocking.
            state |> pushBool false ctx.Thread |> Some

        | _ -> None
