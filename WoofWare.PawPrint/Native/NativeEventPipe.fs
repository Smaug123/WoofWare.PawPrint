namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeEventPipe =
    /// Mint the next opaque EventPipe handle ID. PawPrint never opens a tracing session, so
    /// these are not stored anywhere — they only need to be unique and non-zero (the BCL treats
    /// IntPtr.Zero from `CreateProvider`/`DefineEvent` as "create failed" and throws OOM).
    let private mintEventPipeId (state : IlMachineState) : int64 * IlMachineState =
        let id = state.Kernel.NextEventPipeId

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    NextEventPipeId = kernel.NextEventPipeId + 1L
                }
            )

        id, state

    /// Validate that an IntPtr argument is either a PawPrint-minted EventPipe provider handle
    /// or null. Foreign IntPtr values from other registries (GCHandlePtr, TypeHandlePtr, ...)
    /// fail loudly: that mirrors the real EventPipe contract that only handles minted by
    /// `EventPipeInternal_CreateProvider` are valid here.
    let private providerHandleOfArgument (operation : string) (argName : string) (arg : CliType) : int64 option =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.EventPipeProviderPtr id)) -> Some id
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | other -> failwith $"%s{operation}: expected %s{argName} to be EventPipe provider handle, got %O{other}"

    let private eventHandleOfArgument (operation : string) (argName : string) (arg : CliType) : int64 option =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.EventPipeEventPtr id)) -> Some id
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | other -> failwith $"%s{operation}: expected %s{argName} to be EventPipe event handle, got %O{other}"

    let private uint32Argument (operation : string) (argName : string) (arg : CliType) : uint32 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) -> uint32 i
        | other -> failwith $"%s{operation}: expected %s{argName} to be UInt32 argument, got %O{other}"

    let private pushIntPtrZero (thread : ThreadId) (state : IlMachineState) : ExecutionResult =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)) thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let private pushProviderHandle (id : int64) (thread : ThreadId) (state : IlMachineState) : ExecutionResult =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.EventPipeProviderPtr id)) thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let private pushEventHandle (id : int64) (thread : ThreadId) (state : IlMachineState) : ExecutionResult =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.EventPipeEventPtr id)) thread
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
            ConcreteFunctionPointer _
            ConcretePointer (ConcreteVoid state.ConcreteTypes) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            // The provider name (UTF-16, char* in CoreCLR but System.Char in metadata is U2),
            // the unmanaged callback function pointer, and the callback context are all
            // accepted but discarded: PawPrint never invokes the callback or filters by
            // provider name. We just mint a fresh tagged handle so the BCL sees a non-zero
            // IntPtr and proceeds.
            let id, state = mintEventPipeId state
            state |> pushProviderHandle id ctx.Thread |> Some

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

            // Validate that the supplied provider handle is one we minted. EventSource only
            // calls DefineEvent with a handle returned from CreateProvider, so a foreign or
            // null IntPtr here would indicate a real bug worth catching loudly.
            match providerHandleOfArgument operation "provHandle" instruction.Arguments.[0] with
            | None ->
                failwith $"%s{operation}: provHandle was IntPtr.Zero, but DefineEvent requires a registered provider"
            | Some _ -> ()

            // eventID, keywords, eventVersion, level, metadata pointer and metadataLength are
            // accepted but not retained: the metadata blob is the schema EventPipe would use
            // to format payloads, and PawPrint does not emit payloads anywhere.
            let id, state = mintEventPipeId state
            state |> pushEventHandle id ctx.Thread |> Some

        | "EventPipeInternal_GetProvider",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            // Real EventPipe returns the handle of an already-registered provider with this
            // name, or IntPtr.Zero if none. PawPrint does not retain provider registrations,
            // so the truthful answer is "no provider matches" — return zero. EventSource
            // treats this as "not yet registered" and falls through to CreateProvider.
            state |> pushIntPtrZero ctx.Thread |> Some

        | "EventPipeInternal_DeleteProvider",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "EventPipeInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let operation = "EventPipeInternal_DeleteProvider"

            // EventPipeEventProvider.Dispose calls DeleteProvider unconditionally, including
            // when registration failed and `_provHandle` is still zero. Match the native
            // contract by treating IntPtr.Zero as a no-op rather than a failure, but reject
            // foreign tagged IntPtr values via providerHandleOfArgument's exhaustive check.
            providerHandleOfArgument operation "provHandle" instruction.Arguments.[0]
            |> ignore

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
            // ref-to-value-type to a raw `Guid*` for the QCall. The QCall returns int32: 0 on
            // success, 1 on failure (null thread/pointer or unrecognised control code) — see
            // coreclr/vm/eventpipeinternal.cpp.
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

                let state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state activityIdPtr zeroGuid

                state |> pushInt32 0 ctx.Thread |> Some
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
            // calls this with a handle DefineEvent returned (or zero, if the event was
            // disabled before the call), so a foreign IntPtr here would indicate a real bug
            // worth catching loudly.
            eventHandleOfArgument operation "eventHandle" instruction.Arguments.[0]
            |> ignore

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
