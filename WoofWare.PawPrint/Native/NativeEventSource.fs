namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeEventSource =
    /// QCalls declared on `System.Diagnostics.Tracing.XplatEventLogger` and reached
    /// from CoreLib only when `FEATURE_EVENTSOURCE_XPLAT` was defined at CoreLib
    /// build time (i.e. the Linux-built `System.Private.CoreLib`). They are still
    /// registered unconditionally because PawPrint always dispatches against the
    /// host runtime's CoreLib, which can be the Linux one on a Linux dev box or
    /// CI runner.
    ///
    /// PawPrint does not connect to any external tracing consumer (LTTng,
    /// EventPipe, etc.), so the deterministic answers are:
    ///   * `EventSource_GetClrConfig`: report "no config set" by returning a
    ///     null UTF-16 pointer. CoreLib's `new string((char*)null)` then yields
    ///     `String.Empty`, matching real CoreCLR when the named config knob is
    ///     unset (see NativeString.fs / `String..ctor(char*)`).
    ///   * `IsEventSourceLoggingEnabled`: return `FALSE` so
    ///     `XplatEventLogger.InitializePersistentListener` skips creating the
    ///     listener entirely. This is the truthful answer in a host with no
    ///     tracing consumer attached.
    ///   * `LogEventSource`: void no-op. With `IsEventSourceLoggingEnabled`
    ///     returning false the listener is never created and this entry point
    ///     should be unreachable from `OnEventWritten`, but implementing it
    ///     defensively keeps PawPrint robust against guests that import it
    ///     directly.
    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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
        | "EventSource_GetClrConfig",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "XplatEventLogger",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16) ],
          MethodReturnType.Returns (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char)) ->
            // The single argument is the UTF-16 config-name pointer (marshalled
            // from the C# `string` parameter); intentionally not read because
            // PawPrint has no configured EventSource knobs.
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ManagedPointerSource.Null) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        | "IsEventSourceLoggingEnabled",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "XplatEventLogger",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // The C# wrapper carries `[return: MarshalAs(UnmanagedType.Bool)]`,
            // which causes the LibraryImport source generator to declare the
            // underlying QCall as `int32`-returning (the wrapper converts via
            // `cgt.un`). Push 0 (FALSE) — we have no tracing consumer.
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        | "LogEventSource",
          "System.Private.CoreLib",
          "System.Diagnostics.Tracing",
          "XplatEventLogger",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16) ],
          MethodReturnType.Void ->
            // No-op: PawPrint discards the event. Arguments (event ID, event
            // name, event source name, payload) are intentionally not read.
            state |> NativeHandlerResult.completed |> Some

        | _ -> None
