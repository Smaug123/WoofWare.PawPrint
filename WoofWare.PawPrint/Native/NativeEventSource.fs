namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeEventSource =
    /// Encode `s` as UTF-16-LE bytes followed by a two-byte NUL terminator,
    /// ready to be written into a freshly-allocated native-memory block whose
    /// address is then handed back to the guest as a `char*`. The CoreLib
    /// consumer (`new string((char*)EventSource_GetClrConfig(name))`) scans
    /// for the terminator, so the trailing two zero bytes must be present.
    let private packUtf16WithNullTerminator (s : string) : byte[] =
        let buffer = Array.zeroCreate ((s.Length + 1) * 2)
        let written = System.Text.Encoding.Unicode.GetBytes (s, 0, s.Length, buffer, 0)

        if written <> s.Length * 2 then
            failwith
                $"NativeEventSource.packUtf16WithNullTerminator: expected %d{s.Length * 2} bytes for %d{s.Length} UTF-16 code units, got %d{written}"

        buffer

    /// QCalls declared on `System.Diagnostics.Tracing.XplatEventLogger` and
    /// reached from CoreLib only when `FEATURE_EVENTSOURCE_XPLAT` was defined
    /// at CoreLib build time (i.e. the Linux-built `System.Private.CoreLib`).
    /// They are still registered unconditionally because PawPrint always
    /// dispatches against the host runtime's CoreLib, which can be the Linux
    /// one on a Linux dev box or CI runner.
    ///
    /// All three handlers are faithful to CoreCLR with respect to the guest's
    /// emulated environment:
    ///
    ///   * `EventSource_GetClrConfig(name)` returns the value CLRConfig
    ///     reads for the knob `name` (see `ClrConfigEnvironment.tryGetValue`),
    ///     encoded as a freshly-allocated
    ///     UTF-16 buffer with a NUL terminator. Unset/empty values yield a
    ///     null pointer, matching CoreCLR's behaviour when the knob is
    ///     absent (CoreLib's `new string((char*)null)` then collapses to
    ///     `String.Empty`).
    ///
    ///   * `IsEventSourceLoggingEnabled()` returns the value of
    ///     the `EnableEventLog` knob parsed as a CLRConfig DWORD (hex by
    ///     default; see `ClrConfigEnvironment.tryParseDword`), defaulting to `0`
    ///     (FALSE) when unset or malformed. This matches
    ///     `XplatEventLogger::IsEventLoggingEnabled()` in
    ///     `eventtracebase.h:489`. When the result is FALSE the persistent
    ///     listener is never created and `LogEventSource` is unreachable.
    ///
    ///   * `LogEventSource(...)` fails loud. It is only reachable when the
    ///     guest enabled tracing via `DOTNET_EnableEventLog`, and PawPrint
    ///     has no LTTng / EventPipe consumer to forward the event to.
    ///     Silently dropping the event would hide a real signal — if you
    ///     want a no-op, set `DOTNET_EnableEventLog=0` (or unset it) so
    ///     the listener never gets constructed.
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
            let operation = "EventSource_GetClrConfig"

            let namePtr =
                NativeCall.managedPointerOfPointerArgument operation "configName" instruction.Arguments.[0]

            let configName =
                NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state namePtr

            match ClrConfigEnvironment.tryGetValue operation state.Kernel.Environment configName with
            | None ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ManagedPointerSource.Null) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Some value ->
                let ptr, state =
                    NativeCall.allocateNativeHeapBlob operation (packUtf16WithNullTerminator value) state

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) ctx.Thread
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
            // `cgt.un`).
            let enabled =
                match
                    ClrConfigEnvironment.tryGetValue
                        "IsEventSourceLoggingEnabled"
                        state.Kernel.Environment
                        "EnableEventLog"
                with
                | None -> false
                | Some raw ->
                    match ClrConfigEnvironment.tryParseDword raw with
                    | None -> false
                    | Some value -> value <> 0u

            let resultInt = if enabled then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim resultInt)) ctx.Thread
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
            // Only reachable when the guest enabled tracing via
            // `DOTNET_EnableEventLog`; the `XplatEventLogger` listener forwards
            // every observed `EventSource` event here, expecting CoreCLR to
            // hand it to LTTng. PawPrint has no LTTng / EventPipe consumer,
            // so silently dropping the event would lose data the guest asked
            // us to surface. Fail loud and point at the opt-out knob.
            failwith
                "LogEventSource: PawPrint has no LTTng/EventPipe consumer to forward EventSource events to. Set DOTNET_EnableEventLog=0 (or unset it) so XplatEventLogger.InitializePersistentListener never builds the listener that calls into this QCall."

        | _ -> None
