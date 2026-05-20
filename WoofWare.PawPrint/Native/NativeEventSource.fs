namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeEventSource =
    /// Parse a CLRConfig DWORD env-var value the way CoreCLR does for
    /// `EnableEventLog`: `wcstoul(val, &endPtr, 16)` — hex by default, with the
    /// optional `0x` prefix tolerated. Returns `None` if the value is empty or
    /// fails to parse (CoreCLR's `GetConfigDWORD` falls back to the default
    /// value, which is `0` for `EnableEventLog`).
    ///
    /// `EnableEventLog` is declared via `RETAIL_CONFIG_DWORD_INFO` with no
    /// `ParseIntegerAsBase10` flag (see `clrconfigvalues.h:580`), so the radix
    /// is 16. Both `1` and `0x1` therefore mean TRUE; `10` means 16 (also
    /// TRUE); `0` and `0x0` mean FALSE. We don't need the full DWORD value —
    /// `IsEventSourceLoggingEnabled` only asks whether it's non-zero — but
    /// the parser is shaped to surface the value so future knobs that care
    /// about the numeric magnitude can reuse it.
    ///
    /// Exposed as `internal` so the test assembly can pin the parser's
    /// behaviour directly without going through the QCall dispatcher.
    let internal tryParseClrConfigDword (raw : string) : uint32 option =
        let trimmed = raw.Trim ()

        if System.String.IsNullOrEmpty trimmed then
            None
        else
            let hexBody =
                if trimmed.StartsWith ("0x", System.StringComparison.OrdinalIgnoreCase) then
                    trimmed.Substring 2
                else
                    trimmed

            match
                System.UInt32.TryParse (
                    hexBody,
                    System.Globalization.NumberStyles.HexNumber,
                    System.Globalization.CultureInfo.InvariantCulture
                )
            with
            | true, value -> Some value
            | false, _ -> None

    /// Look up a CLRConfig string knob in the guest's emulated environment,
    /// mirroring CoreCLR's `EnvGetString` priority: try `DOTNET_<name>` first,
    /// then `COMPlus_<name>` as a fallback. Returns `None` for an unset or
    /// empty value (CoreCLR's `GetConfigString` also discards the empty
    /// string via the `*ret != W('\0')` check in `clrconfig.cpp:288`).
    let internal lookupClrConfigString (env : Map<string, string>) (name : string) : string option =
        let tryEnv (key : string) : string option =
            match Map.tryFind key env with
            | Some value when value.Length > 0 -> Some value
            | _ -> None

        match tryEnv $"DOTNET_%s{name}" with
        | Some v -> Some v
        | None -> tryEnv $"COMPlus_%s{name}"

    /// Encode `s` as UTF-16-LE bytes followed by a two-byte NUL terminator,
    /// ready to be written into a freshly-allocated native-memory block whose
    /// address is then handed back to the guest as a `char*`. The CoreLib
    /// consumer (`new string((char*)EventSource_GetClrConfig(name))`) scans
    /// for the terminator, so the trailing two zero bytes are load-bearing.
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
    ///   * `EventSource_GetClrConfig(name)` returns the value of
    ///     `DOTNET_<name>` (or the `COMPlus_<name>` fallback) from
    ///     `state.Kernel.Environment`, encoded as a freshly-allocated
    ///     UTF-16 buffer with a NUL terminator. Unset/empty values yield a
    ///     null pointer, matching CoreCLR's behaviour when the knob is
    ///     absent (CoreLib's `new string((char*)null)` then collapses to
    ///     `String.Empty`).
    ///
    ///   * `IsEventSourceLoggingEnabled()` returns the value of
    ///     `DOTNET_EnableEventLog` parsed as a CLRConfig DWORD (hex by
    ///     default; see `tryParseClrConfigDword`), defaulting to `0`
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

            match lookupClrConfigString state.Kernel.Environment configName with
            | None ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ManagedPointerSource.Null) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Some value ->
                let bytes = packUtf16WithNullTerminator value

                let ptr, state =
                    IlMachineThreadState.allocateNativeMemory
                        MemoryBlockInitialization.ZeroInitialized
                        bytes.Length
                        state

                let blockId =
                    match ptr with
                    | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockId, 0), []) -> blockId
                    | other ->
                        failwith $"%s{operation}: allocateNativeMemory returned an unexpected pointer shape (%O{other})"

                let state =
                    state.MapKernel (fun k ->
                        { k with
                            NativeMemoryPool = NativeMemoryPool.writeBytes blockId 0 bytes k.NativeMemoryPool
                        }
                    )

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
                match lookupClrConfigString state.Kernel.Environment "EnableEventLog" with
                | None -> false
                | Some raw ->
                    match tryParseClrConfigDword raw with
                    | None -> false
                    | Some value -> value <> 0u

            let resultInt = if enabled then 1 else 0

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 resultInt) ctx.Thread
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
