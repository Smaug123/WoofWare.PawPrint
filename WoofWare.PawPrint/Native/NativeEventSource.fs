namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeEventSource =
    /// Parse a CLRConfig DWORD env-var value the way CoreCLR does for
    /// `EnableEventLog` — `u16_strtoul(val, &endPtr, 16)` with the
    /// success condition `errno != ERANGE && endPtr != val` (see
    /// `GetConfigDWORD` in `clrconfig.cpp:228`). The radix is 16
    /// because `EnableEventLog` is declared via `RETAIL_CONFIG_DWORD_INFO`
    /// with no `ParseIntegerAsBase10` flag (`clrconfigvalues.h:580`).
    ///
    /// To mirror `wcstoul` we:
    ///   * skip leading whitespace;
    ///   * accept a single optional `+` / `-` sign (negation wraps via
    ///     two's complement, so `-1` becomes `0xFFFFFFFF` — non-zero,
    ///     which the gate reads as TRUE);
    ///   * accept (but do not require) an optional `0x` / `0X` radix
    ///     prefix, but only when it is followed by at least one hex
    ///     digit (otherwise the leading `0` is itself the digit and the
    ///     `x` becomes a stop character);
    ///   * consume the longest hex-digit prefix and ignore everything
    ///     after it (so `1garbage` parses as 1, matching `wcstoul`).
    ///
    /// We return `None` iff no digits could be consumed (CoreCLR's
    /// `endPtr == val` arm) or the parsed value would overflow `uint32`
    /// (CoreCLR's `errno == ERANGE` arm). The caller treats `None` as
    /// the default `0`, i.e. disabled.
    ///
    /// `IsEventSourceLoggingEnabled` only asks whether the parsed value
    /// is non-zero, but the parser is shaped to surface the full DWORD
    /// so future knobs that care about the numeric magnitude can reuse it.
    ///
    /// Exposed as `internal` so the test assembly can pin the parser's
    /// behaviour directly without going through the QCall dispatcher.
    let internal tryParseClrConfigDword (raw : string) : uint32 option =
        let isHexDigit (c : char) : bool =
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

        let trimmed = raw.TrimStart ()

        if System.String.IsNullOrEmpty trimmed then
            None
        else
            let signStart, negate =
                match trimmed.[0] with
                | '+' -> 1, false
                | '-' -> 1, true
                | _ -> 0, false

            // Optional `0x` / `0X` radix prefix — only treated as a prefix
            // when at least one hex digit follows it. Otherwise the `0`
            // is itself the parsed digit and the `x` becomes the
            // stop character (matching `wcstoul`'s longest-valid-prefix
            // semantics).
            let bodyStart =
                if
                    signStart + 2 < trimmed.Length
                    && trimmed.[signStart] = '0'
                    && (trimmed.[signStart + 1] = 'x' || trimmed.[signStart + 1] = 'X')
                    && isHexDigit trimmed.[signStart + 2]
                then
                    signStart + 2
                else
                    signStart

            let mutable idx = bodyStart

            while idx < trimmed.Length && isHexDigit trimmed.[idx] do
                idx <- idx + 1

            if idx = bodyStart then
                None
            else
                let hexBody = trimmed.Substring (bodyStart, idx - bodyStart)

                match
                    System.UInt32.TryParse (
                        hexBody,
                        System.Globalization.NumberStyles.HexNumber,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | true, value -> if negate then Some (0u - value) else Some value
                | false, _ ->
                    // Overflow — `wcstoul` sets `errno = ERANGE`, and
                    // `GetConfigDWORD` rejects via that arm. Surface as
                    // `None` so the caller falls back to the default.
                    None

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
