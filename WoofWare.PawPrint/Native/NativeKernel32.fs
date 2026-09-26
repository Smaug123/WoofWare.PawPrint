namespace WoofWare.PawPrint

open WoofWare.PosixKernel

[<RequireQualifiedAccess>]
module NativeKernel32 =
    let private errorEnvVarNotFound : int = 203

    type internal GetEnvironmentVariableWPlan =
        {
            ReturnLength : uint32
            LastError : int
            ValueToWrite : string option
        }

    /// What `GetEnvironmentVariableW` answers for `value`, the bytes of the
    /// variable's value if the lookup found one (see
    /// `EnvironmentPal.tryFindValue`), given a buffer of `bufferSize` UTF-16 code
    /// units.
    ///
    /// The Unix PAL (`pal/src/misc/environ.cpp`) holds the environment as the
    /// process's bytes and answers the W entry point through
    /// `GetEnvironmentVariableA`, passing `nSize` through unchanged. So the value
    /// fits when its *byte* length is strictly below `bufferSize`, and then the
    /// return is its length in UTF-16 code units and the value is written with
    /// its terminator; otherwise nothing is written and the return is that byte
    /// length plus one. A value of N non-ASCII characters therefore does not fit
    /// a buffer of N+1 code units, even though the code units would. Measured on
    /// the real runtime: 100 `é`s against a 128-unit buffer answer 201, and
    /// against a 201-unit buffer answer 100.
    ///
    /// Only a value that fits is decoded, as in the PAL, so only such a value can
    /// fail `EnvironmentPal.decodeOrFail`.
    let internal planGetEnvironmentVariableW
        (bufferSize : int)
        (value : UnixByteString option)
        : GetEnvironmentVariableWPlan
        =
        match value with
        | None ->
            {
                ReturnLength = 0u
                LastError = errorEnvVarNotFound
                ValueToWrite = None
            }
        | Some value ->
            let byteLength = UnixByteString.length value

            if byteLength < bufferSize then
                let decoded = EnvironmentPal.decodeOrFail "GetEnvironmentVariableW" value

                {
                    ReturnLength = uint32 decoded.Length
                    LastError = 0
                    ValueToWrite = Some decoded
                }
            else
                {
                    ReturnLength = uint32 (byteLength + 1)
                    LastError = 0
                    ValueToWrite = None
                }

    /// The bytes `GetEnvironmentStringsW` hands back: every entry of
    /// `environment`, in order, decoded to UTF-16 and followed by a NUL code
    /// unit, then one further NUL code unit closing the block, as little-endian
    /// code units because the entry point returns a `char*`. An empty
    /// environment is therefore a lone NUL rather than a null pointer, matching
    /// the PAL, whose only null return is on `malloc` failure.
    ///
    /// This is the PAL's block exactly: it walks its snapshot of `environ` and
    /// converts every entry, so duplicates, entries with no `=`, and entries
    /// beginning with `=` all appear, in the order the process was started with.
    /// Sorting them out is CoreLib's business (`GetEnvironmentVariables` skips
    /// an entry whose first `=` is not after its first character, and keeps the
    /// first of two entries naming the same variable). An empty entry appears
    /// too, as a lone NUL, which ends the block early for any reader.
    ///
    /// Fails, through `EnvironmentPal.decodeOrFail`, on an entry that is not
    /// valid UTF-8.
    let internal environmentBlockBytes (environment : UnixByteString list) : byte array =
        let entries =
            environment |> List.map (EnvironmentPal.decodeOrFail "GetEnvironmentStringsW")

        // Per entry: its code units and its terminator; then one more code unit
        // closing the block. Two bytes each.
        let codeUnits = 1 + (entries |> List.sumBy (fun entry -> entry.Length + 1))

        let size = codeUnits * 2
        let bytes = Array.zeroCreate<byte> size
        let mutable at = 0

        let appendCodeUnit (c : char) : unit =
            bytes.[at] <- byte (uint16 c &&& 0xFFus)
            bytes.[at + 1] <- byte (uint16 c >>> 8)
            at <- at + 2

        for entry in entries do
            for c in entry do
                appendCodeUnit c

            appendCodeUnit (char 0)

        appendCodeUnit (char 0)

        if at <> size then
            failwith
                $"GetEnvironmentStringsW: wrote %d{at} bytes into a %d{size}-byte environment block; this is an interpreter bug"

        bytes

    let private withKernel32LastSystemError
        (thread : ThreadId)
        (error : int)
        (state : IlMachineState)
        : IlMachineState
        =
        // CoreLib's generated P/Invoke wrapper clears and reads this
        // GetLastError slot, then writes LastPInvokeError itself.
        state.MapKernel (EmulatedKernel.withLastSystemError thread error)

    let private writeUtf16Char
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (charConcreteType : ConcreteType<ConcreteTypeHandle>)
        (ptr : ManagedPointerSource)
        (charIndex : int)
        (value : char)
        : IlMachineState
        =
        let ptr =
            ManagedPointerByteView.addByteOffset state charConcreteType (charIndex * 2) ptr

        IlMachineState.writeManagedByrefBytesOrTypedCell
            baseClassTypes
            state
            (ManagedPointerSource.requireAddressed ptr)
            (CliType.ofChar value)

    let private writeNullTerminatedUtf16
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : string)
        : IlMachineState
        =
        let charConcreteType =
            NativeCall.requiredCharConcreteType operation baseClassTypes state

        // Caller must already have checked capacity; this writes value plus
        // the null terminator unconditionally.
        let state =
            ((state, 0), value)
            ||> Seq.fold (fun (state, charIndex) c ->
                writeUtf16Char operation baseClassTypes state charConcreteType ptr charIndex c, charIndex + 1
            )
            |> fst

        writeUtf16Char operation baseClassTypes state charConcreteType ptr value.Length (char 0)

    let private uint32OfArgument (operation : string) (argName : string) (arg : CliType) : uint32 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) when i >= 0 -> uint32 i
        | CliType.Numeric (CliNumericType.Int32 i) ->
            failwith
                $"%s{operation}: %s{argName} was Int32 %d{i}, i.e. UInt32 %u{uint32 i}, which exceeds PawPrint's int32 allocation model"
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim i)) when
            i >= 0L && i <= int64 System.UInt32.MaxValue
            ->
            uint32 i
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim i)) when
            i >= 0L && i <= int64 System.UInt32.MaxValue
            ->
            uint32 i
        | other -> failwith $"%s{operation}: expected %s{argName} to be UInt32, got %O{other}"

    let private checkedBufferSize (operation : string) (value : uint32) : int =
        if value > uint32 System.Int32.MaxValue then
            failwith $"%s{operation}: buffer size %d{value} exceeds PawPrint's int32 allocation model"

        int value

    let private pushUInt32 (value : uint32) (thread : ThreadId) (state : IlMachineState) : NativeHandlerResult =
        state
        |> IlMachineState.pushToEvalStack (NativeCall.cliUInt32 value) thread
        |> NativeHandlerResult.completed

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "GetEnvironmentVariableW",
          "System.Private.CoreLib",
          "Kernel32",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            let operation = "GetEnvironmentVariableW"

            let namePtr =
                NativeCall.managedPointerOfPointerArgument operation "lpName" instruction.Arguments.[0]

            let bufferSize =
                instruction.Arguments.[2]
                |> uint32OfArgument operation "nSize"
                |> checkedBufferSize operation

            let name =
                NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state namePtr

            // The "kernel32!GetEnvironmentVariableW" QCall is a CoreCLR PAL entry on
            // Unix hosts, where the PAL matches names byte for byte (see CoreCLR
            // pal/src/misc/environ.cpp `FindEnvVarValue`). On Windows the real
            // kernel32 entry would be case-insensitive, but PawPrint is baselined
            // against the host runtime — which is the Unix PAL on the macOS/Linux
            // hosts this project actually runs on.
            let plan =
                planGetEnvironmentVariableW bufferSize (EnvironmentPal.tryFindValue name state.Kernel.Environment)

            let state =
                match plan.ValueToWrite with
                | None -> state
                | Some value ->
                    let bufferPtr =
                        NativeCall.managedPointerOfPointerArgument operation "lpBuffer" instruction.Arguments.[1]

                    writeNullTerminatedUtf16 operation ctx.BaseClassTypes state bufferPtr value

            state
            |> withKernel32LastSystemError ctx.Thread plan.LastError
            |> pushUInt32 plan.ReturnLength ctx.Thread
            |> Some
        | "GetEnvironmentStringsW",
          "System.Private.CoreLib",
          "Kernel32",
          [],
          MethodReturnType.Returns (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char)) ->
            let ptr, state =
                NativeCall.allocateNativeHeapBlob
                    "GetEnvironmentStringsW"
                    (environmentBlockBytes state.Kernel.Environment)
                    state

            // The last-error slot is deliberately untouched, unlike in
            // `GetEnvironmentVariableW` above. Both are `LibraryImport("QCall")`,
            // but only that one declares `SetLastError = true`, so only that one
            // gets a generated wrapper that clears the slot, calls, and reads it
            // back — these two are the P/Invoke declaration itself, with no
            // wrapper and no reader. The PAL agrees: it sets a last error only on
            // the `malloc` failure that returns null, which PawPrint cannot
            // reach.
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | "FreeEnvironmentStringsW",
          "System.Private.CoreLib",
          "Kernel32",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics)) when
            boolGenerics.IsEmpty
            ->
            let operation = "FreeEnvironmentStringsW"

            let ptr =
                NativeCall.managedPointerOfPointerArgument operation "lpszEnvironmentBlock" instruction.Arguments.[0]

            // The PAL is a bare `free(lpValue)`, so the same rule as
            // `SystemNative_Free` applies: only the block base a
            // `GetEnvironmentStringsW` returned may be released. Freeing here
            // rather than leaking the block is what makes a guest that keeps
            // reading the block after freeing it report a use-after-free, which
            // is what the real process would do.
            //
            // The null arm is unreachable from the only caller, which passes
            // back the non-null pointer it was handed; it is here because the
            // classifier is shared with `SystemNative_Free`, where
            // `Marshal.FreeHGlobal(IntPtr.Zero)` does reach it, and because the
            // PAL likewise no-ops on null.
            let state =
                match NativeCall.tryResolveNativeHeapFreeTarget ptr with
                | Ok None -> state
                | Ok (Some block) -> IlMachineState.freeNativeMemory block state
                | Error reason -> failwith $"%s{operation}: %s{reason}"

            // The PAL returns TRUE unconditionally, and sets no last-error.
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None
