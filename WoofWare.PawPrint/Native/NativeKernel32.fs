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

    let internal planGetEnvironmentVariableW (bufferSize : int) (value : string option) : GetEnvironmentVariableWPlan =
        match value with
        | None ->
            {
                ReturnLength = 0u
                LastError = errorEnvVarNotFound
                ValueToWrite = None
            }
        | Some value ->
            let requiredSize = value.Length + 1

            if bufferSize < requiredSize then
                {
                    ReturnLength = uint32 requiredSize
                    LastError = 0
                    ValueToWrite = None
                }
            else
                {
                    ReturnLength = uint32 value.Length
                    LastError = 0
                    ValueToWrite = Some value
                }

    /// Re-assert `UnixProcessState.environmentEntryProblem` at the point the map is
    /// flattened back into an environment list.
    ///
    /// `UnixProcessState.withEnvironment` already rejects such an entry when the
    /// table is built, so this is unreachable through `KernelConfig`. It is here
    /// because a kernel assembled by record-copy — as tests do — never passed
    /// through that writer, and emitting the block anyway would hand a guest
    /// variables that differ from the ones `GetEnvironmentVariableW` reports for
    /// the same table. Same reasoning as `systemTimeAsTicks` re-asserting its
    /// epoch bound.
    let private requireBlockRepresentable (name : string) (value : string) : unit =
        match UnixProcessState.environmentEntryProblem name value with
        | None -> ()
        | Some problem ->
            failwith
                $"GetEnvironmentStringsW: the emulated environment holds %s{problem}. A kernel built through KernelConfig cannot reach this, so the table was assembled by record-copy."

    /// The bytes `GetEnvironmentStringsW` hands back: every variable as
    /// `name=value` followed by a NUL code unit, then one further NUL code unit
    /// closing the block, as UTF-16 little-endian code units because the entry
    /// point returns a `char*`. An empty environment is therefore a lone NUL
    /// rather than a null pointer, matching the PAL, whose only null return is
    /// on `malloc` failure.
    ///
    /// Entries appear in ordinal order of their names, which is what iterating
    /// a `Map` gives. The real block's order is that of the process's `environ`
    /// at PAL init, further permuted by `EnvironUnsetenv` filling a hole with
    /// the last entry, so PawPrint's order differs — a guest enumerating the
    /// resulting `Hashtable` could in principle tell, since bucket occupancy
    /// depends on insertion order. Ordering by name is what makes the block a
    /// function of the environment alone, which is what a replay needs; no
    /// fixed order can also match the host's.
    ///
    /// Fails rather than emitting a list that would parse back to a different
    /// table; see `requireBlockRepresentable`.
    let internal environmentBlockBytes (environment : Map<string, string>) : byte array =
        for KeyValue (name, value) in environment do
            requireBlockRepresentable name value

        // Per entry: the name, the `=`, the value, and the entry's terminator;
        // then one more code unit closing the block. Two bytes each.
        let codeUnits =
            1
            + (environment
               |> Seq.sumBy (fun (KeyValue (name, value)) -> name.Length + value.Length + 2))

        let size = codeUnits * 2
        let bytes = Array.zeroCreate<byte> size
        let mutable at = 0

        // Written code unit by code unit rather than through an `Encoding`: the
        // guest never *decodes* this block, it reinterprets the bytes as
        // `char`s, and `Encoding.Unicode` is not faithful at that level — it
        // replaces an unpaired surrogate with U+FFFD, which would make
        // `GetEnvironmentVariables` disagree with `GetEnvironmentVariableW`
        // (which writes value code units verbatim) for the same table.
        let appendCodeUnit (c : char) : unit =
            bytes.[at] <- byte (uint16 c &&& 0xFFus)
            bytes.[at + 1] <- byte (uint16 c >>> 8)
            at <- at + 2

        let appendCodeUnits (s : string) : unit =
            for c in s do
                appendCodeUnit c

        for KeyValue (name, value) in environment do
            appendCodeUnits name
            appendCodeUnit '='
            appendCodeUnits value
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

        IlMachineState.writeManagedByrefBytesOrTypedCell baseClassTypes state ptr (CliType.ofChar value)

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
            // Unix hosts, where the PAL implementation matches env-var names exactly
            // (see CoreCLR pal/src/misc/environ.cpp `FindEnvVarValue`). On Windows
            // the real kernel32 entry would be case-insensitive, but PawPrint is
            // baselined against the host runtime — which is the Unix PAL on the
            // macOS/Linux hosts this project actually runs on — so an exact
            // `Map.tryFind` is the semantics that keeps PawPrint in step with the
            // real runtime.
            let plan =
                planGetEnvironmentVariableW bufferSize (Map.tryFind name state.Kernel.Environment)

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
