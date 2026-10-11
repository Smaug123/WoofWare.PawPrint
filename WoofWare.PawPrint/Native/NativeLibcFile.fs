namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The C library's file entry points that a CoreLib P/Invokes directly, naming
/// the library `libc` rather than going through `libSystem.Native`. So far that
/// is `clonefile`, which a Darwin CoreLib's `File.Copy` tries before the shim's
/// `SystemNative_CopyFile` (FileSystem.TryCloneFile.OSX.cs).
[<RequireQualifiedAccess>]
module NativeLibcFile =

    /// `int clonefile(const char* src, const char* dst, uint32_t flags)`.
    ///
    /// On a Darwin kernel this is the kernel's `clonefile(2)`: the flags
    /// screened before either pathname is read out of guest memory, and the
    /// source read and resolved before the destination is read.
    ///
    /// On a Linux kernel the answer is ENOTSUP. Only a Darwin CoreLib calls
    /// this, and PawPrint runs a CoreLib of either flavour on a kernel of
    /// either; a Darwin process answers ENOTSUP for a volume that cannot clone,
    /// which is what the Linux model's filesystem is (its `ioctl(FICLONE)` is
    /// EOPNOTSUPP, the same number on Linux), and CoreLib then copies through
    /// `SystemNative_CopyFile`, which on a Linux kernel is the Linux shim's.
    let private clonefile (ctx : NativeCallContext) : NativeHandlerResult option =
        let operation = "libc clonefile"
        let state = ctx.State
        let instruction = ctx.Instruction

        let returning (value : int) (state : IlMachineState) : NativeHandlerResult option =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim value)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        let answered
            (outcome : SyscallAnswer * UnixSystem<ThreadId, NativeSignalHandler>)
            : NativeHandlerResult option
            =
            match outcome with
            | SyscallAnswer.Failed error, system -> returning -1 (NativeSystemNative.withErrno ctx error system state)
            | SyscallAnswer.Completed _, system -> returning 0 (NativeSystemNative.withAnswered system state)

        let refused (refusal : CloneFileRefusal) : NativeHandlerResult option =
            failwith $"%s{operation}: %s{CloneFileRefusal.describe refusal}"

        match SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> returning -1 (NativeSystemNative.withErrnoOnly ctx UnixError.ENOTSUP state)
        | SimulatedUnixFlavour.Darwin ->

        let flags = NativeCall.int32Argument operation instruction.Arguments.[2]

        // Each pathname is read out of guest memory only when the kernel
        // reaches its copy-in: flags it refuses are answered without either.
        match UnixNamespace.cloneFileFlagsPhase flags state.Kernel.System with
        | Error refusal -> refused refusal
        | Ok (CloneFileScreen.Answered (answer, system)) -> answered (answer, system)
        | Ok (CloneFileScreen.NeedsSource screened) ->

        match NativeSystemNative.pathArgumentBytes ctx operation "src" instruction.Arguments.[0] state with
        | Error u ->
            NativeHandlerResult.undefinedRead instruction.ExecutingMethod "the path its `src` argument names" u
            |> Some
        | Ok source ->


        match UnixNamespace.cloneFileSourcePhase source screened with
        | Error refusal -> refused refusal
        | Ok (CloneFileProgress.Answered (answer, system)) -> answered (answer, system)
        | Ok (CloneFileProgress.NeedsDestination paused) ->
            match NativeSystemNative.pathArgumentBytes ctx operation "dst" instruction.Arguments.[1] state with
            | Error u ->
                NativeHandlerResult.undefinedRead instruction.ExecutingMethod "the path its `dst` argument names" u
                |> Some
            | Ok destination ->


            match UnixNamespace.cloneFileWithDestination destination paused with
            | Error refusal -> refused refusal
            | Ok outcome -> answered outcome

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        let entryPoint =
            match instruction.ExecutingMethod.TryNativeImport with
            | Some import when import.ModuleName = "libc" -> Some import.EntryPointName
            | _ -> None

        match
            entryPoint,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | Some "clonefile",
          [ ConcretePointer _ ; ConcretePointer _ ; ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ->
            clonefile ctx
        | _ -> None
