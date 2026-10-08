namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// The C library's signal-mask entry points: `pthread_sigmask(3)`,
/// `sigprocmask(2)` and `sigpending(2)`. No BCL code reaches them (System.Native
/// blocks signals only inside its own fork-and-exec), so only a guest that
/// declares them itself, naming the library `libc`, calls these.
///
/// Each reads and writes a `sigset_t` through the guest's pointer, as many
/// bytes of it as the kernel moves: Linux's kernel set is 8 bytes (glibc's own
/// `sigset_t` is 128, of which it passes the kernel the first 8, and the kernel
/// writes 8 back), and Darwin's `sigset_t` is 4.
[<RequireQualifiedAccess>]
module NativeLibcSignalMask =

    let private sigsetBytes (numbering : SignalNumbering) : int =
        match numbering with
        | SignalNumbering.Linux -> 8
        | SignalNumbering.Darwin -> 4

    /// The set a `const sigset_t*` argument points to, or `None` for NULL.
    ///
    /// A non-null pointer naming no storage is refused rather than answered:
    /// glibc reads the set in user space before the kernel sees it, to screen
    /// its own 32 and 33, so a real run would fault there rather than answer
    /// EFAULT.
    let private readSet
        (ctx : NativeCallContext)
        (operation : string)
        (numbering : SignalNumbering)
        (argument : CliType)
        (state : IlMachineState)
        : SignalMask option
        =
        let pointer = NativeSystemNative.bufferPointerArgument operation "set" argument

        match pointer with
        | BufferPointer.RawAddress 0UL -> None
        | _ ->

        match BufferPointer.dereferenceable pointer with
        | None ->
            failwith
                $"%s{operation}: `set` is %O{pointer}, which is not null but names no storage; the C library reads the set before the kernel does, so a real run would fault there, which PawPrint does not model. Pass a real sigset_t."
        | Some storage ->
            let bytes =
                NativeSystemNative.readBytesThrough ctx operation storage (sigsetBytes numbering) state

            let word =
                (0UL, Seq.indexed bytes)
                ||> Seq.fold (fun word (index, b) -> word ||| (uint64 b <<< (8 * index)))

            match SignalMask.ofWord numbering word with
            | Ok mask -> Some mask
            | Error refusal ->
                failwith
                    $"%s{operation}: %d{sigsetBytes numbering} bytes read as a set that does not fit them (%s{SignalMaskRefusal.describe refusal}); this is an interpreter bug."

    /// `mask` written through a `sigset_t*` out-parameter, unless it is NULL.
    let private writeSet
        (ctx : NativeCallContext)
        (operation : string)
        (argName : string)
        (numbering : SignalNumbering)
        (argument : CliType)
        (mask : SignalMask)
        (state : IlMachineState)
        : IlMachineState
        =
        let pointer = NativeSystemNative.bufferPointerArgument operation argName argument

        match pointer with
        | BufferPointer.RawAddress 0UL -> state
        | _ ->

        match BufferPointer.dereferenceable pointer with
        | None ->
            failwith
                $"%s{operation}: `%s{argName}` is %O{pointer}, which is not null but names no storage. The kernel would answer EFAULT having changed the mask already, which PawPrint does not model. Pass a real sigset_t."
        | Some storage ->
            let word = SignalMask.toWord mask

            let bytes =
                ImmutableArray.CreateRange (Seq.init (sigsetBytes numbering) (fun index -> byte (word >>> (8 * index))))

            NativeSystemNative.writeBytesThrough ctx operation storage bytes state

    let private pushInt32 (ctx : NativeCallContext) (value : int) (state : IlMachineState) : NativeHandlerResult =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim value)) ctx.Thread
        |> NativeHandlerResult.completed

    /// How a mask call reports a failure.
    [<RequireQualifiedAccess>]
    type private ErrorConvention =
        /// Returns the error number and leaves errno alone: glibc's
        /// `pthread_sigmask`.
        | Returned
        /// Returns the error number and sets errno to it too: Darwin's
        /// `pthread_sigmask`.
        | ReturnedAndInErrno
        /// Returns -1 and sets errno: `sigprocmask`.
        | InErrno

    /// `pthread_sigmask`'s convention on this flavour. Measured by
    /// `sigprocmask-ops.c`'s "errno" row on Linux 6.18.5 (glibc 2.41) and
    /// Darwin 27.0.0: with an unnamed `how`, both returned 22; errno was then
    /// 22 on Darwin and as it was before the call on Linux. A successful call
    /// left errno alone on both.
    let private pthreadConvention (numbering : SignalNumbering) : ErrorConvention =
        match numbering with
        | SignalNumbering.Linux -> ErrorConvention.Returned
        | SignalNumbering.Darwin -> ErrorConvention.ReturnedAndInErrno

    /// `pthread_sigmask(how, set, oldset)` or `sigprocmask(how, set, oldset)`,
    /// as `call` answers it. Signals the change makes deliverable to the
    /// calling thread are taken as it next returns to user mode (see
    /// `SignalDispatch.poll`).
    let private maskCall
        (ctx : NativeCallContext)
        (operation : string)
        (convention : SignalNumbering -> ErrorConvention)
        (call :
            ThreadId
                -> int
                -> SignalMask option
                -> UnixSystem<ThreadId, NativeSignalHandler>
                -> Result<SignalMask * UnixSystem<ThreadId, NativeSignalHandler>, UnixError>)
        : NativeHandlerResult
        =
        let state = ctx.State
        let arguments = ctx.Instruction.Arguments
        let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform
        let how = NativeCall.int32Argument operation arguments.[0]
        let set = readSet ctx operation numbering arguments.[1] state

        match call ctx.Thread how set state.Kernel.System with
        | Ok (old, system) ->
            NativeSystemNative.withAnswered system state
            |> writeSet ctx operation "oldset" numbering arguments.[2] old
            |> pushInt32 ctx 0
        | Error error ->
            // Measured by `sigprocmask-ops.c`: a failed call writes nothing to
            // `oldset`.
            let raw =
                UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform) error

            match convention numbering with
            | ErrorConvention.Returned -> pushInt32 ctx raw state
            | ErrorConvention.ReturnedAndInErrno ->
                NativeSystemNative.withErrnoOnly ctx error state |> pushInt32 ctx raw
            | ErrorConvention.InErrno -> NativeSystemNative.withErrnoOnly ctx error state |> pushInt32 ctx -1

    /// `sigpending(set)`: the signals pending for the calling thread that its
    /// mask blocks, written through `set`.
    ///
    /// A NULL `set` is refused: Linux answers EFAULT, and what Darwin does has
    /// not been measured.
    let private sigpending (ctx : NativeCallContext) : NativeHandlerResult =
        let operation = "libc sigpending"
        let state = ctx.State
        let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform
        let argument = ctx.Instruction.Arguments.[0]

        match NativeSystemNative.bufferPointerArgument operation "set" argument with
        | BufferPointer.RawAddress 0UL ->
            failwith
                $"%s{operation}: `set` is NULL. Linux answers EFAULT and what Darwin does has not been measured; PawPrint models neither. Pass a real sigset_t."
        | _ ->

        let pending = UnixSignal.sigpending ctx.Thread state.Kernel.System

        state
        |> writeSet ctx operation "set" numbering argument pending
        |> pushInt32 ctx 0

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
        | Some "pthread_sigmask",
          [ ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ; ConcretePointer _ ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ->
            // `int pthread_sigmask(int how, const sigset_t* set, sigset_t* oldset)`.
            maskCall ctx "libc pthread_sigmask" pthreadConvention UnixSignal.pthreadSigmask
            |> Some
        | Some "sigprocmask",
          [ ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ; ConcretePointer _ ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ->
            // `int sigprocmask(int how, const sigset_t* set, sigset_t* oldset)`.
            maskCall ctx "libc sigprocmask" (fun _ -> ErrorConvention.InErrno) UnixSignal.sigprocmask
            |> Some
        | Some "sigpending",
          [ ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ->
            // `int sigpending(sigset_t* set)`.
            sigpending ctx |> Some
        | _ -> None
