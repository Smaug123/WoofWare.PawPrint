namespace WoofWare.PosixKernel

/// The bits `getrandom(2)` accepts in its `flags` argument, as Linux's
/// `<sys/random.h>` numbers them.
///
/// None of them changes what this kernel answers: its pool is initialised from
/// boot, so there is never a wait for `GRND_NONBLOCK` to decline, and every
/// source the other two select is that one pool.
[<RequireQualifiedAccess>]
module GetRandomFlags =
    /// `GRND_NONBLOCK`.
    [<Literal>]
    let NonBlock : uint32 = 1u

    /// `GRND_RANDOM`.
    [<Literal>]
    let Random : uint32 = 2u

    /// `GRND_INSECURE`.
    [<Literal>]
    let Insecure : uint32 = 4u

/// What `getrandom(2)` did, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type GetRandomAnswer =
    /// The bytes to place in the caller's buffer, which the caller fetches
    /// from the draw, all at once or a range at a time; the syscall returns
    /// `EntropyDraw.count draw`. That can be fewer than were asked for: see
    /// `UnixEntropy.getRandomMaxTransfer`. The system that comes back with this
    /// answer has already moved its pool past them.
    ///
    /// A count of zero means the call moved nothing and the buffer was not
    /// touched at all.
    | Completed of draw : EntropyDraw
    /// The syscall returns -1 and the caller stores `error` wherever its libc
    /// keeps errno. Nothing was drawn from the pool.
    | Failed of error : UnixError

/// Why this kernel will not answer a `getrandom`.
[<RequireQualifiedAccess>]
type GetRandomRefusal =
    /// The buffer has no answer at the step the call reached.
    | Buffer of BufferRefusal
    /// The simulated kernel has no `getrandom` system call.
    | NoSuchSyscall of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module GetRandomRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half: which entry point, and what it passed.
    let describe (refusal : GetRandomRefusal) : string =
        match refusal with
        | GetRandomRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | GetRandomRefusal.NoSuchSyscall flavour ->
            $"the simulated kernel is %O{flavour}, which has no getrandom system call; getentropy(2) is how a process asks it for random bytes."

/// What `getentropy(2)` did, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type GetEntropyAnswer =
    /// The bytes to place in the caller's buffer, exactly as many as were asked
    /// for, which the caller fetches from the draw; the syscall returns 0. The
    /// system that comes back with this answer has already moved its pool past
    /// them.
    ///
    /// A count of zero means the call was asked for nothing and the buffer was
    /// not touched at all.
    | Completed of draw : EntropyDraw
    /// The syscall returns -1 and the caller stores `error` wherever its libc
    /// keeps errno. Nothing was drawn from the pool.
    | Failed of error : UnixError

/// Why this kernel will not answer a `getentropy`.
[<RequireQualifiedAccess>]
type GetEntropyRefusal =
    /// The buffer has no answer at the step the call reached.
    | Buffer of BufferRefusal
    /// The simulated kernel has no `getentropy` system call.
    | NoSuchSyscall of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module GetEntropyRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half: which entry point, and what it passed.
    let describe (refusal : GetEntropyRefusal) : string =
        match refusal with
        | GetEntropyRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | GetEntropyRefusal.NoSuchSyscall flavour ->
            $"the simulated kernel is %O{flavour}, which has no getentropy system call; getrandom(2) is how a process asks it for random bytes."

/// The syscalls that hand out bytes from the kernel's entropy pool.
[<RequireQualifiedAccess>]
module UnixEntropy =

    /// The most bytes one `getrandom` moves on `platform`, however many were
    /// asked for: the same limit as one `read(2)`, which on Linux, the one
    /// flavour with a `getrandom`, is `MAX_RW_COUNT` (`INT_MAX` rounded down to
    /// a whole page). A larger request is not an error; it returns this many.
    let getRandomMaxTransfer (platform : SimulatedUnixPlatform) : uint64 =
        // Measured with 4 KiB pages, the only size a Linux platform admits:
        // 0x7FFFF000 on 6.18.5 aarch64 for requests of INT_MAX, 2^31,
        // 2^31 + 4096, 2^40 and SIZE_MAX alike, and the whole request for every
        // size measured up to 256 MiB; and 0x7FFFF000 on 6.12 x86-64 for INT_MAX,
        // 2^31 and SIZE_MAX. `TestEntropyAgainstHost` measures it again on a
        // Linux host.
        uint64 (TransferCountLimit.maxTransfer (SimulatedUnixPlatform.transferCountLimit platform))

    /// The most bytes one `getentropy` will be asked for. A longer request is
    /// EINVAL, and moves nothing.
    [<Literal>]
    let getEntropyMaxLength : uint64 = 256UL

    /// Where a call that has decided how many bytes to move gets them: the
    /// buffer screen, the zero-length shortcut, and the one point at which the
    /// buffer must be able to hold bytes. `Error` is a refusal; `Ok (Error _)`
    /// is an errno.
    let private transfer<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (buffer : UserBuffer)
        (length : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Result<EntropyDraw * UnixSystem<'Task, 'Handler>, UnixError>, BufferRefusal>
        =
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 length)
        with
        | Error refusal -> Error refusal
        | Ok true -> Ok (Error UnixError.EFAULT)
        | Ok false ->

        if length = 0 then
            // Nothing moves, so the buffer is not consulted: measured on both,
            // a zero-length call through a null pointer returns success. On
            // Darwin, which screens nothing, so does one through the last
            // address there is.
            Ok (Ok (fst (EntropyPool.take 0 system.Machine.EntropyPool), system))
        else

        match buffer with
        | UserBuffer.Unmapped _ ->
            // A real kernel draws before it copies out, so its pool has moved on
            // by the time the copy faults. No process can tell where a real
            // kernel's pool is, so this one stays put, which keeps a faulting
            // call from changing the system at all.
            Ok (Error UnixError.EFAULT)
        | UserBuffer.Opaque -> Error BufferRefusal.OpaqueAtTransfer
        | UserBuffer.Addressless -> Error BufferRefusal.AddresslessAtTransfer
        | UserBuffer.Mapped ->

        let draw, pool = EntropyPool.take length system.Machine.EntropyPool

        Ok (
            Ok (
                draw,
                { system with
                    Machine =
                        { system.Machine with
                            EntropyPool = pool
                        }
                }
            )
        )

    /// `getrandom(2)`: fill the caller's buffer with up to `count` bytes from
    /// the entropy pool, and return how many.
    ///
    /// `flags` is the raw argument, any combination of `GetRandomFlags`. It is
    /// EINVAL if it has any other bit set, or both `GetRandomFlags.Random` and
    /// `GetRandomFlags.Insecure`, whatever the buffer and the count.
    ///
    /// Never blocks. Linux-only: under any other flavour the call is refused.
    let getRandom<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (buffer : UserBuffer)
        (count : uint64)
        (flags : uint32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetRandomAnswer * UnixSystem<'Task, 'Handler>, GetRandomRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin as flavour -> Error (GetRandomRefusal.NoSuchSyscall flavour)
        | SimulatedUnixFlavour.Linux ->

        let known =
            GetRandomFlags.NonBlock ||| GetRandomFlags.Random ||| GetRandomFlags.Insecure

        let exclusive = GetRandomFlags.Random ||| GetRandomFlags.Insecure

        // Both checks precede everything else: measured, a malformed `flags`
        // is EINVAL through a null pointer, through the last address there is,
        // and at a count of zero.
        if flags &&& ~~~known <> 0u || flags &&& exclusive = exclusive then
            Ok (GetRandomAnswer.Failed UnixError.EINVAL, system)
        else

        // Clamped before the buffer is screened, so it is the clamped range the
        // screen sees: measured, a request of SIZE_MAX into storage that holds
        // the clamped count succeeds rather than faulting.
        let length = int (min count (getRandomMaxTransfer system.Machine.UnixPlatform))

        match transfer buffer length system with
        | Error refusal -> Error (GetRandomRefusal.Buffer refusal)
        | Ok (Error error) -> Ok (GetRandomAnswer.Failed error, system)
        | Ok (Ok (draw, system)) -> Ok (GetRandomAnswer.Completed draw, system)

    /// `getentropy(2)`: fill all `length` bytes of the caller's buffer from the
    /// entropy pool.
    ///
    /// A `length` above `getEntropyMaxLength` is EINVAL, whatever the buffer.
    ///
    /// Darwin-only: under any other flavour the call is refused.
    let getEntropy<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (buffer : UserBuffer)
        (length : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetEntropyAnswer * UnixSystem<'Task, 'Handler>, GetEntropyRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux as flavour -> Error (GetEntropyRefusal.NoSuchSyscall flavour)
        | SimulatedUnixFlavour.Darwin ->

        // Ahead of the buffer: measured, 257 bytes is EINVAL through a null
        // pointer and through the last address there is, where 256 is EFAULT.
        if length > getEntropyMaxLength then
            Ok (GetEntropyAnswer.Failed UnixError.EINVAL, system)
        else

        match transfer buffer (int length) system with
        | Error refusal -> Error (GetEntropyRefusal.Buffer refusal)
        | Ok (Error error) -> Ok (GetEntropyAnswer.Failed error, system)
        | Ok (Ok (draw, system)) -> Ok (GetEntropyAnswer.Completed draw, system)
