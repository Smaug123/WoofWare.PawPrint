namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// What System.Native's `minipal_get_cryptographically_secure_random_bytes`
/// did with a request.
[<RequireQualifiedAccess>]
type SecureRandomFill =
    /// Every byte asked for; the call returns 0.
    | Filled of bytes : ImmutableArray<byte>
    /// The call returns -1 with errno `error`, having written `written`, a
    /// prefix of the request, into the caller's buffer. The rest of the buffer
    /// keeps what it held.
    | Failed of written : ImmutableArray<byte> * error : UnixError

/// What System.Native's `minipal_get_non_cryptographically_secure_random_bytes`
/// does to the caller's buffer, which returns nothing.
[<RequireQualifiedAccess>]
type NonSecureRandomFill =
    /// The buffer becomes `bytes`.
    | Filled of bytes : ImmutableArray<byte>
    /// The secure read under it failed with `error` after writing `written`, a
    /// prefix; the rest of the buffer keeps what it held, and then `mask` is
    /// XORed over the whole buffer. errno is left as `error`.
    | OverExisting of written : ImmutableArray<byte> * mask : ImmutableArray<byte> * error : UnixError

/// Where CoreCLR's two copies of minipal get random bytes, each asked of the
/// simulated kernel through its syscalls.
///
/// - **System.Native's copy** answers `SystemNative_GetCryptographicallySecureRandomBytes`
///   (behind `Guid.NewGuid`) and `SystemNative_GetNonCryptographicallySecureRandomBytes`
///   (behind `new Random()`, `HashCode`'s seed and Marvin's). Its state is
///   `EmulatedKernel.ProcessRandom`.
/// - **CoreCLR's own copy** answers `minipal_guid_v4_create`, behind a dynamic
///   module's version ID. On Linux it holds a descriptor of its own, opened
///   before `Main`, which is not modelled: its bytes are asked of `getrandom(2)`
///   instead, which hands out what a read of that descriptor would.
///
/// The bytes are not cryptographically secure: anyone who knows the kernel's
/// seed knows them, as nothing inside a deterministic interpreter can be.
[<RequireQualifiedAccess>]
module MinipalRandom =

    let private urandomPath : PathArgumentBytes =
        match UnixByteString.ofString "/dev/urandom" with
        | Ok bytes -> PathArgumentBytes.Bytes bytes
        | Error defect -> failwith $"MinipalRandom: /dev/urandom is not a path: %A{defect}"

    let private libSystemDraw
        (length : int)
        (kernel : EmulatedKernel)
        (generator : LibSystemRandom)
        : ImmutableArray<byte> * EmulatedKernel
        =
        let bytes, generator = LibSystemRandom.draw length generator

        bytes,
        { kernel with
            ProcessRandom = ProcessRandom.LibSystem generator
        }

    /// `length` bytes for CoreCLR's own copy of minipal, and the kernel after
    /// handing them out. `operation` names the entry point that asked, for
    /// diagnostics.
    ///
    /// On Linux, `getrandom(GRND_INSECURE)` in place of CoreCLR's unmodelled
    /// descriptor: the same bytes from the same pool. On Darwin,
    /// `CCRandomGenerateBytes`, from libSystem's generator.
    let coreClrSecureRandomBytes
        (operation : string)
        (length : int)
        (kernel : EmulatedKernel)
        : ImmutableArray<byte> * EmulatedKernel
        =
        if length < 0 then
            failwith $"%s{operation}: asked for %d{length} random bytes, a negative count"

        match kernel.ProcessRandom with
        | ProcessRandom.LibSystem generator -> libSystemDraw length kernel generator
        | ProcessRandom.Minipal _ ->

        let builder = ImmutableArray.CreateBuilder<byte> length

        let rec fill (system : UnixSystem<ThreadId, NativeSignalHandler>) : UnixSystem<ThreadId, NativeSignalHandler> =
            let remaining = length - builder.Count

            if remaining = 0 then
                system
            else

            match UnixEntropy.getRandom UserBuffer.Mapped (uint64 remaining) GetRandomFlags.Insecure system with
            | Ok (GetRandomAnswer.Completed draw, system) ->
                let moved = EntropyDraw.count draw

                if moved <= 0 || moved > remaining then
                    failwith
                        $"%s{operation}: getrandom of %d{remaining} bytes moved %d{moved}; it must move at least one and at most that many (this is a bug in the kernel library)."

                builder.AddRange (EntropyDraw.bytes draw)
                fill system
            | Ok (GetRandomAnswer.Failed error, _) ->
                failwith
                    $"%s{operation}: getrandom of %d{remaining} bytes into storage PawPrint owns answered %O{error} (this is a bug in the kernel library)."
            | Error refusal ->
                failwith
                    $"%s{operation}: getrandom of %d{remaining} bytes was refused: %s{GetRandomRefusal.describe refusal}"

        let system = fill (EmulatedKernel.unix kernel)
        builder.MoveToImmutable (), EmulatedKernel.withUnix system kernel

    /// Linux's `CLOCK_REALTIME_COARSE`, which glibc's `time` reads.
    let private clockRealtimeCoarse : int = 5

    /// Linux minipal's descriptor, opening it if it is not open yet. `Error` is
    /// the errno an `open` failed with, after which the next call tries again.
    let private urandomDescriptor
        (operation : string)
        (kernel : EmulatedKernel)
        : Result<int, UnixError> * EmulatedKernel
        =
        match kernel.ProcessRandom with
        | ProcessRandom.LibSystem _ ->
            failwith $"%s{operation}: a Darwin process has no minipal descriptor (this is a bug in PawPrint)."
        | ProcessRandom.Minipal (MinipalUrandom.Open fd, _) -> Ok fd, kernel
        | ProcessRandom.Minipal (MinipalUrandom.Unopened, lrand48) ->

        // `open("/dev/urandom", O_RDONLY | O_CLOEXEC)`, retried on EINTR, which
        // an open of a device never answers here.
        match
            UnixNamespace.openPath
                (OpenFlagsPal.minipalUrandom kernel.UnixPlatform)
                urandomPath
                0
                (EmulatedKernel.unix kernel)
        with
        | Error refusal -> failwith $"%s{operation}: open(/dev/urandom) was refused: %s{OpenRefusal.describe refusal}"
        | Ok (SyscallAnswer.Completed fd, system) ->
            let kernel = EmulatedKernel.withUnix system kernel

            Ok (int fd),
            { kernel with
                ProcessRandom = ProcessRandom.Minipal (MinipalUrandom.Open (int fd), lrand48)
            }
        | Ok (SyscallAnswer.Failed UnixError.ENOENT, _) ->
            // minipal latches a missing `/dev/urandom` and never asks again. A
            // Linux machine here always has one, and nothing can remove it.
            failwith
                $"%s{operation}: open(/dev/urandom) answered ENOENT, which minipal latches for the process's life; this kernel's devtmpfs always holds the node, so the latch is not modelled (this is a bug in the kernel library)."
        | Ok (SyscallAnswer.Failed error, system) -> Error error, EmulatedKernel.withUnix system kernel

    /// `minipal_get_cryptographically_secure_random_bytes` in System.Native's
    /// copy, called on `thread`, for `length` bytes.
    ///
    /// On Linux: on first use, open `/dev/urandom` and keep the descriptor (the
    /// guest sees it: the next `open` lands one higher, and a guest that closes
    /// it makes later calls fail); then `read` it until the request is full.
    /// Refused where a real minipal would spin or sleep for ever: a read
    /// answering 0, as one does once something at end-of-file has been `dup2`'d
    /// over the descriptor, or one that would sleep, as a pipe's does.
    ///
    /// On Darwin: `CCRandomGenerateBytes`, from libSystem's generator, which
    /// never fails.
    let systemNativeSecureRandomBytes
        (operation : string)
        (thread : ThreadId)
        (length : int)
        (kernel : EmulatedKernel)
        : SecureRandomFill * EmulatedKernel
        =
        if length < 0 then
            failwith $"%s{operation}: asked for %d{length} random bytes, a negative count"

        match kernel.ProcessRandom with
        | ProcessRandom.LibSystem generator ->
            let bytes, kernel = libSystemDraw length kernel generator
            SecureRandomFill.Filled bytes, kernel
        | ProcessRandom.Minipal _ ->

        match urandomDescriptor operation kernel with
        | Error error, kernel -> SecureRandomFill.Failed (ImmutableArray.Empty, error), kernel
        | Ok fd, kernel ->

        let builder = ImmutableArray.CreateBuilder<byte> length

        // A `do ... while (offset != bufferLength)`: one read even of nothing,
        // which a descriptor the guest has closed answers EBADF.
        let rec fill (first : bool) (system : UnixSystem<ThreadId, NativeSignalHandler>) =
            let remaining = length - builder.Count

            if remaining = 0 && not first then
                None, system
            else

            let moved (bytes : ImmutableArray<byte>) =
                if bytes.IsEmpty && remaining > 0 then
                    failwith
                        $"%s{operation}: read of fd %d{fd}, minipal's /dev/urandom descriptor, answered 0 with %d{remaining} bytes still wanted, and minipal would read again for ever; the descriptor no longer names /dev/urandom."

                if bytes.Length > remaining then
                    failwith
                        $"%s{operation}: read of %d{remaining} bytes from fd %d{fd} moved %d{bytes.Length} (this is a bug in the kernel library)."

                builder.AddRange bytes

            match UnixReadWrite.read thread fd UserBuffer.Mapped (uint64 remaining) system with
            | Error refusal ->
                failwith $"%s{operation}: read of fd %d{fd} was refused: %s{ReadRefusal.describe refusal}"
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) ->
                moved bytes
                fill false system
            | Ok (ReadOutcome.Answered (ReadAnswer.Drawn draw), system) ->
                moved (EntropyDraw.bytes draw)
                fill false system
            // minipal reads again after EINTR.
            | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EINTR), system) -> fill first system
            | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), system) -> Some error, system
            | Ok (ReadOutcome.WouldBlock condition, _) ->
                failwith
                    $"%s{operation}: read of fd %d{fd}, minipal's /dev/urandom descriptor, would sleep until %A{condition}; the descriptor no longer names /dev/urandom, and a sleep inside minipal is not modelled."
            | Ok (ReadOutcome.Restarts, _) ->
                failwith
                    $"%s{operation}: read of fd %d{fd} restarted, though it never slept (this is a bug in the kernel library)."

        let error, system = fill true (EmulatedKernel.unix kernel)
        let kernel = EmulatedKernel.withUnix system kernel

        match error with
        | None -> SecureRandomFill.Filled (builder.MoveToImmutable ()), kernel
        | Some error -> SecureRandomFill.Failed (builder.ToImmutable (), error), kernel

    /// `minipal_get_non_cryptographically_secure_random_bytes` in System.Native's
    /// copy, called on `thread`, for `length` bytes.
    ///
    /// On Linux the shipped build has no `arc4random_buf`, so this is the secure
    /// call (whose failure it ignores) and then glibc's `lrand48` XORed over the
    /// buffer, a fresh output at every fourth byte, low byte first. The first
    /// call seeds `lrand48` with `srand48(time(NULL))`, and glibc's `time` reads
    /// `CLOCK_REALTIME_COARSE`.
    ///
    /// On Darwin it is `arc4random_buf`, from the generator the secure call
    /// draws on too.
    let systemNativeNonSecureRandomBytes
        (operation : string)
        (thread : ThreadId)
        (length : int)
        (kernel : EmulatedKernel)
        : NonSecureRandomFill * EmulatedKernel
        =
        if length < 0 then
            failwith $"%s{operation}: asked for %d{length} random bytes, a negative count"

        match kernel.ProcessRandom with
        | ProcessRandom.LibSystem generator ->
            let bytes, kernel = libSystemDraw length kernel generator
            NonSecureRandomFill.Filled bytes, kernel
        | ProcessRandom.Minipal _ ->

        let secure, kernel = systemNativeSecureRandomBytes operation thread length kernel

        let urandom, lrand48 =
            match kernel.ProcessRandom with
            | ProcessRandom.Minipal (urandom, lrand48) -> urandom, lrand48
            | ProcessRandom.LibSystem _ ->
                failwith $"%s{operation}: a Linux process's random state became Darwin's (this is a bug in PawPrint)."

        let lrand48 =
            match lrand48 with
            | Some state -> state
            | None ->
                match UnixClock.clockGettime clockRealtimeCoarse kernel.Machine with
                | Ok (Ok now) -> Lrand48.seed (UnixTimestamp.seconds now)
                | other ->
                    failwith
                        $"%s{operation}: time(NULL), which glibc answers from CLOCK_REALTIME_COARSE, failed: %A{other} (this is a bug in the kernel library)."

        let mask = Array.zeroCreate<byte> length
        let mutable state = lrand48
        let mutable output = 0L

        for i in 0 .. length - 1 do
            if i % 4 = 0 then
                let next, advanced = Lrand48.next state
                output <- next
                state <- advanced

            mask.[i] <- byte output
            output <- output >>> 8

        let kernel =
            { kernel with
                ProcessRandom = ProcessRandom.Minipal (urandom, Some state)
            }

        match secure with
        | SecureRandomFill.Filled bytes ->
            NonSecureRandomFill.Filled (ImmutableArray.CreateRange (Seq.map2 (^^^) bytes mask)), kernel
        | SecureRandomFill.Failed (written, error) ->
            NonSecureRandomFill.OverExisting (written, ImmutableArray.CreateRange mask, error), kernel
