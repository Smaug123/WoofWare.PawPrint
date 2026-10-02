namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// CoreCLR's `minipal_get_cryptographically_secure_random_bytes`: where the
/// runtime's native code gets random bytes it calls secure, asked of the
/// simulated kernel through its syscalls. System.Native's
/// `SystemNative_GetCryptographicallySecureRandomBytes` (behind `Guid.NewGuid`)
/// is a thin wrapper over it, and the runtime's own `minipal_guid_v4_create`
/// (behind a dynamic module's version ID) calls it too.
///
/// It never fails: the simulated kernel's pool is initialised from boot, and
/// the request names storage the caller owns.
[<RequireQualifiedAccess>]
module MinipalRandom =

    /// One kernel call towards a request with `remaining` bytes still to fill:
    /// the bytes it moved, and the system after it.
    let private once
        (operation : string)
        (remaining : int)
        (system : UnixSystem<ThreadId, NativeSignalHandler>)
        : EntropyDraw * UnixSystem<ThreadId, NativeSignalHandler>
        =
        // On Linux, minipal `open`s `/dev/urandom` the first time it is asked,
        // keeps the descriptor in a static, and `read`s it until the request is
        // filled. The simulated kernel has no device nodes, so this asks
        // `getrandom(2)` instead, with `GRND_INSECURE`, which like a
        // `/dev/urandom` read never waits for the pool to be initialised. The
        // bytes and the pool's movement are the read's.
        //
        // The descriptor is not modelled, and a guest can see that it is missing.
        // Measured (docs/plans/2026-08-23-posix-kernel-extraction/
        // minipal-random-descriptors.cs): System.Native's copy of minipal opens
        // its descriptor on the first call to either of its random entry points,
        // since on Linux the non-secure one reads `/dev/urandom` too, so the
        // guest's next `open` lands one higher than it does here. CoreCLR's own
        // copy opens another before `Main`.
        //
        // On Darwin, minipal calls `CCRandomGenerateBytes`, a userspace generator
        // the kernel seeds. This passes the kernel's bytes straight through,
        // `getentropy(2)`'s 256 at a time, rather than modelling a second
        // generator: a process sees only the bytes. No descriptor is involved on
        // Darwin (measured by the same probe).
        match SimulatedUnixPlatform.flavour (UnixMachineState.platform system.Machine) with
        | SimulatedUnixFlavour.Linux ->
            match UnixEntropy.getRandom UserBuffer.Mapped (uint64 remaining) GetRandomFlags.Insecure system with
            | Ok (GetRandomAnswer.Completed draw, system) -> draw, system
            | Ok (GetRandomAnswer.Failed error, _) ->
                failwith
                    $"%s{operation}: getrandom of %d{remaining} bytes into storage PawPrint owns answered %O{error} (this is a bug in the kernel library)."
            | Error refusal ->
                failwith
                    $"%s{operation}: getrandom of %d{remaining} bytes was refused: %s{GetRandomRefusal.describe refusal}"
        | SimulatedUnixFlavour.Darwin ->
            let length = min (uint64 remaining) UnixEntropy.getEntropyMaxLength

            match UnixEntropy.getEntropy UserBuffer.Mapped length system with
            | Ok (GetEntropyAnswer.Completed draw, system) -> draw, system
            | Ok (GetEntropyAnswer.Failed error, _) ->
                failwith
                    $"%s{operation}: getentropy of %d{length} bytes into storage PawPrint owns answered %O{error} (this is a bug in the kernel library)."
            | Error refusal ->
                failwith
                    $"%s{operation}: getentropy of %d{length} bytes was refused: %s{GetEntropyRefusal.describe refusal}"

    /// `length` random bytes from the kernel, and the kernel after handing them
    /// out. `operation` names the entry point that asked, for diagnostics.
    ///
    /// `length` must not be negative. A length of zero makes no kernel call.
    let secureRandomBytes
        (operation : string)
        (length : int)
        (kernel : EmulatedKernel)
        : ImmutableArray<byte> * EmulatedKernel
        =
        if length < 0 then
            failwith $"%s{operation}: asked for %d{length} random bytes, a negative count"

        let builder = ImmutableArray.CreateBuilder<byte> length

        let rec fill (system : UnixSystem<ThreadId, NativeSignalHandler>) : UnixSystem<ThreadId, NativeSignalHandler> =
            let remaining = length - builder.Count

            if remaining = 0 then
                system
            else

            let draw, system = once operation remaining system
            let moved = EntropyDraw.count draw

            if moved <= 0 || moved > remaining then
                failwith
                    $"%s{operation}: a kernel call asked for %d{remaining} random bytes moved %d{moved}; it must move at least one and at most that many (this is a bug in the kernel library)."

            builder.AddRange (EntropyDraw.bytes draw)
            fill system

        let system = fill (EmulatedKernel.unix kernel)
        builder.MoveToImmutable (), EmulatedKernel.withUnix system kernel
