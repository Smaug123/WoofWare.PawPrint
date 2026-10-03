namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Buffers.Binary

/// glibc's `drand48` family's state, which `srand48` sets and `lrand48`
/// advances: the 48-bit `X` of the linear congruential generator.
[<Struct>]
type Lrand48 = private | Lrand48 of x : uint64

[<RequireQualifiedAccess>]
module Lrand48 =
    // glibc's `__drand48_iterate`: X ← (a·X + c) mod 2^48.
    [<Literal>]
    let private multiplier : uint64 = 0x5DEECE66DUL

    [<Literal>]
    let private increment : uint64 = 0xBUL

    [<Literal>]
    let private mask : uint64 = 0xFFFF_FFFF_FFFFUL

    /// `srand48(seed)` under glibc on a 64-bit target: the seed's low 32 bits
    /// become X's high 32, and X's low 16 are `0x330E`. The high half of a
    /// 64-bit `long` is discarded, so 2^32 seeds as 0 does and -1 as
    /// 0xFFFFFFFF does.
    let seed (seed : int64) : Lrand48 =
        Lrand48 (((uint64 seed &&& 0xFFFF_FFFFUL) <<< 16) ||| 0x330EUL)

    /// `lrand48()`: advance, and answer X's top 31 bits as a non-negative
    /// `long`.
    let next (state : Lrand48) : int64 * Lrand48 =
        let (Lrand48 x) = state
        let x = (multiplier * x + increment) &&& mask
        int64 (x >>> 17), Lrand48 x

/// The descriptor System.Native's copy of minipal keeps for `/dev/urandom` on
/// Linux: its static `rand_des`.
///
/// Only the number is kept, as the static keeps it. What that number names is
/// the kernel's descriptor table's business: a guest can close it, or `dup2`
/// something else over it, and minipal goes on reading the number.
[<RequireQualifiedAccess>]
type MinipalUrandom =
    /// Nothing has asked for secure bytes yet, or every `open` so far failed.
    | Unopened
    /// The descriptor the first successful `open` returned.
    | Open of fd : int

/// What a Darwin process's libSystem keeps for random bytes: one generator in
/// the process, which `CCRandomGenerateBytes` and `arc4random_buf` share, and
/// which `libSystem_initializer` seeds with one `getentropy(32)` before `main`.
///
/// The bytes are this generator's own, splitmix64 from a state folded from the
/// seed: corecrypto's CTR-DRBG output cannot be reproduced from the seed
/// without its source, and no process can tell one stream of random bytes
/// from another.
///
/// **Seeded once.** A real one reseeds with another `getentropy(32)` when a
/// draw comes about five seconds after its last reseed: measured between 4.9
/// and 5.1 s after the reseed rather than the draw, for both entry points, on
/// Darwin 27.0 arm64, and Germany's BSI describes corecrypto's process
/// generator as reseeding when its last reseed is "more than 5 seconds ago".
/// Apple's published Libc and libplatform do not state the threshold, so this
/// generator never reseeds, and a long-running process here leaves the
/// kernel's entropy pool where a real one would have moved it.
[<Struct>]
type LibSystemRandom = private | LibSystemRandom of state : uint64

[<RequireQualifiedAccess>]
module LibSystemRandom =
    /// The generator `libSystem_initializer` seeds from the 32 bytes
    /// `getentropy` answered.
    let ofSeed (seed : ImmutableArray<byte>) : LibSystemRandom =
        if seed.Length <> 32 then
            failwith $"LibSystemRandom.ofSeed: libSystem seeds from 32 bytes, not %d{seed.Length}"

        let state =
            (0UL, [ 0..3 ])
            ||> List.fold (fun state word ->
                let value =
                    BinaryPrimitives.ReadUInt64LittleEndian (seed.AsSpan().Slice (8 * word, 8))

                fst (NonCryptoRandom.step (state ^^^ value))
            )

        LibSystemRandom state

    /// `count` bytes from the generator, and the generator after them.
    let draw (count : int) (generator : LibSystemRandom) : ImmutableArray<byte> * LibSystemRandom =
        let (LibSystemRandom state) = generator
        let bytes, state = NonCryptoRandom.drawBytes count state
        ImmutableArray.CreateRange bytes, LibSystemRandom state

/// The random-number state a process's C runtime keeps, which differs by
/// flavour as the shim's `minipal/random.c` does.
[<RequireQualifiedAccess>]
type ProcessRandom =
    /// Linux. System.Native's copy of minipal reads secure bytes from a
    /// `/dev/urandom` descriptor it opens on first use, and its non-secure
    /// bytes are those, XORed with glibc's `lrand48` after one
    /// `srand48(time(NULL))`: the shipped Linux build has no `arc4random_buf`.
    /// `Lrand48` is `None` until the first non-secure call seeds it.
    | Minipal of urandom : MinipalUrandom * lrand48 : Lrand48 option
    /// Darwin: both entry points draw from libSystem's generator.
    | LibSystem of generator : LibSystemRandom
