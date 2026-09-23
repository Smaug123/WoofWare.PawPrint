namespace WoofWare.PosixKernel

open System.Collections.Immutable
open System.Runtime.InteropServices

/// The kernel's entropy pool: the one source of random bytes a simulated
/// kernel hands out, whichever syscall asks for them.
///
/// Deterministic, as everything in this library is. The same seed yields the
/// same bytes on every machine, so a replay hands a simulated process exactly
/// the bytes the recorded run was handed. Nothing drawn from it is secret from
/// anyone who knows the seed.
///
/// The pool is initialised from the moment it exists, so a draw never has to
/// wait for entropy to accumulate.
[<Struct>]
type EntropyPool = private | EntropyPool of state : uint64

[<RequireQualifiedAccess>]
module EntropyPool =
    /// A pool whose stream starts from `seed`. Every seed is usable, zero
    /// included.
    ///
    /// The stream is splitmix64 (Vigna, <http://prng.di.unimi.it/splitmix64.c>)
    /// with `seed` as its state, so a client that must reproduce the bytes
    /// outside this library can.
    let ofSeed (seed : uint64) : EntropyPool = EntropyPool seed

    // One step of splitmix64: advance the state by the golden-ratio increment,
    // then mix it into the output.
    let private step (state : uint64) : uint64 * uint64 =
        let state = state + 0x9E3779B97F4A7C15UL
        let z = state
        let z = (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
        let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z ^^^ (z >>> 31), state

    /// The next `count` bytes from the pool, and the pool after them.
    ///
    /// Each 64-bit output of the generator supplies eight bytes, least
    /// significant first. A draw whose count is not a multiple of eight
    /// discards the rest of its last output, so the next draw starts on a fresh
    /// one.
    ///
    /// `count` must not be negative.
    let draw (count : int) (pool : EntropyPool) : ImmutableArray<byte> * EntropyPool =
        if count < 0 then
            failwith $"EntropyPool.draw: a count of %d{count} bytes is negative"

        let (EntropyPool state) = pool
        let bytes = Array.zeroCreate<byte> count
        let mutable state = state
        let mutable written = 0

        while written < count do
            let output, next = step state
            state <- next
            let chunk = min 8 (count - written)

            for i in 0 .. chunk - 1 do
                bytes.[written + i] <- byte (output >>> (8 * i))

            written <- written + chunk

        // Nothing else holds `bytes`, so handing it over without a copy cannot
        // let the answer change under its holder.
        ImmutableCollectionsMarshal.AsImmutableArray bytes, EntropyPool state
