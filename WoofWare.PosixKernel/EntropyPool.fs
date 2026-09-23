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

/// Bytes taken from an `EntropyPool`: which ones, and how many.
///
/// A description rather than the bytes themselves, so that taking a large
/// count costs nothing until the bytes are wanted, and a caller can then fetch
/// them a range at a time into storage of its own. Every range of the same
/// draw is the same, however often it is fetched.
[<Struct>]
type EntropyDraw = private | EntropyDraw of start : uint64 * count : int

// splitmix64 (Vigna, <http://prng.di.unimi.it/splitmix64.c>). Its state is a
// Weyl sequence, advancing by `Gamma` per output, so the state after `n`
// outputs is `start + n * Gamma` and any output can be computed directly.
[<RequireQualifiedAccess>]
module internal SplitMix64 =
    [<Literal>]
    let Gamma : uint64 = 0x9E3779B97F4A7C15UL

    /// The output whose state is `state`.
    let mix (state : uint64) : uint64 =
        let z = state
        let z = (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
        let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z ^^^ (z >>> 31)

    /// How many outputs a draw of `count` bytes consumes: one per eight bytes,
    /// the last one possibly partly.
    let outputsFor (count : int) : uint64 = (uint64 count + 7UL) / 8UL

[<RequireQualifiedAccess>]
module EntropyDraw =
    /// How many bytes were taken.
    let count (draw : EntropyDraw) : int =
        let (EntropyDraw (_, count)) = draw
        count

    /// The `length` bytes of the draw that start `offset` bytes into it.
    /// Both must be non-negative, and the range must lie within the draw.
    let range (offset : int) (length : int) (draw : EntropyDraw) : ImmutableArray<byte> =
        let (EntropyDraw (start, count)) = draw

        if offset < 0 || length < 0 || int64 offset + int64 length > int64 count then
            failwith
                $"EntropyDraw.range: %d{length} bytes at offset %d{offset} do not lie within a draw of %d{count} bytes"

        let bytes = Array.zeroCreate<byte> length
        // Output k of the draw has state `start + (k + 1) * Gamma`, and supplies
        // bytes 8k to 8k + 7, least significant first. Mixed once per output
        // rather than once per byte.
        let mutable outputIndex = -1
        let mutable output = 0UL

        for i in 0 .. length - 1 do
            let index = offset + i

            if index / 8 <> outputIndex then
                outputIndex <- index / 8
                output <- SplitMix64.mix (start + (uint64 outputIndex + 1UL) * SplitMix64.Gamma)

            bytes.[i] <- byte (output >>> (8 * (index % 8)))

        // Nothing else holds `bytes`, so handing it over without a copy cannot
        // let the answer change under its holder.
        ImmutableCollectionsMarshal.AsImmutableArray bytes

    /// Every byte of the draw.
    let bytes (draw : EntropyDraw) : ImmutableArray<byte> = range 0 (count draw) draw

[<RequireQualifiedAccess>]
module EntropyPool =
    /// A pool whose stream starts from `seed`. Every seed is usable, zero
    /// included.
    ///
    /// The stream is splitmix64 (Vigna, <http://prng.di.unimi.it/splitmix64.c>)
    /// with `seed` as its state, so a client that must reproduce the bytes
    /// outside this library can.
    let ofSeed (seed : uint64) : EntropyPool = EntropyPool seed

    /// Take the next `count` bytes from the pool: which ones they are, and the
    /// pool after them. Costs the same whatever the count, because no byte is
    /// produced until `EntropyDraw` is asked for it.
    ///
    /// Each 64-bit output of the generator supplies eight bytes, least
    /// significant first. A draw whose count is not a multiple of eight
    /// discards the rest of its last output, so the next draw starts on a fresh
    /// one.
    ///
    /// `count` must not be negative.
    let take (count : int) (pool : EntropyPool) : EntropyDraw * EntropyPool =
        if count < 0 then
            failwith $"EntropyPool.take: a count of %d{count} bytes is negative"

        let (EntropyPool state) = pool
        EntropyDraw (state, count), EntropyPool (state + SplitMix64.outputsFor count * SplitMix64.Gamma)

    /// `take`, with every byte of the draw produced at once.
    let draw (count : int) (pool : EntropyPool) : ImmutableArray<byte> * EntropyPool =
        let taken, pool = take count pool
        EntropyDraw.bytes taken, pool
