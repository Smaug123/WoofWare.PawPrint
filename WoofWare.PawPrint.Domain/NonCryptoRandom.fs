namespace WoofWare.PawPrint

/// Deterministic non-cryptographic PRNG that backs
/// `SystemNative_GetNonCryptographicallySecureRandomBytes` and any future
/// in-runtime consumer that wants a reproducible random stream. This is the
/// `splitmix64` step from Vigna's reference at
/// <http://prng.di.unimi.it/splitmix64.c>: stateful, full-period over the
/// 2^64 64-bit states, and famously fine for *seeding* other PRNGs (which
/// is exactly what the BCL does here — it feeds the buffer into
/// `Random.XoshiroImpl`, `Marvin.DefaultSeed`, `HashCode`'s seed, etc.).
///
/// Quality is intentionally non-cryptographic; that matches the entry-point
/// name. The state being non-zero is preserved across the step (splitmix64
/// has no fixed points other than `0 -> 0x9E3779B97F4A7C15`-style transient
/// behaviour from a zero start, which is why `initialState` seeds with the
/// golden-ratio constant rather than zero).
///
/// Why hand-implemented: `System.Random`'s algorithm is implementation-defined
/// and has changed across .NET versions, so "same seed yields the same
/// sequence" isn't actually guaranteed by the framework. Determinism across
/// host versions is a hard requirement for WoofWare.PawPrint, so we pin a
/// stable algorithm with a fully-specified output here. Distinct callers
/// (kernel-backed `Random`, schedule fuzzer, etc.) should each hold their
/// own `uint64` state so that one consumer's draws don't perturb another's
/// stream.
[<RequireQualifiedAccess>]
module NonCryptoRandom =
    /// `floor(2^64 / phi)`, the same constant the reference splitmix64 uses
    /// as its weyl increment. Picked as the default initial state so a
    /// fresh interpreter doesn't start from zero (which would still produce
    /// a non-zero output after one step, but is the documented degenerate
    /// input for splitmix64-style mixers).
    let initialState : uint64 = 0x9E3779B97F4A7C15UL

    /// Advance the splitmix64 state by one step and return the 64-bit
    /// output drawn from the new state. Reference implementation:
    /// <http://prng.di.unimi.it/splitmix64.c>. Pure: same input state
    /// produces the same `(output, newState)` pair on every call.
    let step (state : uint64) : uint64 * uint64 =
        let newState = state + 0x9E3779B97F4A7C15UL
        let mutable z = newState
        z <- (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
        z <- (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z <- z ^^^ (z >>> 31)
        z, newState

    /// Draw `count` pseudo-random bytes from the splitmix64 state. Returns
    /// the bytes in little-endian unpack order from each 64-bit step, plus
    /// the new state. `count` must be non-negative; a negative count is a
    /// caller bug and the function will fail loudly.
    let drawBytes (count : int) (state : uint64) : byte[] * uint64 =
        if count < 0 then
            failwith $"NonCryptoRandom.drawBytes: byte count %d{count} is negative"

        let buffer = Array.zeroCreate<byte> count
        let mutable state = state
        let mutable i = 0

        while i < count do
            let output, newState = step state
            state <- newState
            let remaining = count - i
            let chunk = if remaining > 8 then 8 else remaining

            for j = 0 to chunk - 1 do
                buffer.[i + j] <- byte (output >>> (8 * j))

            i <- i + chunk

        buffer, state

    /// Draw a uniform double in [0.0, 1.0). Uses the top 53 bits of the next
    /// 64-bit output — the maximum exactly representable in an IEEE-754
    /// binary64 mantissa. Discarding the low 11 bits keeps the distribution
    /// unbiased: every representable value in [0, 1) with the canonical
    /// 53-bit-mantissa spacing is equally likely.
    ///
    /// Pure: same input state produces the same `(value, newState)`.
    let nextDouble (state : uint64) : double * uint64 =
        let output, newState = step state
        // 2^53 = 9007199254740992. We multiply by the reciprocal rather than
        // dividing because the reciprocal is exactly representable (it's a
        // power of two) and the multiplication is therefore exact too — no
        // rounding is introduced by the scale.
        let value = double (output >>> 11) * (1.0 / 9007199254740992.0)
        value, newState

    /// Draw a uniform `int` in `[0, bound)`. Requires `bound > 0`; fails loudly
    /// otherwise — a non-positive bound is a caller bug, and silently returning
    /// `0` would hide the upstream mistake.
    ///
    /// Uses unbiased rejection sampling: the small number of `uint64` values
    /// at the top of the range that would map unevenly under `n % bound` are
    /// rejected so every output in `[0, bound)` is equally likely. Rejection
    /// probability is at most `bound / 2^64`, so for any int-sized bound
    /// (≤ 2^31 - 1) the expected number of `step` calls per draw differs
    /// from 1 by less than 2^-33; in practice the loop body essentially
    /// never runs twice.
    ///
    /// Pure: same input state produces the same `(value, newState)`.
    let nextInt32Below (bound : int) (state : uint64) : int * uint64 =
        if bound <= 0 then
            failwith $"NonCryptoRandom.nextInt32Below: bound %d{bound} must be positive"

        let boundU = uint64 bound
        // `threshold` is `2^64 mod bound`: the number of `uint64` values in the
        // partial bucket at the top of the range that must be rejected to keep
        // `n % bound` unbiased. Computed without overflow as
        // `(UInt64.MaxValue mod bound + 1) mod bound`, since
        // `2^64 mod bound = ((2^64 - 1) mod bound + 1) mod bound`.
        let threshold = (System.UInt64.MaxValue % boundU + 1UL) % boundU
        // Acceptable region is `[0, 2^64 - threshold)`. Equivalently,
        // `n` is acceptable iff `n <= UInt64.MaxValue - threshold`. When
        // `threshold = 0` (bound divides 2^64, i.e. bound is a power of two),
        // this is `n <= UInt64.MaxValue`, which trivially accepts everything.
        let acceptUpTo = System.UInt64.MaxValue - threshold

        let rec loop state =
            let n, newState = step state

            if n <= acceptUpTo then
                int (n % boundU), newState
            else
                loop newState

        loop state
