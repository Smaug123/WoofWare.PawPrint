namespace WoofWare.PosixKernel

/// A filesystem timestamp: `struct timespec`, whole seconds since the Unix
/// epoch plus a nanosecond part in `[0, 1e9)`.
///
/// Two fields rather than one nanosecond count. `st_atim.tv_sec` is a 64-bit
/// *second* count, so folding the pair into nanoseconds would cap the
/// representable range at 1677–2262 — and `File.SetLastWriteTime` will happily
/// be handed a `DateTime` outside it, which would then have to overflow or be
/// clamped. Neither is a thing a filesystem does.
///
/// Negative seconds are permitted: a pre-1970 mtime is ordinary, and `tar`
/// archives are full of them. A negative *nanosecond* part is not, matching the
/// kernel's own normalisation, so the pair always compares in the obvious
/// lexicographic order.
///
/// There is no `assertValid` counterpart to `FileName`'s: this type's
/// `Unchecked.defaultof` is `(0L, 0)`, the Unix epoch, which is a perfectly
/// legal timestamp. There is no forged value to catch.
[<Struct>]
type UnixTimestamp =
    private
    | UnixTimestamp of seconds : int64 * nanoseconds : int

    override this.ToString () : string =
        match this with
        | UnixTimestamp (seconds, nanoseconds) ->

        // A timespec is seconds *plus* nanoseconds, and the nanosecond part is
        // never negative — so a pre-epoch instant is not the two fields printed
        // adjacently with a minus in front. `(-1, 500_000_000)` is half a second
        // *before* the epoch; writing it "-1.500000000" would name a moment a
        // second earlier than the one it holds.
        if seconds >= 0L || nanoseconds = 0 then
            $"%d{seconds}.%09d{nanoseconds}"
        else
            // Carry the fraction the other way: s + n/1e9 = (s+1) - (1e9-n)/1e9.
            let whole = seconds + 1L
            let fraction = 1_000_000_000 - nanoseconds

            // `whole` of zero has lost the sign, since "0" and "-0" are the same
            // integer but only one of them is the right side of the epoch.
            if whole = 0L then
                $"-0.%09d{fraction}"
            else
                $"%d{whole}.%09d{fraction}"

[<RequireQualifiedAccess>]
module UnixTimestamp =
    let private nanosecondsPerSecond : int = 1_000_000_000

    let seconds (timestamp : UnixTimestamp) : int64 =
        match timestamp with
        | UnixTimestamp (seconds, _) -> seconds

    let nanoseconds (timestamp : UnixTimestamp) : int =
        match timestamp with
        | UnixTimestamp (_, nanoseconds) -> nanoseconds

    /// A timestamp, or `None` if the nanosecond part is not in `[0, 1e9)`.
    /// Deliberately not normalising an out-of-range part by carrying into the
    /// seconds: a caller who computed 1.5e9 nanoseconds has a unit bug, and
    /// silently absorbing it would hide it.
    let create (seconds : int64) (nanoseconds : int) : UnixTimestamp option =
        if nanoseconds < 0 || nanoseconds >= nanosecondsPerSecond then
            None
        else
            Some (UnixTimestamp (seconds, nanoseconds))

    let createOrFail (context : string) (seconds : int64) (nanoseconds : int) : UnixTimestamp =
        match create seconds nanoseconds with
        | Some timestamp -> timestamp
        | None ->
            failwith
                $"%s{context}: %d{nanoseconds} is not a nanosecond part; it must lie in [0, %d{nanosecondsPerSecond}). A whole-second count belongs in the seconds field."

    let ofSeconds (seconds : int64) : UnixTimestamp = UnixTimestamp (seconds, 0)

    /// A timestamp from a count of milliseconds since the Unix epoch, which is
    /// how the emulated kernel holds its wall clock.
    ///
    /// Floor division, so that a negative millisecond count keeps the
    /// nanosecond part non-negative rather than producing a `timespec` no
    /// kernel would write: -1 ms is (-1 s, 999 000 000 ns), not (0 s, -1e6 ns).
    let ofMillisecondsSinceEpoch (milliseconds : int64) : UnixTimestamp =
        // Derived from the truncating quotient and remainder rather than by
        // biasing the dividend. `(milliseconds - 999L) / 1000L` is the obvious
        // way to floor a negative, and it silently overflows for the bottom 999
        // values of `int64`: it does not throw, it hands back a *positive*
        // second count and a nanosecond part outside `[0, 1e9)` — a value that
        // breaks the very invariant `create` exists to enforce, while bypassing
        // it. Neither `/` nor `%` can overflow for any input here.
        let quotient = milliseconds / 1000L
        let remainder = milliseconds % 1000L

        if remainder >= 0L then
            UnixTimestamp (quotient, int remainder * 1_000_000)
        else
            // The truncating quotient rounded towards zero, so it names a
            // second later than the instant; the remainder is negative by
            // exactly the difference.
            UnixTimestamp (quotient - 1L, int (remainder + 1000L) * 1_000_000)

    /// The Unix epoch itself, which is also what a kernel booted at the default
    /// `WallClockEpochMs` of 0 believes the time to be.
    let epoch : UnixTimestamp = UnixTimestamp (0L, 0)
