namespace WoofWare.PosixKernel

/// <summary>
/// A filesystem timestamp, measured in whole seconds since the Unix epoch
/// plus a nanosecond part in <c>[0, 1e9)</c>.
/// </summary>
/// <remarks>
/// This is a model of <c>struct timespec</c>.
///
/// Negative seconds are permitted; negative nanoseconds are not.
/// </remarks>
[<Struct>]
type UnixTimestamp =
    private
    | UnixTimestamp of seconds : int64 * nanoseconds : int

    /// <summary>
    /// Format this timestamp as a number of seconds since the epoch.
    /// </summary>
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

    /// <summary>
    /// The raw number of seconds in this timestamp (i.e. the timestamp rounded down to the latest second).
    /// </summary>
    let seconds (timestamp : UnixTimestamp) : int64 =
        match timestamp with
        | UnixTimestamp (seconds, _) -> seconds

    /// <summary>
    /// The raw number of nanoseconds in this timestamp (i.e. the timestamp's fractional non-second part).
    /// </summary>
    let nanoseconds (timestamp : UnixTimestamp) : int =
        match timestamp with
        | UnixTimestamp (_, nanoseconds) -> nanoseconds

    /// <summary>
    /// A timestamp formed from "seconds since the epoch" plus a fractional nanosecond part.
    /// </summary>
    /// <returns>
    /// <c>None</c> if the nanosecond part is not in <c>[0, 1e9)</c>.
    /// </returns>
    let create (seconds : int64) (nanoseconds : int) : UnixTimestamp option =
        if nanoseconds < 0 || nanoseconds >= nanosecondsPerSecond then
            None
        else
            Some (UnixTimestamp (seconds, nanoseconds))

    /// <summary>
    /// A timestamp formed from "seconds since the epoch" plus a fractional nanosecond part.
    /// </summary>
    /// <remarks>
    /// This is <c>create</c> except it throws (naming <c>context</c>) instead of returning <c>None</c>.
    /// </remarks>
    let createOrFail (context : string) (seconds : int64) (nanoseconds : int) : UnixTimestamp =
        match create seconds nanoseconds with
        | Some timestamp -> timestamp
        | None ->
            failwith
                $"%s{context}: %d{nanoseconds} is not a nanosecond part; it must lie in [0, %d{nanosecondsPerSecond}). A whole-second count belongs in the seconds field."

    /// <summary>
    /// A timestamp formed from "integer number of seconds since the epoch".
    /// </summary>
    let ofSeconds (seconds : int64) : UnixTimestamp = UnixTimestamp (seconds, 0)

    /// <summary>
    /// A timestamp from an integer count of milliseconds since the Unix epoch.
    /// </summary>
    /// <remarks>
    /// This is how the emulated kernel holds its wall clock.
    /// </remarks>
    let ofMillisecondsSinceEpoch (milliseconds : int64) : UnixTimestamp =
        // Floor division, so that a negative millisecond count keeps the
        // nanosecond part non-negative rather than producing a `timespec` no
        // kernel would write: -1 ms is (-1 s, 999 000 000 ns), not (0 s, -1e6 ns).
        //
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

    /// <summary>
    /// The Unix epoch.
    /// </summary>
    /// <remarks>
    /// This is what what a kernel booted at the default
    /// <c>WallClockEpochMs</c> of 0 believes the time to be.
    /// </remarks>
    let epoch : UnixTimestamp = UnixTimestamp (0L, 0)
