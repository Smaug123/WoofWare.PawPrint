namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The CoreCLR shim's three clock entry points, answered from the emulated
/// kernel's `clock_gettime`, and the 100 ns tick PawPrint's virtual clock counts
/// in because the first of them does.
///
/// Each entry point reads the clock the real shim reads on the simulated flavour:
///
/// - `SystemNative_GetSystemTimeAsTicks` (behind `DateTime.UtcNow`):
///   `CLOCK_REALTIME` on both, in 100 ns ticks since the Unix epoch
///   (`pal_datetime.c`).
/// - `SystemNative_GetTimestamp` (behind `Stopwatch`): `minipal_hires_ticks`,
///   which is `CLOCK_MONOTONIC` on Linux and `CLOCK_UPTIME_RAW` on Darwin, in
///   nanoseconds (`minipal/time.c`).
/// - `SystemNative_GetLowResolutionTimestamp` (behind `Environment.TickCount64`):
///   `minipal_lowres_ticks`, which is `CLOCK_MONOTONIC_COARSE` on Linux and
///   `CLOCK_UPTIME_RAW` on Darwin, in whole milliseconds.
///
/// Every reading is exact in 100 ns ticks because PawPrint only ever advances the
/// kernel's clock by whole ticks; see `EmulatedKernel.VirtualClockTicks`.
[<RequireQualifiedAccess>]
module ClockPal =

    /// Nanoseconds per 100 ns tick, the unit of `DateTime` and of PawPrint's
    /// virtual clock.
    [<Literal>]
    let nanosecondsPerTick : int64 = 100L

    /// 100 ns ticks per millisecond. Converts the quantities that arrive in
    /// milliseconds and meet the virtual clock: `KernelConfig.WallClockEpochMs`,
    /// and a guest's millisecond timeout on its way to a deadline.
    [<Literal>]
    let ticksPerMillisecond : int64 = 10_000L

    [<Literal>]
    let private ticksPerSecond : int64 = 10_000_000L

    /// Largest legal `KernelConfig.WallClockEpochMs`: 9999-12-31T23:59:59.999Z
    /// as milliseconds since the Unix epoch, which is the last whole millisecond
    /// `System.DateTime` can represent
    /// (`(DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks) / ticksPerMillisecond`).
    ///
    /// CoreLib builds `DateTime.UtcNow` with `DateTime`'s *unvalidated* private
    /// ctor (`new DateTime(((ulong)(GetSystemTimeAsTicks() + UnixEpochTicks)) | KindUtc)`
    /// in DateTime.Unix.cs), so a reading beyond this range would reach the guest
    /// as a silently corrupt `DateTime` rather than an exception.
    [<Literal>]
    let maxWallClockEpochMs : int64 = 253402300799999L

    /// Largest reading `systemTimeAsTicks` will hand the guest, in 100 ns ticks
    /// since the Unix epoch: `DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks`.
    /// `DateTime` cannot name an instant beyond it.
    ///
    /// Deliberately *not* `maxWallClockEpochMs * ticksPerMillisecond`, which is
    /// 9,999 ticks smaller: that is the last whole millisecond, the right ceiling
    /// for a knob denominated in milliseconds, while the clock resolves every tick
    /// up to the end of `DateTime`'s range.
    [<Literal>]
    let maxWallClockTicks : int64 = 2534023007999999999L

    // The clock ids are the flavour's own `<time.h>` numbering, which the kernel
    // decodes per flavour.
    let private clockRealtime : int = 0
    let private linuxClockMonotonic : int = 1
    let private linuxClockMonotonicCoarse : int = 6
    let private darwinClockUptimeRaw : int = 8

    /// Read `clockId`, which the real shim reads for `entryPoint` and so must be a
    /// clock the kernel answers: the shim asserts rather than handle a failure.
    let private read (entryPoint : string) (clockId : int) (machine : UnixMachineState) : UnixTimestamp =
        match UnixClock.clockGettime clockId machine with
        | Ok (Ok reading) -> reading
        | Ok (Error error) ->
            failwith
                $"%s{entryPoint}: clock_gettime(%d{clockId}) failed with %O{error} on %O{machine.UnixPlatform}. The real shim reads this clock unconditionally; PawPrint has chosen a clock id the simulated flavour does not have."
        | Error refusal ->
            failwith
                $"%s{entryPoint}: the kernel will not answer clock_gettime(%d{clockId}): %s{ClockGettimeRefusal.describe refusal} The real shim reads this clock, so PawPrint cannot answer without it."

    /// What `SystemNative_GetSystemTimeAsTicks` returns: the realtime clock in
    /// 100 ns ticks since the Unix epoch, and hence (once CoreLib has added
    /// `UnixEpochTicks` and stamped `DateTimeKind.Utc`) what `DateTime.UtcNow`
    /// reports. Refuses a reading `DateTime` cannot represent.
    ///
    /// `DateTime.UtcNow` is therefore only weakly monotonic: two reads between one
    /// clock advance and the next are equal, so it is not a source of unique
    /// values. Real `clock_gettime(CLOCK_REALTIME)` makes no uniqueness guarantee
    /// either.
    let systemTimeAsTicks (machine : UnixMachineState) : int64 =
        let reading = read "SystemNative_GetSystemTimeAsTicks" clockRealtime machine
        let seconds = UnixTimestamp.seconds reading

        // The kernel refuses a boot instant before the epoch, so this is a
        // machine assembled without `withBootTime`.
        if seconds < 0L then
            failwith
                $"SystemNative_GetSystemTimeAsTicks: the realtime clock reads %O{reading}, before the Unix epoch, which PawPrint does not model a simulated process observing."

        // Tested in seconds before the multiply, which would otherwise overflow for
        // a realtime clock far beyond `DateTime`'s range.
        if seconds > maxWallClockTicks / ticksPerSecond then
            failwith
                $"SystemNative_GetSystemTimeAsTicks: the realtime clock reads %O{reading} (seconds since the Unix epoch), past the %d{maxWallClockTicks} ticks that System.DateTime can represent; lower KernelConfig.WallClockEpochMs"

        // As `pal_datetime.c` computes it: whole seconds scaled, plus the
        // nanosecond part truncated to ticks.
        let ticks =
            seconds * ticksPerSecond
            + int64 (UnixTimestamp.nanoseconds reading) / nanosecondsPerTick

        if ticks > maxWallClockTicks then
            failwith
                $"SystemNative_GetSystemTimeAsTicks: the simulated wall clock has reached %d{ticks} ticks since the Unix epoch, past the %d{maxWallClockTicks} that System.DateTime can represent; lower KernelConfig.WallClockEpochMs"

        ticks

    let private nanosecondsPerSecond : int64 = 1_000_000_000L
    let private nanosecondsPerMillisecond : int64 = 1_000_000L

    /// What `SystemNative_GetTimestamp` returns: monotonic time since the
    /// simulated process booted, in nanoseconds, and hence what
    /// `Stopwatch.GetTimestamp()` reports on a Unix CoreLib, whose
    /// `Stopwatch.Frequency` is a fixed 1e9.
    ///
    /// Not offset by the boot instant: the monotonic clock counts from boot, and
    /// CoreLib only ever subtracts two readings of it.
    let monotonicTimestampNanos (machine : UnixMachineState) : int64 =
        let clockId =
            match SimulatedUnixPlatform.flavour machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> linuxClockMonotonic
            | SimulatedUnixFlavour.Darwin -> darwinClockUptimeRaw

        let reading = read "SystemNative_GetTimestamp" clockId machine

        // Both clocks report to the nanosecond, so this is the machine's uptime
        // exactly, which is an int64 of nanoseconds and so cannot overflow here.
        UnixTimestamp.seconds reading * nanosecondsPerSecond
        + int64 (UnixTimestamp.nanoseconds reading)

    /// What `SystemNative_GetLowResolutionTimestamp` returns: the same monotonic
    /// time in whole milliseconds, and hence `Environment.TickCount64`.
    ///
    /// Always exactly `monotonicTimestampNanos` truncated to milliseconds, as
    /// upstream: a guest comparing `Environment.TickCount64` against a
    /// `Stopwatch` must not see them disagree about how much time has passed.
    let lowResolutionTimestampMs (machine : UnixMachineState) : int64 =
        let entryPoint = "SystemNative_GetLowResolutionTimestamp"

        // The arithmetic follows `minipal_lowres_ticks` on each flavour: Linux
        // converts the timespec field by field, Darwin divides
        // `clock_gettime_nsec_np`'s nanoseconds.
        match SimulatedUnixPlatform.flavour machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux ->
            let reading = read entryPoint linuxClockMonotonicCoarse machine

            UnixTimestamp.seconds reading * 1000L
            + int64 (UnixTimestamp.nanoseconds reading) / nanosecondsPerMillisecond
        | SimulatedUnixFlavour.Darwin ->
            let reading = read entryPoint darwinClockUptimeRaw machine

            (UnixTimestamp.seconds reading * nanosecondsPerSecond
             + int64 (UnixTimestamp.nanoseconds reading))
            / nanosecondsPerMillisecond
