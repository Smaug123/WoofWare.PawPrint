namespace WoofWare.PosixKernel

/// Why this kernel will not answer a `clock_gettime`.
///
/// Distinct from an errno: each case is a clock id that a real kernel of the
/// simulated flavour answers, naming a clock this kernel does not model.
[<RequireQualifiedAccess>]
type ClockGettimeRefusal =
    /// `CLOCK_PROCESS_CPUTIME_ID` or `CLOCK_THREAD_CPUTIME_ID`. This kernel does
    /// not account the CPU time a process or thread consumes.
    | CpuTime of clockId : int
    /// A negative clock id on Linux. It encodes the CPU-time clock of a process
    /// or thread by its ID, or a clock device by a file descriptor, and this
    /// kernel models neither.
    | EncodedClock of clockId : int
    /// A clock that reads an approximation of a finer one as of some earlier
    /// moment: Linux's `CLOCK_REALTIME_COARSE`, Darwin's
    /// `CLOCK_MONOTONIC_RAW_APPROX` and `CLOCK_UPTIME_RAW_APPROX`. Not modelled.
    | Coarse of clockId : int
    /// A clock whose reading depends on something this kernel's machine does not
    /// describe: Linux's `CLOCK_REALTIME_ALARM` and `CLOCK_BOOTTIME_ALARM`, which
    /// exist only on a machine with a real-time-clock device, and `CLOCK_TAI`,
    /// whose offset from the realtime clock is whatever a time daemon set.
    | MachineDependent of clockId : int

[<RequireQualifiedAccess>]
module ClockGettimeRefusal =
    /// What this kernel knows about why it cannot answer, for a client composing
    /// a diagnostic.
    let describe (refusal : ClockGettimeRefusal) : string =
        match refusal with
        | ClockGettimeRefusal.CpuTime clockId ->
            $"clock id %d{clockId} is a CPU-time clock on the simulated platform, and this kernel does not account CPU time."
        | ClockGettimeRefusal.EncodedClock clockId ->
            $"clock id %d{clockId} is negative, which Linux reads as the CPU-time clock of a process or thread, or as a clock device named by a file descriptor. This kernel models neither."
        | ClockGettimeRefusal.Coarse clockId ->
            $"clock id %d{clockId} is a coarse approximation of another clock on the simulated platform, which this kernel does not model."
        | ClockGettimeRefusal.MachineDependent clockId ->
            $"clock id %d{clockId} reads a clock whose value depends on the machine (a real-time-clock device, or the TAI offset a time daemon sets), which this kernel's machine does not describe."

/// Which of the machine's two clocks a clock id reads, and to what granularity
/// its flavour reports it.
[<RequireQualifiedAccess>]
type private ClockSource =
    | SinceBoot of granularity : int
    | Realtime of granularity : int

[<RequireQualifiedAccess>]
type private ClockDecoding =
    | Reads of ClockSource
    | Refused of ClockGettimeRefusal
    | Invalid

[<RequireQualifiedAccess>]
module UnixClock =

    // The numbering is each flavour's own `<time.h>` (`<linux/time.h>` for
    // Linux), and the split between answered and EINVAL was measured 2026-09-23
    // by calling `clock_gettime` on every id in [-4096, 4096], on a stride of
    // 65521 across the rest of the int range, and on its four extremes:
    //
    //   Linux 6.18.5 aarch64 (glibc 2.41): 0..9 and 11 answer; nine negative
    //     ids in [-4096, -1] answer (the CPU-time clocks those encode); every
    //     other id is EINVAL, including 10 (the retired CLOCK_SGI_CYCLE) and
    //     16..20, for which `clock_getres` nevertheless answers: those are the
    //     auxiliary clocks Linux 6.17 added, which read EINVAL until configured.
    //   macOS 26 arm64: 0, 4, 5, 6, 8, 9, 12 and 16 answer; every other id,
    //     negative ones included, is EINVAL.
    //
    // No id failed with any errno but EINVAL on either.
    //
    // Measured granularity, from 20,000 readings of each and `clock_getres`:
    // Darwin's CLOCK_REALTIME and CLOCK_MONOTONIC report whole microseconds (its
    // libc's `clock_gettime` derives both from `gettimeofday`), while its _RAW
    // clocks and every Linux clock carry nanosecond digits. Darwin's filesystem
    // stamps carry nanosecond digits too (200 of 200 APFS mtimes), which is why
    // the kernel's own realtime clock is nanosecond-grained and only this
    // interface truncates.
    let private decode (flavour : SimulatedUnixFlavour) (clockId : int) : ClockDecoding =
        let full = 1
        let microsecond = 1000

        match flavour with
        | SimulatedUnixFlavour.Linux ->
            match clockId with
            | 0 -> ClockDecoding.Reads (ClockSource.Realtime full)
            // CLOCK_MONOTONIC.
            | 1 -> ClockDecoding.Reads (ClockSource.SinceBoot full)
            | 2
            | 3 -> ClockDecoding.Refused (ClockGettimeRefusal.CpuTime clockId)
            // CLOCK_MONOTONIC_RAW differs from CLOCK_MONOTONIC only by the NTP
            // frequency correction the latter receives, and this machine has none.
            | 4 -> ClockDecoding.Reads (ClockSource.SinceBoot full)
            | 5 -> ClockDecoding.Refused (ClockGettimeRefusal.Coarse clockId)
            // CLOCK_MONOTONIC_COARSE is the timekeeper's reading as of its last
            // update, without consulting the clock hardware since. A real machine
            // updates at every timer tick (measured: `clock_getres` 4 ms on a
            // CONFIG_HZ=250 kernel), so the coarse clock trails the fine one by up
            // to a tick. This machine's clock moves only when advanced, so its
            // last update is the current reading, and the two read alike.
            | 6 -> ClockDecoding.Reads (ClockSource.SinceBoot full)
            // CLOCK_BOOTTIME differs from CLOCK_MONOTONIC only by time spent
            // suspended, and this machine never suspends.
            | 7 -> ClockDecoding.Reads (ClockSource.SinceBoot full)
            | 8
            | 9
            | 11 -> ClockDecoding.Refused (ClockGettimeRefusal.MachineDependent clockId)
            | _ when clockId < 0 -> ClockDecoding.Refused (ClockGettimeRefusal.EncodedClock clockId)
            | _ -> ClockDecoding.Invalid
        | SimulatedUnixFlavour.Darwin ->
            match clockId with
            | 0 -> ClockDecoding.Reads (ClockSource.Realtime microsecond)
            // CLOCK_MONOTONIC_RAW: uptime including sleep, with no NTP correction.
            | 4 -> ClockDecoding.Reads (ClockSource.SinceBoot full)
            | 5
            | 9 -> ClockDecoding.Refused (ClockGettimeRefusal.Coarse clockId)
            // CLOCK_MONOTONIC: the realtime clock less the boot instant, both as
            // Darwin's libc reads them, in whole microseconds. `withBootTime` keeps
            // a Darwin boot instant on a whole microsecond, so truncating the
            // uptime is the same as truncating the realtime reading.
            | 6 -> ClockDecoding.Reads (ClockSource.SinceBoot microsecond)
            // CLOCK_UPTIME_RAW differs from CLOCK_MONOTONIC_RAW only by time
            // spent asleep, and this machine never sleeps.
            | 8 -> ClockDecoding.Reads (ClockSource.SinceBoot full)
            | 12
            | 16 -> ClockDecoding.Refused (ClockGettimeRefusal.CpuTime clockId)
            | _ -> ClockDecoding.Invalid

    let private nanosecondsPerSecond : int64 = 1_000_000_000L

    /// Drop the digits of `reading`'s nanosecond part finer than `granularity`.
    let private truncate (granularity : int) (reading : UnixTimestamp) : UnixTimestamp =
        let nanoseconds = UnixTimestamp.nanoseconds reading

        UnixTimestamp.createOrFail
            "UnixClock.truncate"
            (UnixTimestamp.seconds reading)
            (nanoseconds - nanoseconds % granularity)

    /// `clock_gettime(2)`: read the clock `clockId` names on this machine's flavour.
    ///
    /// The numbering is the flavour's own: `CLOCK_MONOTONIC` is 1 on Linux and 6
    /// on Darwin. An id that names no clock on the flavour is `EINVAL`, and an id
    /// that names a clock this kernel does not model is a `ClockGettimeRefusal`.
    ///
    /// Every answered id reads one of two clocks. The realtime clock reads
    /// `UnixMachineState.realtime`, since the Unix epoch; every other clock reads
    /// `NanosecondsSinceBoot`, since boot. This machine never suspends and has no
    /// NTP correction, so two clocks that differ only by time spent suspended
    /// (Linux's `CLOCK_BOOTTIME` and `CLOCK_MONOTONIC`, Darwin's
    /// `CLOCK_MONOTONIC_RAW` and `CLOCK_UPTIME_RAW`) or only by frequency
    /// correction (each `_RAW` clock and its corrected counterpart) read the same.
    /// Linux's `CLOCK_MONOTONIC_COARSE` reads the same too: it is the clock as of
    /// its last update, and this machine's clock is only ever updated by being
    /// advanced.
    ///
    /// Darwin's `CLOCK_REALTIME` and `CLOCK_MONOTONIC` report whole microseconds,
    /// dropping the finer digits. Every other answered clock reports to the
    /// nanosecond.
    let clockGettime
        (clockId : int)
        (machine : UnixMachineState)
        : Result<Result<UnixTimestamp, UnixError>, ClockGettimeRefusal>
        =
        match decode (SimulatedUnixPlatform.flavour machine.UnixPlatform) clockId with
        | ClockDecoding.Invalid -> Ok (Error UnixError.EINVAL)
        | ClockDecoding.Refused refusal -> Error refusal
        | ClockDecoding.Reads (ClockSource.Realtime granularity) ->
            Ok (Ok (truncate granularity (UnixMachineState.realtime machine)))
        | ClockDecoding.Reads (ClockSource.SinceBoot granularity) ->
            let sinceBoot = machine.NanosecondsSinceBoot

            // Reachable only by a record-copy past `advanceClock`.
            if sinceBoot < 0L then
                failwith
                    $"UnixClock.clockGettime: the machine has been up for %d{sinceBoot} ns, which is negative. No uptime can be; the machine was assembled without advanceClock."

            let reading =
                UnixTimestamp.createOrFail
                    "UnixClock.clockGettime"
                    (sinceBoot / nanosecondsPerSecond)
                    (int (sinceBoot % nanosecondsPerSecond))

            Ok (Ok (truncate granularity reading))
