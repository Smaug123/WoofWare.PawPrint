namespace WoofWare.PosixKernel

/// What the kernel keeps of the status a process ended with: the argument its last
/// `exit_group(2)` (which `exit(3)` and `_exit(2)` make) was given, or the argument
/// of the thread-exit syscall its last task made.
///
/// Linux keeps the low 8 bits of the argument, and Darwin the low 24. A parent's
/// `waitpid(2)` reports the low 8 bits on both (`ExitStatus.waitpidExitCode`);
/// `waitid(2)` reports everything that was kept (`ExitStatus.waitidStatus`), so it
/// is where the two flavours differ.
///
/// Construct one with `ExitStatus.ofExitArgument`.
type ExitStatus =
    private
        {
            Retained : int32
        }

[<RequireQualifiedAccess>]
module ExitStatus =

    /// What `flavour`'s kernel keeps of `argument`, the status a process ended with.
    let ofExitArgument (flavour : SimulatedUnixFlavour) (argument : int32) : ExitStatus =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/wait-status.c`
        // (exit and _exit) and `last-thread-exit-status.c` (the raw thread-exit
        // syscall, Linux only) over 0, 1, 7, 127, 128, 255, 256, 257, 263, 511, 65535,
        // 65543, -1, -256, INT_MAX and INT_MIN: Linux 6.18.5 (aarch64 and x86-64)
        // reported `n & 0xff` through both waitpid and waitid; Darwin 27.0.0 reported
        // `n & 0xff` through waitpid and `n & 0xffffff` through waitid.
        let mask =
            match flavour with
            | SimulatedUnixFlavour.Linux -> 0xff
            | SimulatedUnixFlavour.Darwin -> 0xffffff

        {
            Retained = argument &&& mask
        }

    /// The exit code a parent's `waitpid(2)` reports (`WEXITSTATUS`): the status's
    /// low 8 bits, on either flavour.
    let waitpidExitCode (status : ExitStatus) : int32 = status.Retained &&& 0xff

    /// The status a parent's `waitid(2)` reports (`si_status`): all the kernel kept.
    /// Never negative.
    let waitidStatus (status : ExitStatus) : int32 = status.Retained

/// How a process ended.
[<RequireQualifiedAccess>]
type ProcessTermination =
    /// The process exited: its last `exit_group(2)`, or its last task's thread-exit
    /// syscall, ended it with this status.
    | Exited of ExitStatus
    /// A signal killed the process, and it wrote a core dump iff `coreDumped`.
    | Signaled of signal : Signal * coreDumped : bool

/// How a parent's `waitid(2)` classifies a child that ended (`si_code`).
[<RequireQualifiedAccess>]
type WaitIdCode =
    /// `CLD_EXITED`: the child exited.
    | Exited
    /// `CLD_KILLED`: a signal killed the child, which wrote no core dump.
    | Killed
    /// `CLD_DUMPED`: a signal killed the child, which wrote a core dump.
    | Dumped

/// Why this library will not render a termination as a status a parent would read:
/// a case whose encoding has not been measured.
[<RequireQualifiedAccess>]
type WaitStatusRefusal =
    /// A death by `signal` that wrote a core dump, on Darwin.
    | CoreDumpOnDarwin of signal : Signal

[<RequireQualifiedAccess>]
module WaitStatusRefusal =

    /// A human-readable account of why the status was refused.
    let describe (refusal : WaitStatusRefusal) : string =
        match refusal with
        | WaitStatusRefusal.CoreDumpOnDarwin signal ->
            $"how Darwin reports a death by %O{signal} that wrote a core dump has not been measured"

[<RequireQualifiedAccess>]
module ProcessTermination =

    // The encodings below were measured by
    // `docs/plans/2026-08-23-posix-kernel-extraction/wait-status.c` on Linux 6.18.5
    // (aarch64 and x86-64) and Darwin 27.0.0: an exit is `(status & 0xff) << 8`
    // through waitpid, and a death by signal `signo`, or `signo | 0x80` with a core
    // dump; waitid says CLD_EXITED, CLD_KILLED or CLD_DUMPED. No core dump could be
    // written on the Darwin machine (a non-root user cannot write `/cores`), so a
    // Darwin death with one is refused.

    /// The status word a parent's `waitpid(2)` stores for a child that ended so, under
    /// `platform`'s signal numbering.
    let waitpidStatus
        (platform : SimulatedUnixPlatform)
        (termination : ProcessTermination)
        : Result<int32, WaitStatusRefusal>
        =
        match termination with
        | ProcessTermination.Exited status -> Ok (ExitStatus.waitpidExitCode status <<< 8)
        | ProcessTermination.Signaled (signal, coreDumped) ->
            let signo =
                Signal.toRawSignoUnder (SimulatedUnixPlatform.signalNumbering platform) signal

            if not coreDumped then
                Ok signo
            else

            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> Ok (signo ||| 0x80)
            | SimulatedUnixFlavour.Darwin -> Error (WaitStatusRefusal.CoreDumpOnDarwin signal)

    /// What a parent's `waitid(2)` reports for a child that ended so (`si_code` and
    /// `si_status`), under `platform`'s signal numbering.
    let waitidStatus
        (platform : SimulatedUnixPlatform)
        (termination : ProcessTermination)
        : Result<WaitIdCode * int32, WaitStatusRefusal>
        =
        match termination with
        | ProcessTermination.Exited status -> Ok (WaitIdCode.Exited, ExitStatus.waitidStatus status)
        | ProcessTermination.Signaled (signal, coreDumped) ->
            let signo =
                Signal.toRawSignoUnder (SimulatedUnixPlatform.signalNumbering platform) signal

            if not coreDumped then
                Ok (WaitIdCode.Killed, signo)
            else

            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> Ok (WaitIdCode.Dumped, signo)
            | SimulatedUnixFlavour.Darwin -> Error (WaitStatusRefusal.CoreDumpOnDarwin signal)

    /// The status a POSIX shell reports in `$?` for a child that ended so, under
    /// `numbering`: the exit code `waitpid(2)` reports, or 128 plus the number of the
    /// signal that killed it, whether or not it dumped core.
    let shellStatus (numbering : SignalNumbering) (termination : ProcessTermination) : int32 =
        match termination with
        | ProcessTermination.Exited status -> ExitStatus.waitpidExitCode status
        | ProcessTermination.Signaled (signal, _) -> 128 + Signal.toRawSignoUnder numbering signal
