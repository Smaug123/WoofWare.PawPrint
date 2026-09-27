namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The exit code this host exits with once a guest's run has ended, standing in for
/// the simulated process's own ending.
[<RequireQualifiedAccess>]
module AppExitCode =

    /// The exit code for a run that ended as `outcome`, on a host that is Windows iff
    /// `hostIsWindows`.
    ///
    /// On a Unix host, this is the code whose rendering by a shell matches how the
    /// simulated process ended: the latched exit code for an exit, and `128 + signo`,
    /// under the simulated platform's numbering, for a death by signal. CoreCLR ends an
    /// abort, and an escaped exception, in `abort()`, so those are SIGABRT's. On a
    /// Windows host, an abort is the fatal error's HRESULT and an escaped exception is
    /// `0xE0434352`, as real .NET on Windows exits with.
    let compute (hostIsWindows : bool) (outcome : RunOutcome) : int =
        let state = RunOutcome.state outcome

        let signalCode (signal : Signal) : int =
            128
            + Signal.toRawSignoUnder (SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform) signal

        match outcome with
        | RunOutcome.NormalExit _
        | RunOutcome.ProcessExit _ ->
            // What the host reads at shutdown: `Main`'s return value if it had one,
            // else the guest's last `Environment.ExitCode` write, else 0.
            state.LatchedExitCode
        | RunOutcome.Aborted (_, _, fatal, _) ->
            // CoreCLR's `EEPolicy::HandleFatalError` ends in
            // `CrashDumpAndTerminateProcess(exitCode)` (eepolicy.cpp:62), where `exitCode`
            // is the `COR_E_*` value it was handed. On Unix that is `abort()`, whichever
            // fatal error it was; on Windows it is `TerminateProcess` with the HRESULT
            // itself.
            if hostIsWindows then
                FatalErrorCode.toHResult fatal.Code
            else
                signalCode Signal.SIGABRT
        | RunOutcome.SignalTerminated (_, signal, _) ->
            // This host exits with the code rather than dying of the signal itself.
            signalCode signal
        | RunOutcome.GuestUnhandledException _ ->
            // On Windows the .NET runtime exits with 0xE0434352 (SEH); on Unix it aborts.
            if hostIsWindows then
                -532462766
            else
                signalCode Signal.SIGABRT

    /// On a Unix host, whether `code` reads to this host's parent shell as a process that
    /// ended by `termination` would, under `numbering`: the shell sees the low 8 bits of
    /// an exit code. `Error` describes the disagreement.
    let checkConsistent
        (numbering : SignalNumbering)
        (code : int)
        (termination : ProcessTermination)
        : Result<unit, string>
        =
        let expected = ProcessTermination.shellStatus numbering termination

        if code &&& 0xff = expected then
            Ok ()
        else
            Error
                $"this host would exit with code %d{code}, which a shell reads as %d{code &&& 0xff}, but the simulated process ended by %O{termination}, which a shell reads as %d{expected}"
