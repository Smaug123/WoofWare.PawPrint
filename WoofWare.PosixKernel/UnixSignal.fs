namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// Why this library will not answer a `kill(2)`. Each is a target it does not
/// model, rather than an error a kernel would report: a real kernel's answer
/// depends on other processes, which this library has none of.
[<RequireQualifiedAccess>]
type KillRefusal =
    /// A positive process ID other than the calling process's own.
    | OtherProcess of pid : int32
    /// Zero or a negative number: a process group, or every process the caller
    /// may signal.
    | ProcessGroup of pid : int32
    /// The calling process is process ID 1. An init process ignores, from
    /// inside its own PID namespace, every signal it has not installed a
    /// handler for, SIGKILL included, and this library does not model that.
    | InitProcess

[<RequireQualifiedAccess>]
module UnixSignal =

    /// `kill(2)`, sent by the calling process to `pid`, with `signo` read under
    /// the process's own signal numbering.
    ///
    /// `liveThreads` are the process's threads that exist at the kernel level;
    /// a signal sent to the process can be received by any of them that does
    /// not block it. See `SignalState.generate` for what the signal then does.
    ///
    /// Only a signal to the calling process itself is answered. Signal number 0
    /// sends nothing, and a number that is neither 0 nor a signal is `EINVAL`.
    let kill<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (liveThreads : ImmutableArray<'Task>)
        (pid : int32)
        (signo : int32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Result<SignalGeneration * UnixSystem<'Task, 'Handler>, UnixError>, KillRefusal>
        =
        let self = ProcessId.toInt32 (UnixSystem.processId system)

        // The target is screened before the number. Linux looks the target up
        // first, so a pid naming no process is ESRCH whatever the number, where
        // Darwin checks the number first and says EINVAL (measured,
        // `docs/plans/2026-08-23-posix-kernel-extraction/kill-arguments.c`).
        // Whether a pid names a process is what this kernel cannot know, so
        // every other target is refused before the number is looked at.
        if pid <= 0 then
            Error (KillRefusal.ProcessGroup pid)
        elif pid <> self then
            Error (KillRefusal.OtherProcess pid)
        elif self = 1 then
            Error KillRefusal.InitProcess
        elif signo = 0 then
            Ok (Ok (SignalGeneration.ProcessContinues, system))
        else

        match Signal.ofRawSignoUnder (SignalState.numbering system.Process.Signals) signo with
        | ValueNone -> Ok (Error UnixError.EINVAL)
        | ValueSome signal ->
            let generation, signals =
                SignalState.generate
                    liveThreads
                    {
                        Signal = signal
                        Target = ValueNone
                    }
                    system.Process.Signals

            Ok (
                Ok (
                    generation,
                    { system with
                        Process =
                            { system.Process with
                                Signals = signals
                            }
                    }
                )
            )
