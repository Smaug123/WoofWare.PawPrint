namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// The signal argument of `kill(2)`.
[<RequireQualifiedAccess>]
type KillSignal =
    /// Signal number 0, the null signal: check that the target exists and may
    /// be signalled, and send nothing.
    | Null
    /// Send this signal.
    | Signal of Signal

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

    /// `kill(2)`, sent by the calling process to `pid`.
    ///
    /// `liveThreads` are the process's threads that exist at the kernel level;
    /// a signal sent to the process can be received by any of them that does
    /// not block it. See `SignalState.generate` for what the signal then does.
    ///
    /// Only a signal to the calling process itself is answered.
    let kill<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (liveThreads : ImmutableArray<'Task>)
        (pid : int32)
        (signal : KillSignal)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SignalGeneration * UnixSystem<'Task, 'Handler>, KillRefusal>
        =
        let self = ProcessId.toInt32 (UnixSystem.processId system)

        if pid <= 0 then
            Error (KillRefusal.ProcessGroup pid)
        elif pid <> self then
            Error (KillRefusal.OtherProcess pid)
        elif self = 1 then
            Error KillRefusal.InitProcess
        else

        match signal with
        | KillSignal.Null -> Ok (SignalGeneration.ProcessContinues, system)
        | KillSignal.Signal signal ->
            let generation, signals =
                SignalState.generate
                    liveThreads
                    {
                        Signal = signal
                        Target = ValueNone
                    }
                    system.Process.Signals

            Ok (
                generation,
                { system with
                    Process =
                        { system.Process with
                            Signals = signals
                        }
                }
            )
