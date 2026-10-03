namespace WoofWare.PawPrint.Test

open WoofWare.PosixKernel

/// A process's signal dispositions, read and set through `sigaction` as a
/// program makes the system call, for a test that needs to arrange or inspect
/// them.
[<RequireQualifiedAccess>]
module KernelSignals =

    let private numberingOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : SignalNumbering
        =
        SimulatedUnixPlatform.signalNumbering (UnixSystem.platform system)

    /// `signal`'s disposition, as `sigaction` reports it. Fails the test where
    /// the kernel will not report it (Darwin's SIGKILL and SIGSTOP).
    let disposition<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (signal : Signal)
        (system : UnixSystem<'Task, 'Handler>)
        : SignalDisposition<'Handler>
        =
        match UnixSignal.sigactionSyscall (Signal.toRawSignoUnder (numberingOf system) signal) None system with
        | Ok (disposition, _) -> disposition
        | Error errno -> failwith $"sigaction will not report %O{signal}'s disposition: %O{errno}"

    /// Every signal whose disposition is not the default.
    let dispositions<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Map<Signal, SignalDisposition<'Handler>>
        =
        let numbering = numberingOf system

        [ 1 .. Signal.highestSignoUnder numbering ]
        |> List.choose (fun signo ->
            match UnixSignal.sigactionSyscall signo None system with
            | Ok (SignalDisposition.Default, _)
            | Error _ -> None
            | Ok (disposition, _) -> Some (Signal.ofRawSignoUnder numbering signo |> ValueOption.get, disposition)
        )
        |> Map.ofList

    /// `system` once `sigaction` has installed `disposition` for `signal`.
    /// Fails the test where the kernel refuses it.
    let setDisposition<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (signal : Signal)
        (disposition : SignalDisposition<'Handler>)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        match
            UnixSignal.sigactionSyscall (Signal.toRawSignoUnder (numberingOf system) signal) (Some disposition) system
        with
        | Ok (_, system) -> system
        | Error errno -> failwith $"sigaction will not install %O{disposition} for %O{signal}: %O{errno}"
