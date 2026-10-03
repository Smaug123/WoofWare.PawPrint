namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open WoofWare.PosixKernel

/// What is pending when a task starts a transfer whose real kernel checks
/// `signal_pending` partway through: a read or write of `/dev/urandom`, or
/// `getrandom(2)`.
[<RequireQualifiedAccess>]
type PendingSignal =
    | Nothing
    /// SIGUSR1, caught, sent to the transferring task.
    | CaughtForTransferrer
    /// SIGUSR1, caught, sent to another task.
    | CaughtForAnother
    /// SIGUSR1, ignored, sent to the transferring task, which discards it.
    | IgnoredForTransferrer
    /// SIGCONT at its default, sent to the transferring task: it stays pending.
    | DefaultForTransferrer

[<RequireQualifiedAccess>]
module PendingSignal =

    let gen : Gen<PendingSignal> =
        Gen.elements
            [
                PendingSignal.Nothing
                PendingSignal.CaughtForTransferrer
                PendingSignal.CaughtForAnother
                PendingSignal.IgnoredForTransferrer
                PendingSignal.DefaultForTransferrer
            ]

    /// Whether `pending` leaves a signal the transferring task takes as it
    /// returns to user mode, which is what Linux's `signal_pending` asks.
    let transferrerHasSignal (pending : PendingSignal) : bool =
        match pending with
        | PendingSignal.CaughtForTransferrer
        | PendingSignal.DefaultForTransferrer -> true
        | PendingSignal.Nothing
        | PendingSignal.CaughtForAnother
        | PendingSignal.IgnoredForTransferrer -> false

    /// `system` with task 1 beside `transferrer`, which must not be 1, and
    /// `pending` made so through `sigaction` and `pthread_kill`.
    let make
        (transferrer : int)
        (pending : PendingSignal)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        if transferrer = 1 then
            failwith "PendingSignal.make: task 1 is the other task"

        let system = Tasks.ensure 1 system

        let install (disposition : SignalDisposition<string>) (system : UnixSystem<int, string>) =
            match UnixSignal.sigaction 10 (Some disposition) system with
            | Ok (_, system) -> system
            | Error error -> failwith $"sigaction: %O{error}"

        let send (target : int) (signo : int) (system : UnixSystem<int, string>) =
            match UnixSignal.pthreadKill target signo system with
            | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
            | other -> failwith $"pthread_kill %d{target} %d{signo}: %A{other}"

        match pending with
        | PendingSignal.Nothing -> system
        | PendingSignal.CaughtForTransferrer ->
            system
            |> install (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
            |> send transferrer 10
        | PendingSignal.CaughtForAnother ->
            system
            |> install (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
            |> send 1 10
        | PendingSignal.IgnoredForTransferrer -> system |> install SignalDisposition.Ignore |> send transferrer 10
        | PendingSignal.DefaultForTransferrer -> system |> send transferrer 18
