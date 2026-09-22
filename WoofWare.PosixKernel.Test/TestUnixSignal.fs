namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

[<TestFixture>]
module TestUnixSignal =

    let private linux : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.linuxX64

    let private withPid (pid : int32) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process = UnixProcessState.withProcessId "test" (ProcessId.parseOrFail "test" pid) system.Process
        }

    let private live : ImmutableArray<int> = ImmutableArray.Create 0

    let private self : int32 = ProcessId.toInt32 (UnixSystem.processId linux)

    [<Test>]
    let ``kill of the calling process with SIGKILL terminates it`` () : unit =
        match UnixSignal.kill live self (KillSignal.Signal (Signal.Other 9)) linux with
        | Ok (generation, _) -> generation |> shouldEqual (SignalGeneration.ProcessTerminated (Signal.Other 9))
        | Error refusal -> failwith $"unexpected refusal: %O{refusal}"

    [<Test>]
    let ``the null signal to the calling process changes nothing`` () : unit =
        UnixSignal.kill live self KillSignal.Null linux
        |> shouldEqual (Ok (SignalGeneration.ProcessContinues, linux))

    [<Test>]
    let ``a signal the calling process cannot yet receive is left pending`` () : unit =
        // No live thread, so SIGTERM waits in the process-wide pending set.
        match UnixSignal.kill ImmutableArray.Empty self (KillSignal.Signal Signal.SIGTERM) linux with
        | Ok (generation, after) ->
            generation |> shouldEqual SignalGeneration.ProcessContinues

            SignalState.pending after.Process.Signals
            |> shouldEqual
                [
                    {
                        Signal = Signal.SIGTERM
                        Target = ValueNone
                    }
                ]
        | Error refusal -> failwith $"unexpected refusal: %O{refusal}"

    [<Test>]
    let ``kill is answered only for the calling process`` () : unit =
        // A pid that differs from the caller's by one, so a comparison against
        // the wrong field (or an off-by-one) is caught.
        UnixSignal.kill live (self + 1) KillSignal.Null linux
        |> shouldEqual (Error (KillRefusal.OtherProcess (self + 1)))

        for pid in [ 0 ; -1 ; -self ] do
            UnixSignal.kill live pid KillSignal.Null linux
            |> shouldEqual (Error (KillRefusal.ProcessGroup pid))

        // The same pid is the caller's own once the process is configured with it.
        UnixSignal.kill live (self + 1) KillSignal.Null (withPid (self + 1) linux)
        |> shouldEqual (Ok (SignalGeneration.ProcessContinues, withPid (self + 1) linux))

    [<Test>]
    let ``kill by an init process is refused`` () : unit =
        let init = withPid 1 linux

        UnixSignal.kill live 1 (KillSignal.Signal (Signal.Other 9)) init
        |> shouldEqual (Error KillRefusal.InitProcess)
