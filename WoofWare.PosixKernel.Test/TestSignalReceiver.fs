namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Which task receives a signal sent to the whole process, held to the rows of
/// `docs/plans/2026-08-23-posix-kernel-extraction/signal-receiver.c`, checked in
/// as `signalReceiver/linux.txt` (Linux 6.18.5, aarch64) and
/// `signalReceiver/darwin.txt` (Darwin 27.0.0).
///
/// There, four threads (T0 the main thread) handled SIGUSR1, each set of them
/// short of all four blocked it, and each thread in turn sent it eight times.
/// Whenever T0 did not block it, every send went to T0; the library models that.
/// When T0 blocked it, the flavours chose another thread each by its own rule,
/// Linux's depending on earlier sends; the library refuses those.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalReceiver =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    /// A system of four tasks, 0 its leader, under `flavour`, with SIGUSR1
    /// at `disposition`, and every task in `blocking` inside a handler that
    /// blocks it.
    let private systemWith
        (flavour : SimulatedUnixFlavour)
        (disposition : SignalDisposition<string>)
        (blocking : Set<int>)
        : UnixSystem<int, string>
        =
        let system =
            UnixSystem.initial (HostPlatform.platformOf flavour)
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> Tasks.spawn 1
            |> Tasks.spawn 2
            |> Tasks.spawn 3

        let system =
            { system with
                Process =
                    { system.Process with
                        Signals = SignalState.setDisposition Signal.SIGUSR1 disposition system.Process.Signals
                    }
            }

        (system, blocking)
        ||> Set.fold (fun system task -> HandlerFrames.enterIn "carrier" task (Set.singleton Signal.SIGUSR1) system)

    let private usr1 (flavour : SimulatedUnixFlavour) : int =
        Signal.toRawSignoUnder (SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)) Signal.SIGUSR1

    let private self (system : UnixSystem<int, string>) : int32 =
        ProcessId.toInt32 (UnixSystem.processId system)

    /// The handler frames each of the four tasks takes next, in task order:
    /// `None` for a task that takes nothing.
    let private takenBy (system : UnixSystem<int, string>) : HandlerFrame<int, string> list option list =
        [ 0..3 ]
        |> List.map (fun task ->
            match UnixSignal.onReturnToUser task system with
            | Ok (ReturnToUserOutcome.Resumes _) -> None
            | Ok (ReturnToUserOutcome.RunHandlers (frames, _)) -> Some frames
            | Ok (ReturnToUserOutcome.ProcessStopped _ as other)
            | Ok (ReturnToUserOutcome.ContinueDiscarded _ as other)
            | Ok (ReturnToUserOutcome.ProcessEnded _ as other) -> failwith $"task %d{task} took %A{other}"
            | Error refusal -> failwith $"task %d{task} was refused: %O{refusal}"
        )

    /// Send SIGUSR1 to the process, and say which tasks take it.
    let private receivers (flavour : SimulatedUnixFlavour) (blocking : Set<int>) : Result<int list, KillRefusal> =
        let system =
            systemWith flavour (SignalDisposition.Catch (SignalCatch.ofHandler "h")) blocking

        match UnixSignal.kill (self system) (usr1 flavour) system with
        | Ok (Ok (KillOutcome.ProcessContinues system)) ->
            takenBy system
            |> List.indexed
            |> List.choose (fun (task, delivery) ->
                match delivery with
                | Some [ frame ] when frame.Entry.Signal = Signal.SIGUSR1 && frame.Action.Handler = "h" -> Some task
                | None -> None
                | Some other -> failwith $"task %d{task} took %A{other}"
            )
            |> Ok
        | Ok other -> failwith $"kill did not leave the signal to be taken: %A{other}"
        | Error refusal -> Error refusal

    let private rows (flavour : string) : string list =
        let assembly = Assembly.GetExecutingAssembly ()
        let name = $"WoofWare.PosixKernel.Test.signalReceiver.%s{flavour}.txt"

        use stream =
            match assembly.GetManifestResourceStream name with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.filter (fun line -> not (line.StartsWith ("#", StringComparison.Ordinal)))

    [<Literal>]
    let private RowPattern =
        @"^(sleep|spin) +round\d blocked=\{((?:T\d)*)\} sender=T(\d) receivers:((?: T\d)+)$"

    /// Replay every row of one flavour's file, answering how many rows the model
    /// answered and how many it refused.
    let private replay (flavour : SimulatedUnixFlavour) (file : string) : int * int =
        let mutable answered = 0
        let mutable refused = 0

        for line in rows file do
            let row = Regex.Match (line, RowPattern)

            if not row.Success then
                failwith $"%s{file}: unrecognised row: %s{line}"

            let tasksIn (text : string) : int list =
                Regex.Matches (text, @"T(\d)")
                |> Seq.map (fun m -> Int32.Parse m.Groups.[1].Value)
                |> Seq.toList

            let blocking = tasksIn row.Groups.[2].Value |> Set.ofList
            let measured = tasksIn row.Groups.[4].Value |> List.distinct

            match receivers flavour blocking with
            | Ok taken ->
                if taken <> measured then
                    failwith
                        $"%s{file}: the model gave the signal to %A{taken}, the kernel to %A{measured}, in: %s{line}"

                answered <- answered + 1
            | Error refusal ->
                refusal
                |> shouldEqual (KillRefusal.Receiver (SignalReceiverRefusal.LeaderBlocks Signal.SIGUSR1))

                // Refused exactly when the kernel gave it to another thread.
                if List.contains 0 measured then
                    failwith $"%s{file}: the model refused a signal the kernel gave to the leader, in: %s{line}"

                refused <- refused + 1

        answered, refused

    [<Test>]
    let ``every Linux row of the receiver probe`` () : unit =
        // 2 idle modes x 3 rounds x 4 senders: 8 sets without T0 are answered,
        // and 7 with it refused.
        replay SimulatedUnixFlavour.Linux "linux" |> shouldEqual (192, 168)

    [<Test>]
    let ``every Darwin row of the receiver probe`` () : unit =
        replay SimulatedUnixFlavour.Darwin "darwin" |> shouldEqual (192, 168)

    [<Test>]
    let ``a caught signal sent to the process goes to the leader whatever the other tasks block`` () : unit =
        let property (flavour : SimulatedUnixFlavour) (others : bool list) : unit =
            let blocking =
                others
                |> List.truncate 3
                |> List.indexed
                |> List.choose (fun (i, blocks) -> if blocks then Some (i + 1) else None)
                |> Set.ofList

            receivers flavour blocking |> shouldEqual (Ok [ 0 ])

        Check.One (propertyConfig, property)

    [<Test>]
    let ``a signal every task blocks is left pending on the process`` () : unit =
        for flavour in flavours do
            let system =
                systemWith flavour (SignalDisposition.Catch (SignalCatch.ofHandler "h")) (Set.ofList [ 0..3 ])

            match UnixSignal.kill (self system) (usr1 flavour) system with
            | Ok (Ok (KillOutcome.ProcessContinues after)) ->
                SignalState.pending after.Process.Signals
                |> shouldEqual
                    [
                        {
                            Signal = Signal.SIGUSR1
                            Target = ValueNone
                        }
                    ]

                takenBy after |> shouldEqual [ None ; None ; None ; None ]
            | other -> failwith $"%O{flavour}: %A{other}"

    [<Test>]
    let ``a signal whose default ends the process is answered when the leader blocks it`` () : unit =
        // Which task receives it does not matter: SIGUSR1 at its default ends the
        // process.
        for flavour in flavours do
            let system = systemWith flavour SignalDisposition.Default (Set.ofList [ 0 ; 2 ])

            match UnixSignal.kill (self system) (usr1 flavour) system with
            | Ok (Ok (KillOutcome.ProcessEnded ended)) ->
                ended.Termination
                |> shouldEqual (ProcessTermination.Signaled (Signal.SIGUSR1, false))
            | other -> failwith $"%O{flavour}: %A{other}"

    [<Test>]
    let ``an ignored signal the leader blocks is discarded under Darwin and refused under Linux`` () : unit =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/sigcont-generation.c`
        // (the "gen USR1 IGN" rows): Darwin hands it to a thread that does not
        // block it, which discards it; Linux leaves it pending on the process,
        // where the leader's sigpending sees it, until another thread takes it.
        for flavour in flavours do
            let system = systemWith flavour SignalDisposition.Ignore (Set.ofList [ 0 ; 2 ])

            match flavour, UnixSignal.kill (self system) (usr1 flavour) system with
            | SimulatedUnixFlavour.Darwin, Ok (Ok (KillOutcome.ProcessContinues after)) ->
                SignalState.pending after.Process.Signals |> shouldBeEmpty
            | SimulatedUnixFlavour.Linux, Error refusal ->
                refusal
                |> shouldEqual (KillRefusal.Receiver (SignalReceiverRefusal.LeaderBlocks Signal.SIGUSR1))
            | _, other -> failwith $"%O{flavour}: %A{other}"

    [<Test>]
    let ``every task is refused while a pending caught signal could reach only a task other than the leader``
        ()
        : unit
        =
        // Pending while every task blocks it; then task 2 unblocks it, so a real
        // kernel gives it to task 2 at once.
        for flavour in flavours do
            let system =
                systemWith flavour (SignalDisposition.Catch (SignalCatch.ofHandler "h")) (Set.ofList [ 0..3 ])

            match UnixSignal.kill (self system) (usr1 flavour) system with
            | Ok (Ok (KillOutcome.ProcessContinues after)) ->
                let after =
                    { after with
                        Process =
                            { after.Process with
                                Signals = HandlerFrames.leave 2 after.Process.Signals
                            }
                    }

                for task in 0..3 do
                    match UnixSignal.onReturnToUser task after with
                    | Error refusal -> refusal |> shouldEqual (SignalReceiverRefusal.LeaderBlocks Signal.SIGUSR1)
                    | Ok answer -> failwith $"%O{flavour}: task %d{task} was answered %A{answer}"
            | other -> failwith $"%O{flavour}: %A{other}"

    [<Test>]
    let ``a signal pending on one task alone is taken by that task only`` () : unit =
        for flavour in flavours do
            let system =
                systemWith flavour (SignalDisposition.Catch (SignalCatch.ofHandler "h")) Set.empty

            let signals =
                SignalState.enqueue
                    {
                        Signal = Signal.SIGUSR1
                        Target = ValueSome 2
                    }
                    system.Process.Signals

            let system =
                { system with
                    Process =
                        { system.Process with
                            Signals = signals
                        }
                }

            match takenBy system with
            | [ None ; None ; Some [ frame ] ; None ] ->
                frame.Entry
                |> shouldEqual
                    {
                        Signal = Signal.SIGUSR1
                        Target = ValueSome 2
                    }

                frame.Action.Handler |> shouldEqual "h"
            | other -> failwith $"%O{flavour}: expected task 2 alone to take it, got %A{other}"

    [<Test>]
    let ``asking for a task that does not exist fails loudly`` () : unit =
        let system =
            systemWith SimulatedUnixFlavour.Linux (SignalDisposition.Catch (SignalCatch.ofHandler "h")) Set.empty

        let exn =
            Assert.Throws<exn> (fun () -> UnixSignal.onReturnToUser 7 system |> ignore)

        exn.Message |> shouldContainText "not one of the process's tasks"
