namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A process launched with an output stream whose reader the client closed
/// before it started (`LaunchDescriptor.Gone`): every write there answers
/// EPIPE and raises SIGPIPE, as a write into any pipe with no reader does,
/// while the other streams carry on as launched.
///
/// The oracle is a reference small enough to read at a glance: a write to a
/// gone stream raises SIGPIPE at the writing task on Linux and at the process
/// on Darwin, except Linux's zero-length write, which answers 0; what the
/// signal does is the disposition's to say; and a write to a drained stream
/// is delivered whole.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStreamReaderGone =

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private payload (seed : int) (count : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (Array.init count (fun i -> byte ((seed + i) % 251)))

    /// The launch table with standard output and standard error each drained or
    /// gone, as `gone` says of descriptors 1 and 2.
    let private launch (gone : Set<int>) : Map<int, LaunchDescriptor> =
        (UnixSystem.pipedStandardStreams, gone)
        ||> Set.fold (fun table fd -> Map.add fd LaunchDescriptor.Gone table)

    let private withSigPipe
        (disposition : SignalDisposition<string>)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    Signals = SignalState.setDisposition Signal.SIGPIPE disposition system.Process.Signals
                }
        }

    /// A process with a leader, 0, and a worker, 1.
    let private systemOn
        (platform : SimulatedUnixPlatform)
        (gone : Set<int>)
        (disposition : SignalDisposition<string>)
        : UnixSystem<int, string>
        =
        UnixSystem.initial platform (launch gone) 0 (CpuId 0)
        |> UnixBootImage.boot
        |> Tasks.spawn 1
        |> withSigPipe disposition

    let private sigPipeFrom (flavour : SimulatedUnixFlavour) (task : int) : PendingSignal<int> =
        {
            Signal = Signal.SIGPIPE
            Target =
                match flavour with
                | SimulatedUnixFlavour.Linux -> ValueSome task
                | SimulatedUnixFlavour.Darwin -> ValueNone
        }

    let private dispositions : SignalDisposition<string> list =
        [
            SignalDisposition.Default
            SignalDisposition.Ignore
            SignalDisposition.Catch (SignalCatch.ofHandler "on SIGPIPE")
        ]

    let private assertSound (context : string) (system : UnixSystem<int, string>) : unit =
        match UnixSystem.checkInvariants system with
        | [] -> ()
        | defects -> failwith $"%s{context}: %A{defects}"

    [<Test>]
    let ``every write to a launched output stream answers as the reference says, gone or drained`` () : unit =
        let property
            (
                platform : SimulatedUnixPlatform,
                gone : Set<int>,
                disposition : SignalDisposition<string>,
                ops : (int * int * int) list
            )
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform
            let mutable system = systemOn platform gone disposition
            let mutable delivered : (int * byte list) list = []
            let mutable pending : Set<PendingSignal<int>> = Set.empty
            let mutable ended = false

            for i, (task, fd, count) in List.indexed ops |> Seq.takeWhile (fun _ -> not ended) do
                let where =
                    $"%O{platform}, gone %A{gone}, %A{disposition}, call %d{i}: task %d{task} writes %d{count} to fd %d{fd}"

                let bytes = payload i count

                let actual = WriteOutcomes.admitThenWrite task fd UserBuffer.Mapped bytes system

                // `write` without the admission, as a caller holding the bytes
                // may make it, answers the same.
                UnixReadWrite.write task fd bytes system |> shouldEqual actual

                if Set.contains fd gone && not (flavour = SimulatedUnixFlavour.Linux && count = 0) then
                    let raised = sigPipeFrom flavour task

                    match disposition, actual with
                    | SignalDisposition.Default, Ok (WriteOutcome.ProcessEnded endedProcess) ->
                        endedProcess.Termination
                        |> shouldEqual (ProcessTermination.Signaled (Signal.SIGPIPE, false))

                        ended <- true
                    | SignalDisposition.Ignore,
                      Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, signal, after)) ->
                        signal |> shouldEqual raised
                        after |> shouldEqual system
                    | SignalDisposition.Catch _,
                      Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, signal, after)) ->
                        signal |> shouldEqual raised
                        pending <- Set.add raised pending
                        Set.ofList (SignalState.pending after.Process.Signals) |> shouldEqual pending

                        { after with
                            Process =
                                { after.Process with
                                    Signals = system.Process.Signals
                                }
                        }
                        |> shouldEqual system

                        system <- after
                    | _ -> failwith $"%s{where}: expected EPIPE and SIGPIPE, got %A{actual}"
                else
                    match actual with
                    | Ok (WriteOutcome.Returns (WriteAnswer.Completed written, after)) ->
                        written |> shouldEqual (int64 count)

                        if count > 0 then
                            delivered <- (fd, List.ofSeq bytes) :: delivered

                        system <- after
                    | other -> failwith $"%s{where}: expected the whole write, got %A{other}"

                assertSound where system

                DeliveryLog.toList system.Machine.Delivered
                |> List.map (fun delivery ->
                    let (ExternalEndpoint fd) = delivery.Endpoint
                    fd, List.ofSeq delivery.Bytes
                )
                |> shouldEqual (List.rev delivered)

        let gen =
            gen {
                let! platform = Gen.elements platforms
                let! goneOut = Gen.elements [ true ; false ]
                let! goneErr = Gen.elements [ true ; false ]
                let! disposition = Gen.elements dispositions

                let op =
                    Gen.zip3
                        (Gen.elements [ 0 ; 1 ])
                        (Gen.elements [ 1 ; 2 ])
                        (Gen.frequency [ 1, Gen.constant 0 ; 4, Gen.choose (1, 3000) ; 1, Gen.constant 70000 ])

                let! ops = Gen.listOf op |> Gen.map (List.truncate 30)

                let gone =
                    Set.union
                        (if goneOut then Set.singleton 1 else Set.empty)
                        (if goneErr then Set.singleton 2 else Set.empty)

                return platform, gone, disposition, ops
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)

    /// The Darwin signal is the process's, which its leader takes; one the
    /// leader blocks while another task does not would go to that other task,
    /// which is not modelled. Linux's is the writing task's own, so the same
    /// write from the blocking leader leaves it pending there.
    [<Test>]
    let ``a caught SIGPIPE that only a task other than the leader could take is refused on Darwin`` () : unit =
        for platform in platforms do
            let system =
                systemOn platform (Set.singleton 1) (SignalDisposition.Catch (SignalCatch.ofHandler "on SIGPIPE"))

            let tasks = system.Tasks |> Map.keys |> Set.ofSeq

            let system =
                { system with
                    Process =
                        { system.Process with
                            Signals =
                                HandlerFrames.enter
                                    "carrier"
                                    system.Leader
                                    tasks
                                    system.Leader
                                    (Set.singleton Signal.SIGPIPE)
                                    system.Process.Signals
                        }
                }

            let actual =
                WriteOutcomes.admitThenWrite system.Leader 1 UserBuffer.Mapped (payload 0 5) system

            match SimulatedUnixPlatform.flavour platform, actual with
            | SimulatedUnixFlavour.Darwin,
              Error (WriteRefusal.SignalReceiver (_, SignalReceiverRefusal.LeaderBlocks Signal.SIGPIPE)) -> ()
            | SimulatedUnixFlavour.Linux,
              Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, signal, after)) ->
                signal |> shouldEqual (sigPipeFrom SimulatedUnixFlavour.Linux system.Leader)
                SignalState.pending after.Process.Signals |> shouldEqual [ signal ]
            | flavour, other -> failwith $"%O{flavour}: %A{other}"

    /// A gone stream's write end polls as a pipe's with no reader does, and
    /// closing it frees the pipe, which nothing else holds.
    [<Test>]
    let ``a gone stream polls ERR on Linux, and its pipe goes with its last descriptor`` () : unit =
        for platform in platforms do
            let system = systemOn platform (Set.singleton 1) SignalDisposition.Ignore

            let pipe =
                match FileDescriptorRegistry.tryFindTarget 1 system.Process.FileDescriptors with
                | Some (OpenFileTarget.Pipe (pipeId, PipeEnd.Write)) -> pipeId
                | other -> failwith $"fd 1 is %A{other}"

            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin -> ()
            | SimulatedUnixFlavour.Linux ->
                let id =
                    FileDescriptorRegistry.tryFindId 1 system.Process.FileDescriptors |> Option.get

                // OUT|WRNORM, the empty pipe having room, and ERR: measured
                // (pipe-epipe-sweep.c), 0xc through `poll(POLLOUT)`.
                LinuxReadiness.ofDescription id system
                |> shouldEqual (EpollEvents.Out ||| EpollEvents.WrNorm ||| EpollEvents.Err)

            assertSound "launched" system

            let closed =
                match UnixDescriptor.close 1 system with
                | Ok (SyscallAnswer.Completed 0L, system) -> system
                | other -> failwith $"close 1: %A{other}"

            Map.containsKey pipe closed.Machine.Pipes |> shouldEqual false
            assertSound "closed" closed

    /// A write is made by one of the process's tasks, which the signal it may
    /// raise is aimed at; a task the process does not have is the client's
    /// mistake, refused before anything is answered.
    [<Test>]
    let ``a write by a task the process does not have fails loudly`` () : unit =
        for platform in platforms do
            let system = systemOn platform Set.empty SignalDisposition.Ignore

            for fd in [ 1 ; 99 ] do
                let admit =
                    Assert.Throws (fun () ->
                        UnixReadWrite.admitWrite 7 fd UserBuffer.Mapped 1UL system
                        |> ignore<Result<WriteOutcome<WriteAdmission, int, string>, WriteRefusal>>
                    )

                admit.Message |> shouldContainText "task 7 is not one of the process's tasks"

                let write =
                    Assert.Throws (fun () ->
                        UnixReadWrite.write 7 fd (payload 0 1) system
                        |> ignore<Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>>
                    )

                write.Message |> shouldContainText "task 7 is not one of the process's tasks"

    /// Process ID 1 is an init process, which ignores a signal it has no
    /// handler for; that is not modelled, so its broken-pipe write is refused,
    /// as `kill` refuses it. Linux's zero-length write raises nothing, so it is
    /// answered.
    [<Test>]
    let ``a broken-pipe write by process ID 1 is refused`` () : unit =
        for platform in platforms do
            for disposition in dispositions do
                let system =
                    UnixSystem.initial platform (launch (Set.singleton 1)) 0 (CpuId 0)
                    |> UnixBootImage.withProcessId "test" (ProcessId.parseOrFail "test" 1)
                    |> UnixBootImage.boot
                    |> withSigPipe disposition

                match WriteOutcomes.admitThenWrite system.Leader 1 UserBuffer.Mapped (payload 0 5) system with
                | Error (WriteRefusal.InitProcess _) -> ()
                | other -> failwith $"%O{platform}, %A{disposition}: %A{other}"

                match
                    SimulatedUnixPlatform.flavour platform,
                    UnixReadWrite.write system.Leader 1 ImmutableArray.Empty system
                with
                | SimulatedUnixFlavour.Linux, Ok (WriteOutcome.Returns (WriteAnswer.Completed 0L, _))
                | SimulatedUnixFlavour.Darwin, Error (WriteRefusal.InitProcess _) -> ()
                | flavour, other -> failwith $"%O{flavour}, %A{disposition}, zero-length: %A{other}"
