namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The standard streams of a process launched with `UnixSystem.pipedStandardStreams`
/// answer as the standard streams did before they were pipe ends: standard
/// input at end of file, each output stream a whole write recorded entry for
/// entry, and the readiness constants of the launch shape.
///
/// The oracle is a reference model of that earlier behaviour, small enough to
/// read at a glance. It knows nothing of pipes: a descriptor names a stream
/// (input, output or error) or something else, and a write to an output stream
/// appends to one log. The property drives both through the same random
/// sequence of descriptor operations and requires every stream answer and the
/// whole log to agree, the log as what the client draining each launched pipe
/// received.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLaunchedStreams =

    [<RequireQualifiedAccess>]
    type private Stream =
        | Input
        | Output
        | Error

    [<RequireQualifiedAccess>]
    type private ReferenceKind =
        | Stream of Stream
        /// A pipe end the process made, which the reference does not model.
        | Other

    type private ReferenceDescription =
        {
            Kind : ReferenceKind
            NonBlocking : bool
        }

    /// The earlier standard streams, and the log of writes to them, newest first.
    type private Reference =
        {
            Fds : Map<int, int>
            Descriptions : Map<int, ReferenceDescription>
            NextId : int
            Log : (Stream * byte list) list
        }

    [<RequireQualifiedAccess>]
    type private Op =
        | Write of fd : int * count : int
        | Read of fd : int * count : int
        | Dup of fd : int
        | Close of fd : int
        | SetNonBlocking of fd : int * value : bool
        | Poll of fd : int
        | Pipe2

    /// What one operation answered, in terms both sides share.
    [<RequireQualifiedAccess>]
    type private Answer =
        | Wrote of int64
        | WriteFailed of UnixError
        | ReadBytes of int
        | ReadFailed of UnixError
        | Syscall of SyscallAnswer
        | NonBlocking of SetNonBlockingAnswer
        | Level of uint32

    let private initialReference : Reference =
        let stream (kind : Stream) =
            {
                Kind = ReferenceKind.Stream kind
                NonBlocking = false
            }

        {
            Fds = Map.ofList [ 0, 0 ; 1, 1 ; 2, 2 ]
            Descriptions = Map.ofList [ 0, stream Stream.Input ; 1, stream Stream.Output ; 2, stream Stream.Error ]
            NextId = 3
            Log = []
        }

    let private lowestFree (fds : Map<int, int>) : int =
        Seq.initInfinite id |> Seq.find (fun fd -> not (Map.containsKey fd fds))

    let private referenceDescription (fd : int) (reference : Reference) : ReferenceDescription option =
        Map.tryFind fd reference.Fds
        |> Option.map (fun id -> reference.Descriptions.[id])

    let private flavourNonBlock (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 0x800
        | SimulatedUnixFlavour.Darwin -> 0x4

    let private payload (seed : int) (count : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (Array.init count (fun i -> byte ((seed * 31 + i) % 251)))

    /// What the earlier model did with the bytes the client received.
    let private streamOf (endpoint : ExternalEndpoint) : Stream =
        match endpoint with
        | ExternalEndpoint 1 -> Stream.Output
        | ExternalEndpoint 2 -> Stream.Error
        | other -> failwith $"a delivery reached %O{other}, which pipedStandardStreams does not drain"

    let private deliveredLog (system : UnixSystem<int, string>) : (Stream * byte list) list =
        DeliveryLog.toList system.Machine.Delivered
        |> List.map (fun delivery -> streamOf delivery.Endpoint, List.ofSeq delivery.Bytes)

    /// One write as a client issues it: admitted, and then given the bytes the
    /// admission asked for. A pipe the process made can lose its reader, and a
    /// write into it then raises SIGPIPE, which the process ignores here
    /// (`initialSystem`), so the write answers EPIPE. `Error None` is a write
    /// that sleeps, and `Error (Some refusal)` one the system refused.
    let private write
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<int, string>)
        : Result<WriteAnswer * UnixSystem<int, string>, WriteRefusal option>
        =
        match WriteOutcomes.admitThenWrite system.Leader fd UserBuffer.Mapped bytes system with
        | Error refusal -> Error (Some refusal)
        | Ok (WriteOutcome.Returns (answer, after))
        | Ok (WriteOutcome.ReturnsRaising (answer, _, after)) -> Ok (answer, after)
        | Ok (WriteOutcome.ProcessEnded _ as outcome) ->
            failwith $"a write ended the process, whose SIGPIPE is ignored: %A{outcome}"
        | Ok (WriteOutcome.WouldBlock _) -> Error None
        | Ok (WriteOutcome.Restarts _ as outcome) -> failwith $"a write that never slept restarted: %A{outcome}"

    /// What the earlier standard streams answered, if the descriptor named one
    /// (`None` for a descriptor the reference does not model), and the
    /// reference afterwards. `Error ()` is the earlier model's refusal: a
    /// non-blocking write longer than an empty pipe holds.
    let private referenceStep
        (index : int)
        (op : Op)
        (reference : Reference)
        : Result<Answer option * Reference, unit>
        =
        match op with
        | Op.Write (fd, count) ->
            match referenceDescription fd reference with
            | None -> Ok (Some (Answer.WriteFailed UnixError.EBADF), reference)
            | Some {
                       Kind = ReferenceKind.Other
                   } -> Ok (None, reference)
            | Some {
                       Kind = ReferenceKind.Stream Stream.Input
                   } -> Ok (Some (Answer.WriteFailed UnixError.EBADF), reference)
            | Some {
                       Kind = ReferenceKind.Stream stream
                       NonBlocking = nonBlocking
                   } ->
                if count = 0 then
                    Ok (Some (Answer.Wrote 0L), reference)
                elif nonBlocking && count > 65536 then
                    Error ()
                else
                    Ok (
                        Some (Answer.Wrote (int64 count)),
                        { reference with
                            Log = (stream, List.ofSeq (payload index count)) :: reference.Log
                        }
                    )
        | Op.Read (fd, _) ->
            match referenceDescription fd reference with
            | None -> Ok (Some (Answer.ReadFailed UnixError.EBADF), reference)
            | Some {
                       Kind = ReferenceKind.Other
                   } -> Ok (None, reference)
            | Some {
                       Kind = ReferenceKind.Stream Stream.Input
                   } -> Ok (Some (Answer.ReadBytes 0), reference)
            | Some {
                       Kind = ReferenceKind.Stream _
                   } -> Ok (Some (Answer.ReadFailed UnixError.EBADF), reference)
        | Op.Dup fd ->
            match Map.tryFind fd reference.Fds with
            | None -> Ok (Some (Answer.Syscall (SyscallAnswer.Failed UnixError.EBADF)), reference)
            | Some id ->
                let newFd = lowestFree reference.Fds

                Ok (
                    Some (Answer.Syscall (SyscallAnswer.Completed (int64 newFd))),
                    { reference with
                        Fds = Map.add newFd id reference.Fds
                    }
                )
        | Op.Close fd ->
            match Map.tryFind fd reference.Fds with
            | None -> Ok (Some (Answer.Syscall (SyscallAnswer.Failed UnixError.EBADF)), reference)
            | Some id ->
                let fds = Map.remove fd reference.Fds

                let descriptions =
                    if fds |> Map.exists (fun _ other -> other = id) then
                        reference.Descriptions
                    else
                        Map.remove id reference.Descriptions

                Ok (
                    Some (Answer.Syscall (SyscallAnswer.Completed 0L)),
                    { reference with
                        Fds = fds
                        Descriptions = descriptions
                    }
                )
        | Op.SetNonBlocking (fd, value) ->
            match Map.tryFind fd reference.Fds with
            | None -> Ok (Some (Answer.NonBlocking (SetNonBlockingAnswer.Failed UnixError.EBADF)), reference)
            | Some id ->
                let description = reference.Descriptions.[id]

                let answer =
                    match description.Kind with
                    | ReferenceKind.Stream _ -> Some (Answer.NonBlocking SetNonBlockingAnswer.Set)
                    | ReferenceKind.Other -> None

                Ok (
                    answer,
                    { reference with
                        Descriptions =
                            Map.add
                                id
                                { description with
                                    NonBlocking = value
                                }
                                reference.Descriptions
                    }
                )
        | Op.Poll fd ->
            match referenceDescription fd reference with
            | Some {
                       Kind = ReferenceKind.Stream Stream.Input
                   } -> Ok (Some (Answer.Level EpollEvents.Hup), reference)
            | Some {
                       Kind = ReferenceKind.Stream _
                   } -> Ok (Some (Answer.Level (EpollEvents.Out ||| EpollEvents.WrNorm)), reference)
            | Some {
                       Kind = ReferenceKind.Other
                   }
            | None -> Ok (None, reference)
        | Op.Pipe2 ->
            let readFd = lowestFree reference.Fds
            let fds = Map.add readFd reference.NextId reference.Fds
            let writeFd = lowestFree fds

            let other =
                {
                    Kind = ReferenceKind.Other
                    NonBlocking = true
                }

            Ok (
                None,
                { reference with
                    Fds = Map.add writeFd (reference.NextId + 1) fds
                    Descriptions =
                        reference.Descriptions
                        |> Map.add reference.NextId other
                        |> Map.add (reference.NextId + 1) other
                    NextId = reference.NextId + 2
                }
            )

    /// A process launched with `UnixSystem.pipedStandardStreams`, ignoring
    /// SIGPIPE.
    let private initialSystem (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system = UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

        { system with
            Process =
                { system.Process with
                    Signals = SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore system.Process.Signals
                }
        }

    /// What the launched streams answer, or `None` where the system refused or
    /// the call would sleep, and the system afterwards.
    let private systemStep
        (index : int)
        (op : Op)
        (system : UnixSystem<int, string>)
        : (Answer * UnixSystem<int, string>) option
        =
        match op with
        | Op.Write (fd, count) ->
            match write fd (payload index count) system with
            | Ok (WriteAnswer.Completed written, after) -> Some (Answer.Wrote written, after)
            | Ok (WriteAnswer.Failed error, after) -> Some (Answer.WriteFailed error, after)
            | Error _ -> None
        | Op.Read (fd, count) ->
            match UnixReadWrite.read system.Leader fd UserBuffer.Mapped (uint64 count) system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                Some (Answer.ReadBytes bytes.Length, after)
            | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), after) -> Some (Answer.ReadFailed error, after)
            // A read that sleeps, on a pipe the process made, which the
            // earlier streams had no notion of.
            | Ok (ReadOutcome.WouldBlock _, _) -> None
            | Ok (ReadOutcome.Restarts, _) -> failwith "a read that never slept restarted"
            | Error _ -> None
        | Op.Dup fd ->
            let answer, after = UnixDescriptor.dup fd system
            Some (Answer.Syscall answer, after)
        | Op.Close fd ->
            match UnixDescriptor.close fd system with
            | Ok (answer, after) -> Some (Answer.Syscall answer, after)
            | Error _ -> None
        | Op.SetNonBlocking (fd, value) ->
            let answer, after = UnixSocket.setNonBlocking fd value system
            Some (Answer.NonBlocking answer, after)
        | Op.Poll fd ->
            match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
            | None -> Some (Answer.Level 0u, system)
            | Some id -> Some (Answer.Level (LinuxReadiness.ofDescription id system), system)
        | Op.Pipe2 ->
            match UnixPipe.pipe2 (flavourNonBlock system.Machine.UnixPlatform) UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created _, after) -> Some (Answer.Syscall (SyscallAnswer.Completed 0L), after)
            | Ok (Pipe2Answer.Failed error, after) -> Some (Answer.Syscall (SyscallAnswer.Failed error), after)
            | Error _ -> None

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private opGen : Gen<Op> =
        let fd = Gen.choose (0, 6)

        let count =
            Gen.frequency
                [
                    8, Gen.choose (0, 64)
                    2, Gen.choose (0, 70000)
                    1, Gen.elements [ 0 ; 1 ; 4096 ; 65536 ; 65537 ; 200000 ]
                ]

        Gen.frequency
            [
                6, Gen.map2 (fun fd count -> Op.Write (fd, count)) fd count
                2, Gen.map2 (fun fd count -> Op.Read (fd, count)) fd count
                2, Gen.map Op.Dup fd
                2, Gen.map Op.Close fd
                2, Gen.map2 (fun fd value -> Op.SetNonBlocking (fd, value)) fd (Gen.elements [ true ; false ])
                1, Gen.map Op.Poll fd
                1, Gen.constant Op.Pipe2
            ]

    [<Test>]
    let ``the launched standard streams answer and log as the earlier standard streams did`` () : unit =
        let property (platform : SimulatedUnixPlatform, ops : Op list) : unit =
            let linux =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> true
                | SimulatedUnixFlavour.Darwin -> false

            let rec run (index : int) (ops : Op list) (reference : Reference) (system : UnixSystem<int, string>) =
                UnixSystem.checkInvariants system |> shouldEqual []
                deliveredLog system |> shouldEqual (List.rev reference.Log)

                match ops with
                | [] -> ()
                // Darwin's readiness is not modelled, by either side.
                | Op.Poll _ :: rest when not linux -> run (index + 1) rest reference system
                | op :: rest ->

                match referenceStep index op reference with
                // The earlier process ended here, refused; nothing after it
                // happened, so nothing after it can be compared.
                | Error () -> ()
                | Ok (expected, reference) ->

                match systemStep index op system, expected with
                | None, Some expected ->
                    failwith
                        $"step %d{index}, %A{op}: the reference answered %A{expected}, the launched streams refused"
                // Both refuse an operation on a pipe the process made where a
                // real kernel would wait or signal, and the process ends there.
                | None, None -> ()
                | Some (answer, after), expected ->
                    match expected with
                    | Some expected when answer <> expected ->
                        failwith
                            $"step %d{index}, %A{op}: the reference answered %A{expected}, the launched streams %A{answer}"
                    | Some _
                    | None -> run (index + 1) rest reference after

            run 0 ops initialReference (initialSystem platform)

        let gen =
            Gen.zip (Gen.elements platforms) (Gen.listOf opGen |> Gen.map (List.truncate 60))

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 400, Prop.forAll (Arb.fromGen gen) property)

    /// What one edge-triggered `EPOLLOUT` registration on standard output
    /// reports at once, and then after each of `writes`: the rows of
    /// drained-pipe-epoll.c, which wrote into a pipe and drained it at once, on
    /// Linux 6.18.5.
    let private edgesAfter (writes : int list) : bool * bool list =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let port, system =
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok (port, system)) -> port, system
            | other -> failwith $"%A{other}"

        let portId =
            match FileDescriptorRegistry.tryFindId port system.Process.FileDescriptors with
            | Some id -> id
            | None -> failwith "the port is not open"

        let system =
            match
                UnixPoll.epollCtl
                    port
                    1
                    1
                    (EpollEventArgument.Readable (EpollEvents.Out ||| EpollEvents.EdgeTriggered, 7UL))
                    system
            with
            | Ok (EpollCtlAnswer.Changed, system) -> system
            | other -> failwith $"%A{other}"

        let reported (system : UnixSystem<int, string>) : bool * UnixSystem<int, string> =
            let rows, system = SocketEventPort.drain portId 4 system
            not rows.IsEmpty, system

        let atAdd, system = reported system

        let _, edges =
            ((system, []), writes)
            ||> List.fold (fun (system, edges) count ->
                match write 1 (payload count count) system with
                | Ok (WriteAnswer.Completed written, system) when written = int64 count ->
                    UnixSystem.checkInvariants system |> shouldEqual []
                    let edge, system = reported system
                    system, edge :: edges
                | other -> failwith $"write of %d{count} answered %A{Result.map fst other}"
            )

        atAdd, List.rev edges

    [<Test>]
    let ``standard output's write end is woken by the client's drain exactly when a write filled the pipe`` () : unit =
        // The registration is ready at once, as on the host. 61440 bytes take
        // fifteen of the sixteen slots and leave one free; 65535 and 65536 take
        // all sixteen.
        edgesAfter [ 1 ; 100 ; 4096 ; 4097 ; 32768 ; 61440 ; 65535 ; 65536 ; 1 ; 200000 ]
        |> shouldEqual (true, [ false ; false ; false ; false ; false ; false ; true ; true ; false ; true ])

    [<Test>]
    let ``standard input and output may be registered with epoll, and a pipe the process made may not`` () : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let port, system =
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok (port, system)) -> port, system
            | other -> failwith $"%A{other}"

        let edge = EpollEvents.In ||| EpollEvents.Out ||| EpollEvents.EdgeTriggered

        for fd in [ 0 ; 1 ; 2 ] do
            match UnixPoll.epollCtl port 1 fd (EpollEventArgument.Readable (edge, 0UL)) system with
            | Ok (EpollCtlAnswer.Changed, _) -> ()
            | other -> failwith $"fd %d{fd}: %A{other}"

        let readFd, system =
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (readFd, _), system) -> readFd, system
            | other -> failwith $"%A{other}"

        match UnixPoll.epollCtl port 1 readFd (EpollEventArgument.Readable (edge, 0UL)) system with
        | Error (EpollCtlRefusal.PipeTarget refused) -> refused |> shouldEqual readFd
        | other -> failwith $"%A{other}"

    let private pipeOf (fd : int) (system : UnixSystem<int, string>) : PipeId =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Pipe (pipeId, _)) -> pipeId
        | other -> failwith $"fd %d{fd} is %A{other}, not a pipe end"

    /// Closing every launched descriptor frees standard input's pipe, whose
    /// writer had already gone, and leaves the output pipes, whose read ends the
    /// client still holds.
    [<Test>]
    let ``a launched pipe lives while the client holds its far end`` () : unit =
        for platform in platforms do
            let system : UnixSystem<int, string> =
                UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let input, output, error = pipeOf 0 system, pipeOf 1 system, pipeOf 2 system

            let closed =
                (system, [ 0 ; 1 ; 2 ])
                ||> List.fold (fun system fd ->
                    match UnixDescriptor.close fd system with
                    | Ok (SyscallAnswer.Completed 0L, system) -> system
                    | other -> failwith $"close %d{fd}: %A{other}"
                )

            closed.Machine.Pipes
            |> Map.keys
            |> Set.ofSeq
            |> shouldEqual (Set.ofList [ output ; error ])

            Map.containsKey input closed.Machine.Pipes |> shouldEqual false
            UnixSystem.checkInvariants closed |> shouldEqual []

    [<Test>]
    let ``a drained pipe holding bytes is a defect`` () : unit =
        for platform in platforms do
            let system : UnixSystem<int, string> =
                UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let output = pipeOf 1 system
            let pipe = UnixMachineState.pipe output system.Machine

            let forged =
                { system with
                    Machine =
                        { system.Machine with
                            Pipes =
                                Map.add
                                    output
                                    { pipe with
                                        Buffer = snd (PipeBuffer.write (payload 0 5) pipe.Buffer)
                                    }
                                    system.Machine.Pipes
                        }
                }

            UnixSystem.checkInvariants forged
            |> shouldEqual [ UnixSystemDefect.DrainedPipeHoldsBytes (output, 5) ]
