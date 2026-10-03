namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One syscall on a pipe, by descriptor, as the reference-model property below
/// generates them. Descriptor numbers index into the descriptors the property has
/// open, so that a call can name either end, a `dup`, or a closed number.
[<RequireQualifiedAccess>]
type PipeOp =
    | Write of fd : int * count : int * mapped : bool
    | Read of fd : int * count : int * mapped : bool
    | SetNonBlocking of fd : int * value : bool
    | Dup of fd : int
    | Close of fd : int
    | Poll of fd : int
    | BytesAvailable of fd : int
    | FStat of fd : int

/// `UnixPipe.pipe2`, and every syscall on a pipe's ends.
///
/// The property at the heart of it holds the syscalls to a naive reference: a
/// pipe as `PipeBufferReference` states each flavour's buffer, with each end open
/// while some descriptor names it, and the measured rule for each call written
/// out again in a different shape. `TestPipeAgainstHost` holds both to the
/// kernel running the suite.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPipe =

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private flavourOf (system : UnixSystem<int, string>) : SimulatedUnixFlavour =
        SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

    /// Each flavour's `O_NONBLOCK`, as its `<fcntl.h>` numbers it.
    let private nonBlockFlag (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x800
        | SimulatedUnixFlavour.Darwin -> 0x4

    let private pipeOrFail (flags : int) (system : UnixSystem<int, string>) : (int * int) * UnixSystem<int, string> =
        match UnixPipe.pipe2 flags UserBuffer.Mapped system with
        | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> (readFd, writeFd), system
        | other -> failwith $"pipe2 0x%x{flags} did not make a pipe: %A{other}"

    /// The pipes the process made, leaving out the ones it was launched with.
    let private madePipes (system : UnixSystem<int, string>) : Map<PipeId, PipeState> =
        system.Machine.Pipes
        |> Map.filter (fun _ pipe ->
            match pipe.Origin with
            | PipeOrigin.Made _ -> true
            | PipeOrigin.Launched _ -> false
        )

    /// The pipe descriptor `fd` names an end of.
    let private pipeOf (fd : int) (system : UnixSystem<int, string>) : PipeId =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Pipe (pipeId, _)) -> pipeId
        | other -> failwith $"fd %d{fd} is %A{other}, not a pipe end"

    /// `pipe` reporting `inodes`. Only a pipe the process made reports any.
    let private withInodes (inodes : PipeInodes) (pipe : PipeState) : PipeState =
        match pipe.Origin with
        | PipeOrigin.Made status ->
            { pipe with
                Origin =
                    PipeOrigin.Made
                        { status with
                            Inodes = inodes
                        }
            }
        | PipeOrigin.Launched _ -> failwith "a launched pipe reports no inodes"

    let private assertSound (context : string) (system : UnixSystem<int, string>) : unit =
        match UnixSystem.checkInvariants system with
        | [] -> ()
        | defects -> failwith $"%s{context}: %A{defects}"

        FileDescriptorRegistry.assertInvariants context system.Process.FileDescriptors
        |> ignore

    let private payload (start : int) (count : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (Array.init count (fun i -> byte ((start + i) % 251)))

    let private writeOrFail
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<int, string>)
        : WriteAnswer * UnixSystem<int, string>
        =
        match WriteOutcomes.admitWrite fd UserBuffer.Mapped (uint64 bytes.Length) system with
        | Error refusal -> failwith $"admitWrite refused: %A{refusal}"
        | Ok (WriteAdmission.Answered answer, system) -> answer, system
        | Ok (WriteAdmission.Transfer count, admitted) ->
            if count > bytes.Length then
                failwith $"admitWrite asked for %d{count} of %d{bytes.Length} bytes"

            match WriteOutcomes.write fd (ImmutableArray.Create (bytes, 0, count)) admitted with
            | Error refusal -> failwith $"write refused: %A{refusal}"
            | Ok result -> result
        | Ok (WriteAdmission.TransferThenSleep _ as admission, _) -> failwith $"the write would sleep: %A{admission}"

    let private readOrFail
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : ReadAnswer * UnixSystem<int, string>
        =
        match ReadOutcomes.read fd UserBuffer.Mapped (uint64 count) system with
        | Error refusal -> failwith $"read refused: %A{refusal}"
        | Ok result -> result

    let private fstatOrFail (fd : int) (system : UnixSystem<int, string>) : FileStatus =
        match UnixPathResolution.fstat fd system with
        | Ok (FileStatusAnswer.Reported status) -> status
        | other -> failwith $"fstat of fd %d{fd}: %A{other}"

    // --- the reference ---

    /// A pipe's buffer under either flavour's reference rule.
    type private BufferModel =
        | Linux of PipeBufferReference.Linux
        | Darwin of PipeBufferReference.Darwin

    let private bufferModelFor (platform : SimulatedUnixPlatform) : BufferModel =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            BufferModel.Linux (
                PipeBufferReference.linuxEmpty (SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform))
            )
        | SimulatedUnixFlavour.Darwin -> BufferModel.Darwin PipeBufferReference.darwinEmpty

    let private modelWrite (bytes : byte list) (model : BufferModel) : int * BufferModel =
        match model with
        | BufferModel.Linux s ->
            let n, s = PipeBufferReference.linuxWrite bytes s
            n, BufferModel.Linux s
        | BufferModel.Darwin s ->
            let n, s = PipeBufferReference.darwinWrite bytes s
            n, BufferModel.Darwin s

    let private modelRead (count : int) (model : BufferModel) : byte list * BufferModel =
        match model with
        | BufferModel.Linux s ->
            let b, s = PipeBufferReference.linuxRead count s
            b, BufferModel.Linux s
        | BufferModel.Darwin s ->
            let b, s = PipeBufferReference.darwinRead count s
            b, BufferModel.Darwin s

    /// `model` as a write that took none of its bytes leaves it, given `grown`,
    /// the model after that write had taken what it could: the size `grown`
    /// reached, and the bytes `model` held.
    let private modelWithoutTaking (grown : BufferModel) (model : BufferModel) : BufferModel =
        match grown, model with
        | BufferModel.Darwin grown, BufferModel.Darwin model ->
            BufferModel.Darwin
                { model with
                    Size = grown.Size
                }
        | _, model -> model

    let private modelHeld (model : BufferModel) : int =
        match model with
        | BufferModel.Linux s -> List.length s.Bytes
        | BufferModel.Darwin s -> List.length s.Bytes

    let private modelWritable (model : BufferModel) : bool =
        match model with
        | BufferModel.Linux s -> PipeBufferReference.linuxWritable s
        | BufferModel.Darwin s -> PipeBufferReference.darwinWritable s

    /// What the property expects of one pipe: its buffer, which open descriptor
    /// names which end and which shared description, each description's
    /// `O_NONBLOCK`, and the timestamps each flavour moves.
    type private Reference =
        {
            Buffer : BufferModel
            /// Open descriptor to (end, description group).
            Fds : Map<int, PipeEnd * int>
            NonBlocking : Map<int, bool>
            Offered : int
            ReadAccess : UnixTimestamp
            Modified : UnixTimestamp
            Created : UnixTimestamp
        }

    let private endOpen (pipeEnd : PipeEnd) (r : Reference) : bool =
        r.Fds |> Map.exists (fun _ (e, _) -> e = pipeEnd)

    let private opGen : Gen<PipeOp> =
        let fd = Gen.choose (0, 9)

        let count =
            Gen.frequency
                [
                    3, Gen.choose (0, 20)
                    2, Gen.elements [ 511 ; 512 ; 513 ; 4095 ; 4096 ; 4097 ; 16384 ; 65536 ; 70000 ]
                    2, Gen.choose (1, 9000)
                ]

        Gen.frequency
            [
                5,
                Gen.map3
                    (fun fd count mapped -> PipeOp.Write (fd, count, mapped))
                    fd
                    count
                    (Gen.frequency [ 5, Gen.constant true ; 1, Gen.constant false ])
                4,
                Gen.map3
                    (fun fd count mapped -> PipeOp.Read (fd, count, mapped))
                    fd
                    count
                    (Gen.frequency [ 5, Gen.constant true ; 1, Gen.constant false ])
                2, Gen.map2 (fun fd value -> PipeOp.SetNonBlocking (fd, value)) fd (Gen.elements [ true ; false ])
                1, Gen.map PipeOp.Dup fd
                1, Gen.map PipeOp.Close fd
                1, Gen.map PipeOp.Poll fd
                1, Gen.map PipeOp.BytesAvailable fd
                1, Gen.map PipeOp.FStat fd
            ]

    /// `system` with `SIGPIPE`'s disposition `disposition`.
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

    /// `system` with `signals` in place of its own.
    let private withSignals
        (signals : SignalState<int, string>)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    Signals = signals
                }
        }

    /// The `SIGPIPE` a write by `task` into a pipe with no reader raises: the
    /// writing task's own on Linux, and the process's on Darwin.
    let private sigPipeFrom (flavour : SimulatedUnixFlavour) (task : int) : PendingSignal<int> =
        {
            Signal = Signal.SIGPIPE
            Target =
                match flavour with
                | SimulatedUnixFlavour.Linux -> ValueSome task
                | SimulatedUnixFlavour.Darwin -> ValueNone
        }

    let private sigPipeDispositions : SignalDisposition<string> list =
        [
            SignalDisposition.Default
            SignalDisposition.Ignore
            SignalDisposition.Catch (SignalCatch.ofHandler "on SIGPIPE")
        ]

    [<Test>]
    let ``every call on a pipe answers as the reference says`` () : unit =
        let property
            (
                platform : SimulatedUnixPlatform,
                startNonBlocking : bool,
                sigPipe : SignalDisposition<string>,
                ops : PipeOp list
            )
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform
            let darwin = flavour = SimulatedUnixFlavour.Darwin

            let flags = if startNonBlocking then nonBlockFlag flavour else 0

            // Without the standard streams, so that every descriptor the
            // property can name is one of the pipe's or a dup of one.
            let bare =
                (systemOn platform, [ 0 ; 1 ; 2 ])
                ||> List.fold (fun system fd ->
                    match UnixDescriptor.close fd system with
                    | Ok (_, system) -> system
                    | Error refusal -> failwith $"%A{refusal}"
                )
                |> withSigPipe sigPipe

            let (readFd, writeFd), system = pipeOrFail flags bare
            let created = UnixMachineState.realtime system.Machine
            let mutable system = system
            let mutable nextGroup = 2
            // Set once a write's SIGPIPE has ended the process, after which
            // there is nothing left to call.
            let mutable ended = false
            // How many reads have taken bytes from the pipe.
            let mutable reads = 0L

            let mutable reference =
                {
                    Buffer = bufferModelFor platform
                    Fds = Map.ofList [ readFd, (PipeEnd.Read, 0) ; writeFd, (PipeEnd.Write, 1) ]
                    NonBlocking = Map.ofList [ 0, startNonBlocking ; 1, startNonBlocking ]
                    Offered = 0
                    ReadAccess = created
                    Modified = created
                    Created = created
                }

            for i, op in List.indexed ops |> Seq.takeWhile (fun _ -> not ended) do
                // Each call at its own instant, so that a timestamp that moves is
                // seen to.
                system <-
                    { system with
                        Machine = UnixMachineState.advanceClock 1000L system.Machine
                    }

                let now = UnixMachineState.realtime system.Machine
                let where = $"%O{platform}, call %d{i} (%A{op})"

                let nonBlockingOf (fd : int) =
                    reference.NonBlocking.[snd reference.Fds.[fd]]

                match op with
                | PipeOp.Write (fd, count, mapped) ->
                    let buffer =
                        if mapped then
                            UserBuffer.Mapped
                        else
                            UserBuffer.Unmapped 8UL

                    let mutable transferred = None

                    let actual : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal> =
                        match UnixReadWrite.admitWrite system.Leader fd buffer (uint64 count) system with
                        | Error refusal -> Error refusal
                        | Ok (WriteOutcome.Returns (WriteAdmission.Answered answer, after)) ->
                            Ok (WriteOutcome.Returns (answer, after))
                        | Ok (WriteOutcome.ReturnsRaising (WriteAdmission.Answered answer, signal, after)) ->
                            Ok (WriteOutcome.ReturnsRaising (answer, signal, after))
                        | Ok (WriteOutcome.ProcessEnded endedProcess) -> Ok (WriteOutcome.ProcessEnded endedProcess)
                        | Ok (WriteOutcome.WouldBlock (condition, after)) ->
                            Ok (WriteOutcome.WouldBlock (condition, after))
                        | Ok (WriteOutcome.Restarts _) as other ->
                            failwith $"%s{where}: a write that never slept restarted: %A{other}"
                        | Ok (WriteOutcome.Returns (WriteAdmission.Transfer n, admitted)) ->
                            transferred <- Some n
                            UnixReadWrite.write system.Leader fd (payload reference.Offered n) admitted
                        | Ok (WriteOutcome.Returns (WriteAdmission.TransferThenSleep (n, total), admitted)) ->
                            transferred <- Some n
                            total |> shouldEqual count

                            UnixReadWrite.writeThenSleep system.Leader fd total (payload reference.Offered n) admitted
                        | Ok (WriteOutcome.ReturnsRaising (WriteAdmission.Transfer _, _, _))
                        | Ok (WriteOutcome.ReturnsRaising (WriteAdmission.TransferThenSleep _, _, _)) as other ->
                            failwith $"%s{where}: an admission that raised a signal asked for bytes: %A{other}"

                    match Map.tryFind fd reference.Fds with
                    | None ->
                        match actual with
                        | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EBADF, after)) ->
                            after |> shouldEqual system
                        | other -> failwith $"%s{where}: expected EBADF, got %A{other}"
                    | Some (PipeEnd.Read, _) ->
                        match actual with
                        | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EBADF, after)) ->
                            after |> shouldEqual system
                        | other -> failwith $"%s{where}: expected EBADF on the read end, got %A{other}"
                    | Some (PipeEnd.Write, _) ->

                    let nonBlocking = nonBlockingOf fd
                    let readerOpen = endOpen PipeEnd.Read reference

                    // A write with no reader answers EPIPE and raises SIGPIPE,
                    // except Linux's zero-length one; it takes nothing and moves
                    // no timestamp, so all it changes is the signal state.
                    if not readerOpen && not (flavour = SimulatedUnixFlavour.Linux && count = 0) then
                        let raised = sigPipeFrom flavour system.Leader

                        match sigPipe, actual with
                        | SignalDisposition.Default, Ok (WriteOutcome.ProcessEnded endedProcess) ->
                            endedProcess.Termination
                            |> shouldEqual (ProcessTermination.Signaled (Signal.SIGPIPE, false))

                            endedProcess.Machine |> shouldEqual system.Machine
                            ended <- true
                        | SignalDisposition.Ignore,
                          Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, signal, after)) ->
                            signal |> shouldEqual raised
                            after |> shouldEqual system
                        | SignalDisposition.Catch _,
                          Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, signal, after)) ->
                            signal |> shouldEqual raised
                            // Pending once, however many writes raised it.
                            SignalState.pending after.Process.Signals |> shouldEqual [ raised ]
                            withSignals system.Process.Signals after |> shouldEqual system
                            system <- after
                        | _ -> failwith $"%s{where}: expected EPIPE and SIGPIPE under %A{sigPipe}, got %A{actual}"
                    else

                    let takes, grown =
                        modelWrite (List.ofSeq (payload reference.Offered count)) reference.Buffer

                    // What the reference expects: an answer (and whether it reached
                    // the pipe), or a refusal.
                    let expected : Result<WriteAnswer * bool, unit> =
                        if flavour = SimulatedUnixFlavour.Linux && count = 0 then
                            Ok (WriteAnswer.Completed 0L, false)
                        elif count = 0 then
                            Ok (WriteAnswer.Completed 0L, true)
                        elif takes = 0 then
                            if nonBlocking then
                                Ok (WriteAnswer.Failed UnixError.EAGAIN, true)
                            else
                                Error ()
                        elif not mapped then
                            Ok (WriteAnswer.Failed UnixError.EFAULT, true)
                        elif takes < count && not nonBlocking then
                            Error ()
                        else
                            Ok (WriteAnswer.Completed (int64 takes), true)

                    match expected, actual with
                    | Error (), Ok (WriteOutcome.WouldBlock (condition, after)) when readerOpen ->
                        // The write sleeps, having put in what fits. The
                        // property's one task is then asleep, so the call is
                        // checked here and not kept: the sequence carries on
                        // from the system it was made in.
                        let writer =
                            FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors |> Option.get

                        let parked =
                            {
                                Writer = writer
                                Buffer = buffer
                                Count = count
                                Written = takes
                                ReadsSeen = reads
                            }

                        UnixTaskTable.parkedFor system.Leader after.Tasks
                        |> shouldEqual (Some (ParkedSyscall.PipeWrite parked))

                        condition |> shouldEqual (WakeCondition.ofPark (ParkedSyscall.PipeWrite parked))

                        let pipe = UnixMachineState.pipe (pipeOf fd system) after.Machine
                        PipeBuffer.held pipe.Buffer |> shouldEqual (modelHeld reference.Buffer + takes)
                        // A write that sleeps reads only the bytes it puts in,
                        // and the rest only once it can put them in.
                        transferred |> shouldEqual (if takes = 0 then None else Some takes)
                    | Ok (answer, _), Ok (WriteOutcome.Returns (actualAnswer, after)) when answer = actualAnswer ->
                        match answer with
                        | WriteAnswer.Completed n when n > 0L ->
                            // The caller extracts only what the pipe takes.
                            transferred |> shouldEqual (Some takes)

                            reference <-
                                { reference with
                                    Buffer = grown
                                    Offered = reference.Offered + takes
                                }
                        | WriteAnswer.Failed UnixError.EAGAIN
                        | WriteAnswer.Failed UnixError.EFAULT ->
                            // Nothing taken, but Darwin's buffer grows as the
                            // write would have grown it.
                            reference <-
                                { reference with
                                    Buffer = modelWithoutTaking grown reference.Buffer
                                }
                        | _ -> ()

                        if darwin then
                            reference <-
                                { reference with
                                    Modified = now
                                }

                        system <- after
                    | _ -> failwith $"%s{where}: expected %A{expected}, got %A{actual}"
                | PipeOp.Read (fd, count, mapped) ->
                    let buffer =
                        if mapped then
                            UserBuffer.Mapped
                        else
                            UserBuffer.Unmapped 8UL

                    let actual = UnixReadWrite.read system.Leader fd buffer (uint64 count) system

                    match Map.tryFind fd reference.Fds with
                    | None
                    | Some (PipeEnd.Write, _) ->
                        match actual with
                        | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EBADF), after) ->
                            after |> shouldEqual system
                        | other -> failwith $"%s{where}: expected EBADF, got %A{other}"
                    | Some (PipeEnd.Read, _) ->

                    let held = modelHeld reference.Buffer

                    let expected : Result<ReadAnswer, unit> =
                        if count = 0 then
                            Ok (ReadAnswer.Completed ImmutableArray.Empty)
                        elif held = 0 then
                            if not (endOpen PipeEnd.Write reference) then
                                Ok (ReadAnswer.Completed ImmutableArray.Empty)
                            elif nonBlockingOf fd then
                                Ok (ReadAnswer.Failed UnixError.EAGAIN)
                            else
                                Error ()
                        elif not mapped then
                            Ok (ReadAnswer.Failed UnixError.EFAULT)
                        else
                            let bytes, _ = modelRead count reference.Buffer
                            Ok (ReadAnswer.Completed (ImmutableArray.CreateRange bytes))

                    match expected, actual with
                    | Error (), Ok (ReadOutcome.WouldBlock condition, after) ->
                        // The read sleeps, having touched nothing but the
                        // timestamps it moves on Darwin. Checked here and not
                        // kept, as a sleeping write is.
                        let parked =
                            {
                                Reader =
                                    FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors |> Option.get
                                Buffer = buffer
                                Count = count
                            }

                        UnixTaskTable.parkedFor system.Leader after.Tasks
                        |> shouldEqual (Some (ParkedSyscall.PipeRead parked))

                        condition |> shouldEqual (WakeCondition.ofPark (ParkedSyscall.PipeRead parked))

                        let pipe = UnixMachineState.pipe (pipeOf fd system) after.Machine
                        PipeBuffer.held pipe.Buffer |> shouldEqual 0
                    | Ok (ReadAnswer.Completed expectedBytes),
                      Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                        List.ofSeq bytes |> shouldEqual (List.ofSeq expectedBytes)
                        let _, drained = modelRead bytes.Length reference.Buffer

                        if not bytes.IsEmpty then
                            reads <- reads + 1L

                        reference <-
                            { reference with
                                Buffer = drained
                                ReadAccess = if darwin then now else reference.ReadAccess
                            }

                        system <- after
                    | Ok (ReadAnswer.Failed error), Ok (ReadOutcome.Answered (ReadAnswer.Failed actualError), after) when
                        error = actualError
                        ->
                        reference <-
                            { reference with
                                ReadAccess = if darwin then now else reference.ReadAccess
                            }

                        system <- after
                    | _ -> failwith $"%s{where}: expected %A{expected}, got %A{actual}"
                | PipeOp.SetNonBlocking (fd, value) ->
                    let answer, after = UnixSocket.setNonBlocking fd value system

                    match Map.tryFind fd reference.Fds with
                    | None -> answer |> shouldEqual (SetNonBlockingAnswer.Failed UnixError.EBADF)
                    | Some (_, group) ->
                        answer |> shouldEqual SetNonBlockingAnswer.Set

                        reference <-
                            { reference with
                                NonBlocking = Map.add group value reference.NonBlocking
                            }

                    system <- after
                | PipeOp.Dup fd ->
                    let answer, after = UnixDescriptor.dup fd system

                    match Map.tryFind fd reference.Fds, answer with
                    | None, SyscallAnswer.Failed UnixError.EBADF -> ()
                    | Some shared, SyscallAnswer.Completed newFd ->
                        reference <-
                            { reference with
                                Fds = Map.add (int newFd) shared reference.Fds
                            }
                    | other -> failwith $"%s{where}: %A{other}"

                    system <- after
                | PipeOp.Close fd ->
                    match UnixDescriptor.close fd system, Map.tryFind fd reference.Fds with
                    | Ok (SyscallAnswer.Failed UnixError.EBADF, _), None -> ()
                    | Ok (SyscallAnswer.Completed 0L, after), Some _ ->
                        reference <-
                            { reference with
                                Fds = Map.remove fd reference.Fds
                            }

                        system <- after
                    | other -> failwith $"%s{where}: %A{other}"
                | PipeOp.Poll fd ->
                    match flavour with
                    | SimulatedUnixFlavour.Darwin -> ()
                    | SimulatedUnixFlavour.Linux ->

                    match Map.tryFind fd reference.Fds with
                    | None -> ()
                    | Some (pipeEnd, _) ->

                    let id =
                        FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors |> Option.get

                    let expected =
                        match pipeEnd with
                        | PipeEnd.Read ->
                            (if modelHeld reference.Buffer > 0 then 0x41u else 0u)
                            ||| (if endOpen PipeEnd.Write reference then 0u else 0x10u)
                        | PipeEnd.Write ->
                            (if modelWritable reference.Buffer then 0x104u else 0u)
                            ||| (if endOpen PipeEnd.Read reference then 0u else 0x8u)

                    LinuxReadiness.ofDescription id system |> shouldEqual expected
                | PipeOp.BytesAvailable fd ->
                    let actual = UnixDescriptor.bytesAvailable fd UserBuffer.Mapped system

                    match Map.tryFind fd reference.Fds with
                    | None -> actual |> shouldEqual (Ok (BytesAvailableAnswer.Failed UnixError.EBADF))
                    | Some (pipeEnd, _) ->
                        let expected =
                            match flavour, pipeEnd with
                            | SimulatedUnixFlavour.Darwin, PipeEnd.Write -> 0
                            | _ -> modelHeld reference.Buffer

                        actual |> shouldEqual (Ok (BytesAvailableAnswer.Reported expected))
                | PipeOp.FStat fd ->
                    match Map.tryFind fd reference.Fds with
                    | None ->
                        UnixPathResolution.fstat fd system
                        |> shouldEqual (Ok (FileStatusAnswer.Failed UnixError.EBADF))
                    | Some (pipeEnd, _) ->
                        let status = fstatOrFail fd system
                        status.Mode &&& 0o170000 |> shouldEqual 0o010000

                        let expectedSize =
                            match flavour, pipeEnd with
                            | SimulatedUnixFlavour.Linux, _ -> 0L
                            | SimulatedUnixFlavour.Darwin, PipeEnd.Read -> int64 (modelHeld reference.Buffer)
                            | SimulatedUnixFlavour.Darwin, PipeEnd.Write ->
                                if endOpen PipeEnd.Read reference then
                                    int64 (modelHeld reference.Buffer)
                                else
                                    0L

                        status.Size |> shouldEqual expectedSize

                        let expectedAccess =
                            match pipeEnd with
                            | PipeEnd.Read -> reference.ReadAccess
                            | PipeEnd.Write -> reference.Created

                        (status.AccessTime, status.ModificationTime, status.StatusChangeTime)
                        |> shouldEqual (expectedAccess, reference.Modified, reference.Modified)

                assertSound where system

                // The pipe lives exactly while some descriptor names an end.
                Map.count (madePipes system)
                |> shouldEqual (if Map.isEmpty reference.Fds then 0 else 1)

        let gen =
            gen {
                let! platform = Gen.elements platforms
                let! startNonBlocking = Gen.elements [ true ; false ]
                let! sigPipe = Gen.elements sigPipeDispositions
                let! ops = Gen.listOf opGen |> Gen.map (List.truncate 50)
                return platform, startNonBlocking, sigPipe, ops
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 400, Prop.forAll (Arb.fromGen gen) property)

    // --- pipe2's flags ---

    /// What each single bit of `pipe2`'s flag word answered, as
    /// `pipe-syscalls.c` measured it: `None` for EINVAL, `Some true` for a pipe
    /// whose ends carry `O_NONBLOCK`, `Some false` for one whose do not, and the
    /// rest refused. The x86-64 Linux column differs from aarch64's in
    /// `O_DIRECT` alone, which the uapi header numbers 0x4000 there.
    let private singleBitAnswer (platform : SimulatedUnixPlatform) (bit : int) : Result<bool option, unit> =
        let flag = 1 <<< bit

        match SimulatedUnixPlatform.flavour platform, SimulatedUnixPlatform.architecture platform with
        | SimulatedUnixFlavour.Linux, architecture ->
            let direct =
                match architecture with
                | SimulatedUnixArchitecture.Arm64 -> 0x10000
                | SimulatedUnixArchitecture.X64 -> 0x4000

            if flag = 0x800 then Ok (Some true)
            elif flag = 0x80000 then Ok (Some false)
            elif flag = direct || flag = 0x80 then Error ()
            else Ok None
        | SimulatedUnixFlavour.Darwin, _ ->
            if flag = 0x4 then
                Ok (Some true)
            elif flag = 0x1000000 || flag = 0x8000000 then
                Ok (Some false)
            else
                Ok None

    [<Test>]
    let ``pipe2 answers each single flag bit as measured`` () : unit =
        for platform in platforms do
            for bit in 0..31 do
                let flags = 1 <<< bit
                let initial = systemOn platform

                match singleBitAnswer platform bit, UnixPipe.pipe2 flags UserBuffer.Mapped initial with
                | Ok None, Ok (Pipe2Answer.Failed UnixError.EINVAL, after) -> after |> shouldEqual initial
                | Ok (Some nonBlocking), Ok (Pipe2Answer.Created (readFd, writeFd), after) ->
                    (readFd, writeFd) |> shouldEqual (3, 4)
                    UnixSocket.isNonBlocking readFd after |> shouldEqual (Some nonBlocking)
                    UnixSocket.isNonBlocking writeFd after |> shouldEqual (Some nonBlocking)
                    assertSound $"%O{platform} bit %d{bit}" after
                | Error (), Error (Pipe2Refusal.PacketMode _)
                | Error (), Error (Pipe2Refusal.NotificationPipe _) -> ()
                | expected, actual -> failwith $"%O{platform}, bit %d{bit}: expected %A{expected}, got %A{actual}"

    [<Test>]
    let ``pipe2 rejects a flag word exactly when some bit in it is rejected alone`` () : unit =
        let property (platform : SimulatedUnixPlatform, flags : int) : unit =
            let rejected =
                [ 0..31 ]
                |> List.exists (fun bit -> flags &&& (1 <<< bit) <> 0 && singleBitAnswer platform bit = Ok None)

            match UnixPipe.pipe2 flags UserBuffer.Mapped (systemOn platform) with
            | Ok (Pipe2Answer.Failed UnixError.EINVAL, _) when rejected -> ()
            | Ok (Pipe2Answer.Created _, _)
            | Error _ when not rejected -> ()
            | other -> failwith $"%O{platform}, flags 0x%x{flags}: %A{other}"

        let flagsGen =
            Gen.oneof
                [
                    ArbMap.defaults |> ArbMap.generate<int>
                    Gen.subListOf [ 0x4 ; 0x80 ; 0x800 ; 0x4000 ; 0x10000 ; 0x80000 ; 0x1000000 ; 0x8000000 ]
                    |> Gen.map (List.fold (|||) 0)
                ]

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.elements platforms) flagsGen)) property
        )

    [<Test>]
    let ``pipe2 through a bad destination: EFAULT on Linux, leaving nothing behind, and refused on Darwin`` () : unit =
        let linux = systemOn SimulatedUnixPlatform.linuxArm64

        UnixPipe.pipe2 0 (UserBuffer.Unmapped 8UL) linux
        |> shouldEqual (Ok (Pipe2Answer.Failed UnixError.EFAULT, linux))

        // The flags first, on both: measured, an invalid flag word is EINVAL
        // through NULL as through a good array.
        UnixPipe.pipe2 1 (UserBuffer.Unmapped 0UL) linux
        |> shouldEqual (Ok (Pipe2Answer.Failed UnixError.EINVAL, linux))

        let darwin = systemOn SimulatedUnixPlatform.macOsArm64

        UnixPipe.pipe2 0 (UserBuffer.Unmapped 0UL) darwin
        |> shouldEqual (Error Pipe2Refusal.FatalToTheProcess)

        UnixPipe.pipe2 1 (UserBuffer.Unmapped 0UL) darwin
        |> shouldEqual (Ok (Pipe2Answer.Failed UnixError.EINVAL, darwin))

        UnixPipe.pipe2 0 UserBuffer.Opaque linux
        |> shouldEqual (Error (Pipe2Refusal.Buffer BufferRefusal.OpaqueAtTransfer))

    [<Test>]
    let ``pipe2 takes the lowest free descriptor for the read end and the next for the write end`` () : unit =
        for platform in platforms do
            let (r, w), system = pipeOrFail 0 (systemOn platform)
            (r, w) |> shouldEqual (3, 4)

            let system =
                match UnixDescriptor.close r system with
                | Ok (_, system) -> system
                | Error refusal -> failwith $"%A{refusal}"

            let (r, w), _ = pipeOrFail 0 system
            (r, w) |> shouldEqual (3, 5)

    // --- fstat ---

    [<Test>]
    let ``fstat of a Linux pipe: 0600 FIFO, one inode for both ends, the pipe device, and nothing moves`` () : unit =
        let system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> UnixSystem.withCredentials
                "test"
                (Credentials.ofIds (UserId.parseOrFail "test" 1234u) (GroupId.parseOrFail "test" 99u) [])

        let (r, w), system = pipeOrFail 0 system
        let (r2, w2), system = pipeOrFail 0 system

        let system =
            { system with
                Machine = UnixMachineState.advanceClock 5_000_000L system.Machine
            }

        let _, system = writeOrFail w (payload 0 10) system
        let _, system = readOrFail r 3 system

        let read = fstatOrFail r system
        let write = fstatOrFail w system
        read.Mode |> shouldEqual 0o010600
        write.Mode |> shouldEqual 0o010600
        read.Inode |> shouldEqual write.Inode
        (fstatOrFail r2 system).Inode |> shouldEqual (fstatOrFail w2 system).Inode
        (fstatOrFail r2 system).Inode |> shouldNotEqual read.Inode
        read.DeviceId |> shouldEqual 0xcL
        (read.Size, write.Size) |> shouldEqual (0L, 0L)

        (read.UserId, read.GroupId)
        |> shouldEqual (UserId.parseOrFail "test" 1234u, GroupId.parseOrFail "test" 99u)

        read.BirthTime |> shouldEqual None

        for status in [ read ; write ] do
            (status.AccessTime, status.ModificationTime, status.StatusChangeTime)
            |> shouldEqual (UnixTimestamp.epoch, UnixTimestamp.epoch, UnixTimestamp.epoch)

    [<Test>]
    let ``fstat of a Darwin pipe: 0660 FIFO, an inode per end, device 0, the bytes held, and birth at the epoch``
        ()
        : unit
        =
        // Made a second after boot, so that a birth time of the pipe's creation
        // would not read as the epoch.
        let booted = systemOn SimulatedUnixPlatform.macOsArm64

        let (r, w), system =
            pipeOrFail
                0
                { booted with
                    Machine = UnixMachineState.advanceClock 1_000_000_000L booted.Machine
                }

        let _, system = writeOrFail w (payload 0 10) system
        let _, system = readOrFail r 3 system
        let read = fstatOrFail r system
        let write = fstatOrFail w system
        (read.Mode, write.Mode) |> shouldEqual (0o010660, 0o010660)
        read.Inode |> shouldNotEqual write.Inode
        (read.DeviceId, write.DeviceId) |> shouldEqual (0L, 0L)
        (read.Size, write.Size) |> shouldEqual (7L, 7L)
        read.BirthTime |> shouldEqual (Some UnixTimestamp.epoch)

        // Once the read end has gone, the write end reports nothing held.
        let system =
            match UnixDescriptor.close r system with
            | Ok (_, system) -> system
            | Error refusal -> failwith $"%A{refusal}"

        (fstatOrFail w system).Size |> shouldEqual 0L

    [<Test>]
    let ``on Darwin a read moves the read end's atime and a write both ends' mtime and ctime, whatever they answer``
        ()
        : unit
        =
        let (r, w), system = pipeOrFail 0 (systemOn SimulatedUnixPlatform.macOsArm64)

        let tick (system : UnixSystem<int, string>) =
            { system with
                Machine = UnixMachineState.advanceClock 1_000_000L system.Machine
            }

        let times fd system =
            let status = fstatOrFail fd system
            status.AccessTime, status.ModificationTime, status.StatusChangeTime

        let start = UnixMachineState.realtime system.Machine

        // A blocking read of 0 bytes: answered 0, and the read end's atime moves.
        let system = tick system
        let t1 = UnixMachineState.realtime system.Machine
        let answer, system = readOrFail r 0 system
        answer |> shouldEqual (ReadAnswer.Completed ImmutableArray.Empty)
        times r system |> shouldEqual (t1, start, start)
        times w system |> shouldEqual (start, start, start)

        // A write of 0 bytes moves both ends' mtime and ctime.
        let system = tick system
        let t2 = UnixMachineState.realtime system.Machine
        let answer, system = writeOrFail w ImmutableArray.Empty system
        answer |> shouldEqual (WriteAnswer.Completed 0L)
        times r system |> shouldEqual (t1, t2, t2)
        times w system |> shouldEqual (start, t2, t2)

        // An EFAULT write moves them too, and changes nothing else.
        let system = tick system
        let t3 = UnixMachineState.realtime system.Machine

        match WriteOutcomes.admitWrite w (UserBuffer.Unmapped 8UL) 5UL system with
        | Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT), after) ->
            times w after |> shouldEqual (start, t3, t3)
            (fstatOrFail r after).Size |> shouldEqual 0L
        | other -> failwith $"%A{other}"

    [<Test>]
    let ``on Linux no call moves a pipe's timestamps`` () : unit =
        let (r, w), system = pipeOrFail 0x800 (systemOn SimulatedUnixPlatform.linuxArm64)
        let before = fstatOrFail r system

        let system =
            { system with
                Machine = UnixMachineState.advanceClock 1_000_000L system.Machine
            }

        let _, system = readOrFail r 5 system
        let _, system = writeOrFail w (payload 0 10) system
        let _, system = readOrFail r 5 system
        let _, system = writeOrFail w ImmutableArray.Empty system

        for fd in [ r ; w ] do
            let after = fstatOrFail fd system

            (after.AccessTime, after.ModificationTime, after.StatusChangeTime)
            |> shouldEqual (before.AccessTime, before.ModificationTime, before.StatusChangeTime)

    // --- EPIPE, blocking, and what else a transfer answers ---

    /// Every row of `pipe-epipe-sweep.c` the model can be asked: a write into a
    /// pipe whose reader has closed, from the leader, for each fill, flag,
    /// count, buffer and disposition the probe swept.
    [<Test>]
    let ``a write with no reader answers EPIPE and raises SIGPIPE, except Linux's zero-length write`` () : unit =
        for platform in platforms do
            let flavour = SimulatedUnixPlatform.flavour platform
            let pipeBuf = PipeBuffer.atomicWriteLimit (PipeBuffer.empty platform)

            for fill in [ 0 ; 1000 ; -1 ] do
                for nonBlocking in [ false ; true ] do
                    for count in [ 0 ; 1 ; pipeBuf ; pipeBuf + 1 ; 65536 ; 100000 ] do
                        for buffer in [ UserBuffer.Mapped ; UserBuffer.Unmapped 0UL ; UserBuffer.Unmapped 8UL ] do
                            for disposition in sigPipeDispositions do
                                let where =
                                    $"%O{platform}, fill %d{fill}, O_NONBLOCK %b{nonBlocking}, count %d{count}, %A{buffer}, %A{disposition}"

                                let (r, w), system = pipeOrFail (nonBlockFlag flavour) (systemOn platform)

                                // Filled through the non-blocking write end, then
                                // the flag set as the row asks.
                                let system =
                                    match fill with
                                    | 0 -> system
                                    | -1 ->
                                        let rec fillUp (system : UnixSystem<int, string>) =
                                            match writeOrFail w (payload 0 4096) system with
                                            | WriteAnswer.Completed _, system -> fillUp system
                                            | WriteAnswer.Failed UnixError.EAGAIN, system ->
                                                match writeOrFail w (payload 0 1) system with
                                                | WriteAnswer.Completed _, system -> fillUp system
                                                | _, system -> system
                                            | other -> failwith $"%s{where}: filling: %A{other}"

                                        fillUp system
                                    | n -> writeOrFail w (payload 0 n) system |> snd

                                let system = UnixSocket.setNonBlocking w nonBlocking system |> snd

                                let system =
                                    match UnixDescriptor.close r system with
                                    | Ok (_, system) -> system
                                    | Error refusal -> failwith $"%A{refusal}"
                                    |> withSigPipe disposition
                                    |> fun system ->
                                        { system with
                                            Machine = UnixMachineState.advanceClock 1000L system.Machine
                                        }

                                let actual =
                                    WriteOutcomes.admitThenWrite system.Leader w buffer (payload 0 count) system

                                let raised = sigPipeFrom flavour system.Leader

                                // Every row but Linux's zero-length one raises.
                                let raises = not (flavour = SimulatedUnixFlavour.Linux && count = 0)

                                match raises, disposition, actual with
                                | false, _, Ok (WriteOutcome.Returns (answer, after)) ->
                                    // Nothing raised, nothing changed.
                                    answer |> shouldEqual (WriteAnswer.Completed 0L)
                                    after |> shouldEqual system
                                | true, SignalDisposition.Default, Ok (WriteOutcome.ProcessEnded endedProcess) ->
                                    endedProcess.Termination
                                    |> shouldEqual (ProcessTermination.Signaled (Signal.SIGPIPE, false))
                                | true,
                                  SignalDisposition.Ignore,
                                  Ok (WriteOutcome.ReturnsRaising (answer, signal, after)) ->
                                    (answer, signal) |> shouldEqual (WriteAnswer.Failed UnixError.EPIPE, raised)
                                    // No timestamp moves, even on Darwin.
                                    after |> shouldEqual system
                                | true,
                                  SignalDisposition.Catch _,
                                  Ok (WriteOutcome.ReturnsRaising (answer, signal, after)) ->
                                    (answer, signal) |> shouldEqual (WriteAnswer.Failed UnixError.EPIPE, raised)
                                    SignalState.pending after.Process.Signals |> shouldEqual [ raised ]
                                    withSignals system.Process.Signals after |> shouldEqual system
                                | _ -> failwith $"%s{where}: %A{actual}"

    [<Test>]
    let ``a blocking read of an empty pipe with a writer sleeps; with no writer it is end of file`` () : unit =
        for platform in platforms do
            let (r, w), system = pipeOrFail 0 (systemOn platform)

            match UnixReadWrite.read system.Leader r UserBuffer.Mapped 10UL system with
            | Ok (ReadOutcome.WouldBlock _, _) -> ()
            | other -> failwith $"%O{platform}: %A{other}"

            // A NULL buffer is not consulted before the read sleeps.
            match UnixReadWrite.read system.Leader r (UserBuffer.Unmapped 0UL) 10UL system with
            | Ok (ReadOutcome.WouldBlock _, _) -> ()
            | other -> failwith $"%O{platform}: %A{other}"

            let system =
                match UnixDescriptor.close w system with
                | Ok (_, system) -> system
                | Error refusal -> failwith $"%A{refusal}"

            match ReadOutcomes.read r (UserBuffer.Unmapped 0UL) 10UL system with
            | Ok (ReadAnswer.Completed bytes, _) -> bytes.Length |> shouldEqual 0
            | other -> failwith $"%O{platform}: %A{other}"

    [<Test>]
    let ``a blocking write that would not fit sleeps having put in what fits, and one that fits completes`` () : unit =
        for platform in platforms do
            let (_, w), system = pipeOrFail 0 (systemOn platform)

            match WriteOutcomes.admitThenWrite system.Leader w UserBuffer.Mapped (payload 0 70000) system with
            | Ok (WriteOutcome.WouldBlock (_, after)) ->
                match UnixTaskTable.parkedFor system.Leader after.Tasks with
                | Some (ParkedSyscall.PipeWrite parked) -> (parked.Count, parked.Written) |> shouldEqual (70000, 65536)
                | other -> failwith $"%O{platform}: %A{other}"
            | other -> failwith $"%O{platform}: %A{other}"

            let answer, _ = writeOrFail w (payload 0 65536) system
            answer |> shouldEqual (WriteAnswer.Completed 65536L)

    [<Test>]
    let ``lseek, pread, pwrite, ftruncate, posix_fadvise and getdents answer a pipe end as measured`` () : unit =
        for platform in platforms do
            let (r, w), system = pipeOrFail 0 (systemOn platform)
            let flavour = SimulatedUnixPlatform.flavour platform

            for fd in [ r ; w ] do
                for whence in 0..4 do
                    match UnixDescriptor.lseek fd 0L whence system with
                    | Ok (SyscallAnswer.Failed UnixError.ESPIPE, _) -> ()
                    | other -> failwith $"%O{platform}: lseek(%d{fd}, 0, %d{whence}): %A{other}"

                // Linux checks the whence first; Darwin the descriptor.
                let expected =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> UnixError.EINVAL
                    | SimulatedUnixFlavour.Darwin -> UnixError.ESPIPE

                match UnixDescriptor.lseek fd 0L 99 system with
                | Ok (SyscallAnswer.Failed error, _) -> error |> shouldEqual expected
                | other -> failwith $"%O{platform}: %A{other}"

                match UnixDescriptor.ftruncate fd 0L system with
                | Ok (SyscallAnswer.Failed UnixError.EINVAL, _) -> ()
                | other -> failwith $"%O{platform}: %A{other}"

                match UnixNamespace.readDirectoryEntry fd system with
                | Ok (ReadDirectoryAnswer.Failed error, _) ->
                    error
                    |> shouldEqual (
                        match flavour with
                        | SimulatedUnixFlavour.Linux -> UnixError.ENOTDIR
                        | SimulatedUnixFlavour.Darwin -> UnixError.ENOTSUP
                    )
                | other -> failwith $"%O{platform}: %A{other}"

            // The pread tie on the write end, and the pwrite tie on the read end:
            // Linux answers unseekability, Darwin the access mode.
            let tie =
                match flavour with
                | SimulatedUnixFlavour.Linux -> UnixError.ESPIPE
                | SimulatedUnixFlavour.Darwin -> UnixError.EBADF

            PReadUnchanged.pread r UserBuffer.Mapped 1UL 0L system
            |> shouldEqual (Ok (ReadAnswer.Failed UnixError.ESPIPE))

            PReadUnchanged.pread w UserBuffer.Mapped 1UL 0L system
            |> shouldEqual (Ok (ReadAnswer.Failed tie))

            match UnixReadWrite.admitPWrite 0 r UserBuffer.Mapped 1UL 0L system with
            | Ok (PWriteAdmission.Answered (WriteAnswer.Failed error)) -> error |> shouldEqual tie
            | other -> failwith $"%O{platform}: %A{other}"

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                UnixDescriptor.posixFadvise r 0L 0L 0 system
                |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.ESPIPE))
            | SimulatedUnixFlavour.Darwin -> ()

    [<Test>]
    let ``flock on Linux contends between a pipe's two ends and not with another pipe; Darwin refuses it`` () : unit =
        // The process's leader, which every system here starts with.
        let task = 0

        let lockExNb = 2 ||| 4

        let (r, w), system = pipeOrFail 0 (systemOn SimulatedUnixPlatform.linuxX64)

        let (r2, _), system = pipeOrFail 0 system

        let flockOf fd system =
            match UnixDescriptor.flock task fd lockExNb system with
            | Ok (SyscallOutcome.Answered answer, system) -> answer, system
            | other -> failwith $"%A{other}"

        let first, system = flockOf r system
        first |> shouldEqual (SyscallAnswer.Completed 0L)
        let second, system = flockOf w system
        second |> shouldEqual (SyscallAnswer.Failed UnixError.EAGAIN)
        let third, _ = flockOf r2 system
        third |> shouldEqual (SyscallAnswer.Completed 0L)

        let (r, _), darwin = pipeOrFail 0 (systemOn SimulatedUnixPlatform.macOsArm64)

        match UnixDescriptor.flock task r lockExNb darwin with
        | Error (FLockRefusal.DarwinPipe _) -> ()
        | other -> failwith $"%A{other}"

    [<Test>]
    let ``fstatfs of a pipe end is the pipe filesystem on Linux and EINVAL on Darwin`` () : unit =
        let (r, _), linux = pipeOrFail 0 (systemOn SimulatedUnixPlatform.linuxX64)

        match UnixPathResolution.fstatfs r linux with
        | FileSystemStatisticsAnswer.Reported statistics ->
            FileSystemStatistics.typeFields statistics
            |> shouldEqual (FileSystemTypeFields.Linux 0x50495045L)
        | other -> failwith $"%A{other}"

        let (r, _), darwin = pipeOrFail 0 (systemOn SimulatedUnixPlatform.macOsArm64)

        UnixPathResolution.fstatfs r darwin
        |> shouldEqual (FileSystemStatisticsAnswer.Failed UnixError.EINVAL)

    // --- FIONREAD and the terminal questions ---

    [<Test>]
    let ``FIONREAD: EBADF first, then EFAULT for a bad destination, and a non-pipe is refused`` () : unit =
        for platform in platforms do
            let (r, w), system = pipeOrFail 0 (systemOn platform)
            let _, system = writeOrFail w (payload 0 5) system

            UnixDescriptor.bytesAvailable 99 (UserBuffer.Unmapped 8UL) system
            |> shouldEqual (Ok (BytesAvailableAnswer.Failed UnixError.EBADF))

            for fd in [ r ; w ] do
                UnixDescriptor.bytesAvailable fd (UserBuffer.Unmapped 0UL) system
                |> shouldEqual (Ok (BytesAvailableAnswer.Failed UnixError.EFAULT))

            // The launched standard streams are pipes too, and empty.
            UnixDescriptor.bytesAvailable 1 UserBuffer.Mapped system
            |> shouldEqual (Ok (BytesAvailableAnswer.Reported 0))

            let socketFd, system =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            match UnixDescriptor.bytesAvailable socketFd UserBuffer.Mapped system with
            | Error (BytesAvailableRefusal.UnmodelledTarget fd) when fd = socketFd -> ()
            | other -> failwith $"%A{other}"

    [<Test>]
    let ``FIONREAD on the write end reports the bytes held on Linux, even with no reader, and 0 on Darwin`` () : unit =
        for platform in platforms do
            let (r, w), system = pipeOrFail 0 (systemOn platform)
            let _, system = writeOrFail w (payload 0 5) system

            let system =
                match UnixDescriptor.close r system with
                | Ok (_, system) -> system
                | Error refusal -> failwith $"%A{refusal}"

            let expected =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> 5
                | SimulatedUnixFlavour.Darwin -> 0

            UnixDescriptor.bytesAvailable w UserBuffer.Mapped system
            |> shouldEqual (Ok (BytesAvailableAnswer.Reported expected))

    [<Test>]
    let ``tcgetattr answers each descriptor kind as measured`` () : unit =
        for platform in platforms do
            let flavour = SimulatedUnixPlatform.flavour platform
            let (r, w), system = pipeOrFail 0 (systemOn platform)

            let answer fd system =
                match UnixDescriptor.terminalAttributes fd system with
                | TerminalAttributesAnswer.NotATerminal error -> error

            for fd in [ 0 ; 1 ; 2 ; r ; w ] do
                answer fd system |> shouldEqual UnixError.ENOTTY

            answer 99 system |> shouldEqual UnixError.EBADF

            let socket domain kind system =
                match UnixSocket.socket domain kind 0 system with
                | Ok (Ok (fd, system)) -> fd, system
                | other -> failwith $"%A{other}"

            let inet = SimulatedUnixPlatform.internetAddressFamily
            // AF_UNIX, which both flavours number 1.
            let unixDomain = 1
            let tcp, system = socket inet 1 system
            let local, system = socket unixDomain 1 system

            let expectedInet, expectedUnix =
                match flavour with
                | SimulatedUnixFlavour.Linux -> UnixError.ENOTTY, UnixError.ENOTTY
                | SimulatedUnixFlavour.Darwin -> UnixError.ENXIO, UnixError.EOPNOTSUPP

            answer tcp system |> shouldEqual expectedInet
            answer local system |> shouldEqual expectedUnix

    // --- poll and epoll ---

    [<Test>]
    let ``poll reports a pipe's levels as measured on Linux`` () : unit =
        let (r, w), system = pipeOrFail 0x800 (systemOn SimulatedUnixPlatform.linuxX64)

        let level fd system =
            let id =
                FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors |> Option.get

            LinuxReadiness.ofDescription id system

        (level r system, level w system) |> shouldEqual (0u, 0x104u)
        let _, full = writeOrFail w (payload 0 65536) system
        (level r full, level w full) |> shouldEqual (0x41u, 0u)

        let closeOrFail fd system =
            match UnixDescriptor.close fd system with
            | Ok (_, system) -> system
            | Error refusal -> failwith $"%A{refusal}"

        level w (closeOrFail r system) |> shouldEqual 0x10cu
        level w (closeOrFail r full) |> shouldEqual 0x8u
        level r (closeOrFail w system) |> shouldEqual 0x10u
        level r (closeOrFail w full) |> shouldEqual 0x51u

    [<Test>]
    let ``epoll will not register a pipe end, and waits on one are not a port's`` () : unit =
        let (r, w), system = pipeOrFail 0 (systemOn SimulatedUnixPlatform.linuxX64)

        let port, system =
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok (port, system)) -> port, system
            | other -> failwith $"%A{other}"

        let edge = EpollEvents.In ||| EpollEvents.EdgeTriggered

        for fd in [ r ; w ] do
            match UnixPoll.epollCtl port 1 fd (EpollEventArgument.Readable (edge, 0UL)) system with
            | Error (EpollCtlRefusal.PipeTarget refused) -> refused |> shouldEqual fd
            | other -> failwith $"%A{other}"

            match UnixPoll.epollCtl port 2 fd (EpollEventArgument.Readable (edge, 0UL)) system with
            | Ok (EpollCtlAnswer.Failed EpollCtlError.NotRegistered, _) -> ()
            | other -> failwith $"%A{other}"

            match UnixPoll.epollWait 1 fd 1 UserBuffer.Mapped 0 (Tasks.ensure 1 system) with
            | Ok (EpollWaitOutcome.Failed UnixError.EINVAL, _) -> ()
            | other -> failwith $"%A{other}"

    // --- the table ---

    [<Test>]
    let ``a pipe lives while any descriptor names either end, and a dup keeps an end open`` () : unit =
        for platform in platforms do
            let (r, w), system = pipeOrFail 0 (systemOn platform)

            let closeOrFail fd system =
                match UnixDescriptor.close fd system with
                | Ok (SyscallAnswer.Completed _, system) -> system
                | other -> failwith $"%A{other}"

            let dupped, system =
                match UnixDescriptor.dup w system with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%A{other}"

            let system = closeOrFail w system

            // The dup holds the write end open: an empty read is not end of file.
            match UnixReadWrite.read system.Leader r UserBuffer.Mapped 1UL system with
            | Ok (ReadOutcome.WouldBlock _, _) -> ()
            | other -> failwith $"%A{other}"

            let system = closeOrFail r system
            Map.count (madePipes system) |> shouldEqual 1
            let system = closeOrFail dupped system
            Map.isEmpty (madePipes system) |> shouldEqual true
            assertSound $"%O{platform}" system

    [<Test>]
    let ``wouldTake is the count write takes, and offering only that prefix changes nothing`` () : unit =
        let property (platform : SimulatedUnixPlatform, ops : PipeBufferOp list, count : int) : unit =
            let mutable buffer = PipeBuffer.empty platform

            for op in ops do
                match op with
                | PipeBufferOp.Write n -> buffer <- snd (PipeBuffer.write (payload 0 n) buffer)
                | PipeBufferOp.Read n -> buffer <- snd (PipeBuffer.read n buffer)

            let taken = PipeBuffer.wouldTake count buffer
            let written, afterWhole = PipeBuffer.write (payload 0 count) buffer
            taken |> shouldEqual written

            // Offered only the prefix it takes, the buffer takes all of it and
            // ends as it would have offered the whole.
            let prefixWritten, afterPrefix = PipeBuffer.write (payload 0 taken) buffer
            prefixWritten |> shouldEqual taken
            afterPrefix |> shouldEqual afterWhole

        let countGen =
            Gen.frequency
                [
                    3, Gen.choose (0, 70000)
                    1, Gen.elements [ 0 ; 1 ; 512 ; 513 ; 4096 ; 4097 ; 65536 ]
                ]

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip3 (Gen.elements platforms) TestPipeBuffer.opsGen countGen)) property
        )

    // --- the table's invariants: each defect one edit away from a sound pipe ---

    /// A Linux system holding one pipe with both ends open, which is sound.
    let private withPipe (platform : SimulatedUnixPlatform) : UnixSystem<int, string> * PipeId =
        let (r, _), system = pipeOrFail 0 (systemOn platform)
        system, pipeOf r system

    let private defectsOf (system : UnixSystem<int, string>) : UnixSystemDefect<int> list =
        UnixSystem.checkInvariants system

    let private mapPipe (pipeId : PipeId) (f : PipeState -> PipeState) (system : UnixSystem<int, string>) =
        { system with
            Machine =
                { system.Machine with
                    Pipes = Map.add pipeId (f system.Machine.Pipes.[pipeId]) system.Machine.Pipes
                }
        }

    [<Test>]
    let ``a system holding a pipe is sound`` () : unit =
        for platform in platforms do
            defectsOf (fst (withPipe platform)) |> shouldEqual []

    [<Test>]
    let ``a description naming a pipe the table does not hold is a defect`` () : unit =
        let system, pipeId = withPipe SimulatedUnixPlatform.linuxX64

        let forged =
            { system with
                Machine =
                    { system.Machine with
                        Pipes = Map.remove pipeId system.Machine.Pipes
                    }
            }

        let defects = defectsOf forged
        defects |> List.length |> shouldEqual 2

        defects
        |> List.forall (fun defect ->
            match defect with
            | UnixSystemDefect.DanglingPipe (_, dangling) -> dangling = pipeId
            | _ -> false
        )
        |> shouldEqual true

    [<Test>]
    let ``a pipe no description names is a defect`` () : unit =
        let system, pipeId = withPipe SimulatedUnixPlatform.linuxX64

        let forged =
            { system with
                Machine =
                    { system.Machine with
                        Pipes = Map.add (PipeId 7L) system.Machine.Pipes.[pipeId] system.Machine.Pipes
                        NextPipeId = PipeId 8L
                        NextPipeInode = InodeNumber 100L
                    }
            }
            |> mapPipe (PipeId 7L) (withInodes (PipeInodes.Shared (InodeNumber 50L)))

        defectsOf forged
        |> shouldEqual [ UnixSystemDefect.UnreferencedPipe (PipeId 7L) ]

    [<Test>]
    let ``a pipe at or past the next identity is a defect`` () : unit =
        let system, pipeId = withPipe SimulatedUnixPlatform.linuxX64

        let forged =
            { system with
                Machine =
                    { system.Machine with
                        NextPipeId = pipeId
                    }
            }

        defectsOf forged
        |> shouldEqual [ UnixSystemDefect.NextPipeIdNotFresh (pipeId, pipeId) ]

    [<Test>]
    let ``a pipe inode at or past the next to mint, or shared by two pipes, is a defect`` () : unit =
        let system, pipeId = withPipe SimulatedUnixPlatform.linuxX64
        let inode = InodeNumber 1L

        let atNext =
            { system with
                Machine =
                    { system.Machine with
                        NextPipeInode = inode
                    }
            }

        defectsOf atNext
        |> shouldEqual [ UnixSystemDefect.PipeInodeNotFresh (inode, pipeId, inode) ]

        let (second, _), twoPipes = pipeOrFail 0 system

        let shared =
            twoPipes
            |> mapPipe (pipeOf second twoPipes) (withInodes (PipeInodes.Shared inode))

        defectsOf shared |> shouldEqual [ UnixSystemDefect.DuplicatePipeInode inode ]

    [<Test>]
    let ``a pipe of the other flavour's shape is a defect`` () : unit =
        let linux, pipeId = withPipe SimulatedUnixPlatform.linuxX64
        let darwin, _ = withPipe SimulatedUnixPlatform.macOsArm64

        let perEnd =
            linux
            |> mapPipe pipeId (withInodes (PipeInodes.PerEnd (InodeNumber 1L, InodeNumber 2L)))

        let perEnd =
            { perEnd with
                Machine =
                    { perEnd.Machine with
                        NextPipeInode = InodeNumber 3L
                    }
            }

        defectsOf perEnd
        |> shouldEqual [ UnixSystemDefect.PipeNotOfPlatform (pipeId, SimulatedUnixPlatform.linuxX64) ]

        let darwinBuffer =
            linux
            |> mapPipe
                pipeId
                (fun pipe ->
                    { pipe with
                        Buffer = darwin.Machine.Pipes.[pipeId].Buffer
                    }
                )

        defectsOf darwinBuffer
        |> shouldEqual [ UnixSystemDefect.PipeNotOfPlatform (pipeId, SimulatedUnixPlatform.linuxX64) ]


    [<Test>]
    let ``a pipe device no machine of the flavour reports is a defect, and the setter refuses one`` () : unit =
        let darwin, _ = withPipe SimulatedUnixPlatform.macOsArm64

        let forged =
            { darwin with
                Machine =
                    { darwin.Machine with
                        PipeDevice = 5L
                    }
            }

        defectsOf forged
        |> shouldEqual [ UnixSystemDefect.PipeDeviceNotOfFlavour (5L, SimulatedUnixFlavour.Darwin) ]

        Assert.Throws<System.Exception> (fun () -> UnixMachineState.withPipeDevice (Some 5L) darwin.Machine |> ignore)
        |> ignore

        let linux, _ = withPipe SimulatedUnixPlatform.linuxX64

        (UnixMachineState.withPipeDevice (Some 42L) linux.Machine).PipeDevice
        |> shouldEqual 42L

        (UnixMachineState.withPipeDevice None linux.Machine).PipeDevice
        |> shouldEqual 0xcL

        let negative =
            { linux with
                Machine =
                    { linux.Machine with
                        PipeDevice = -1L
                    }
            }

        defectsOf negative
        |> shouldEqual [ UnixSystemDefect.PipeDeviceNotOfFlavour (-1L, SimulatedUnixFlavour.Linux) ]

    // --- what a write that takes nothing leaves behind ---

    [<Test>]
    let ``a write that takes nothing leaves the buffer as write would, and growing first changes no later write``
        ()
        : unit
        =
        let property (platform : SimulatedUnixPlatform, ops : PipeBufferOp list, count : int) : unit =
            let mutable buffer = PipeBuffer.empty platform

            for op in ops do
                match op with
                | PipeBufferOp.Write n -> buffer <- snd (PipeBuffer.write (payload 0 n) buffer)
                | PipeBufferOp.Read n -> buffer <- snd (PipeBuffer.read n buffer)

            let taken, afterWrite = PipeBuffer.write (payload 0 count) buffer
            let untaken = PipeBuffer.withoutTaking count buffer

            if taken = 0 then
                untaken |> shouldEqual afterWrite

            PipeBuffer.held untaken |> shouldEqual (PipeBuffer.held buffer)
            PipeBuffer.write (payload 0 count) untaken |> shouldEqual (taken, afterWrite)

        let countGen =
            Gen.frequency
                [
                    3, Gen.choose (0, 70000)
                    1, Gen.elements [ 0 ; 1 ; 513 ; 4097 ; 8193 ; 16385 ; 65536 ]
                ]

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip3 (Gen.elements platforms) TestPipeBuffer.opsGen countGen)) property
        )

    /// Measured by pipe-fault-aftermath.c: a write through a bad pointer, then
    /// the writes named, and whether the write end then polls ready. On Linux
    /// the fault leaves nothing behind; on Darwin it grows the buffer, which a
    /// pipe holding exactly 16384 bytes shows.
    let private measuredChains : (int * int list * bool * bool) list =
        [
            8193, [ 16384 ], true, false
            4097, [ 8192 ; 8192 ], false, true
            513, [ 1024 ; 1024 ; 2048 ; 4096 ; 8192 ], true, false
            16385, [ 16000 ], false, true
        ]

    /// Whether the write end of a fresh pipe on `platform` is ready after
    /// `writes`, preceded by a write of `fault` bytes through a bad pointer if
    /// `faultFirst`.
    let private readyAfter
        (platform : SimulatedUnixPlatform)
        (fault : int)
        (faultFirst : bool)
        (writes : int list)
        : bool
        =
        let (_, w), system = pipeOrFail 0 (systemOn platform)

        let system =
            if faultFirst then
                match WriteOutcomes.admitWrite w (UserBuffer.Unmapped 8UL) (uint64 fault) system with
                | Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT), after) -> after
                | other -> failwith $"%A{other}"
            else
                system

        let system =
            (system, writes)
            ||> List.fold (fun system count ->
                match writeOrFail w (payload 0 count) system with
                | WriteAnswer.Completed n, after when n = int64 count -> after
                | other -> failwith $"%A{other}"
            )

        PipeBuffer.writable system.Machine.Pipes.[pipeOf w system].Buffer

    [<Test>]
    let ``on Darwin a faulting write grows the buffer as measured, and on Linux it changes nothing`` () : unit =
        for fault, writes, control, faulted in measuredChains do
            let darwin = SimulatedUnixPlatform.macOsArm64

            (readyAfter darwin fault false writes, readyAfter darwin fault true writes)
            |> shouldEqual (control, faulted)

            let linux = SimulatedUnixPlatform.linuxX64

            readyAfter linux fault true writes
            |> shouldEqual (readyAfter linux fault false writes)
