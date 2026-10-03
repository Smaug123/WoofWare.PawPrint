namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One step of the blocking-pipe property: a call by a task, a descriptor
/// operation, or a step of the client's scheduler.
///
/// A task is named by its index, modulo their number, among the tasks the step
/// can be taken by: those making no call, for a call; those asleep or woken,
/// for a signal; those woken, for a finish.
[<RequireQualifiedAccess>]
type BlockingPipeOp =
    /// A task reads up to `count` bytes through descriptor `fd`.
    | Read of task : int * fd : int * count : int * mapped : bool
    /// A task writes `count` bytes through descriptor `fd`.
    | Write of task : int * fd : int * count : int * mapped : bool
    | Close of fd : int
    | Dup of fd : int
    | SetNonBlocking of fd : int * value : bool
    /// A caught `SIGUSR1` is sent to a task in a call.
    | Signal of task : int
    /// The client asks which sleepers the system wakes.
    | Wake
    /// The client finishes the call of a woken task.
    | Finish of task : int

/// Blocking `read(2)` and `write(2)` on a pipe, by several tasks: what parks,
/// who a change wakes, and how each woken call finishes.
///
/// The property holds the library to a reference written out again from the
/// measurements in `pipe-blocking.c`, over the buffer rules
/// `PipeBufferReference` states: a blocking transfer that would wait sleeps,
/// having put in what fits; on Linux bytes and room wake one sleeper, the
/// first to park, while on Darwin they wake every one; an end closing wakes
/// every sleeper on the other; a woken call finishes as section D, E, G and H
/// measured, or sleeps again at the back.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBlockingPipe =

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private tasks : int list = [ 0 ; 1 ; 2 ; 3 ]

    /// What a call came to, in a shape both the library and the reference can
    /// produce.
    [<RequireQualifiedAccess>]
    type private Seen =
        | ReadBytes of byte list
        | Wrote of int64
        | Failed of UnixError
        | Sleeps
        | Restarts
        /// The library will not say whether the call's own answer or a signal
        /// ends it.
        | Refused

    // --- the reference ---

    /// A pipe's buffer under either flavour's reference rule.
    type private Buffer =
        | Linux of PipeBufferReference.Linux
        | Darwin of PipeBufferReference.Darwin

    let private held (buffer : Buffer) : int =
        match buffer with
        | Buffer.Linux s -> List.length s.Bytes
        | Buffer.Darwin s -> List.length s.Bytes

    let private bufferWrite (bytes : byte list) (buffer : Buffer) : int * Buffer =
        match buffer with
        | Buffer.Linux s ->
            let n, s = PipeBufferReference.linuxWrite bytes s
            n, Buffer.Linux s
        | Buffer.Darwin s ->
            let n, s = PipeBufferReference.darwinWrite bytes s
            n, Buffer.Darwin s

    let private bufferRead (count : int) (buffer : Buffer) : byte list * Buffer =
        match buffer with
        | Buffer.Linux s ->
            let b, s = PipeBufferReference.linuxRead count s
            b, Buffer.Linux s
        | Buffer.Darwin s ->
            let b, s = PipeBufferReference.darwinRead count s
            b, Buffer.Darwin s

    /// `buffer` as a write that took nothing leaves it: on Darwin, grown as
    /// the write would have grown it.
    let private untaken (bytes : byte list) (buffer : Buffer) : Buffer =
        match bufferWrite bytes buffer, buffer with
        | (_, Buffer.Darwin grown), Buffer.Darwin s ->
            Buffer.Darwin
                { s with
                    Size = grown.Size
                }
        | _, buffer -> buffer

    /// How many of `rest` a sleeping write of `count` bytes in all puts in as
    /// it resumes: a page into each free slot on Linux; on Darwin what the
    /// room holds, all or nothing for a write of at most 512 bytes.
    let private resumes (count : int) (rest : byte list) (buffer : Buffer) : int * Buffer =
        match buffer with
        | Buffer.Linux s ->
            let mutable taken = 0
            let mutable slots = s.Slots
            let n = List.length rest

            while taken < n && List.length slots < 16 do
                let c = min s.Page (n - taken)
                slots <- slots @ [ 0, c ]
                taken <- taken + c

            taken,
            Buffer.Linux
                { s with
                    Slots = slots
                    Bytes = s.Bytes @ List.take taken rest
                }
        | Buffer.Darwin s ->
            if s.Size <> 65536 then
                failwith $"reference: a Darwin write sleeping with its buffer at %d{s.Size}"

            let free = s.Size - List.length s.Bytes
            let n = List.length rest

            let taken =
                if count <= 512 then
                    (if free >= n then n else 0)
                else
                    min n free

            taken,
            Buffer.Darwin
                { s with
                    Bytes = s.Bytes @ List.take taken rest
                }

    /// A call a task is in while the client holds it asleep or has woken it.
    type private Call =
        | Reading of description : int * count : int * mapped : bool
        | Writing of description : int * payload : byte list * written : int * mapped : bool

    type private Park =
        {
            Call : Call
            /// The descriptor the call was made through; `None` once a close of
            /// it has ended the call, under Darwin.
            Through : int option
            Ordinal : int
            /// The client has woken it, and not yet finished it.
            Woken : bool
            /// How many reads had taken bytes when it parked.
            ReadsSeen : int
        }

    type private Reference =
        {
            Linux : bool
            Restart : bool
            Buffer : Buffer
            /// Open descriptor to (end, description).
            Fds : Map<int, PipeEnd * int>
            NonBlocking : Map<int, bool>
            NextDescription : int
            Parks : Map<int, Park>
            NextOrdinal : int
            Signalled : Set<int>
            Writes : int
            /// How many reads have taken bytes.
            Reads : int
        }

    let private endOfCall (call : Call) : PipeEnd =
        match call with
        | Call.Reading _ -> PipeEnd.Read
        | Call.Writing _ -> PipeEnd.Write

    let private descriptionOf (call : Call) : int =
        match call with
        | Call.Reading (description, _, _)
        | Call.Writing (description, _, _, _) -> description

    /// The descriptions something still references, and the end each is onto:
    /// a descriptor names it, or a call in progress (asleep or woken, and not
    /// yet answered) holds it.
    let private liveDescriptions (r : Reference) : Map<int, PipeEnd> =
        let named =
            r.Fds
            |> Map.toList
            |> List.map (fun (_, (pipeEnd, description)) -> description, pipeEnd)

        // A call a Darwin close has ended holds nothing.
        let held =
            r.Parks
            |> Map.toList
            |> List.filter (fun (_, park) -> park.Through.IsSome)
            |> List.map (fun (_, park) -> descriptionOf park.Call, endOfCall park.Call)

        Map.ofList (named @ held)

    let private endOpen (pipeEnd : PipeEnd) (r : Reference) : bool =
        liveDescriptions r |> Map.exists (fun _ e -> e = pipeEnd)

    let private payload (index : int) (count : int) : byte list =
        List.init count (fun i -> byte ((index * 37 + i) % 251))

    let private park (task : int) (fd : int) (call : Call) (r : Reference) : Reference =
        { r with
            Parks =
                Map.add
                    task
                    {
                        Call = call
                        Through = Some fd
                        Ordinal = r.NextOrdinal
                        Woken = false
                        ReadsSeen = r.Reads
                    }
                    r.Parks
            NextOrdinal = r.NextOrdinal + 1
        }

    /// The task's call answered: the signal it had pending is taken as it
    /// returns.
    let private answered (task : int) (r : Reference) : Reference =
        { r with
            Parks = Map.remove task r.Parks
            Signalled = Set.remove task r.Signalled
        }

    let private referenceRead (task : int) (fd : int) (count : int) (mapped : bool) (r : Reference) : Seen * Reference =
        match Map.tryFind fd r.Fds with
        | None
        | Some (PipeEnd.Write, _) -> Seen.Failed UnixError.EBADF, r
        | Some (PipeEnd.Read, description) ->
            if count = 0 then
                Seen.ReadBytes [], r
            elif held r.Buffer > 0 then
                if mapped then
                    let bytes, buffer = bufferRead count r.Buffer

                    Seen.ReadBytes bytes,
                    { r with
                        Buffer = buffer
                        Reads = r.Reads + 1
                    }
                else
                    Seen.Failed UnixError.EFAULT, r
            elif not (endOpen PipeEnd.Write r) then
                Seen.ReadBytes [], r
            elif r.NonBlocking.[description] then
                Seen.Failed UnixError.EAGAIN, r
            else
                Seen.Sleeps, park task fd (Call.Reading (description, count, mapped)) r

    let private referenceWrite
        (task : int)
        (fd : int)
        (count : int)
        (mapped : bool)
        (r : Reference)
        : Seen * Reference
        =
        match Map.tryFind fd r.Fds with
        | None
        | Some (PipeEnd.Read, _) -> Seen.Failed UnixError.EBADF, r
        | Some (PipeEnd.Write, description) ->
            let bytes = payload r.Writes count

            let r =
                { r with
                    Writes = r.Writes + 1
                }

            if r.Linux && count = 0 then
                Seen.Wrote 0L, r
            elif not (endOpen PipeEnd.Read r) then
                // SIGPIPE is ignored here, so discarded as it is raised.
                Seen.Failed UnixError.EPIPE, r
            elif count = 0 then
                Seen.Wrote 0L, r
            else

            let takes, grown = bufferWrite bytes r.Buffer
            let nonBlocking = r.NonBlocking.[description]

            if takes = 0 then
                let r =
                    { r with
                        Buffer = untaken bytes r.Buffer
                    }

                if nonBlocking then
                    Seen.Failed UnixError.EAGAIN, r
                else
                    Seen.Sleeps, park task fd (Call.Writing (description, bytes, 0, mapped)) r
            elif not mapped then
                Seen.Failed UnixError.EFAULT,
                { r with
                    Buffer = untaken bytes r.Buffer
                }
            elif takes < count && not nonBlocking then
                Seen.Sleeps,
                park
                    task
                    fd
                    (Call.Writing (description, bytes, takes, mapped))
                    { r with
                        Buffer = grown
                    }
            else
                Seen.Wrote (int64 takes),
                { r with
                    Buffer = grown
                }

    /// How many bytes a sleeping write would put in now.
    let private wouldResume (payload : byte list) (written : int) (r : Reference) : int =
        fst (resumes (List.length payload) (List.skip written payload) r.Buffer)

    /// The sleepers the client holds asleep that the reference wakes, in the
    /// order they parked.
    let private referenceWakes (r : Reference) : int list =
        let asleep = r.Parks |> Map.toList |> List.filter (fun (_, park) -> not park.Woken)

        let readerWoken =
            r.Parks
            |> Map.exists (fun _ park ->
                match park.Call with
                | Call.Reading _ -> park.Woken
                | Call.Writing _ -> false
            )

        let writerWoken =
            r.Parks
            |> Map.exists (fun _ park ->
                match park.Call with
                | Call.Writing _ -> park.Woken
                | Call.Reading _ -> false
            )

        // On Linux the one sleeper that bytes or room wake: the first to park
        // of those it would satisfy, unless one of its kind is woken already.
        let first (wants : Call -> bool) (blocked : bool) : int option =
            if blocked || not r.Linux then
                None
            else
                asleep
                |> List.filter (fun (_, park) -> wants park.Call)
                |> List.sortBy (fun (_, park) -> park.Ordinal)
                |> List.tryHead
                |> Option.map fst

        let hasBytes (call : Call) =
            match call with
            | Call.Reading _ -> held r.Buffer > 0
            | Call.Writing _ -> false

        let hasRoom (call : Call) =
            match call with
            | Call.Writing (_, payload, written, _) -> wouldResume payload written r > 0
            | Call.Reading _ -> false

        let firstReader = first hasBytes readerWoken
        let firstWriter = first hasRoom writerWoken

        asleep
        |> List.filter (fun (task, park) ->
            park.Through.IsNone
            || Set.contains task r.Signalled
            || (
                match park.Call with
                | Call.Reading _ -> not (endOpen PipeEnd.Write r)
                | Call.Writing _ -> not (endOpen PipeEnd.Read r)
            )
            || (hasBytes park.Call && (not r.Linux || firstReader = Some task))
            // Darwin wakes every writer at each read, and one whose description
            // has become non-blocking gives up.
            || (
                match park.Call with
                | Call.Writing (description, _, _, _) ->
                    not r.Linux && r.NonBlocking.[description] && r.Reads > park.ReadsSeen
                | Call.Reading _ -> false
            )
            || (hasRoom park.Call && (not r.Linux || firstWriter = Some task))
        )
        |> List.sortBy (fun (_, park) -> park.Ordinal)
        |> List.map fst

    let private interrupted (r : Reference) : Seen =
        if r.Restart then
            Seen.Restarts
        else
            Seen.Failed UnixError.EINTR

    /// The woken task's call, finished.
    let private referenceFinish (task : int) (r : Reference) : Seen * Reference =
        let park = r.Parks.[task]
        let signalled = Set.contains task r.Signalled

        let reparked =
            { r with
                Parks =
                    Map.add
                        task
                        { park with
                            Woken = false
                            Ordinal = r.NextOrdinal
                            ReadsSeen = r.Reads
                        }
                        r.Parks
                NextOrdinal = r.NextOrdinal + 1
            }

        // Darwin wakes every sleeper, and one that finds nothing gives up if
        // its description has become non-blocking; Linux's sleeps on.
        let givesUp = not r.Linux && r.NonBlocking.[descriptionOf park.Call]

        match park.Call with
        // Ended by a Darwin close: end of file, or EPIPE whatever was put in
        // (`close-ends-call.c` sections P1-P5), whatever has happened since.
        | Call.Reading _ when park.Through.IsNone -> Seen.ReadBytes [], answered task r
        | Call.Writing _ when park.Through.IsNone -> Seen.Failed UnixError.EPIPE, answered task r
        | Call.Reading (_, count, mapped) ->
            if held r.Buffer > 0 || not (endOpen PipeEnd.Write r) then
                if signalled && not r.Linux then
                    Seen.Refused, r
                elif held r.Buffer = 0 then
                    Seen.ReadBytes [], answered task r
                elif not mapped then
                    Seen.Failed UnixError.EFAULT, answered task r
                else
                    let bytes, buffer = bufferRead count r.Buffer

                    Seen.ReadBytes bytes,
                    answered
                        task
                        { r with
                            Buffer = buffer
                            Reads = r.Reads + 1
                        }
            elif signalled then
                interrupted r, answered task r
            elif givesUp then
                Seen.Failed UnixError.EAGAIN, answered task r
            else
                Seen.Sleeps, reparked
        | Call.Writing (description, payload, written, mapped) ->
            let count = List.length payload

            if not (endOpen PipeEnd.Read r) then
                if signalled && not r.Linux then
                    Seen.Refused, r
                elif r.Linux && written > 0 then
                    Seen.Wrote (int64 written), answered task r
                else
                    Seen.Failed UnixError.EPIPE, answered task r
            elif wouldResume payload written r > 0 then
                if signalled && not r.Linux then
                    Seen.Refused, r
                elif not mapped then
                    Seen.Failed UnixError.EFAULT, answered task r
                else
                    let taken, buffer = resumes count (List.skip written payload) r.Buffer
                    let written = written + taken

                    let r =
                        { r with
                            Buffer = buffer
                        }

                    if written = count then
                        Seen.Wrote (int64 count), answered task r
                    elif signalled || r.NonBlocking.[description] then
                        Seen.Wrote (int64 written), answered task r
                    else
                        Seen.Sleeps,
                        { r with
                            Parks =
                                Map.add
                                    task
                                    { park with
                                        Call = Call.Writing (description, payload, written, mapped)
                                        Woken = false
                                        Ordinal = r.NextOrdinal
                                        ReadsSeen = r.Reads
                                    }
                                    r.Parks
                            NextOrdinal = r.NextOrdinal + 1
                        }
            elif signalled || (givesUp && written > 0) then
                if written > 0 then
                    Seen.Wrote (int64 written), answered task r
                else
                    interrupted r, answered task r
            elif givesUp then
                Seen.Failed UnixError.EAGAIN, answered task r
            else
                Seen.Sleeps, reparked

    // --- the library, driven as a client drives it ---

    let private fromRead (outcome : Result<ReadOutcome * UnixSystem<int, string>, ReadRefusal>) =
        match outcome with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) -> Seen.ReadBytes (List.ofSeq bytes), Some after
        | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), after) -> Seen.Failed error, Some after
        | Ok (ReadOutcome.Answered (ReadAnswer.Drawn _), _) -> failwith "a read of a pipe drew from the entropy pool"
        | Ok (ReadOutcome.WouldBlock _, after) -> Seen.Sleeps, Some after
        | Ok (ReadOutcome.Restarts, after) -> Seen.Restarts, Some after
        | Error (ReadRefusal.Interruption _) -> Seen.Refused, None
        | Error refusal -> failwith $"read refused: %A{refusal}"

    let private fromWrite (outcome : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>) =
        match outcome with
        | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, after))
        | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Completed n, _, after)) -> Seen.Wrote n, Some after
        | Ok (WriteOutcome.Returns (WriteAnswer.Failed error, after))
        | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed error, _, after)) -> Seen.Failed error, Some after
        | Ok (WriteOutcome.WouldBlock (_, after)) -> Seen.Sleeps, Some after
        | Ok (WriteOutcome.Restarts after) -> Seen.Restarts, Some after
        | Ok (WriteOutcome.ProcessEnded _ as outcome) -> failwith $"a write ended the process: %A{outcome}"
        | Error (WriteRefusal.Interruption _) -> Seen.Refused, None
        | Error refusal -> failwith $"write refused: %A{refusal}"

    /// A write by `task`, admitted and then given the bytes it asks for.
    let private libraryWrite
        (task : int)
        (fd : int)
        (bytes : byte list)
        (mapped : bool)
        (system : UnixSystem<int, string>)
        =
        let buffer =
            if mapped then
                UserBuffer.Mapped
            else
                UserBuffer.Unmapped 8UL

        match UnixReadWrite.admitWrite task fd buffer (uint64 (List.length bytes)) system with
        | Error refusal -> Error refusal
        | Ok (WriteOutcome.Returns (WriteAdmission.Transfer n, admitted)) ->
            UnixReadWrite.write task fd (ImmutableArray.CreateRange (List.take n bytes)) admitted
        | Ok (WriteOutcome.Returns (WriteAdmission.TransferThenSleep (n, total), admitted)) ->
            total |> shouldEqual (List.length bytes)
            UnixReadWrite.writeThenSleep task fd total (ImmutableArray.CreateRange (List.take n bytes)) admitted
        | Ok (WriteOutcome.Returns (WriteAdmission.Answered answer, after)) -> Ok (WriteOutcome.Returns (answer, after))
        | Ok (WriteOutcome.ReturnsRaising (WriteAdmission.Answered answer, signal, after)) ->
            Ok (WriteOutcome.ReturnsRaising (answer, signal, after))
        | Ok (WriteOutcome.WouldBlock (condition, after)) -> Ok (WriteOutcome.WouldBlock (condition, after))
        | Ok other -> failwith $"admitWrite: %A{other}"

    /// The woken write of `task`, finished: admitted, and given the bytes of
    /// `payload` it asks for.
    let private libraryFinishWrite (task : int) (payload : byte list) (system : UnixSystem<int, string>) =
        match UnixReadWrite.admitFinishWrite task system with
        | Error refusal -> Error refusal
        | Ok (WriteOutcome.Returns (WriteResumption.Transfer (offset, count), admitted)) ->
            let bytes = payload |> List.skip offset |> List.take count
            UnixReadWrite.finishWrite task (ImmutableArray.CreateRange bytes) admitted
        | Ok (WriteOutcome.Returns (WriteResumption.Answered answer, after)) ->
            Ok (WriteOutcome.Returns (answer, after))
        | Ok (WriteOutcome.ReturnsRaising (WriteResumption.Answered answer, signal, after)) ->
            Ok (WriteOutcome.ReturnsRaising (answer, signal, after))
        | Ok (WriteOutcome.WouldBlock (condition, after)) -> Ok (WriteOutcome.WouldBlock (condition, after))
        | Ok (WriteOutcome.Restarts after) -> Ok (WriteOutcome.Restarts after)
        | Ok other -> failwith $"admitFinishWrite: %A{other}"

    /// `task` returns to user mode: every handler it takes runs and returns.
    let rec private returnToUser (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixSignal.onReturnToUser task system with
        | Ok (None, system) -> system
        | Ok (Some (SignalDelivery.RunHandlers frames), system) ->
            (system, frames)
            ||> List.fold (fun system frame -> UnixSignal.sigreturn task frame.Id system)
            |> returnToUser task
        | other -> failwith $"returning task %d{task} to user mode: %A{other}"

    /// `weights` are the frequencies of a read, a write, a close, a dup, a
    /// change of `O_NONBLOCK`, a signal, a wake and a finish.
    let private opGenWeighted (weights : int * int * int * int * int * int * int * int) : Gen<BlockingPipeOp> =
        let reads, writes, closes, dups, nonBlocking, signals, wakes, finishes = weights
        let task = Gen.elements tasks
        let fd = Gen.choose (0, 4)

        let count =
            Gen.frequency
                [
                    2, Gen.choose (0, 20)
                    5,
                    Gen.elements
                        [
                            1
                            100
                            511
                            512
                            513
                            4095
                            4096
                            4097
                            8192
                            65535
                            65536
                            65537
                            70000
                            140000
                        ]
                ]

        let mapped = Gen.frequency [ 9, Gen.constant true ; 1, Gen.constant false ]

        Gen.frequency
            [
                reads,
                gen {
                    let! t = task
                    let! fd = fd
                    let! c = count
                    let! m = mapped
                    return BlockingPipeOp.Read (t, fd, c, m)
                }
                writes,
                gen {
                    let! t = task
                    let! fd = fd
                    let! c = count
                    let! m = mapped
                    return BlockingPipeOp.Write (t, fd, c, m)
                }
                closes, Gen.map BlockingPipeOp.Close fd
                dups, Gen.map BlockingPipeOp.Dup fd
                nonBlocking,
                Gen.map2 (fun fd v -> BlockingPipeOp.SetNonBlocking (fd, v)) fd (Gen.elements [ true ; false ])
                signals, Gen.map BlockingPipeOp.Signal task
                wakes, Gen.constant BlockingPipeOp.Wake
                finishes, Gen.map BlockingPipeOp.Finish task
            ]

    let private opGen : Gen<BlockingPipeOp> = opGenWeighted (5, 5, 1, 1, 1, 2, 6, 8)

    /// Weighted towards transfers asleep through several descriptors and the
    /// closes that end them, which `opGen` reaches only now and then.
    let private closingOpGen : Gen<BlockingPipeOp> =
        opGenWeighted (4, 4, 4, 4, 1, 1, 6, 8)

    /// The label `covered` records for a call that came to `seen`.
    let private label (flavour : string) (what : string) (seen : Seen) : string =
        let kind =
            match seen with
            | Seen.ReadBytes [] -> "end of file"
            | Seen.ReadBytes _ -> "bytes"
            | Seen.Wrote _ -> "wrote"
            | Seen.Failed error -> $"%O{error}"
            | Seen.Sleeps -> "sleeps"
            | Seen.Restarts -> "restarts"
            | Seen.Refused -> "refused"

        $"%s{flavour} %s{what}: %s{kind}"

    [<Test>]
    let ``blocking transfers park, wake and finish as the reference says`` () : unit =
        let covered = System.Collections.Concurrent.ConcurrentDictionary<string, int> ()

        let cover (label : string) =
            covered.AddOrUpdate (label, 1, (fun _ n -> n + 1)) |> ignore

        let property (platform : SimulatedUnixPlatform, restart : bool, ops : BlockingPipeOp list) : unit =
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            let bare =
                (UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                 |> UnixBootImage.boot,
                 [ 0 ; 1 ; 2 ])
                ||> List.fold (fun system fd ->
                    match UnixDescriptor.close fd system with
                    | Ok (_, system) -> system
                    | Error refusal -> failwith $"%A{refusal}"
                )

            let bare =
                (bare, List.tail tasks)
                ||> List.fold (fun system task -> Tasks.spawn task system)

            let bare =
                { bare with
                    Process =
                        { bare.Process with
                            Signals =
                                bare.Process.Signals
                                |> SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore
                                |> SignalState.setDisposition
                                    Signal.SIGUSR1
                                    (SignalDisposition.Catch
                                        { SignalCatch.ofHandler "h" with
                                            Restart = restart
                                        })
                        }
                }

            let mutable system =
                match UnixPipe.pipe2 0 UserBuffer.Mapped bare with
                | Ok (Pipe2Answer.Created (0, 1), system) -> system
                | other -> failwith $"pipe2: %A{other}"

            let pipeId =
                match FileDescriptorRegistry.tryFindTarget 0 system.Process.FileDescriptors with
                | Some (OpenFileTarget.Pipe (pipeId, PipeEnd.Read)) -> pipeId
                | other -> failwith $"fd 0 is %A{other}"

            // The reference's descriptions 0 and 1, as the library names them.
            let libraryDescription : Map<int, OpenFileDescriptionId> =
                [ 0 ; 1 ]
                |> List.map (fun fd ->
                    match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
                    | Some id -> fd, id
                    | None -> failwith $"fd %d{fd} names no description"
                )
                |> Map.ofList

            let mutable reference =
                {
                    Linux = linux
                    Restart = restart
                    Buffer =
                        if linux then
                            Buffer.Linux (
                                PipeBufferReference.linuxEmpty (
                                    SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform)
                                )
                            )
                        else
                            Buffer.Darwin PipeBufferReference.darwinEmpty
                    Fds = Map.ofList [ 0, (PipeEnd.Read, 0) ; 1, (PipeEnd.Write, 1) ]
                    NonBlocking = Map.ofList [ 0, false ; 1, false ]
                    NextDescription = 2
                    Parks = Map.empty
                    NextOrdinal = 0
                    Signalled = Set.empty
                    Writes = 0
                    Reads = 0
                }

            let mutable stopped = false

            let flavourName = if linux then "Linux" else "Darwin"

            let compare (where : string) (expected : Seen) (actual : Seen) =
                if expected <> actual then
                    failwith $"%s{where}: expected %A{expected}, got %A{actual}"

            // After an answer the task returns to user mode, taking its signal.
            let settle (task : int) (seen : Seen) (after : UnixSystem<int, string>) =
                match seen with
                | Seen.Sleeps -> after
                | Seen.ReadBytes _
                | Seen.Wrote _
                | Seen.Failed _
                | Seen.Restarts -> returnToUser task after
                | Seen.Refused -> failwith "a refusal leaves no system"

            for i, op in List.indexed ops |> Seq.takeWhile (fun _ -> not stopped) do
                let pick (eligible : int -> bool) (index : int) : int option =
                    match List.filter eligible tasks with
                    | [] -> None
                    | candidates -> Some candidates.[index % List.length candidates]

                let idle (task : int) =
                    not (Map.containsKey task reference.Parks)

                let inCall (task : int) = Map.containsKey task reference.Parks

                let woken (task : int) =
                    Map.tryFind task reference.Parks |> Option.exists (fun park -> park.Woken)

                // The op with its task resolved, or `None` where no task can
                // take it.
                let resolved =
                    match op with
                    | BlockingPipeOp.Read (index, fd, count, mapped) ->
                        pick idle index
                        |> Option.map (fun t -> BlockingPipeOp.Read (t, fd, count, mapped))
                    | BlockingPipeOp.Write (index, fd, count, mapped) ->
                        pick idle index
                        |> Option.map (fun t -> BlockingPipeOp.Write (t, fd, count, mapped))
                    | BlockingPipeOp.Signal index -> pick inCall index |> Option.map BlockingPipeOp.Signal
                    | BlockingPipeOp.Finish index -> pick woken index |> Option.map BlockingPipeOp.Finish
                    | BlockingPipeOp.Close _
                    | BlockingPipeOp.Dup _
                    | BlockingPipeOp.SetNonBlocking _
                    | BlockingPipeOp.Wake -> Some op

                match resolved with
                | None -> ()
                | Some op ->

                let where = $"%O{platform}, restart %b{restart}, op %d{i} (%A{op})"

                match op with
                | BlockingPipeOp.Read (task, fd, count, mapped) ->
                    let expected, after = referenceRead task fd count mapped reference

                    let buffer =
                        if mapped then
                            UserBuffer.Mapped
                        else
                            UserBuffer.Unmapped 8UL

                    let seen, actual =
                        fromRead (UnixReadWrite.read task fd buffer (uint64 count) system)

                    compare where expected seen
                    cover (label flavourName "read" seen)
                    system <- settle task seen (Option.get actual)
                    reference <- after
                | BlockingPipeOp.Write (task, fd, count, mapped) ->
                    let bytes = payload reference.Writes count
                    let expected, after = referenceWrite task fd count mapped reference
                    let seen, actual = fromWrite (libraryWrite task fd bytes mapped system)
                    compare where expected seen

                    match seen, after.Parks |> Map.tryFind task with
                    | Seen.Sleeps,
                      Some {
                               Call = Call.Writing (_, _, written, _)
                           } when written > 0 -> cover $"%s{flavourName} write: sleeps having put some in"
                    | _ -> cover (label flavourName "write" seen)

                    system <- settle task seen (Option.get actual)
                    reference <- after
                | BlockingPipeOp.Close fd ->
                    let holder =
                        Map.tryFind fd reference.Fds
                        |> Option.bind (fun (_, description) ->
                            reference.Parks
                            |> Map.tryFindKey (fun _ park -> descriptionOf park.Call = description)
                            |> Option.map (fun task -> description, task)
                        )

                    // Linux's sleeping call holds its description, so no close
                    // ends it (`pipe-blocking.c` section K); Darwin's ends every
                    // call made through the descriptor closed
                    // (`close-ends-call.c` sections P1-P7), unless something
                    // had already woken one, which is refused.
                    let through =
                        if linux then
                            []
                        else
                            reference.Parks
                            |> Map.toList
                            |> List.filter (fun (_, park) -> park.Through = Some fd)
                            |> List.map fst

                    let woken (task : int) =
                        let park = reference.Parks.[task]

                        Set.contains task reference.Signalled
                        || (
                            match park.Call with
                            | Call.Reading _ -> held reference.Buffer > 0
                            | Call.Writing (description, payload, written, _) ->
                                wouldResume payload written reference > 0
                                || (reference.NonBlocking.[description] && reference.Reads > park.ReadsSeen)
                        )

                    let refusedFor = through |> List.filter woken |> List.tryHead

                    if holder.IsSome && linux then
                        if
                            reference.Fds
                            |> Map.filter (fun _ (_, d) -> Some d = (holder |> Option.map fst))
                            |> Map.count = 1
                        then
                            cover "Linux close: the last descriptor onto a sleeping call's description"

                    match refusedFor, UnixDescriptor.close fd system with
                    | Some task, Error (CloseRefusal.DarwinWokenTransfer (description, refused)) ->
                        refused |> shouldEqual task

                        description
                        |> shouldEqual libraryDescription.[descriptionOf reference.Parks.[task].Call]

                        cover "Darwin close: refused, the transfer woken"
                    | None, Ok (answer, after) ->
                        answer
                        |> shouldEqual (
                            if Map.containsKey fd reference.Fds then
                                SyscallAnswer.Completed 0L
                            else
                                SyscallAnswer.Failed UnixError.EBADF
                        )

                        if not (List.isEmpty through) then
                            for task in through do
                                match reference.Parks.[task].Call with
                                | Call.Reading _ -> cover "Darwin close: ends a read"
                                | Call.Writing _ -> cover "Darwin close: ends a write"

                            if
                                reference.Parks
                                |> Map.exists (fun _ park ->
                                    park.Through.IsSome
                                    && park.Through <> Some fd
                                    && Some (descriptionOf park.Call) = (Map.tryFind fd reference.Fds |> Option.map snd)
                                )
                            then
                                cover "Darwin close: ends a transfer, one through a dup sleeping on"

                        system <- after

                        reference <-
                            { reference with
                                Fds = Map.remove fd reference.Fds
                                Parks =
                                    (reference.Parks, through)
                                    ||> List.fold (fun parks task ->
                                        Map.add
                                            task
                                            { parks.[task] with
                                                Through = None
                                            }
                                            parks
                                    )
                            }
                    | expected, other -> failwith $"%s{where}: close expected refusal for %A{expected}, got %A{other}"
                | BlockingPipeOp.Dup fd ->
                    let answer, after = UnixDescriptor.dup fd system

                    match Map.tryFind fd reference.Fds with
                    | None -> answer |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF)
                    | Some named ->
                        let lowest =
                            Seq.initInfinite id |> Seq.find (fun n -> not (Map.containsKey n reference.Fds))

                        answer |> shouldEqual (SyscallAnswer.Completed (int64 lowest))

                        reference <-
                            { reference with
                                Fds = Map.add lowest named reference.Fds
                            }

                    system <- after
                | BlockingPipeOp.SetNonBlocking (fd, value) ->
                    let _, after = UnixSocket.setNonBlocking fd value system
                    system <- after

                    match Map.tryFind fd reference.Fds with
                    | None -> ()
                    | Some (_, description) ->
                        reference <-
                            { reference with
                                NonBlocking = Map.add description value reference.NonBlocking
                            }
                | BlockingPipeOp.Signal task ->
                    system <-
                        { system with
                            Process =
                                { system.Process with
                                    Signals =
                                        SignalState.enqueue
                                            {
                                                Signal = Signal.SIGUSR1
                                                Target = ValueSome task
                                            }
                                            system.Process.Signals
                                }
                        }

                    reference <-
                        { reference with
                            Signalled = Set.add task reference.Signalled
                        }
                | BlockingPipeOp.Wake ->
                    let asleep =
                        reference.Parks
                        |> Map.filter (fun _ park -> not park.Woken)
                        |> Map.keys
                        |> Set.ofSeq

                    let woken = UnixWait.wakes asleep system |> List.map fst

                    if woken <> referenceWakes reference then
                        failwith $"%s{where}: woke %A{woken}, expected %A{referenceWakes reference}"

                    if List.length woken > 1 then
                        cover $"%s{flavourName} wake: several"

                    // Bytes or room that would satisfy several sleepers.
                    let satisfiable =
                        asleep
                        |> Set.filter (fun task ->
                            match reference.Parks.[task].Call with
                            | Call.Reading _ -> held reference.Buffer > 0
                            | Call.Writing (_, payload, written, _) -> wouldResume payload written reference > 0
                        )

                    if Set.count satisfiable > 1 && List.length woken < Set.count satisfiable then
                        cover $"%s{flavourName} wake: fewer than could proceed"

                    reference <-
                        { reference with
                            Parks =
                                (reference.Parks, woken)
                                ||> List.fold (fun parks task ->
                                    Map.add
                                        task
                                        { parks.[task] with
                                            Woken = true
                                        }
                                        parks
                                )
                        }
                | BlockingPipeOp.Finish task ->
                    let call = reference.Parks.[task].Call
                    let expected, after = referenceFinish task reference

                    let seen, actual =
                        match call with
                        | Call.Reading _ -> fromRead (UnixReadWrite.finishRead task system)
                        | Call.Writing (_, payload, _, _) -> fromWrite (libraryFinishWrite task payload system)

                    compare where expected seen

                    let what =
                        match call, seen with
                        | Call.Reading _, _ when reference.Parks.[task].Through.IsNone -> "finish read ended by a close"
                        | Call.Writing _, _ when reference.Parks.[task].Through.IsNone ->
                            "finish write ended by a close"
                        | Call.Writing (_, payload, written, _), Seen.Wrote n when
                            int n < List.length payload && int n = written
                            ->
                            "finish write, with the count already in"
                        | Call.Writing (_, payload, _, _), Seen.Wrote n when int n < List.length payload ->
                            "finish write, short"
                        | Call.Writing _, _ -> "finish write"
                        | Call.Reading _, _ -> "finish read"

                    cover (label flavourName what seen)

                    if
                        Map.containsKey (descriptionOf call) (liveDescriptions reference)
                        && not (Map.containsKey (descriptionOf call) (liveDescriptions after))
                    then
                        cover $"%s{flavourName} finish: the call's return releases its description"

                    match actual with
                    | None -> stopped <- true
                    | Some actual -> system <- settle task seen actual

                    reference <- after

                if not stopped then
                    match UnixSystem.checkInvariants system with
                    | [] -> ()
                    | defects -> failwith $"%s{where}: %A{defects}"

                    // Every park agrees with the reference's, by progress.
                    for task in tasks do
                        let expected =
                            Map.tryFind task reference.Parks
                            |> Option.map (fun park ->
                                match park.Call with
                                | Call.Reading (_, count, _) -> 0, count, 0
                                | Call.Writing (_, payload, written, _) -> 1, List.length payload, written
                            )

                        let actual =
                            UnixTaskTable.parkedFor task system.Tasks
                            |> Option.map (fun parked ->
                                match parked with
                                | ParkedSyscall.PipeRead read -> 0, read.Count, 0
                                | ParkedSyscall.PipeWrite write -> 1, write.Count, write.Written
                                | other -> failwith $"%s{where}: task %d{task} parked in %A{other}"
                            )

                        if expected <> actual then
                            failwith $"%s{where}: task %d{task} parked as %A{actual}, expected %A{expected}"

                    // A description exists exactly while something references
                    // it: a descriptor, or a call that has not yet returned.
                    let expectedDescriptions =
                        liveDescriptions reference
                        |> Map.keys
                        |> Seq.map (fun d -> libraryDescription.[d])
                        |> Set.ofSeq

                    let actualDescriptions =
                        FileDescriptorRegistry.descriptions system.Process.FileDescriptors
                        |> Map.filter (fun _ description ->
                            match description.Target with
                            | OpenFileTarget.Pipe (p, _) -> p = pipeId
                            | _ -> false
                        )
                        |> Map.keys
                        |> Set.ofSeq

                    if expectedDescriptions <> actualDescriptions then
                        failwith
                            $"%s{where}: descriptions %A{actualDescriptions} exist, expected %A{expectedDescriptions}"

                    // Gone once nothing references either end.
                    match Map.tryFind pipeId system.Machine.Pipes with
                    | Some pipe -> PipeBuffer.held pipe.Buffer |> shouldEqual (held reference.Buffer)
                    | None -> Map.isEmpty (liveDescriptions reference) |> shouldEqual true

        let gen =
            gen {
                let! platform = Gen.elements platforms
                let! restart = ArbMap.defaults |> ArbMap.generate<bool>
                let! length = Gen.choose (0, 80)
                let! ops = Gen.listOfLength length opGen
                return platform, restart, ops
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen gen) property)

        let closingGen =
            Gen.zip
                (ArbMap.defaults |> ArbMap.generate<bool>)
                (Gen.choose (0, 80)
                 |> Gen.bind (fun length -> Gen.listOfLength length closingOpGen))
            |> Gen.map (fun (restart, ops) -> SimulatedUnixPlatform.macOsArm64, restart, ops)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen closingGen) property)

        // The outcomes a run of this size reaches dozens of times over. The
        // rarer endings of a sleeping call, which need a particular state
        // and signal at once, are enumerated by the table test below.
        let required =
            [
                for flavour in [ "Linux" ; "Darwin" ] do
                    $"%s{flavour} read: sleeps"
                    $"%s{flavour} write: sleeps"
                    $"%s{flavour} write: sleeps having put some in"
                    $"%s{flavour} wake: several"
                    $"%s{flavour} finish read: bytes"
                    $"%s{flavour} finish read: EINTR"
                    $"%s{flavour} finish read: restarts"
                    $"%s{flavour} finish write, with the count already in: wrote"
                "Linux finish read: end of file"
                "Linux finish write: wrote"
                "Linux close: the last descriptor onto a sleeping call's description"
                "Linux finish: the call's return releases its description"
                "Linux wake: fewer than could proceed"
                "Darwin finish read: refused"
                "Darwin finish write: refused"
                "Darwin close: ends a read"
                "Darwin close: ends a write"
                "Darwin close: ends a transfer, one through a dup sleeping on"
                "Darwin close: refused, the transfer woken"
                "Darwin finish read ended by a close: end of file"
                "Darwin finish write ended by a close: EPIPE"
            ]

        let missing = required |> List.filter (fun label -> not (covered.ContainsKey label))

        if not (List.isEmpty missing) then
            failwith $"the property never reached %A{missing}; it reached %A{List.ofSeq covered.Keys |> List.sort}"

    // --- the measured rows, one at a time ---

    /// The task that sleeps in the tests below; the leader makes every change.
    let private sleeper : int = 1

    /// A blocking pipe, its read end on fd 3 and its write end on fd 4, holding
    /// `prefill` bytes, with `sleeper` a task and `SIGUSR1` caught, with
    /// `SA_RESTART` if `restart`, and `SIGPIPE` ignored.
    let private pipeHolding
        (platform : SimulatedUnixPlatform)
        (restart : bool)
        (prefill : int)
        : UnixSystem<int, string>
        =
        let system =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot
            |> Tasks.spawn sleeper

        let system =
            { system with
                Process =
                    { system.Process with
                        Signals =
                            system.Process.Signals
                            |> SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore
                            |> SignalState.setDisposition
                                Signal.SIGUSR1
                                (SignalDisposition.Catch
                                    { SignalCatch.ofHandler "h" with
                                        Restart = restart
                                    })
                    }
            }

        let system =
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (3, 4), system) -> system
            | other -> failwith $"pipe2: %A{other}"

        // Filled by non-blocking writes, as the probe fills it.
        let _, system = UnixSocket.setNonBlocking 4 true system

        let rec fill (remaining : int) (system : UnixSystem<int, string>) =
            if remaining = 0 then
                system
            else
                match
                    WriteOutcomes.admitThenWrite
                        system.Leader
                        4
                        UserBuffer.Mapped
                        (ImmutableArray.CreateRange (payload 99 (min remaining 4096)))
                        system
                with
                | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, system)) -> fill (remaining - int n) system
                | other -> failwith $"filling: %A{other}"

        let system = fill prefill system
        let _, system = UnixSocket.setNonBlocking 4 false system
        system

    let private readerAsleep (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixReadWrite.read sleeper 3 UserBuffer.Mapped 16UL system with
        | Ok (ReadOutcome.WouldBlock _, system) -> system
        | other -> failwith $"expected the read to sleep, got %A{other}"

    let private writerAsleep (count : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match libraryWrite sleeper 4 (payload 7 count) true system with
        | Ok (WriteOutcome.WouldBlock (_, system)) -> system
        | other -> failwith $"expected the write of %d{count} to sleep, got %A{other}"

    let private leaderReads (count : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match ReadOutcomes.read 3 UserBuffer.Mapped (uint64 count) system with
        | Ok (ReadAnswer.Completed bytes, system) when bytes.Length = count -> system
        | other -> failwith $"expected to read %d{count}, got %A{other}"

    let private leaderWrites (count : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match
            WriteOutcomes.admitThenWrite
                system.Leader
                4
                UserBuffer.Mapped
                (ImmutableArray.CreateRange (payload 3 count))
                system
        with
        | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, system)) when int n = count -> system
        | other -> failwith $"expected to write %d{count}, got %A{other}"

    let private closed (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed 0L, system) -> system
        | other -> failwith $"closing %d{fd}: %A{other}"

    let private signalled (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Signals =
                        SignalState.enqueue
                            {
                                Signal = Signal.SIGUSR1
                                Target = ValueSome sleeper
                            }
                            system.Process.Signals
                }
        }

    /// The sleeper's call, finished.
    let private finished (payloadOfWrite : byte list option) (system : UnixSystem<int, string>) : Seen =
        match payloadOfWrite with
        | None -> fst (fromRead (UnixReadWrite.finishRead sleeper system))
        | Some payload -> fst (fromWrite (libraryFinishWrite sleeper payload system))

    /// What the sleeper sleeps in, in the table test.
    [<RequireQualifiedAccess>]
    type private Sleep =
        /// A read of up to 16 bytes from an empty pipe.
        | Reads
        /// A write of `count` bytes into a full pipe.
        | WritesIntoFull of count : int
        /// A write of 70000 bytes into an empty pipe, which puts 65536 in.
        | WritesPartly

    /// What then happens to the pipe.
    [<RequireQualifiedAccess>]
    type private Change =
        | Nothing
        /// The leader writes 3 bytes, or reads 4096.
        | Ready
        /// The leader reads 65536 bytes, room for the whole of any write here.
        | RoomForAll
        /// The leader closes the other end.
        | OtherEndCloses

    /// How the sleeper's call ends, from the measured rows alone
    /// (`pipe-blocking.c`).
    let private measured (linux : bool) (sleep : Sleep) (change : Change) (signal : bool option) : Seen =
        let signalled = signal.IsSome

        let interrupted =
            match signal with
            | Some true -> Seen.Restarts
            | Some false
            | None -> Seen.Failed UnixError.EINTR

        // Darwin answers whichever reached the sleeper first (section J).
        if signalled && change <> Change.Nothing && not linux then
            Seen.Refused
        else

        match sleep, change with
        | Sleep.Reads, Change.Ready -> Seen.ReadBytes (payload 3 3)
        | Sleep.Reads, Change.OtherEndCloses -> Seen.ReadBytes []
        | Sleep.Reads, _ -> if signalled then interrupted else Seen.Sleeps
        | Sleep.WritesIntoFull _, Change.OtherEndCloses -> Seen.Failed UnixError.EPIPE
        | Sleep.WritesIntoFull count, Change.Ready ->
            // Section F: 4096 bytes freed, a slot on Linux. A write of more
            // takes 4096 and then ends with that count if a signal is
            // pending (section H2), and sleeps on otherwise.
            if count <= 4096 then Seen.Wrote (int64 count)
            elif signalled then Seen.Wrote 4096L
            else Seen.Sleeps
        | Sleep.WritesIntoFull count, Change.RoomForAll -> Seen.Wrote (int64 count)
        | Sleep.WritesIntoFull _, Change.Nothing -> if signalled then interrupted else Seen.Sleeps
        // Section E: Linux the count put in, Darwin EPIPE.
        | Sleep.WritesPartly, Change.OtherEndCloses ->
            if linux then
                Seen.Wrote 65536L
            else
                Seen.Failed UnixError.EPIPE
        | Sleep.WritesPartly, Change.Ready -> if signalled then Seen.Wrote 69632L else Seen.Sleeps
        | Sleep.WritesPartly, Change.RoomForAll -> Seen.Wrote 70000L
        // Section D: a write with bytes in returns their count, restart or
        // not.
        | Sleep.WritesPartly, Change.Nothing -> if signalled then Seen.Wrote 65536L else Seen.Sleeps

    [<Test>]
    let ``a sleeping transfer ends as the measured rows say`` () : unit =
        let sleeps =
            [
                Sleep.Reads
                Sleep.WritesIntoFull 1
                Sleep.WritesIntoFull 512
                Sleep.WritesIntoFull 600
                Sleep.WritesIntoFull 4096
                Sleep.WritesIntoFull 8192
                Sleep.WritesPartly
            ]

        let changes =
            [ Change.Nothing ; Change.Ready ; Change.RoomForAll ; Change.OtherEndCloses ]

        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            for sleep in sleeps do
                for change in changes do
                    for signal in [ None ; Some false ; Some true ] do
                        let applicable =
                            match sleep, change with
                            | Sleep.Reads, Change.RoomForAll -> false
                            | _ -> true

                        if applicable then
                            let where = $"%O{platform}, %A{sleep}, %A{change}, signal %A{signal}"
                            let restart = signal = Some true

                            let system, writing =
                                match sleep with
                                | Sleep.Reads -> pipeHolding platform restart 0 |> readerAsleep, None
                                | Sleep.WritesIntoFull count ->
                                    pipeHolding platform restart 65536 |> writerAsleep count, Some (payload 7 count)
                                | Sleep.WritesPartly ->
                                    pipeHolding platform restart 0 |> writerAsleep 70000, Some (payload 7 70000)

                            let system =
                                match change, writing with
                                | Change.Nothing, _ -> system
                                | Change.Ready, None -> leaderWrites 3 system
                                | Change.Ready, Some _ -> leaderReads 4096 system
                                | Change.RoomForAll, _ -> leaderReads 65536 system
                                | Change.OtherEndCloses, None -> closed 4 system
                                | Change.OtherEndCloses, Some _ -> closed 3 system

                            let system = if signal.IsSome then signalled system else system

                            // Woken by any change, or by the signal.
                            UnixWait.wakes (Set.singleton sleeper) system
                            |> List.map fst
                            |> shouldEqual (
                                if change <> Change.Nothing || signal.IsSome then
                                    [ sleeper ]
                                else
                                    []
                            )

                            let expected = measured linux sleep change signal
                            let actual = finished writing system

                            if actual <> expected then
                                failwith $"%s{where}: expected %A{expected}, got %A{actual}"

    [<Test>]
    let ``bytes and room wake the first sleeper to park on Linux, and every sleeper on Darwin`` () : unit =
        // Sections A1 and B1: three sleepers parked in each order.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            for order in [ [ 1 ; 2 ; 3 ] ; [ 3 ; 1 ; 2 ] ; [ 2 ; 3 ; 1 ] ] do
                for writing in [ false ; true ] do
                    let system =
                        pipeHolding platform false (if writing then 65536 else 0)
                        |> Tasks.spawn 2
                        |> Tasks.spawn 3

                    let system =
                        (system, order)
                        ||> List.fold (fun system task ->
                            if writing then
                                match libraryWrite task 4 [ byte task ] true system with
                                | Ok (WriteOutcome.WouldBlock (_, system)) -> system
                                | other -> failwith $"%A{other}"
                            else
                                match UnixReadWrite.read task 3 UserBuffer.Mapped 1UL system with
                                | Ok (ReadOutcome.WouldBlock _, system) -> system
                                | other -> failwith $"%A{other}"
                        )

                    let asleep = Set.ofList order
                    UnixWait.wakes asleep system |> shouldEqual []

                    // One byte, or (Linux) one slot or (Darwin) one byte of room.
                    let system =
                        if writing then
                            leaderReads (if linux then 4096 else 1) system
                        else
                            leaderWrites 1 system

                    let woken = UnixWait.wakes asleep system |> List.map fst
                    woken |> shouldEqual (if linux then [ List.head order ] else order)

                    // A woken sleeper not yet finished stops the next waking.
                    let rest = Set.remove (List.head order) asleep

                    let system =
                        if writing then
                            leaderReads (if linux then 4096 else 1) system
                        else
                            leaderWrites 1 system

                    UnixWait.wakes rest system
                    |> List.map fst
                    |> shouldEqual (if linux then [] else List.tail order)

    [<Test>]
    let ``a sleeper that finds nothing to take sleeps again behind the others`` () : unit =
        // Section A3: two readers, the first woken; another read takes the
        // byte first.
        let system = pipeHolding SimulatedUnixPlatform.linuxX64 false 0 |> Tasks.spawn 2

        let system =
            match UnixReadWrite.read 2 3 UserBuffer.Mapped 1UL (readerAsleep system) with
            | Ok (ReadOutcome.WouldBlock _, system) -> system
            | other -> failwith $"%A{other}"

        let system = leaderWrites 1 system

        UnixWait.wakes (Set.ofList [ sleeper ; 2 ]) system
        |> List.map fst
        |> shouldEqual [ sleeper ]

        let system = leaderReads 1 system

        match UnixReadWrite.finishRead sleeper system with
        | Ok (ReadOutcome.WouldBlock _, system) ->
            let system = leaderWrites 1 system

            UnixWait.wakes (Set.ofList [ sleeper ; 2 ]) system
            |> List.map fst
            |> shouldEqual [ 2 ]
        | other -> failwith $"%A{other}"

    [<Test>]
    let ``a sleeping write of at most PIPE_BUF bytes waits for room for all of it`` () : unit =
        // Section F.
        let linux =
            pipeHolding SimulatedUnixPlatform.linuxX64 false 65536 |> writerAsleep 4096

        let linux = leaderReads 100 linux
        UnixWait.wakes (Set.singleton sleeper) linux |> shouldEqual []
        let linux = leaderReads 3996 linux

        UnixWait.wakes (Set.singleton sleeper) linux
        |> List.map fst
        |> shouldEqual [ sleeper ]

        finished (Some (payload 7 4096)) linux |> shouldEqual (Seen.Wrote 4096L)

        let darwin =
            pipeHolding SimulatedUnixPlatform.macOsArm64 false 65536 |> writerAsleep 512

        let darwin = leaderReads 100 darwin
        UnixWait.wakes (Set.singleton sleeper) darwin |> shouldEqual []
        let darwin = leaderReads 411 darwin
        UnixWait.wakes (Set.singleton sleeper) darwin |> shouldEqual []
        let darwin = leaderReads 1 darwin

        UnixWait.wakes (Set.singleton sleeper) darwin
        |> List.map fst
        |> shouldEqual [ sleeper ]

        finished (Some (payload 7 512)) darwin |> shouldEqual (Seen.Wrote 512L)

        // A write of more than 512 bytes takes what room there is.
        let darwin =
            pipeHolding SimulatedUnixPlatform.macOsArm64 false 65536 |> writerAsleep 600

        let darwin = leaderReads 100 darwin
        finished (Some (payload 7 600)) darwin |> shouldEqual Seen.Sleeps

    [<Test>]
    let ``a sleeping transfer through a buffer naming no storage faults once there is something to move`` () : unit =
        // Section G.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let system = pipeHolding platform false 0

            let system =
                match UnixReadWrite.read sleeper 3 (UserBuffer.Unmapped 1UL) 8UL system with
                | Ok (ReadOutcome.WouldBlock _, system) -> leaderWrites 3 system
                | other -> failwith $"%A{other}"

            match UnixReadWrite.finishRead sleeper system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EFAULT), system) ->
                PipeBuffer.held (UnixMachineState.pipe (PipeId 3L) system.Machine).Buffer
                |> shouldEqual 3
            | other -> failwith $"%O{platform}: %A{other}"

            let system = pipeHolding platform false 65536

            let system =
                match UnixReadWrite.admitWrite sleeper 4 (UserBuffer.Unmapped 1UL) 100UL system with
                | Ok (WriteOutcome.WouldBlock (_, system)) -> leaderReads 4096 system
                | other -> failwith $"%A{other}"

            match UnixReadWrite.admitFinishWrite sleeper system with
            | Ok (WriteOutcome.Returns (WriteResumption.Answered (WriteAnswer.Failed UnixError.EFAULT), system)) ->
                PipeBuffer.held (UnixMachineState.pipe (PipeId 3L) system.Machine).Buffer
                |> shouldEqual 61440
            | other -> failwith $"%O{platform}: %A{other}"

    /// The write by the leader of `count` bytes through `fd`, which must return
    /// without sleeping: its answer.
    let private leaderWriteAnswer (count : int) (fd : int) (system : UnixSystem<int, string>) =
        match
            WriteOutcomes.admitThenWrite
                system.Leader
                fd
                UserBuffer.Mapped
                (ImmutableArray.CreateRange (payload 5 count))
                system
        with
        | Ok (WriteOutcome.Returns (answer, system))
        | Ok (WriteOutcome.ReturnsRaising (answer, _, system)) -> answer, system
        | other -> failwith $"expected the write to return, got %A{other}"

    let private descriptionExists (description : OpenFileDescriptionId) (system : UnixSystem<int, string>) : bool =
        FileDescriptorRegistry.descriptions system.Process.FileDescriptors
        |> Map.containsKey description

    /// `open-file-references.c` section B on Linux: the last close of the read
    /// end under a sleeping read wakes nothing, the read then completes when
    /// given bytes, and the read end closes as the read returns, so the next
    /// write is EPIPE (B1). With a `dup` kept, the end stays open (B2).
    [<Test>]
    let ``Linux: a sleeping read holds the read end open past its last descriptor, until it returns`` () : unit =
        for dupKept in [ false ; true ] do
            let system = pipeHolding SimulatedUnixPlatform.linuxX64 false 0 |> readerAsleep

            let reader =
                FileDescriptorRegistry.tryFindId 3 system.Process.FileDescriptors |> Option.get

            let system =
                if dupKept then
                    match UnixDescriptor.dup 3 system with
                    | SyscallAnswer.Completed _, system -> system
                    | other -> failwith $"%A{other}"
                else
                    system

            let system = closed 3 system
            UnixWait.wakes (Set.singleton sleeper) system |> shouldEqual []
            descriptionExists reader system |> shouldEqual true
            UnixSystem.checkInvariants system |> shouldEqual []

            let system = leaderWrites 3 system

            UnixWait.wakes (Set.singleton sleeper) system
            |> List.map fst
            |> shouldEqual [ sleeper ]

            let system =
                match UnixReadWrite.finishRead sleeper system with
                | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) ->
                    List.ofSeq bytes |> shouldEqual (payload 3 3)
                    returnToUser sleeper system
                | other -> failwith $"dup kept %b{dupKept}: expected the read to return 3 bytes, got %A{other}"

            descriptionExists reader system |> shouldEqual dupKept
            UnixSystem.checkInvariants system |> shouldEqual []

            leaderWriteAnswer 1 4 system
            |> fst
            |> shouldEqual (
                if dupKept then
                    WriteAnswer.Completed 1L
                else
                    WriteAnswer.Failed UnixError.EPIPE
            )

    /// `open-file-references.c` section C on Linux: the last close of the write
    /// end under a sleeping write wakes nothing, the write then completes when
    /// given room, and the write end closes as the write returns, so a reader of
    /// the drained pipe sees end of file (C1). With a `dup` kept, the end stays
    /// open and a non-blocking read is EAGAIN (C2).
    [<Test>]
    let ``Linux: a sleeping write holds the write end open past its last descriptor, until it returns`` () : unit =
        for dupKept in [ false ; true ] do
            let system =
                pipeHolding SimulatedUnixPlatform.linuxX64 false 65536 |> writerAsleep 1

            let writer =
                FileDescriptorRegistry.tryFindId 4 system.Process.FileDescriptors |> Option.get

            let system =
                if dupKept then
                    match UnixDescriptor.dup 4 system with
                    | SyscallAnswer.Completed _, system -> system
                    | other -> failwith $"%A{other}"
                else
                    system

            let system = closed 4 system
            UnixWait.wakes (Set.singleton sleeper) system |> shouldEqual []
            descriptionExists writer system |> shouldEqual true
            UnixSystem.checkInvariants system |> shouldEqual []

            let system = leaderReads 4096 system

            UnixWait.wakes (Set.singleton sleeper) system
            |> List.map fst
            |> shouldEqual [ sleeper ]

            let system =
                match libraryFinishWrite sleeper (payload 7 1) system with
                | Ok (WriteOutcome.Returns (WriteAnswer.Completed 1L, system)) -> returnToUser sleeper system
                | other -> failwith $"dup kept %b{dupKept}: expected the write to return 1, got %A{other}"

            descriptionExists writer system |> shouldEqual dupKept
            UnixSystem.checkInvariants system |> shouldEqual []

            let system = leaderReads (65536 - 4096 + 1) system
            let _, system = UnixSocket.setNonBlocking 3 true system

            match ReadOutcomes.read 3 UserBuffer.Mapped 1UL system with
            | Ok (answer, _) ->
                answer
                |> shouldEqual (
                    if dupKept then
                        ReadAnswer.Failed UnixError.EAGAIN
                    else
                        ReadAnswer.Completed ImmutableArray.Empty
                )
            | Error refusal -> failwith $"%A{refusal}"

    /// `task` asleep in a read of up to 16 bytes through `fd`.
    let private readerAsleepThrough (task : int) (fd : int) (system : UnixSystem<int, string>) =
        match UnixReadWrite.read task fd UserBuffer.Mapped 16UL system with
        | Ok (ReadOutcome.WouldBlock _, system) -> system
        | other -> failwith $"expected task %d{task}'s read through fd %d{fd} to sleep, got %A{other}"

    let private dupped (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixDescriptor.dup fd system with
        | SyscallAnswer.Completed newFd, system -> int newFd, system
        | other -> failwith $"dup of %d{fd}: %A{other}"

    /// Measured (`close-ends-call.c`, sections P1-P5, and `pipe-blocking.c`
    /// section K): on Darwin, closing the descriptor a sleeping transfer was
    /// made through ends it at once, and every other made through it: a read
    /// with end of file, a write with EPIPE whatever it had put in, leaving the
    /// bytes in the pipe, and raising SIGPIPE for the process. The call holds
    /// nothing once the close returns, so with no `dup` its end is gone then;
    /// with one, a later call through the `dup` is answered normally.
    [<Test>]
    let ``Darwin: closing the descriptor a transfer was made through ends every transfer made through it`` () : unit =
        let platform = SimulatedUnixPlatform.macOsArm64

        for keepDup in [ false ; true ] do
            // Two readers through the read end.
            let system =
                pipeHolding platform false 0
                |> Tasks.spawn 2
                |> readerAsleep
                |> readerAsleepThrough 2 3

            let reader =
                FileDescriptorRegistry.tryFindId 3 system.Process.FileDescriptors |> Option.get

            let duplicate, system = if keepDup then dupped 3 system else -1, system
            let system = closed 3 system
            UnixSystem.checkInvariants system |> shouldEqual []
            descriptionExists reader system |> shouldEqual keepDup

            UnixWait.wakes (Set.ofList [ sleeper ; 2 ]) system
            |> List.map fst
            |> shouldEqual [ sleeper ; 2 ]

            let system =
                (system, [ sleeper ; 2 ])
                ||> List.fold (fun system task ->
                    match UnixReadWrite.finishRead task system with
                    | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) when bytes.IsEmpty -> system
                    | other ->
                        failwith
                            $"dup kept %b{keepDup}: expected task %d{task}'s read to end at end of file, got %A{other}"
                )

            UnixSystem.checkInvariants system |> shouldEqual []

            if keepDup then
                let system = leaderWrites 3 system

                match ReadOutcomes.read duplicate UserBuffer.Mapped 16UL system with
                | Ok (ReadAnswer.Completed bytes, _) -> List.ofSeq bytes |> shouldEqual (payload 3 3)
                | other -> failwith $"a later read through the dup: %A{other}"

            // A write of one byte into a full pipe, and one of 70000 bytes into
            // an empty one that has put 65536 in.
            for count, prefill in [ 1, 65536 ; 70000, 0 ] do
                let system = pipeHolding platform false prefill |> writerAsleep count

                let writer =
                    FileDescriptorRegistry.tryFindId 4 system.Process.FileDescriptors |> Option.get

                let duplicate, system = if keepDup then dupped 4 system else -1, system
                let system = closed 4 system
                UnixSystem.checkInvariants system |> shouldEqual []
                descriptionExists writer system |> shouldEqual keepDup

                UnixWait.wakes (Set.singleton sleeper) system
                |> List.map fst
                |> shouldEqual [ sleeper ]

                let system =
                    match libraryFinishWrite sleeper (payload 7 count) system with
                    | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, raised, system)) ->
                        raised
                        |> shouldEqual
                            {
                                Signal = Signal.SIGPIPE
                                Target = ValueNone
                            }

                        system
                    | other ->
                        failwith
                            $"dup kept %b{keepDup}, a write of %d{count}: expected EPIPE and SIGPIPE, got %A{other}"

                UnixSystem.checkInvariants system |> shouldEqual []

                let pipe =
                    match FileDescriptorRegistry.tryFindTarget 3 system.Process.FileDescriptors with
                    | Some (OpenFileTarget.Pipe (pipeId, _)) -> UnixMachineState.pipe pipeId system.Machine
                    | other -> failwith $"%A{other}"

                PipeBuffer.held pipe.Buffer |> shouldEqual 65536

                if keepDup then
                    let system = leaderReads 4096 system

                    match
                        WriteOutcomes.admitThenWrite
                            sleeper
                            duplicate
                            UserBuffer.Mapped
                            (ImmutableArray.CreateRange (payload 5 1))
                            system
                    with
                    | Ok (WriteOutcome.Returns (WriteAnswer.Completed 1L, _)) -> ()
                    | other -> failwith $"a later write through the dup: %A{other}"

    /// The condition a sleeping call was handed as it went to sleep still says
    /// when a close ends it, once the description it named has gone with the
    /// close.
    [<Test>]
    let ``Darwin: the condition a transfer slept on holds once a close has ended it`` () : unit =
        let system = pipeHolding SimulatedUnixPlatform.macOsArm64 false 0

        let condition, system =
            match UnixReadWrite.read sleeper 3 UserBuffer.Mapped 16UL system with
            | Ok (ReadOutcome.WouldBlock condition, system) -> condition, system
            | other -> failwith $"expected the read to sleep, got %A{other}"

        let reader =
            FileDescriptorRegistry.tryFindId 3 system.Process.FileDescriptors |> Option.get

        WakeCondition.satisfied sleeper condition system |> shouldEqual Set.empty

        let system = closed 3 system
        descriptionExists reader system |> shouldEqual false

        WakeCondition.satisfied sleeper condition system
        |> shouldEqual (Set.singleton WakePrimitive.EndedByClose)

    /// Measured (`close-ends-call.c`, sections P6 and P7, and `pipe-blocking.c`
    /// section K): a transfer asleep through a `dup` of the descriptor closed
    /// sleeps on, and finishes normally.
    [<Test>]
    let ``Darwin: a transfer asleep through a dup of the descriptor closed sleeps on`` () : unit =
        let system =
            pipeHolding SimulatedUnixPlatform.macOsArm64 false 0
            |> Tasks.spawn 2
            |> readerAsleep

        let duplicate, system = dupped 3 system
        let system = readerAsleepThrough 2 duplicate system |> closed 3

        UnixWait.wakes (Set.ofList [ sleeper ; 2 ]) system
        |> List.map fst
        |> shouldEqual [ sleeper ]

        let system =
            match UnixReadWrite.finishRead sleeper system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) when bytes.IsEmpty -> system
            | other -> failwith $"expected end of file, got %A{other}"

        let system = leaderWrites 3 system
        UnixWait.wakes (Set.singleton 2) system |> List.map fst |> shouldEqual [ 2 ]

        match UnixReadWrite.finishRead 2 system with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) ->
            List.ofSeq bytes |> shouldEqual (payload 3 3)
            UnixSystem.checkInvariants system |> shouldEqual []
        | other -> failwith $"expected the bytes, got %A{other}"

        // And the same for a closed dup, the sleeper's own descriptor kept.
        let system = pipeHolding SimulatedUnixPlatform.macOsArm64 false 0 |> readerAsleep
        let duplicate, system = dupped 3 system
        let system = closed duplicate system
        UnixWait.wakes (Set.singleton sleeper) system |> shouldEqual []
        UnixSystem.checkInvariants system |> shouldEqual []

    /// A transfer that something besides the close had woken: bytes, room, a
    /// signal, or a read at which a non-blocking writer gives up. Which of the
    /// two Darwin answers is unmeasured, so the close is refused. The reader
    /// left with no writer, and the writer with no reader, would answer as the
    /// close does, so those are served.
    [<Test>]
    let ``Darwin: closing the descriptor of a woken transfer is refused, unless it would answer alike`` () : unit =
        let platform = SimulatedUnixPlatform.macOsArm64

        let refused (fd : int) (system : UnixSystem<int, string>) =
            let description =
                FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors |> Option.get

            match UnixDescriptor.close fd system with
            | Error (CloseRefusal.DarwinWokenTransfer (refusedOn, task)) ->
                (refusedOn, task) |> shouldEqual (description, sleeper)
            | other -> failwith $"expected the close of fd %d{fd} to be refused, got %A{other}"

        pipeHolding platform false 0 |> readerAsleep |> leaderWrites 1 |> refused 3
        pipeHolding platform false 0 |> readerAsleep |> signalled |> refused 3

        pipeHolding platform false 65536
        |> writerAsleep 1
        |> leaderReads 4096
        |> refused 4

        pipeHolding platform false 65536 |> writerAsleep 1 |> signalled |> refused 4

        pipeHolding platform false 65536
        |> writerAsleep 1000
        |> fun system -> UnixSocket.setNonBlocking 4 true system |> snd
        |> leaderReads 1
        |> refused 4

        let system = pipeHolding platform false 0 |> readerAsleep |> closed 4 |> closed 3

        match UnixReadWrite.finishRead sleeper system with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), _) when bytes.IsEmpty -> ()
        | other -> failwith $"expected end of file, got %A{other}"

        let system =
            pipeHolding platform false 65536 |> writerAsleep 1 |> closed 3 |> closed 4

        match libraryFinishWrite sleeper (payload 7 1) system with
        | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, _, _)) -> ()
        | other -> failwith $"expected EPIPE, got %A{other}"

    /// The write a close ends returns before the close does, raising SIGPIPE
    /// for the process then: under the disposition the signal has at the
    /// close, whatever it has by the time the writer's task finishes. One that
    /// would end the process is refused, a close having no way to say so.
    [<Test>]
    let ``Darwin: the close raises an ended write's SIGPIPE under the disposition it has then`` () : unit =
        let withSigpipe (disposition : SignalDisposition<string>) (system : UnixSystem<int, string>) =
            { system with
                Process =
                    { system.Process with
                        Signals = SignalState.setDisposition Signal.SIGPIPE disposition system.Process.Signals
                    }
            }

        let asleep () =
            pipeHolding SimulatedUnixPlatform.macOsArm64 false 65536 |> writerAsleep 1

        // Ignored at the close, and the default by the finish: discarded.
        let system = asleep () |> closed 4 |> withSigpipe SignalDisposition.Default
        SignalState.pending system.Process.Signals |> shouldEqual []

        match libraryFinishWrite sleeper (payload 7 1) system with
        | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, _, after)) ->
            SignalState.pending after.Process.Signals |> shouldEqual []
        | other -> failwith $"expected EPIPE, the process carrying on, got %A{other}"

        // Caught: pending for the process from the close on.
        let system =
            asleep ()
            |> withSigpipe (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
            |> closed 4

        SignalState.pending system.Process.Signals
        |> List.map (fun pending -> pending.Signal, pending.Target)
        |> shouldEqual [ Signal.SIGPIPE, ValueNone ]

        // The default at the close: the process would end.
        match UnixDescriptor.close 4 (asleep () |> withSigpipe SignalDisposition.Default) with
        | Error (CloseRefusal.DarwinEndedWriteSignal (task, EndedWriteSignalRefusal.TerminatesProcess)) ->
            task |> shouldEqual sleeper
        | other -> failwith $"expected the close to be refused, got %A{other}"

    /// Measured (`close-ends-call.c`, section P8): the ending moves the pipe's
    /// timestamps at the close, as the call's return does, and the finish later
    /// moves nothing.
    [<Test>]
    let ``Darwin: the close moves the timestamps of the transfer it ends`` () : unit =
        let later (system : UnixSystem<int, string>) =
            { system with
                Machine = UnixMachineState.advanceClock 1_000_000_000L system.Machine
            }

        let times (fd : int) (system : UnixSystem<int, string>) =
            match UnixPathResolution.fstat fd system with
            | Ok (FileStatusAnswer.Reported status) ->
                status.AccessTime, status.ModificationTime, status.StatusChangeTime
            | other -> failwith $"%A{other}"

        let platform = SimulatedUnixPlatform.macOsArm64

        // The reader: the read end's atime.
        let system = pipeHolding platform false 0 |> later |> readerAsleep |> later
        let duplicate, system = dupped 3 system
        let system = closed 3 system
        let closedAt = UnixMachineState.realtime system.Machine
        let access, _, _ = times duplicate system
        access |> shouldEqual closedAt

        match UnixReadWrite.finishRead sleeper (later system) with
        | Ok (_, after) -> times duplicate after |> shouldEqual (times duplicate system)
        | other -> failwith $"%A{other}"

        // The writer: the write end's mtime and ctime.
        let system = pipeHolding platform false 65536 |> later |> writerAsleep 1 |> later
        let duplicate, system = dupped 4 system
        let system = closed 4 system
        let closedAt = UnixMachineState.realtime system.Machine
        let _, modification, change = times duplicate system
        (modification, change) |> shouldEqual (closedAt, closedAt)

        match libraryFinishWrite sleeper (payload 7 1) (later system) with
        | Ok (WriteOutcome.ReturnsRaising (_, _, after)) ->
            times duplicate after |> shouldEqual (times duplicate system)
        | other -> failwith $"%A{other}"

    [<Test>]
    let ``a sleeping transfer moves Darwin's timestamps when it ends, and not while it sleeps`` () : unit =
        // Section L: the write end's mtime and ctime, and the read end's
        // atime, stay put while the call sleeps, even once a write has put
        // bytes in, and are the moment the call ends, however it ends.
        let later (system : UnixSystem<int, string>) =
            { system with
                Machine = UnixMachineState.advanceClock 1_000_000_000L system.Machine
            }

        let times (fd : int) (system : UnixSystem<int, string>) =
            match UnixPathResolution.fstat fd system with
            | Ok (FileStatusAnswer.Reported status) ->
                status.AccessTime, status.ModificationTime, status.StatusChangeTime
            | other -> failwith $"%A{other}"

        let platform = SimulatedUnixPlatform.macOsArm64

        // Each sleeper, how it is made to end, the descriptor whose times it
        // moves, and whether the change itself moves them first.
        let cases
            : (string *
              bool *
              (UnixSystem<int, string> -> UnixSystem<int, string>) *
              (UnixSystem<int, string> -> UnixSystem<int, string>) *
              int *
              byte list option) list =
            [
                "a read given bytes", false, readerAsleep, leaderWrites 3, 3, None
                "a read signalled", false, readerAsleep, signalled, 3, None
                "a read restarted", true, readerAsleep, signalled, 3, None
                "a read at end of file", false, readerAsleep, closed 4, 3, None
                "a write given room", false, writerAsleep 1, leaderReads 4096, 4, Some (payload 7 1)
                "a write signalled", false, writerAsleep 1, signalled, 4, Some (payload 7 1)
                "a write restarted", true, writerAsleep 1, signalled, 4, Some (payload 7 1)
                "a write whose reader goes", false, writerAsleep 1, closed 3, 4, Some (payload 7 1)
                "a part-written write signalled", false, writerAsleep 70000, signalled, 4, Some (payload 7 70000)
                "a part-written write whose reader goes", false, writerAsleep 70000, closed 3, 4, Some (payload 7 70000)
            ]

        for name, restart, sleep, ending, fd, writing in cases do
            let prefill =
                match writing with
                | Some [ _ ] -> 65536
                | _ -> 0

            let before = pipeHolding platform restart prefill |> later
            let atStart = times fd before
            let asleep = sleep before |> later
            // Nothing moved while it slept.
            times fd asleep |> shouldEqual atStart
            let ended = ending asleep |> later
            let now = UnixMachineState.realtime ended.Machine

            let after =
                match writing with
                | None ->
                    match UnixReadWrite.finishRead sleeper ended with
                    | Ok (ReadOutcome.WouldBlock _, _) -> failwith $"%s{name}: slept again"
                    | Ok (_, after) -> after
                    | other -> failwith $"%s{name}: %A{other}"
                | Some payload ->
                    match libraryFinishWrite sleeper payload ended with
                    | Ok (WriteOutcome.WouldBlock _) -> failwith $"%s{name}: slept again"
                    | Ok (WriteOutcome.Returns (_, after))
                    | Ok (WriteOutcome.ReturnsRaising (_, _, after))
                    | Ok (WriteOutcome.Restarts after) -> after
                    | other -> failwith $"%s{name}: %A{other}"

            let access, modification, change = times fd after

            match writing with
            | None -> access |> shouldEqual now
            | Some _ -> (modification, change) |> shouldEqual (now, now)

    [<Test>]
    let ``a write admitted with room for part of it reads only that part before it sleeps`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let system = pipeHolding platform false 0

            match UnixReadWrite.admitWrite sleeper 4 UserBuffer.Mapped 200000UL system with
            | Ok (WriteOutcome.Returns (WriteAdmission.TransferThenSleep (65536, 200000), admitted)) ->
                match
                    UnixReadWrite.writeThenSleep
                        sleeper
                        4
                        200000
                        (ImmutableArray.CreateRange (payload 7 65536))
                        admitted
                with
                | Ok (WriteOutcome.WouldBlock (_, asleep)) ->
                    match UnixTaskTable.parkedFor sleeper asleep.Tasks with
                    | Some (ParkedSyscall.PipeWrite parked) ->
                        (parked.Count, parked.Written) |> shouldEqual (200000, 65536)
                    | other -> failwith $"%O{platform}: %A{other}"
                | other -> failwith $"%O{platform}: %A{other}"
            | other -> failwith $"%O{platform}: %A{other}"

    [<Test>]
    let ``a write with bytes in returns their count whatever its signals' handlers' flags`` () : unit =
        // Section D: a part-written write returns its count with or without
        // SA_RESTART, so handlers that disagree about it do not matter; one
        // with nothing in is where they would.
        let bothSignals (system : UnixSystem<int, string>) =
            { system with
                Process =
                    { system.Process with
                        Signals =
                            system.Process.Signals
                            |> SignalState.setDisposition
                                Signal.SIGUSR2
                                (SignalDisposition.Catch (SignalCatch.ofHandler "h2"))
                            |> SignalState.enqueue
                                {
                                    Signal = Signal.SIGUSR2
                                    Target = ValueSome sleeper
                                }
                    }
            }
            |> signalled

        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let system = pipeHolding platform true 0 |> writerAsleep 70000 |> bothSignals
            finished (Some (payload 7 70000)) system |> shouldEqual (Seen.Wrote 65536L)

            let system = pipeHolding platform true 65536 |> writerAsleep 1 |> bothSignals

            match UnixReadWrite.admitFinishWrite sleeper system with
            | Error (WriteRefusal.Interruption (SyscallInterruptionRefusal.MixedRestartFlags _)) -> ()
            | other -> failwith $"%O{platform}: %A{other}"

        // Linux: room as well, which the write fills before it returns.
        let system =
            pipeHolding SimulatedUnixPlatform.linuxX64 true 0
            |> writerAsleep 70000
            |> leaderReads 4096
            |> bothSignals

        finished (Some (payload 7 70000)) system |> shouldEqual (Seen.Wrote 69632L)

    [<Test>]
    let ``O_NONBLOCK set while a transfer sleeps ends it once it has made what progress it can`` () : unit =
        // Sections N1 and N2, on both: a write woken by room fills it and
        // returns its count, nothing in or part in.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            for prefill, expected in [ 65536, 4096L ; 0, 69632L ] do
                let system = pipeHolding platform false prefill |> writerAsleep 200000
                // Setting the flag wakes nothing (section I).
                let _, system = UnixSocket.setNonBlocking 4 true system
                UnixWait.wakes (Set.singleton sleeper) system |> shouldEqual []
                let system = leaderReads 4096 system
                finished (Some (payload 7 200000)) system |> shouldEqual (Seen.Wrote expected)

        // Section N5: two sleepers, room or bytes for one. Linux wakes one and
        // the other sleeps on; Darwin wakes both, and the one that finds
        // nothing answers EAGAIN.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            for writing in [ false ; true ] do
                let system =
                    pipeHolding platform false (if writing then 65536 else 0) |> Tasks.spawn 2

                let sleep (task : int) (system : UnixSystem<int, string>) =
                    if writing then
                        match libraryWrite task 4 (payload task 4096) true system with
                        | Ok (WriteOutcome.WouldBlock (_, system)) -> system
                        | other -> failwith $"%A{other}"
                    else
                        match UnixReadWrite.read task 3 UserBuffer.Mapped 1UL system with
                        | Ok (ReadOutcome.WouldBlock _, system) -> system
                        | other -> failwith $"%A{other}"

                let system = system |> sleep sleeper |> sleep 2
                let _, system = UnixSocket.setNonBlocking (if writing then 4 else 3) true system

                let system =
                    if writing then
                        leaderReads 4096 system
                    else
                        leaderWrites 1 system

                let woken = UnixWait.wakes (Set.ofList [ sleeper ; 2 ]) system |> List.map fst
                woken |> shouldEqual (if linux then [ sleeper ] else [ sleeper ; 2 ])

                let finish (task : int) (system : UnixSystem<int, string>) =
                    if writing then
                        fromWrite (libraryFinishWrite task (payload task 4096) system)
                    else
                        fromRead (UnixReadWrite.finishRead task system)

                let first, after = finish sleeper system

                first
                |> shouldEqual (
                    if writing then
                        Seen.Wrote 4096L
                    else
                        Seen.ReadBytes (payload 3 1)
                )

                if not linux then
                    fst (finish 2 (Option.get after)) |> shouldEqual (Seen.Failed UnixError.EAGAIN)

        // Section N3: on Linux a sleeper woken and beaten to what woke it
        // sleeps on, whatever the flag says.
        let system = pipeHolding SimulatedUnixPlatform.linuxX64 false 0 |> readerAsleep
        let _, system = UnixSocket.setNonBlocking 3 true system
        let system = leaderWrites 1 system

        UnixWait.wakes (Set.singleton sleeper) system
        |> List.map fst
        |> shouldEqual [ sleeper ]

        let system = leaderReads 1 system

        fst (fromRead (UnixReadWrite.finishRead sleeper system))
        |> shouldEqual Seen.Sleeps

        let system =
            pipeHolding SimulatedUnixPlatform.linuxX64 false 65536 |> writerAsleep 4096

        let _, system = UnixSocket.setNonBlocking 4 true system
        let system = leaderReads 4096 system |> leaderWrites 4096

        fst (fromWrite (libraryFinishWrite sleeper (payload 7 4096) system))
        |> shouldEqual Seen.Sleeps

    [<Test>]
    let ``on Darwin a read wakes a write too large for its room, which gives up if non-blocking`` () : unit =
        // Darwin wakes every writer at each read, room or not; a 512-byte write
        // with 50 bytes of room sleeps on, unless its description has become
        // non-blocking, when it answers EAGAIN. Linux wakes a writer only when
        // it can write.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
            let size = if linux then 4096 else 512

            for nonBlocking in [ false ; true ] do
                let system = pipeHolding platform false 65536 |> writerAsleep size
                let _, system = UnixSocket.setNonBlocking 4 nonBlocking system
                let system = leaderReads 50 system
                let woken = UnixWait.wakes (Set.singleton sleeper) system |> List.map fst
                woken |> shouldEqual (if nonBlocking && not linux then [ sleeper ] else [])

                if not woken.IsEmpty then
                    finished (Some (payload 7 size)) system
                    |> shouldEqual (Seen.Failed UnixError.EAGAIN)
