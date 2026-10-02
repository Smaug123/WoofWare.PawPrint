namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A process launched with `LaunchDescriptor.Supplied` bytes on descriptor 0:
/// the client's one blocking write of them, which sleeps while the pipe is
/// full and goes on as the process reads.
///
/// Three oracles: the reads a real reader made of a real pipe while a real
/// writer slept in such a write (`suppliedPipe/*.txt`, replayed); a reference
/// model of the measured rule over random payloads and reads; and the epoll
/// wakes a real Linux gave (supplied-pipe-epoll.c), replayed.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSuppliedPipe =

    /// The pattern `supplied-pipe-refill.c` writes, so a replayed read can be
    /// checked byte for byte.
    let private pattern (i : int64) : byte =
        byte ((i * 131L + (i >>> 8) * 7L) &&& 0xffL)

    let private patterned (length : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (Array.init length (fun i -> pattern (int64 i)))

    let private launch (platform : SimulatedUnixPlatform) (bytes : ImmutableArray<byte>) : UnixSystem<int, string> =
        UnixSystem.initial
            platform
            (Map.add 0 (LaunchDescriptor.Supplied bytes) UnixSystem.pipedStandardStreams)
            0
            (CpuId 0)

    /// A read of `count` from descriptor 0, which must be answered.
    let private read (count : int) (system : UnixSystem<int, string>) : ReadAnswer * UnixSystem<int, string> =
        match ReadOutcomes.read 0 UserBuffer.Mapped (uint64 count) system with
        | Ok (answer, system) -> answer, system
        | Error refusal -> failwith $"read(0, %d{count}) was refused: %s{ReadRefusal.describe refusal}"

    let private fionread (system : UnixSystem<int, string>) : int =
        match UnixDescriptor.bytesAvailable 0 UserBuffer.Mapped system with
        | Ok (BytesAvailableAnswer.Reported count) -> count
        | other -> failwith $"FIONREAD on descriptor 0 answered %A{other}"

    /// What `poll` reports for descriptor 0 on Linux.
    let private linuxLevel (system : UnixSystem<int, string>) : uint32 =
        match FileDescriptorRegistry.tryFindId 0 system.Process.FileDescriptors with
        | Some id -> LinuxReadiness.ofDescription id system
        | None -> failwith "descriptor 0 is not open"

    let private stdinPipe (system : UnixSystem<int, string>) : PipeState = system.Machine.Pipes.[PipeId 0L]

    let private writerOpen (system : UnixSystem<int, string>) : bool =
        PipeState.heldByClient PipeEnd.Write (stdinPipe system)

    let private assertInvariants (where : string) (system : UnixSystem<int, string>) : unit =
        match UnixSystem.checkInvariants system with
        | [] -> ()
        | defects -> failwith $"%s{where}: %A{defects}"

    type private Row =
        {
            Case : string
            Length : int
            IsRead : bool
            Count : int
            Returned : int
            Errno : int
            Held : int
            Poll : int
        }

    let private rows (flavour : string) : Row list =
        let assembly = Assembly.GetExecutingAssembly ()
        let name = $"WoofWare.PosixKernel.Test.suppliedPipe.%s{flavour}.txt"

        use stream =
            match assembly.GetManifestResourceStream name with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.filter (fun line -> not (line.StartsWith ("#", StringComparison.Ordinal)))
        |> List.map (fun line ->
            match line.Split ('|') |> Array.map (fun part -> part.Trim ()) with
            | [| case ; call ; held ; poll |] ->
                let case = case.Split (' ', StringSplitOptions.RemoveEmptyEntries)
                let call = call.Split (' ', StringSplitOptions.RemoveEmptyEntries)

                {
                    Case = case.[0]
                    Length = int case.[1]
                    IsRead =
                        match call.[0] with
                        | "R" -> true
                        | "I" -> false
                        | other -> failwith $"unknown operation %s{other} in: %s{line}"
                    Count = int call.[1]
                    Returned = int call.[3]
                    Errno = int call.[4]
                    Held = int held
                    Poll = Convert.ToInt32 (poll, 16)
                }
            | _ -> failwith $"malformed corpus row: %s{line}"
        )

    /// Replay every case from a fresh launch, checking each read's count and
    /// bytes, `FIONREAD`, and whether the client still holds its write end,
    /// which every row's poll states through HUP. On Linux, `poll`'s whole
    /// answer is checked too.
    let private replay (platform : SimulatedUnixPlatform) (flavour : string) (expectedRows : int) : unit =
        let rows = rows flavour
        rows |> List.length |> shouldEqual expectedRows

        let linux =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        let check (where : string) (row : Row) (system : UnixSystem<int, string>) : unit =
            assertInvariants where system
            fionread system |> shouldEqual row.Held
            writerOpen system |> shouldEqual (row.Poll &&& 0x10 = 0)

            if linux then
                linuxLevel system |> shouldEqual (uint32 row.Poll)

        rows
        |> List.groupBy _.Case
        |> List.iter (fun (case, caseRows) ->
            match caseRows with
            | first :: rest when not first.IsRead ->
                let system = launch platform (patterned first.Length)
                check $"%s{case}, before any read" first system

                ((system, 0L, 0), rest)
                ||> List.fold (fun (system, position, index) row ->
                    let where = $"%s{case}, read %d{index}"

                    if not row.IsRead || row.Errno <> 0 then
                        failwith $"%s{where}: the corpus has a row this replay does not expect: %A{row}"

                    let answer, system = read row.Count system

                    match answer with
                    | ReadAnswer.Completed bytes ->
                        bytes.Length |> shouldEqual row.Returned

                        bytes
                        |> Seq.iteri (fun i b ->
                            if b <> pattern (position + int64 i) then
                                failwith $"%s{where}: byte %d{position + int64 i} read back as %d{b}"
                        )
                    | ReadAnswer.Failed error -> failwith $"%s{where}: read failed with %O{error}"

                    check where row system
                    system, position + int64 row.Returned, index + 1
                )
                |> fun (_, position, _) -> position |> shouldEqual (int64 first.Length)
            | _ -> failwith $"%s{case} does not start with the state before its first read"
        )

    [<Test>]
    let ``a Linux pipe the client supplies fills as a real one did`` () : unit =
        replay SimulatedUnixPlatform.linuxArm64 "linux" 3585

    [<Test>]
    let ``a Darwin pipe the client supplies fills as a real one did`` () : unit =
        replay SimulatedUnixPlatform.macOsArm64 "darwin" 3581

    /// The measured rule, counting bytes only: Linux's slots as their lengths,
    /// Darwin's buffer as how many it holds. `Left` is what the client has not
    /// yet written.
    type private Reference =
        {
            Slots : int list
            Held : int
            Left : int
        }

    let private referenceRefill (platform : SimulatedUnixPlatform) (reference : Reference) : Reference =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            let page = SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform)
            let mutable slots = reference.Slots
            let mutable left = reference.Left

            while left > 0 && List.length slots < 16 do
                let taken = min page left
                slots <- slots @ [ taken ]
                left <- left - taken

            {
                Slots = slots
                Held = List.sum slots
                Left = left
            }
        | SimulatedUnixFlavour.Darwin ->
            let taken = min reference.Left (65536 - reference.Held)

            { reference with
                Held = reference.Held + taken
                Left = reference.Left - taken
            }

    let private referenceLaunch (platform : SimulatedUnixPlatform) (length : int) : Reference =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            referenceRefill
                platform
                {
                    Slots = []
                    Held = 0
                    Left = length
                }
        | SimulatedUnixFlavour.Darwin ->
            // The first write grows the buffer to fit, as far as 64 KiB.
            let held = min length 65536

            {
                Slots = []
                Held = held
                Left = length - held
            }

    let private referenceRead
        (platform : SimulatedUnixPlatform)
        (count : int)
        (reference : Reference)
        : int * Reference
        =
        let taking = min count reference.Held

        let rec drain (remaining : int) (slots : int list) =
            match slots with
            | slot :: rest when remaining > 0 ->
                if slot <= remaining then
                    drain (remaining - slot) rest
                else
                    (slot - remaining) :: rest
            | _ -> slots

        let slots = drain taking reference.Slots

        taking,
        referenceRefill
            platform
            {
                Slots = slots
                Held = reference.Held - taking
                Left = reference.Left
            }

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private lengthGen : Gen<int> =
        Gen.frequency
            [
                2, Gen.choose (0, 600)
                2, Gen.choose (60000, 70000)
                3, Gen.choose (0, 300000)
                1, Gen.elements [ 65535 ; 65536 ; 65537 ; 69632 ; 1048576 ]
            ]

    let private countGen : Gen<int> =
        Gen.frequency
            [
                3, Gen.choose (1, 64)
                3, Gen.choose (1, 5000)
                2, Gen.choose (1, 70000)
                1, Gen.elements [ 511 ; 512 ; 513 ; 4095 ; 4096 ; 4097 ; 65536 ]
            ]

    [<Test>]
    let ``a process reads every supplied byte in order, then end of file, as the reference fills the pipe`` () : unit =
        let property (platform : SimulatedUnixPlatform, length : int, counts : int list) : unit =
            let bytes = patterned length
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            let check (where : string) (reference : Reference) (system : UnixSystem<int, string>) : unit =
                assertInvariants where system
                fionread system |> shouldEqual reference.Held
                writerOpen system |> shouldEqual (reference.Left > 0)

                if linux then
                    linuxLevel system
                    |> shouldEqual (
                        (if reference.Held > 0 then
                             EpollEvents.In ||| EpollEvents.RdNorm
                         else
                             0u)
                        ||| (if reference.Left = 0 then EpollEvents.Hup else 0u)
                    )

            let system = launch platform bytes
            let reference = referenceLaunch platform length
            check "at launch" reference system

            let mutable system = system
            let mutable reference = reference
            let mutable position = 0
            let mutable index = 0

            // The generated counts, cycled for 300 reads; then reads of 64 KiB
            // until end of file, so that small counts finish a large payload in
            // a bounded number of steps; then the generated counts once more,
            // all past end of file.
            let counts =
                seq {
                    yield! Seq.replicate 300 counts |> Seq.concat |> Seq.truncate 300

                    while position < length do
                        yield 65536

                    yield! counts
                }

            for count in counts do
                let answer, after = read count system
                let expected, afterReference = referenceRead platform count reference
                let where = $"read %d{index} of %d{count} at %d{position}"

                match answer with
                | ReadAnswer.Completed got ->
                    got.Length |> shouldEqual expected
                    // End of file only once every byte has been read.
                    (got.Length = 0) |> shouldEqual (position = length)

                    for i in 0 .. got.Length - 1 do
                        if got.[i] <> bytes.[position + i] then
                            failwith $"%s{where}: byte %d{position + i} read back wrong"
                | ReadAnswer.Failed error -> failwith $"%s{where}: failed with %O{error}"

                check where afterReference after
                position <- position + expected
                system <- after
                reference <- afterReference
                index <- index + 1

            position |> shouldEqual length

        let gen =
            gen {
                let! platform = Gen.elements platforms
                let! length = lengthGen
                let! counts = Gen.nonEmptyListOf countGen
                return platform, length, counts
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``no bytes supplied is end of file at once, the launch every client used to get`` () : unit =
        for platform in platforms do
            let system = launch platform ImmutableArray.Empty
            assertInvariants "at launch" system
            writerOpen system |> shouldEqual false
            fionread system |> shouldEqual 0

            match read 10 system with
            | ReadAnswer.Completed bytes, _ -> bytes.IsEmpty |> shouldEqual true
            | other -> failwith $"%A{other}"

    /// The edge-triggered epoll registrations of supplied-pipe-epoll.c on a
    /// Linux read end, each step's reads and what `epoll_wait(0)` returned
    /// after it: the events, or `None` for no entry.
    let private epollCases : (string * int * uint32 * (int * int * uint32 option) list * uint32 option) list =
        let inRdNorm = EpollEvents.In ||| EpollEvents.RdNorm

        [
            "closed",
            20000,
            inRdNorm,
            [
                100, 100, None
                1000, 1000, None
                4096, 4096, None
                10000, 10000, None
                10000, 4804, None
            ],
            Some 0x51u
            "blocked 200000",
            200000,
            inRdNorm,
            [
                100, 100, None
                4000, 4000, None
                65536, 65532, Some 0x41u
                65536, 65536, Some 0x51u
                65536, 64832, None
                65536, 0, None
                65536, 0, None
            ],
            Some 0x41u
            "blocked 70000",
            70000,
            inRdNorm,
            [
                100, 100, None
                4000, 4000, None
                65536, 65532, Some 0x51u
                65536, 368, None
                65536, 0, None
                65536, 0, None
                65536, 0, None
            ],
            Some 0x41u
            "last", 65636, inRdNorm, [ 4096, 4096, Some 0x51u ; 65536, 61540, None ; 65536, 0, None ], Some 0x41u
            "lastout", 65636, EpollEvents.Out, [ 4096, 4096, Some 0x10u ; 65536, 61540, None ; 65536, 0, None ], None
            "blockedout",
            200000,
            EpollEvents.Out,
            [
                100, 100, None
                4000, 4000, None
                65536, 65532, None
                65536, 65536, Some 0x10u
                65536, 64832, None
                65536, 0, None
                65536, 0, None
            ],
            None
        ]

    [<Test>]
    let ``epoll on a supplied pipe's read end is woken as a real Linux woke it`` () : unit =
        for name, length, interest, steps, atAdd in epollCases do
            let system = launch SimulatedUnixPlatform.linuxArm64 (patterned length)

            let port, system =
                match UnixPoll.epollCreate1 0 system with
                | Ok (Ok (port, system)) -> port, system
                | other -> failwith $"%s{name}: epoll_create1 answered %A{other}"

            let system =
                match
                    UnixPoll.epollCtl
                        port
                        1
                        0
                        (EpollEventArgument.Readable (interest ||| EpollEvents.EdgeTriggered, 7UL))
                        system
                with
                | Ok (EpollCtlAnswer.Changed, system) -> system
                | other -> failwith $"%s{name}: the registration answered %A{other}"

            let wait (where : string) (system : UnixSystem<int, string>) : uint32 option * UnixSystem<int, string> =
                match UnixPoll.epollWait 0 port 8 UserBuffer.Mapped 0 system with
                | Ok (EpollWaitOutcome.Answered [], system) -> None, system
                | Ok (EpollWaitOutcome.Answered [ 7UL, events ], system) -> Some events, system
                | other -> failwith $"%s{where}: epoll_wait answered %A{other}"

            let reported, system = wait $"%s{name} at ADD" system
            reported |> shouldEqual atAdd

            (system, steps)
            ||> List.fold (fun system (count, returned, expected) ->
                let where = $"%s{name}, read of %d{count}"

                match read count system with
                | ReadAnswer.Completed bytes, system ->
                    bytes.Length |> shouldEqual returned
                    let reported, system = wait where system
                    reported |> shouldEqual expected
                    assertInvariants where system
                    system
                | other -> failwith $"%s{where}: %A{other}"
            )
            |> fun system ->
                let reported, _ = wait $"%s{name} when idle" system
                reported |> shouldEqual None

    [<Test>]
    let ``closing the last descriptor onto a pipe the client is still writing frees it`` () : unit =
        for platform in platforms do
            let system = launch platform (patterned 200000)
            writerOpen system |> shouldEqual true

            let duplicate, system =
                match UnixDescriptor.dup 0 system with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"dup(0) answered %A{other}"

            let closeOrFail (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
                match UnixDescriptor.close fd system with
                | Ok (SyscallAnswer.Completed 0L, system) -> system
                | other -> failwith $"close(%d{fd}) answered %A{other}"

            // A duplicate still reads the pipe, so the client goes on writing.
            let system = closeOrFail 0 system
            assertInvariants "after closing descriptor 0" system
            writerOpen system |> shouldEqual true

            // The last reader gone, the client's write fails and it closes, and
            // nothing holds the pipe.
            let system = closeOrFail duplicate system
            assertInvariants "after closing the duplicate" system
            Map.containsKey (PipeId 0L) system.Machine.Pipes |> shouldEqual false

    [<Test>]
    let ``a Linux writer resuming fills a free slot rather than merging into the newest`` () : unit =
        let platform = SimulatedUnixPlatform.linuxX64
        // Fourteen full slots and a fifteenth holding 100 bytes, so one slot is
        // free and the newest has room after its end.
        let _, buffer =
            PipeBuffer.write (patterned (14 * 4096 + 100)) (PipeBuffer.empty platform)

        PipeBuffer.writable buffer |> shouldEqual true

        let taken, buffer = PipeBuffer.resume (patterned 50) 0 buffer
        taken |> shouldEqual 50
        // A write would have merged its 50 bytes into the newest slot, leaving
        // a slot free and the write end ready; the resumed write took the free
        // slot.
        PipeBuffer.writable buffer |> shouldEqual false
        PipeBuffer.held buffer |> shouldEqual (14 * 4096 + 150)

    [<Test>]
    let ``a Darwin writer resuming takes what fits, however few bytes it has left`` () : unit =
        let platform = SimulatedUnixPlatform.macOsArm64
        let written, buffer = PipeBuffer.write (patterned 65836) (PipeBuffer.empty platform)
        written |> shouldEqual 65536
        let _, buffer = PipeBuffer.read 50 buffer
        // 300 bytes left: a fresh write of 300 would wait for room for all of
        // them, but the resumed write takes the 50 there is room for.
        let taken, buffer = PipeBuffer.resume (patterned 65836) written buffer
        taken |> shouldEqual 50
        PipeBuffer.held buffer |> shouldEqual 65536

    [<Test>]
    let ``a Darwin buffer that has not grown to its largest is not resumed`` () : unit =
        let platform = SimulatedUnixPlatform.macOsArm64
        let _, buffer = PipeBuffer.write (patterned 1000) (PipeBuffer.empty platform)

        Assert.Throws<Exception> (fun () -> PipeBuffer.resume (patterned 10) 0 buffer |> ignore)
        |> ignore

    [<Test>]
    let ``a pipe with room while its client sleeps in a write is a defect`` () : unit =
        for platform in platforms do
            let system = launch platform (patterned 100000)
            assertInvariants "at launch" system

            // Emptied behind the client's back: it would have written into the
            // room, so a state where it has not is not one the kernel reaches.
            let emptied =
                { system with
                    Machine =
                        { system.Machine with
                            Pipes =
                                Map.add
                                    (PipeId 0L)
                                    { stdinPipe system with
                                        Buffer = snd (PipeBuffer.read 65536 (stdinPipe system).Buffer)
                                    }
                                    system.Machine.Pipes
                        }
                }

            UnixSystem.checkInvariants emptied
            |> shouldEqual [ UnixSystemDefect.SuppliedPipeHasRoom (PipeId 0L) ]
