namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One step of the descriptor-table property.
[<RequireQualifiedAccess>]
type FcntlOp =
    /// `open(2)` of the file `f`, with the access mode, `O_CLOEXEC`, `O_SYNC`
    /// and `O_NOFOLLOW` as asked.
    | Open of access : FileAccessMode * closeOnExec : bool * synchronous : bool * noFollow : bool
    /// `pipe2(2)`, with `O_NONBLOCK` and `O_CLOEXEC` as asked.
    | Pipe of nonBlocking : bool * closeOnExec : bool
    | Dup of fd : int
    /// `F_DUPFD`, or with `closeOnExec` `F_DUPFD_CLOEXEC`.
    | DupFd of fd : int * minimum : int * closeOnExec : bool
    | GetFd of fd : int
    | SetFd of fd : int * word : int
    | GetFl of fd : int
    | SetFl of fd : int * word : int
    | Dup2 of oldFd : int * newFd : int
    | Dup3 of oldFd : int * newFd : int * closeOnExec : bool * stray : bool
    /// A one-byte `write(2)`.
    | Write of fd : int
    | Close of fd : int

/// `fcntl(2)`, `dup2(2)` and `dup3(2)` against a reference descriptor table
/// written out again from the measurements: each descriptor names a
/// description and carries its own flags, each description carries the status
/// flags, and the flag words are spelled in each flavour's numbering
/// independently of the library's tables.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFcntlReference =

    let private context : string = "TestFcntlReference"

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    [<RequireQualifiedAccess>]
    type private Kind =
        | File
        | PipeRead
        | PipeWrite

    type private Description =
        {
            Kind : Kind
            /// For a pipe's write end made by `Pipe`, its read end's description;
            /// a launched output stream's reader never goes.
            Reader : int option
            Access : int
            NonBlocking : bool
            Synchronous : bool
            DataSynchronous : bool
            NoFollow : bool
            Written : bool
        }

    /// What a call came to: an answer, a refusal at the descriptor bound, or
    /// any other refusal.
    [<RequireQualifiedAccess>]
    type private Outcome =
        | Answered of SyscallAnswer
        | RefusedAtBound of DescriptorLimitRefusal
        | Refused

    type private Reference =
        {
            Darwin : bool
            Platform : SimulatedUnixPlatform
            /// Each descriptor: its description, and FD_CLOEXEC and FD_CLOFORK.
            Fds : Map<int, int * bool * bool>
            Descriptions : Map<int, Description>
            Next : int
        }

    // Each flavour's numbering, written out from the headers and the probe's
    // HEADER rows rather than read from the library.
    let private nonBlockBit (r : Reference) : int = if r.Darwin then 0x4 else 0x800
    let private appendBit (r : Reference) : int = if r.Darwin then 0x8 else 0x400
    let private asyncBit (r : Reference) : int = if r.Darwin then 0x40 else 0x2000
    let private syncBit (r : Reference) : int = if r.Darwin then 0x80 else 0x100000
    let private dsyncBit (r : Reference) : int = if r.Darwin then 0x400000 else 0x1000
    let private cloexecBit (r : Reference) : int = if r.Darwin then 0x1000000 else 0x80000

    let private x64 (r : Reference) : bool =
        SimulatedUnixPlatform.architecture r.Platform = SimulatedUnixArchitecture.X64

    let private largeFileBit (r : Reference) : int = if x64 r then 0x8000 else 0x20000
    let private directBit (r : Reference) : int = if x64 r then 0x4000 else 0x10000
    let private noAtimeBit (r : Reference) : int = 0x40000
    let private noFollowBit (r : Reference) : int = if x64 r then 0x20000 else 0x8000

    let private unmodelled (r : Reference) : int =
        if r.Darwin then
            appendBit r ||| asyncBit r
        else
            appendBit r ||| asyncBit r ||| directBit r ||| noAtimeBit r

    let private statusWord (r : Reference) (d : Description) : int =
        let bit (set : bool) (value : int) = if set then value else 0

        d.Access
        ||| bit d.NonBlocking (nonBlockBit r)
        ||| bit d.Synchronous (syncBit r)
        ||| bit d.DataSynchronous (dsyncBit r)
        ||| bit (not r.Darwin && d.Kind = Kind.File) (largeFileBit r)
        ||| bit (not r.Darwin && d.NoFollow) (noFollowBit r)
        ||| bit (r.Darwin && d.Written) 0x10000

    let private bound (r : Reference) : int =
        SimulatedUnixPlatform.descriptorBound r.Platform

    let private lowestFreeFrom (minimum : int) (r : Reference) : int =
        Seq.initInfinite (fun i -> minimum + i)
        |> Seq.find (fun fd -> not (Map.containsKey fd r.Fds))

    let private add (description : Description) (fd : int) (cloexec : bool) (r : Reference) : Reference =
        { r with
            Fds = Map.add fd (r.Next, cloexec, false) r.Fds
            Descriptions = Map.add r.Next description r.Descriptions
            Next = r.Next + 1
        }

    /// What the reference answers for `op`. Every descriptor a call would make
    /// must lie below the bound; one that would not is refused, naming the
    /// lowest it could have had.
    let private step (op : FcntlOp) (r : Reference) : Outcome * Reference =
        let ok (value : int) =
            Outcome.Answered (SyscallAnswer.Completed (int64 value))

        let failed (error : UnixError) =
            Outcome.Answered (SyscallAnswer.Failed error)

        let atBound (descriptor : int) =
            Outcome.RefusedAtBound
                {
                    Descriptor = descriptor
                    Bound = bound r
                }

        let fresh (kind : Kind) (access : int) =
            {
                Kind = kind
                Reader = None
                Access = access
                NonBlocking = false
                Synchronous = false
                DataSynchronous = false
                NoFollow = false
                Written = false
            }

        let duplicate (oldFd : int) (newFd : int) (cloexec : bool) (r : Reference) =
            let description, _, _ = r.Fds.[oldFd]

            { r with
                Fds = Map.add newFd (description, cloexec, false) r.Fds
            }

        match op with
        | FcntlOp.Open (_, _, _, _) when lowestFreeFrom 0 r >= bound r -> atBound (lowestFreeFrom 0 r), r
        | FcntlOp.Open (access, cloexec, synchronous, noFollow) ->
            let fd = lowestFreeFrom 0 r

            let access =
                match access with
                | FileAccessMode.ReadOnly -> 0
                | FileAccessMode.WriteOnly -> 1
                | FileAccessMode.ReadWrite -> 2

            ok fd,
            add
                { fresh Kind.File access with
                    Synchronous = synchronous
                    DataSynchronous = synchronous && not r.Darwin
                    NoFollow = noFollow
                }
                fd
                cloexec
                r
        | FcntlOp.Pipe _ when lowestFreeFrom (lowestFreeFrom 0 r + 1) r >= bound r ->
            atBound (lowestFreeFrom (lowestFreeFrom 0 r + 1) r), r
        | FcntlOp.Pipe (nonBlocking, cloexec) ->
            let readFd = lowestFreeFrom 0 r

            let r =
                add
                    { fresh Kind.PipeRead 0 with
                        NonBlocking = nonBlocking
                    }
                    readFd
                    cloexec
                    r

            let writeFd = lowestFreeFrom 0 r
            let readDescription = r.Next - 1

            ok readFd,
            add
                { fresh Kind.PipeWrite 1 with
                    NonBlocking = nonBlocking
                    Reader = Some readDescription
                }
                writeFd
                cloexec
                r
        | FcntlOp.Dup fd ->
            if not (Map.containsKey fd r.Fds) then
                failed UnixError.EBADF, r
            elif lowestFreeFrom 0 r >= bound r then
                atBound (lowestFreeFrom 0 r), r
            else
                let newFd = lowestFreeFrom 0 r
                ok newFd, duplicate fd newFd false r
        | FcntlOp.DupFd (fd, minimum, cloexec) ->
            if not (Map.containsKey fd r.Fds) then
                failed UnixError.EBADF, r
            elif minimum < 0 then
                failed UnixError.EINVAL, r
            elif lowestFreeFrom minimum r >= bound r then
                atBound (lowestFreeFrom minimum r), r
            else
                let newFd = lowestFreeFrom minimum r
                ok newFd, duplicate fd newFd cloexec r
        | FcntlOp.GetFd fd ->
            match Map.tryFind fd r.Fds with
            | None -> failed UnixError.EBADF, r
            | Some (_, cloexec, clofork) -> ok ((if cloexec then 1 else 0) ||| (if clofork then 2 else 0)), r
        | FcntlOp.SetFd (fd, word) ->
            match Map.tryFind fd r.Fds with
            | None -> failed UnixError.EBADF, r
            | Some (description, _, _) ->
                ok 0,
                { r with
                    Fds = Map.add fd (description, word &&& 1 <> 0, r.Darwin && word &&& 2 <> 0) r.Fds
                }
        | FcntlOp.GetFl fd ->
            match Map.tryFind fd r.Fds with
            | None -> failed UnixError.EBADF, r
            | Some (description, _, _) -> ok (statusWord r r.Descriptions.[description]), r
        | FcntlOp.SetFl (fd, word) ->
            match Map.tryFind fd r.Fds with
            | None -> failed UnixError.EBADF, r
            | Some _ when word &&& unmodelled r <> 0 -> Outcome.Refused, r
            | Some (description, _, _) ->
                let d = r.Descriptions.[description]

                let d =
                    if r.Darwin then
                        { d with
                            NonBlocking = word &&& nonBlockBit r <> 0
                            Synchronous = word &&& syncBit r <> 0
                            DataSynchronous = word &&& dsyncBit r <> 0
                        }
                    else
                        { d with
                            NonBlocking = word &&& nonBlockBit r <> 0
                        }

                ok 0,
                { r with
                    Descriptions = Map.add description d r.Descriptions
                }
        | FcntlOp.Dup2 (oldFd, newFd) ->
            if newFd < 0 || not (Map.containsKey oldFd r.Fds) then
                failed UnixError.EBADF, r
            elif oldFd = newFd then
                ok newFd, r
            elif newFd >= bound r then
                atBound newFd, r
            else
                ok newFd, duplicate oldFd newFd false r
        | FcntlOp.Dup3 (oldFd, newFd, cloexec, stray) ->
            if r.Darwin then
                Outcome.Refused, r
            elif stray then
                failed UnixError.EINVAL, r
            elif oldFd = newFd then
                failed UnixError.EINVAL, r
            elif newFd < 0 || not (Map.containsKey oldFd r.Fds) then
                failed UnixError.EBADF, r
            elif newFd >= bound r then
                atBound newFd, r
            else
                ok newFd, duplicate oldFd newFd cloexec r
        | FcntlOp.Write fd ->
            match Map.tryFind fd r.Fds with
            | None -> failed UnixError.EBADF, r
            | Some (description, _, _) ->
                let d = r.Descriptions.[description]

                match d.Kind with
                | Kind.PipeRead -> failed UnixError.EBADF, r
                | Kind.File when d.Access = 0 -> failed UnixError.EBADF, r
                | Kind.PipeWrite when
                    d.Reader
                    |> Option.exists (fun reader -> not (r.Fds |> Map.exists (fun _ (named, _, _) -> named = reader)))
                    ->
                    // No reader: EPIPE, with SIGPIPE ignored, and nothing moved.
                    failed UnixError.EPIPE, r
                | Kind.File
                | Kind.PipeWrite ->
                    ok 1,
                    { r with
                        Descriptions =
                            Map.add
                                description
                                { d with
                                    Written = true
                                }
                                r.Descriptions
                    }
        | FcntlOp.Close fd ->
            if Map.containsKey fd r.Fds then
                ok 0,
                { r with
                    Fds = Map.remove fd r.Fds
                }
            else
                failed UnixError.EBADF, r

    let private system (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let seed =
            Map.ofList
                [
                    DirectoryEntryName.parseOrFail context "f",
                    SeedEntry.File (ImmutableArray<byte>.Empty, PermissionBits.parseOrFail context 0o666, None)
                ]

        let image = UnixSystem.initial platform

        match
            UnixBootImage.withFileSystem (UnixTimestamp.ofMillisecondsSinceEpoch 0L) Owners.linuxDefault seed image
        with
        | Ok image ->
            let system =
                (Launched.bootWith (Launched.credentials Owners.root) UnixSystem.pipedStandardStreams 0 (CpuId 0)) image

            // SIGPIPE ignored, so that a write with no reader answers EPIPE.
            { system with
                Process =
                    { system.Process with
                        Signals =
                            SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore system.Process.Signals
                    }
            }
        | Error fault -> failwith $"could not build the system: %A{fault}"

    /// The library's answer to `op`.
    let private library
        (r : Reference)
        (op : FcntlOp)
        (system : UnixSystem<int, string>)
        : Outcome * UnixSystem<int, string>
        =
        let platform = r.Platform

        let ofFcntl (result : Result<SyscallAnswer * UnixSystem<int, string>, FcntlRefusal>) =
            match result with
            | Ok (answer, system) -> Outcome.Answered answer, system
            | Error (FcntlRefusal.UnmodelledStatusFlags _) -> Outcome.Refused, system
            | Error (FcntlRefusal.DescriptorLimit refusal) -> Outcome.RefusedAtBound refusal, system
            | Error refusal -> failwith $"fcntl refused: %s{FcntlRefusal.describe refusal}"

        match op with
        | FcntlOp.Open (access, cloexec, synchronous, noFollow) ->
            let flags =
                { FcntlWorld.opening access with
                    CloseOnExec = cloexec
                    Synchronous = synchronous
                    DataSynchronous = synchronous && not r.Darwin
                    NoFollow = noFollow
                }

            match OpenFlagWords.openPath flags (PathArg.ofText "f") 0o644 system with
            | Ok (answer, system) -> Outcome.Answered answer, system
            | Error (OpenRefusal.DescriptorLimit refusal) -> Outcome.RefusedAtBound refusal, system
            | Error refusal -> failwith $"open refused: %A{refusal}"
        | FcntlOp.Pipe (nonBlocking, cloexec) ->
            let flags =
                (if nonBlocking then nonBlockBit r else 0)
                ||| (if cloexec then cloexecBit r else 0)

            match UnixPipe.pipe2 flags UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (readFd, _), system) ->
                Outcome.Answered (SyscallAnswer.Completed (int64 readFd)), system
            | Error (Pipe2Refusal.DescriptorLimit refusal) -> Outcome.RefusedAtBound refusal, system
            | other -> failwith $"pipe2: %A{other}"
        | FcntlOp.Dup fd ->
            match UnixDescriptor.dup fd system with
            | Ok (answer, system) -> Outcome.Answered answer, system
            | Error refusal -> Outcome.RefusedAtBound refusal, system
        | FcntlOp.DupFd (fd, minimum, cloexec) ->
            let command =
                if cloexec then
                    FcntlWorld.dupFdCloexec platform
                else
                    FcntlWorld.DupFd

            ofFcntl (UnixDescriptor.fcntl fd command minimum system)
        | FcntlOp.GetFd fd -> ofFcntl (UnixDescriptor.fcntl fd FcntlWorld.GetFd 0 system)
        | FcntlOp.SetFd (fd, word) -> ofFcntl (UnixDescriptor.fcntl fd FcntlWorld.SetFd word system)
        | FcntlOp.GetFl fd -> ofFcntl (UnixDescriptor.fcntl fd FcntlWorld.GetFl 0 system)
        | FcntlOp.SetFl (fd, word) -> ofFcntl (UnixDescriptor.fcntl fd FcntlWorld.SetFl word system)
        | FcntlOp.Dup2 (oldFd, newFd) ->
            match UnixDescriptor.dup2 oldFd newFd system with
            | Ok (answer, system) -> Outcome.Answered answer, system
            | Error (Dup2Refusal.DescriptorLimit refusal) -> Outcome.RefusedAtBound refusal, system
            | Error refusal -> failwith $"dup2 refused: %s{Dup2Refusal.describe refusal}"
        | FcntlOp.Dup3 (oldFd, newFd, cloexec, stray) ->
            let flags = (if cloexec then cloexecBit r else 0) ||| (if stray then 0x800 else 0)

            match UnixDescriptor.dup3 oldFd newFd flags system with
            | Ok (answer, system) -> Outcome.Answered answer, system
            | Error (Dup3Refusal.NotProvided _) -> Outcome.Refused, system
            | Error (Dup3Refusal.DescriptorLimit refusal) -> Outcome.RefusedAtBound refusal, system
            | Error refusal -> failwith $"dup3 refused: %s{Dup3Refusal.describe refusal}"
        | FcntlOp.Write fd ->
            match
                WriteOutcomes.admitThenWrite system.Leader fd UserBuffer.Mapped (ImmutableArray.Create 1uy) system
            with
            | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, system))
            | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Completed n, _, system)) ->
                Outcome.Answered (SyscallAnswer.Completed n), system
            | Ok (WriteOutcome.Returns (WriteAnswer.Failed error, system))
            | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed error, _, system)) ->
                Outcome.Answered (SyscallAnswer.Failed error), system
            | other -> failwith $"write: %A{other}"
        | FcntlOp.Close fd ->
            match UnixDescriptor.close fd system with
            | Ok (answer, system) -> Outcome.Answered answer, system
            | Error refusal -> failwith $"close refused: %s{CloseRefusal.describe refusal}"

    /// Which descriptors `fds` name a description in common: the partition the
    /// library and the reference must agree on.
    let private partition (fds : Map<int, 'a>) : Set<Set<int>> =
        fds
        |> Map.toList
        |> List.groupBy snd
        |> List.map (fun (_, members) -> members |> List.map fst |> Set.ofList)
        |> Set.ofList

    let private wordGen (platform : SimulatedUnixPlatform) : Gen<int> =
        let darwin = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Darwin

        let modelled =
            if darwin then
                [ 0x4 ; 0x80 ; 0x400000 ]
            else
                [ 0x800 ; 0x100000 ; 0x1000 ]

        // Bits the flavour's F_SETFL ignores: the access mode, O_CREAT,
        // O_LARGEFILE or Darwin's FWASWRITTEN and FHASLOCK, and a few
        // undefined ones.
        let ignored =
            if darwin then
                [ 1 ; 2 ; 3 ; 0x200 ; 0x4000 ; 0x10000 ; 0x40000000 ]
            else
                [ 1 ; 2 ; 3 ; 0x40 ; 0x8000 ; 0x20000 ; 0x10000 ; 0x4000000 ]

        let unmodelled =
            if darwin then
                [ 0x8 ; 0x40 ]
            else
                [ 0x400 ; 0x2000 ; 0x40000 ; 0x4000 ; 0x10000 ]

        gen {
            let! modelledBits = Gen.subListOf modelled
            let! ignoredBits = Gen.subListOf ignored
            let! stray = Gen.frequency [ 9, Gen.constant [] ; 1, Gen.subListOf unmodelled ]
            return List.fold (|||) 0 (modelledBits @ ignoredBits @ stray)
        }

    let private opGen (platform : SimulatedUnixPlatform) : Gen<FcntlOp> =
        let bound = SimulatedUnixPlatform.descriptorBound platform
        // Around the bound, so that F_DUPFD and dup2 put descriptors just below
        // it and then reach it.
        let nearBound =
            Gen.elements [ bound - 3 ; bound - 2 ; bound - 1 ; bound ; bound + 1 ]

        let fd =
            Gen.frequency
                [
                    12, Gen.choose (0, 12)
                    1, Gen.elements [ -1 ; 40 ; System.Int32.MinValue ]
                    2, nearBound
                ]

        let bool = Gen.elements [ true ; false ]

        let access =
            Gen.elements
                [
                    FileAccessMode.ReadOnly
                    FileAccessMode.WriteOnly
                    FileAccessMode.ReadWrite
                ]

        Gen.frequency
            [
                3, Gen.map4 (fun a c s n -> FcntlOp.Open (a, c, s, n)) access bool bool bool
                2, Gen.map2 (fun n c -> FcntlOp.Pipe (n, c)) bool bool
                2, Gen.map FcntlOp.Dup fd
                3,
                Gen.map3
                    (fun f m c -> FcntlOp.DupFd (f, m, c))
                    fd
                    (Gen.frequency
                        [
                            8, Gen.choose (0, 14)
                            1, Gen.elements [ -1 ; System.Int32.MinValue ]
                            3, nearBound
                        ])
                    bool
                3, Gen.map FcntlOp.GetFd fd
                3, Gen.map2 (fun f w -> FcntlOp.SetFd (f, w)) fd (Gen.elements [ 0 ; 1 ; 2 ; 3 ; -1 ; 4 ; 0x80000 ])
                3, Gen.map FcntlOp.GetFl fd
                3, Gen.map2 (fun f w -> FcntlOp.SetFl (f, w)) fd (wordGen platform)
                4, Gen.map2 (fun o n -> FcntlOp.Dup2 (o, n)) fd fd
                2,
                Gen.map3
                    (fun (o, n) c s -> FcntlOp.Dup3 (o, n, c, s))
                    (Gen.zip fd fd)
                    bool
                    (Gen.frequency [ 5, Gen.constant false ; 1, Gen.constant true ])
                2, Gen.map FcntlOp.Write fd
                3, Gen.map FcntlOp.Close fd
            ]

    [<Test>]
    let ``every descriptor-table call answers as the reference does, and leaves the same table`` () : unit =
        let property (cover : string -> unit) (platform : SimulatedUnixPlatform, ops : FcntlOp list) : unit =
            let mutable system = system platform
            let darwin = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Darwin

            // The launched standard streams, as the reference sees them: each
            // its own pipe end, the first read and the others write.
            let mutable reference =
                {
                    Darwin = darwin
                    Platform = platform
                    Fds = Map.ofList [ 0, (0, false, false) ; 1, (1, false, false) ; 2, (2, false, false) ]
                    Descriptions =
                        Map.ofList
                            [
                                0,
                                {
                                    Kind = Kind.PipeRead
                                    Reader = None
                                    Access = 0
                                    NonBlocking = false
                                    Synchronous = false
                                    DataSynchronous = false
                                    NoFollow = false
                                    Written = false
                                }
                                1,
                                {
                                    Kind = Kind.PipeWrite
                                    Reader = None
                                    Access = 1
                                    NonBlocking = false
                                    Synchronous = false
                                    DataSynchronous = false
                                    NoFollow = false
                                    Written = false
                                }
                                2,
                                {
                                    Kind = Kind.PipeWrite
                                    Reader = None
                                    Access = 1
                                    NonBlocking = false
                                    Synchronous = false
                                    DataSynchronous = false
                                    NoFollow = false
                                    Written = false
                                }
                            ]
                    Next = 3
                }

            for i, op in List.indexed ops do
                let where = $"%O{platform}, op %d{i} (%A{op})"
                let expected, after = step op reference
                let actual, afterSystem = library reference op system

                if actual <> expected then
                    failwith $"%s{where}: expected %A{expected}, got %A{actual}"

                cover (
                    match op, actual with
                    | _, Outcome.Refused -> "refused"
                    | FcntlOp.DupFd _, Outcome.RefusedAtBound _ -> "F_DUPFD at the bound"
                    | FcntlOp.Dup2 _, Outcome.RefusedAtBound _ -> "dup2 at the bound"
                    | _, Outcome.RefusedAtBound _ -> "at the bound"
                    | FcntlOp.Dup2 (o, n), Outcome.Answered (SyscallAnswer.Completed _) when
                        o <> n && Map.containsKey n reference.Fds
                        ->
                        "dup2 onto an open descriptor"
                    | FcntlOp.DupFd (_, m, _), Outcome.Answered (SyscallAnswer.Completed fd) when fd > int64 m ->
                        "F_DUPFD past a taken minimum"
                    | _, Outcome.Answered (SyscallAnswer.Failed error) -> $"%A{error}"
                    | op, Outcome.Answered (SyscallAnswer.Completed _) -> (sprintf "%A" op).Split(' ').[0]
                )

                reference <- after
                system <- afterSystem

                // The whole table, through the queries a client has.
                let registry = UnixSystemState.fileDescriptors system
                let libraryFds = FileDescriptorRegistry.fds registry

                (where, libraryFds |> Map.keys |> Set.ofSeq)
                |> shouldEqual (where, reference.Fds |> Map.keys |> Set.ofSeq)

                (where, partition libraryFds)
                |> shouldEqual (where, partition (reference.Fds |> Map.map (fun _ (d, _, _) -> d)))

                for fd in Map.keys reference.Fds do
                    let _, cloexec, clofork = reference.Fds.[fd]

                    (where, fd, FileDescriptorRegistry.tryFindFlags fd registry)
                    |> shouldEqual (
                        where,
                        fd,
                        Some
                            {
                                CloseOnExec = cloexec
                                CloseOnFork = clofork
                            }
                    )

                UnixSystem.checkInvariants system
                |> fun defects -> (where, defects) |> shouldEqual (where, [])

        let gen =
            gen {
                let! platform = Gen.elements platforms
                let! ops = Gen.listOf (opGen platform)
                return platform, ops
            }

        let coverage =
            CoverageSample.check
                (CoverageSample.inParallel (Config.QuickThrowOnFailure.WithMaxTest 1000))
                (Arb.fromGen gen)
                property

        for label in
            [
                "refused"
                "F_DUPFD at the bound"
                "dup2 at the bound"
                "dup2 onto an open descriptor"
                "F_DUPFD past a taken minimum"
                "EBADF"
                "EINVAL"
                "SetFl"
                "Dup3"
                "Write"
            ] do
            if coverage.Count label = 0 then
                failwith $"the property never reached %s{label}; reached %A{coverage.Reached |> List.sort}"
