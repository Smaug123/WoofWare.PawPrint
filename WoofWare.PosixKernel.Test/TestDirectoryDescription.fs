namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One step of a script run against two open file descriptions onto one
/// directory: `A`, reachable through two descriptors (the second a `dup` of the
/// first), and `B`, a separate `open` of the same directory.
[<RequireQualifiedAccess>]
type DirectoryScriptStep =
    /// Read one entry of `A` through its first descriptor.
    | ReadA
    /// Read one entry of `A` through its `dup`.
    | ReadADup
    /// Read one entry of `B`.
    | ReadB
    /// `lseek(fd, 0, SEEK_SET)` on `A`, through its `dup`.
    | RewindA
    /// `lseek(fd, 0, SEEK_SET)` on `B`.
    | RewindB
    /// Bind a regular file of this name, if it is not bound already.
    | Create of name : string
    /// Unlink this name, if it is bound.
    | Unlink of name : string

/// `UnixNamespace.readDirectoryEntry` and the directory arm of `lseek`: the
/// position of a directory's open file description, what reading it yields,
/// and what the kernel answers for everything that is not a readable
/// directory.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectoryDescription =

    let private context : string = "TestDirectoryDescription"

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private creating : OpenFlags =
        {
            Access = FileAccessMode.WriteOnly
            Create = true
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            Directory = false
        }

    let private reading : OpenFlags =
        { creating with
            Access = FileAccessMode.ReadOnly
            Create = false
        }

    let private directoryReading : OpenFlags =
        { reading with
            Directory = true
        }

    let private rooted (relative : string) : UnixPath =
        UnixPath.parseOrFail context $"/%s{relative}"

    let private completed
        (what : string)
        (answer : SyscallAnswer, system : UnixSystem<int, string>)
        : int64 * UnixSystem<int, string>
        =
        match answer with
        | SyscallAnswer.Completed value -> value, system
        | SyscallAnswer.Failed error -> failwith $"%s{what} failed with %O{error}"

    let private closeFd (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed _, system) -> system
        | other -> failwith $"close %d{fd}: %A{other}"

    let private createFile (path : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let fd, system =
            UnixNamespace.openPath creating (rooted path) 0o644 system
            |> completed $"creat %s{path}"

        closeFd (int fd) system

    let private makeDirectory (path : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixNamespace.mkdir (rooted path) 0o755 system
        |> completed $"mkdir %s{path}"
        |> snd

    let private unlink (path : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixNamespace.unlink (rooted path) system |> completed $"unlink %s{path}" |> snd

    let private openAt (flags : OpenFlags) (path : string) (system : UnixSystem<int, string>) =
        let fd, system =
            UnixNamespace.openPath flags (rooted path) 0 system
            |> completed $"open %s{path}"

        int fd, system

    let private dup (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system = UnixDescriptor.dup fd system |> completed $"dup %d{fd}"
        int fd, system

    let private read (fd : int) (system : UnixSystem<int, string>) =
        match UnixNamespace.readDirectoryEntry fd system with
        | Ok result -> result
        | Error refusal -> failwith $"read of fd %d{fd} was refused: %s{ReadDirectoryRefusal.describe refusal}"

    let private seek (fd : int) (offset : int64) (whence : int) (system : UnixSystem<int, string>) =
        match UnixDescriptor.lseek fd offset whence system with
        | Ok result -> result
        | Error refusal ->
            failwith $"lseek(%d{fd}, %d{offset}, %d{whence}) was refused: %s{LSeekRefusal.describe refusal}"

    let private text (name : DirectoryStreamName) : string = name.ToString ()

    /// Every entry `fd` yields until end-of-directory, capped so that a
    /// position that failed to advance fails the test rather than hanging it.
    let private drain (fd : int) (system : UnixSystem<int, string>) : DirectoryRecord list * UnixSystem<int, string> =
        let rec go (fuel : int) (acc : DirectoryRecord list) (system : UnixSystem<int, string>) =
            if fuel <= 0 then
                failwith $"fd %d{fd} did not reach end-of-directory: %A{List.rev acc |> List.truncate 8}"

            match read fd system with
            | ReadDirectoryAnswer.EndOfDirectory, system -> List.rev acc, system
            | ReadDirectoryAnswer.Entry record, system -> go (fuel - 1) (record :: acc) system
            | ReadDirectoryAnswer.Failed error, _ -> failwith $"reading fd %d{fd} failed with %O{error}"

        go 1000 [] system

    /// A system on `platform` holding `/d` with `names` bound in it as regular
    /// files, and a descriptor open on `/d`.
    let private withDirectory (platform : SimulatedUnixPlatform) (names : string list) : int * UnixSystem<int, string> =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform |> makeDirectory "d"

        let system =
            (system, names) ||> List.fold (fun system n -> createFile $"d/%s{n}" system)

        openAt directoryReading "d" system

    let private inodeOf (path : string) (system : UnixSystem<int, string>) : InodeNumber =
        match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (rooted path) system with
        | Ok (FileStatusAnswer.Reported status) -> status.Inode
        | other -> failwith $"stat %s{path}: %A{other}"

    /// Unsigned byte order over the names' UTF-8, a proper prefix first. Not
    /// `compare` on the byte arrays, which F# orders by length before content.
    let private byteOrder (x : string) (y : string) : int =
        Seq.compareWith compare (Text.Encoding.UTF8.GetBytes x) (Text.Encoding.UTF8.GetBytes y)

    /// Names drawn from a small alphabet, including bytes above 0x7F, so that
    /// a generated directory really does exercise ordering by bytes.
    let private nameGen : Gen<string> =
        Gen.elements [ "a" ; "b" ; "c" ; "A" ; "Z" ; "aa" ; "ab" ; "é" ; "中" ]

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    // ------------------------------------------------------------ a whole scan

    [<Test>]
    let ``a scan yields every name once, then dot-dot and dot, each with the inode stat reports`` () : unit =
        let property (platform : SimulatedUnixPlatform) (names : string list) : unit =
            let names = List.distinct names
            let fd, system = withDirectory platform names
            let records, system = drain fd system

            records
            |> List.map (fun r -> text r.Name)
            |> shouldEqual ((List.sortWith byteOrder names) @ [ ".." ; "." ])

            for record in records do
                let path =
                    match record.Name with
                    | DirectoryStreamName.Dot -> "d"
                    | DirectoryStreamName.DotDot -> ""
                    | DirectoryStreamName.Entry _ -> $"d/%s{text record.Name}"

                record.Inode |> shouldEqual (inodeOf path system)

                let expectedKind =
                    match record.Name with
                    | DirectoryStreamName.Dot
                    | DirectoryStreamName.DotDot -> DirectoryEntryKind.Directory
                    | DirectoryStreamName.Entry _ -> DirectoryEntryKind.RegularFile

                record.Kind |> shouldEqual expectedKind

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip (Gen.elements platforms) (Gen.listOf nameGen)))
                (fun (p, n) -> property p n)
        )

    [<Test>]
    let ``a subdirectory is reported as a directory`` () : unit =
        for platform in platforms do
            let system =
                UnixSystem.initial platform |> makeDirectory "d" |> makeDirectory "d/sub"

            let fd, system = openAt directoryReading "d" system
            let records, _ = drain fd system

            records
            |> List.map (fun r -> text r.Name, r.Kind)
            |> shouldEqual
                [
                    "sub", DirectoryEntryKind.Directory
                    "..", DirectoryEntryKind.Directory
                    ".", DirectoryEntryKind.Directory
                ]

    [<Test>]
    let ``end-of-directory stays end-of-directory`` () : unit =
        for platform in platforms do
            let fd, system = withDirectory platform [ "a" ]
            let _, system = drain fd system

            match read fd system with
            | ReadDirectoryAnswer.EndOfDirectory, _ -> ()
            | other -> failwith $"%O{platform}: expected end-of-directory, got %A{other}"

    // ------------------------------------------------ the position is shared

    /// What the next read of one description yields, computed from the set of
    /// names alone: the reference the property below holds the kernel to.
    [<RequireQualifiedAccess>]
    type private OraclePosition =
        | Start
        | After of name : string
        | ReturnedDotDot
        | ReturnedDot


    let private oracleNext (names : Set<string>) (position : OraclePosition) : (string * OraclePosition) option =
        let fromNames (above : string option) =
            let candidates =
                names
                |> Set.toList
                |> List.filter (fun n ->
                    match above with
                    | None -> true
                    | Some above -> byteOrder n above > 0
                )
                |> List.sortWith byteOrder

            match candidates with
            | least :: _ -> Some (least, OraclePosition.After least)
            | [] -> Some ("..", OraclePosition.ReturnedDotDot)

        match position with
        | OraclePosition.Start -> fromNames None
        | OraclePosition.After name -> fromNames (Some name)
        | OraclePosition.ReturnedDotDot -> Some (".", OraclePosition.ReturnedDot)
        | OraclePosition.ReturnedDot -> None

    let private stepGen : Gen<DirectoryScriptStep> =
        Gen.frequency
            [
                4, Gen.constant DirectoryScriptStep.ReadA
                4, Gen.constant DirectoryScriptStep.ReadADup
                4, Gen.constant DirectoryScriptStep.ReadB
                1, Gen.constant DirectoryScriptStep.RewindA
                1, Gen.constant DirectoryScriptStep.RewindB
                2, Gen.map DirectoryScriptStep.Create nameGen
                2, Gen.map DirectoryScriptStep.Unlink nameGen
            ]

    [<Test>]
    let ``every read follows the description's own position, through any dup, rewind and mutation`` () : unit =
        // One state machine for the three things a description's position must
        // get right at once: a `dup` shares it, a second `open` does not, and
        // `lseek(0)` rewinds it. Mutations are interleaved because the
        // position is a name, and a name can be deleted from under it.
        let property (platform : SimulatedUnixPlatform) (initial : string list) (steps : DirectoryScriptStep list) =
            let initial = List.distinct initial
            let a, system = withDirectory platform initial
            let aDup, system = dup a system
            let b, system = openAt directoryReading "d" system

            let readThrough
                (fd : int)
                (names : Set<string>)
                (position : OraclePosition)
                (system : UnixSystem<int, string>)
                =
                let answer, system = read fd system

                match oracleNext names position, answer with
                | None, ReadDirectoryAnswer.EndOfDirectory -> position, system
                | Some (expected, next), ReadDirectoryAnswer.Entry record when text record.Name = expected ->
                    next, system
                | expected, actual ->
                    failwith $"fd %d{fd} at %A{position} over %A{names}: expected %A{expected}, got %A{actual}"

            let rewind (fd : int) (system : UnixSystem<int, string>) =
                match seek fd 0L 0 system with
                | SyscallAnswer.Completed 0L, system -> system
                | other -> failwith $"lseek(%d{fd}, 0, SEEK_SET): %A{other}"

            ((Set.ofList initial, OraclePosition.Start, OraclePosition.Start, system), steps)
            ||> List.fold (fun (names, positionA, positionB, system) step ->
                match step with
                | DirectoryScriptStep.ReadA ->
                    let positionA, system = readThrough a names positionA system
                    names, positionA, positionB, system
                | DirectoryScriptStep.ReadADup ->
                    let positionA, system = readThrough aDup names positionA system
                    names, positionA, positionB, system
                | DirectoryScriptStep.ReadB ->
                    let positionB, system = readThrough b names positionB system
                    names, positionA, positionB, system
                | DirectoryScriptStep.RewindA -> names, OraclePosition.Start, positionB, rewind aDup system
                | DirectoryScriptStep.RewindB -> names, positionA, OraclePosition.Start, rewind b system
                | DirectoryScriptStep.Create name when not (Set.contains name names) ->
                    Set.add name names, positionA, positionB, createFile $"d/%s{name}" system
                | DirectoryScriptStep.Unlink name when Set.contains name names ->
                    Set.remove name names, positionA, positionB, unlink $"d/%s{name}" system
                | DirectoryScriptStep.Create _
                | DirectoryScriptStep.Unlink _ -> names, positionA, positionB, system
            )
            |> fun (_, _, _, system) -> UnixSystem.checkInvariants system |> shouldEqual []

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip3 (Gen.elements platforms) (Gen.listOf nameGen) (Gen.listOf stepGen |> Gen.resize 60)
                ))
                (fun (p, i, s) -> property p i s)
        )

    [<Test>]
    let ``a dup continues where the original stopped`` () : unit =
        // The example the property above generalises, stated for a reader.
        for platform in platforms do
            let fd, system = withDirectory platform [ "a" ; "b" ; "c" ]
            let _, system = read fd system
            let copy, system = dup fd system
            let rest, _ = drain copy system

            rest
            |> List.map (fun r -> text r.Name)
            |> shouldEqual [ "b" ; "c" ; ".." ; "." ]

    // ----------------------------------------------------------------- lseek

    [<Test>]
    let ``SEEK_CUR answers 0 at the start and is refused part of the way through`` () : unit =
        for platform in platforms do
            let fd, system = withDirectory platform [ "a" ]

            match seek fd 0L 1 system with
            | SyscallAnswer.Completed 0L, _ -> ()
            | other -> failwith $"%O{platform}: SEEK_CUR at the start: %A{other}"

            let _, system = read fd system

            match UnixDescriptor.lseek fd 0L 1 system with
            | Error (LSeekRefusal.DirectoryPosition _) -> ()
            | other -> failwith $"%O{platform}: SEEK_CUR part of the way through: %A{other}"

    [<Test>]
    let ``SEEK_SET to any positive offset is answered, and the next read is refused`` () : unit =
        let property (platform : SimulatedUnixPlatform) (offset : int64) (readFirst : bool) : unit =
            let offset = if offset = Int64.MinValue then 1L else max 1L (abs offset)
            let fd, system = withDirectory platform [ "a" ; "b" ]
            let system = if readFirst then snd (read fd system) else system

            let system =
                match seek fd offset 0 system with
                | SyscallAnswer.Completed answered, system when answered = offset -> system
                | other -> failwith $"lseek(%d{offset}, SEEK_SET): %A{other}"

            match seek fd 0L 1 system with
            | SyscallAnswer.Completed answered, _ when answered = offset -> ()
            | other -> failwith $"SEEK_CUR after SEEK_SET %d{offset}: %A{other}"

            match UnixNamespace.readDirectoryEntry fd system with
            | Error (ReadDirectoryRefusal.UnenumerablePosition (_, refused)) -> refused |> shouldEqual offset
            | other -> failwith $"read after SEEK_SET %d{offset}: %A{other}"

            // And zero rewinds from there.
            let _, system = seek fd 0L 0 system
            let records, _ = drain fd system

            records
            |> List.map (fun r -> text r.Name)
            |> shouldEqual [ "a" ; "b" ; ".." ; "." ]

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip3
                        (Gen.elements platforms)
                        (ArbMap.defaults |> ArbMap.generate<int64>)
                        (Gen.elements [ true ; false ])
                ))
                (fun (p, o, r) -> property p o r)
        )

    [<Test>]
    let ``SEEK_CUR from an lseek-chosen offset is a regular file's arithmetic`` () : unit =
        // Measured on both, with every pair of these starting offsets and
        // SEEK_CUR increments (directory-descriptors.c): the answers are
        // exactly a regular file's, EINVAL below zero, and past INT64_MAX
        // EINVAL on Linux and EOVERFLOW on Darwin.
        let cases : (int64 * int64 * (SimulatedUnixFlavour -> Result<int64, UnixError>)) list =
            [
                5L, -5L, (fun _ -> Ok 0L)
                5L, -6L, (fun _ -> Error UnixError.EINVAL)
                5L, 1L, (fun _ -> Ok 6L)
                Int64.MaxValue,
                1L,
                (fun flavour ->
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> Error UnixError.EINVAL
                    | SimulatedUnixFlavour.Darwin -> Error UnixError.EOVERFLOW
                )
            ]

        for platform in platforms do
            let flavour = SimulatedUnixPlatform.flavour platform

            for start, increment, expected in cases do
                let fd, system = withDirectory platform [ "a" ]
                let _, system = seek fd start 0 system

                let answer =
                    match seek fd increment 1 system with
                    | SyscallAnswer.Completed position, _ -> Ok position
                    | SyscallAnswer.Failed error, _ -> Error error

                answer |> shouldEqual (expected flavour)

    [<Test>]
    let ``SEEK_CUR back to zero reads from the first entry`` () : unit =
        for platform in platforms do
            let fd, system = withDirectory platform [ "a" ]
            let _, system = seek fd 5L 0 system
            let _, system = seek fd -5L 1 system
            let records, _ = drain fd system
            records |> List.map (fun r -> text r.Name) |> shouldEqual [ "a" ; ".." ; "." ]

    [<Test>]
    let ``a failed seek leaves the position where it was`` () : unit =
        for platform in platforms do
            let fd, system = withDirectory platform [ "a" ; "b" ]
            let _, system = read fd system

            match seek fd -1L 0 system with
            | SyscallAnswer.Failed UnixError.EINVAL, system ->
                let rest, _ = drain fd system
                rest |> List.map (fun r -> text r.Name) |> shouldEqual [ "b" ; ".." ; "." ]
            | other -> failwith $"%O{platform}: lseek(-1, SEEK_SET): %A{other}"

    // ------------------------------------------------------ a removed directory

    [<Test>]
    let ``a removed directory yields nothing from any position: ENOENT on Linux, end-of-directory on Darwin``
        ()
        : unit
        =
        // Measured one call at a time on a fresh description, after a partial
        // read and after a full one (directory-descriptors.c, `REMOVED` rows).
        let property (platform : SimulatedUnixPlatform) (names : string list) (readFirst : int) : unit =
            let names = List.distinct names
            let fd, system = withDirectory platform names

            let system =
                (system, List.replicate (readFirst % (List.length names + 4)) ())
                ||> List.fold (fun system () -> snd (read fd system))

            let system =
                (system, names) ||> List.fold (fun system n -> unlink $"d/%s{n}" system)

            let system = UnixNamespace.rmdir (rooted "d") system |> completed "rmdir d" |> snd

            let expected =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> ReadDirectoryAnswer.Failed UnixError.ENOENT
                | SimulatedUnixFlavour.Darwin -> ReadDirectoryAnswer.EndOfDirectory

            let answer, system = read fd system
            answer |> shouldEqual expected

            // Rewinding changes nothing: measured on both.
            let _, system = seek fd 0L 0 system
            fst (read fd system) |> shouldEqual expected

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 (Gen.elements platforms) (Gen.listOf nameGen) (Gen.choose (0, 20))))
                (fun (p, n, r) -> property p n r)
        )

    [<Test>]
    let ``a removed directory at an lseek-chosen offset is ENOENT on Linux and refused on Darwin`` () : unit =
        // Linux answers ENOENT at 5, 2^31-1 and 2^62 alike. Darwin answers 0 at
        // 5 and 2^31-1 but EAGAIN at 2^62, so the offset decides, and this
        // kernel does not model how.
        for platform in platforms do
            let fd, system = withDirectory platform []
            let _, system = seek fd 5L 0 system
            let system = UnixNamespace.rmdir (rooted "d") system |> completed "rmdir d" |> snd

            match SimulatedUnixPlatform.flavour platform, UnixNamespace.readDirectoryEntry fd system with
            | SimulatedUnixFlavour.Linux, Ok (ReadDirectoryAnswer.Failed UnixError.ENOENT, _) -> ()
            | SimulatedUnixFlavour.Darwin, Error (ReadDirectoryRefusal.UnenumerablePosition (_, 5L)) -> ()
            | flavour, other -> failwith $"%O{flavour}: %A{other}"

    // ------------------------------------------------------ not a directory

    [<Test>]
    let ``every descriptor that is not a directory answers its flavour's errno`` () : unit =
        // Measured on every descriptor kind (directory-descriptors.c, `KIND`
        // rows): Linux says ENOTDIR for all of them; Darwin says EINVAL for a
        // readable regular file, EBADF for a write-only one, and ENOTSUP for
        // pipes, sockets and kqueues.
        for platform in platforms do
            let flavour = SimulatedUnixPlatform.flavour platform
            let system : UnixSystem<int, string> = UnixSystem.initial platform |> createFile "f"
            let readable, system = openAt reading "f" system

            let writeOnly, system =
                openAt
                    { reading with
                        Access = FileAccessMode.WriteOnly
                    }
                    "f"
                    system

            let socket, system =
                UnixSocket.createSocket SocketDomain.Unix SocketKind.Stream SocketProtocol.Unspecified system

            let port, registry =
                FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

            let system =
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors = registry
                        }
                }

            let expect (fd : int) (linux : UnixError) (darwin : UnixError) =
                let expected =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> linux
                    | SimulatedUnixFlavour.Darwin -> darwin

                match read fd system with
                | ReadDirectoryAnswer.Failed error, after ->
                    error |> shouldEqual expected
                    after |> shouldEqual system
                | other -> failwith $"%O{flavour}: fd %d{fd}: expected %O{expected}, got %A{other}"

            expect readable UnixError.ENOTDIR UnixError.EINVAL
            expect writeOnly UnixError.ENOTDIR UnixError.EBADF
            expect 0 UnixError.ENOTDIR UnixError.ENOTSUP
            expect 1 UnixError.ENOTDIR UnixError.ENOTSUP
            expect socket UnixError.ENOTDIR UnixError.ENOTSUP
            expect port UnixError.ENOTDIR UnixError.ENOTSUP
            expect 999 UnixError.EBADF UnixError.EBADF

    [<Test>]
    let ``read on a directory descriptor is EISDIR`` () : unit =
        for platform in platforms do
            let fd, system = withDirectory platform [ "a" ]

            match UnixReadWrite.read fd UserBuffer.Mapped 5 system with
            | Ok (ReadAnswer.Failed UnixError.EISDIR, _) -> ()
            | other -> failwith $"%O{platform}: %A{other}"

    // ------------------------------------------------------------------ open

    [<Test>]
    let ``an open without O_DIRECTORY on a directory is readable as one`` () : unit =
        for platform in platforms do
            let system = UnixSystem.initial platform |> makeDirectory "d" |> createFile "d/a"
            let fd, system = openAt reading "d" system
            let records, _ = drain fd system
            records |> List.map (fun r -> text r.Name) |> shouldEqual [ "a" ; ".." ; "." ]

    [<Test>]
    let ``O_DIRECTORY on a regular file is ENOTDIR`` () : unit =
        for platform in platforms do
            let system = UnixSystem.initial platform |> createFile "f"

            match UnixNamespace.openPath directoryReading (rooted "f") 0 system with
            | SyscallAnswer.Failed UnixError.ENOTDIR, _ -> ()
            | other -> failwith $"%O{platform}: %A{other}"

    [<Test>]
    let ``O_DIRECTORY with a flag whose order against it is unmeasured is refused`` () : unit =
        let unmeasured : OpenFlags list =
            [
                { directoryReading with
                    Access = FileAccessMode.WriteOnly
                }
                { directoryReading with
                    Access = FileAccessMode.ReadWrite
                }
                { directoryReading with
                    Create = true
                }
                { directoryReading with
                    Truncate = true
                }
                { directoryReading with
                    NoFollow = true
                }
            ]

        for platform in platforms do
            let system = UnixSystem.initial platform |> makeDirectory "d"

            for flags in unmeasured do
                (fun () -> UnixNamespace.openPath flags (rooted "d") 0 system |> ignore)
                |> shouldFail

    // ------------------------------------------------------------ invariants

    let private forgeTarget
        (fd : int)
        (target : OpenFileTarget)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        let id =
            match FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors with
            | Some (id, _) -> id
            | None -> failwith $"fd %d{fd} is not live"

        { system with
            Process =
                { system.Process with
                    FileDescriptors =
                        FileDescriptorRegistry.Unchecked.mapDescription
                            id
                            (fun d ->
                                { d with
                                    Target = target
                                }
                            )
                            system.Process.FileDescriptors
                }
        }

    [<Test>]
    let ``a description's kind must match the inode it names`` () : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64
            |> makeDirectory "d"
            |> createFile "f"

        let dirFd, system = openAt reading "d" system
        let fileFd, system = openAt reading "f" system
        UnixSystem.checkInvariants system |> shouldEqual []

        let dirInode = inodeOf "d" system
        let fileInode = inodeOf "f" system

        let asFile = forgeTarget dirFd (OpenFileTarget.File (dirInode, 0L)) system

        UnixSystem.checkInvariants asFile
        |> List.exists (fun defect ->
            match defect with
            | UnixSystemDefect.DescriptionKindMismatch (_, inode) -> inode = dirInode
            | _ -> false
        )
        |> shouldEqual true

        let asDirectory =
            forgeTarget
                fileFd
                (OpenFileTarget.Directory (fileInode, DirectoryPosition.Cursor DirectoryCursor.Start))
                system

        UnixSystem.checkInvariants asDirectory
        |> List.exists (fun defect ->
            match defect with
            | UnixSystemDefect.DescriptionKindMismatch (_, inode) -> inode = fileInode
            | _ -> false
        )
        |> shouldEqual true

    let private someInode : InodeNumber = InodeNumber 5L

    let private registryWith (target : OpenFileTarget) (access : FileAccessMode) : FileDescriptorRegistry =
        FileDescriptorRegistry.Unchecked.ofParts
            (Map.ofList [ 0, OpenFileDescriptionId 7L ])
            (Map.ofList
                [
                    OpenFileDescriptionId 7L,
                    {
                        Target = target
                        AccessMode = access
                        NonBlocking = false
                        Flock = None
                    }
                ])
            (OpenFileDescriptionId 9L)

    [<Test>]
    let ``a directory description at an lseek offset must be past zero`` () : unit =
        // Zero is the start, which is readable; holding it as an offset would
        // refuse a read both kernels answer.
        for offset in [ 0L ; -1L ; Int64.MinValue ] do
            FileDescriptorRegistry.checkInvariants (
                registryWith
                    (OpenFileTarget.Directory (someInode, DirectoryPosition.Unenumerable offset))
                    FileAccessMode.ReadOnly
            )
            |> shouldEqual
                [
                    FileDescriptorRegistryDefect.UnenumerableDirectoryPositionNotPositive (
                        OpenFileDescriptionId 7L,
                        offset
                    )
                ]

        FileDescriptorRegistry.checkInvariants (
            registryWith
                (OpenFileTarget.Directory (someInode, DirectoryPosition.Unenumerable 1L))
                FileAccessMode.ReadOnly
        )
        |> shouldEqual []

    [<Test>]
    let ``a directory description is never writable`` () : unit =
        let start =
            OpenFileTarget.Directory (someInode, DirectoryPosition.Cursor DirectoryCursor.Start)

        for access in [ FileAccessMode.WriteOnly ; FileAccessMode.ReadWrite ] do
            FileDescriptorRegistry.checkInvariants (registryWith start access)
            |> shouldEqual [ FileDescriptorRegistryDefect.WritableDirectory (OpenFileDescriptionId 7L) ]

        FileDescriptorRegistry.checkInvariants (registryWith start FileAccessMode.ReadOnly)
        |> shouldEqual []

    [<Test>]
    let ``after Darwin reports a removed directory at its end, SEEK_CUR is not answered as the start`` () : unit =
        // Measured: Darwin's position moves to its end-of-directory value when
        // it reads a removed directory, so the model must not be left at the
        // start, where SEEK_CUR would answer 0.
        let fd, system = withDirectory SimulatedUnixPlatform.macOsArm64 []
        let system = UnixNamespace.rmdir (rooted "d") system |> completed "rmdir d" |> snd
        let _, system = read fd system

        match UnixDescriptor.lseek fd 0L 1 system with
        | Error (LSeekRefusal.DirectoryPosition _) -> ()
        | other -> failwith $"%A{other}"
