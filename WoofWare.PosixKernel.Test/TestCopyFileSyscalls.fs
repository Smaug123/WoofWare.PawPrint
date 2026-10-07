namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The Linux syscalls a file copy is made of: `futimens(2)` with explicit
/// times (glibc's `utimensat(2)` with a null pathname), `ioctl(FICLONE)` and
/// `copy_file_range(2)` at the descriptions' own offsets.
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/copy-file-syscalls.c`,
/// run on Linux 6.18.5 (aarch64, root in the container, tmpfs and ext4 alike);
/// its output is beside it, and both kinds matrices are replayed cell by cell
/// from that output, embedded; the rest are
/// literals here, and the copy's arithmetic is held to a naive reference over
/// generated files.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCopyFileSyscalls =

    let private context : string = "TestCopyFileSyscalls"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw
    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw

    let private owner (user : uint32) (group : uint32) : InodeOwner =
        {
            User = uid user
            Group = gid group
        }

    let private mode (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private bytes (text : string) : ImmutableArray<byte> =
        Text.Encoding.ASCII.GetBytes text |> ImmutableArray.CreateRange

    let private u1000 : Credentials = Credentials.ofIds (uid 1000u) (gid 1000u) []

    let private root : Credentials = Owners.root

    /// The clock `systemOn` starts every process at, past every timestamp of
    /// a hand-built filesystem.
    let private later : int64 = 5_000_000_000L

    /// A filesystem holding `files`, each `(name, owner, mode, contents)`
    /// directly under the root, every inode stamped at the epoch.
    let private filesystem (files : (string * InodeOwner * int * string) list) : VirtualFileSystem =
        files
        |> List.fold
            (fun vfs (fileName, by, bits, contents) ->
                VirtualFileSystem.createFile
                    (VirtualFileSystem.root vfs)
                    (name fileName)
                    (mode bits)
                    by
                    epoch
                    (bytes contents)
                    vfs
                |> function
                    | Ok (_, vfs) -> vfs
                    | Error error -> failwith $"creating %s{fileName}: %O{error}"
            )
            (VirtualFileSystem.empty epoch (owner 0u 0u))

    let private systemOnWith
        (configure : UnixBootImage<int, string> -> UnixBootImage<int, string>)
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (vfs : VirtualFileSystem)
        : UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context credentials
            |> configure
            |> UnixBootImage.boot

        { system with
            Machine =
                { UnixMachineState.advanceClock later system.Machine with
                    FileSystem = vfs
                }
            Process =
                { system.Process with
                    CurrentDirectoryInode = VirtualFileSystem.root vfs
                }
        }

    let private systemOn
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (vfs : VirtualFileSystem)
        : UnixSystem<int, string>
        =
        systemOnWith id platform credentials vfs

    let private inodeAt (system : UnixSystem<int, string>) (fileName : string) : InodeNumber =
        match
            UnixPathResolution.resolvePath
                AtDirectory.CurrentDirectory
                SymlinkPolicy.NoFollowFinal
                (UnixPath.parseOrFail context ("/" + fileName))
                system
        with
        | Ok inode -> inode
        | Error error -> failwith $"/%s{fileName}: %O{error}"

    let private entryOf (system : UnixSystem<int, string>) (fileName : string) : Inode =
        match VirtualFileSystem.tryGet (inodeAt system fileName) system.Machine.FileSystem with
        | Some entry -> entry
        | None -> failwith $"/%s{fileName} is absent"

    let private contentsOf (system : UnixSystem<int, string>) (fileName : string) : byte[] =
        match (entryOf system fileName).Content with
        | InodeContent.RegularFile (contents, _) -> contents |> Seq.toArray
        | other -> failwith $"/%s{fileName} is %A{other}"

    let private modeOf (system : UnixSystem<int, string>) (fileName : string) : int =
        PermissionBits.toInt (Inode.permissions (entryOf system fileName))

    let private withRegistry
        (registry : FileDescriptorRegistry)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        UnixSystemState.withFileDescriptors registry system

    let private opened
        (fileName : string)
        (access : FileAccessMode)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, registry =
            FileDescriptorRegistry.openFile (inodeAt system fileName) access (UnixSystemState.fileDescriptors system)

        fd, withRegistry registry system

    let private offsetOf (fd : int) (system : UnixSystem<int, string>) : int64 =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.File (_, offset)) -> offset
        | other -> failwith $"fd %d{fd} names %A{other}"

    let private seekTo (fd : int) (offset : int64) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        withRegistry (FileDescriptorRegistry.setOffset fd offset (UnixSystemState.fileDescriptors system)) system

    let private answerText (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed count -> string<int64> count
        | SyscallAnswer.Failed error -> $"%O{error}"

    // ------------------------------------------------------------- descriptor kinds

    /// The kinds of descriptor the probe made, by the name it printed.
    let private kinds : string list =
        [
            "file-ro"
            "file-wo"
            "file-rw"
            "dir"
            "pipe-r"
            "pipe-w"
            "socket"
            "port"
            "closed"
            "fd9999"
        ]

    /// A descriptor of the kind the probe called `kind`, onto `fileName` where
    /// it names a file. "closed" is a number nothing was ever given, as the
    /// probe's was by the time it was used.
    let private make
        (kind : string)
        (fileName : string)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        match kind with
        | "file-ro" -> opened fileName FileAccessMode.ReadOnly system
        | "file-wo" -> opened fileName FileAccessMode.WriteOnly system
        | "file-rw" -> opened fileName FileAccessMode.ReadWrite system
        | "dir" ->
            let fd, registry =
                FileDescriptorRegistry.openDirectory
                    (VirtualFileSystem.root system.Machine.FileSystem)
                    (UnixSystemState.fileDescriptors system)

            fd, withRegistry registry system
        | "pipe-r"
        | "pipe-w" ->
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (readFd, writeFd), system) ->
                (if kind = "pipe-r" then readFd else writeFd), system
            | other -> failwith $"pipe2: %A{other}"
        | "socket" ->
            match UnixSocket.socket 2 1 0 system with
            | Ok (Ok (fd, system)) -> fd, system
            | other -> failwith $"socket: %A{other}"
        | "port" ->
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok (fd, system)) -> fd, system
            | other -> failwith $"epoll_create1: %A{other}"
        | "closed" -> 700, system
        | "fd9999" -> 9999, system
        | other -> failwith $"unknown kind %s{other}"

    let private kindsSystemWith
        (configure : UnixBootImage<int, string> -> UnixBootImage<int, string>)
        : UnixSystem<int, string>
        =
        systemOnWith
            configure
            SimulatedUnixPlatform.linuxX64
            root
            (filesystem [ "ksrc", owner 0u 0u, 0o644, "hello" ; "kdst", owner 0u 0u, 0o644, "" ])

    let private kindsSystem () : UnixSystem<int, string> = kindsSystemWith id

    /// The probe's two kinds matrices as it printed them on tmpfs: the lines
    /// starting "in=" between the tmpfs run's header and the ext4 run's, whose
    /// rows are identical.
    let private kindsRows : Lazy<string list> =
        lazy
            let assembly = Assembly.GetExecutingAssembly ()
            let resource = "WoofWare.PosixKernel.Test.copyFileSyscalls.linux.txt"

            use stream =
                match assembly.GetManifestResourceStream resource with
                | null -> failwith $"embedded resource %s{resource} not found"
                | stream -> stream

            use reader = new StreamReader (stream)

            let lines =
                reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
                |> Array.toList
                |> List.map _.Trim()

            let tmpfs =
                lines
                |> List.skipWhile (fun line -> not (line.StartsWith ("base /dev/shm", StringComparison.Ordinal)))
                |> List.takeWhile (fun line -> line <> "===EXT4")

            let ext4 = lines |> List.skipWhile (fun line -> line <> "===EXT4")

            let rows (section : string list) =
                section
                |> List.filter (fun line -> line.StartsWith ("in=", StringComparison.Ordinal))

            if rows tmpfs <> rows ext4 then
                failwith
                    "the probe's tmpfs and ext4 kinds rows differ; decide which filesystem the model is answering for"

            rows tmpfs

    let private copyFileRangeAnswer
        (inFd : int)
        (outFd : int)
        (length : uint64)
        (flags : int)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match UnixReadWrite.copyFileRange inFd outFd length flags system with
        | Ok result -> result
        | Error refusal ->
            failwith
                $"copy_file_range(%d{inFd}, %d{outFd}, %d{length}, %d{flags}): %s{CopyFileRangeRefusal.describe refusal}"

    [<Test>]
    let ``copy_file_range answers the measured matrix over every pair of descriptor kinds`` () : unit =
        let rows = kindsRows.Force () |> List.filter (fun line -> line.Contains "len5=")

        rows.Length |> shouldEqual 100

        for row in rows do
            let parts = row.Split ('|', StringSplitOptions.TrimEntries)
            let header = parts.[0].Split (' ', StringSplitOptions.RemoveEmptyEntries)
            let inKind = header.[0].Substring "in=".Length
            let outKind = header.[1].Substring "out=".Length

            let measured =
                parts.[1..] |> Array.map (fun cell -> cell.Substring (cell.IndexOf '=' + 1))

            let actual =
                [| 5UL, 0 ; 0UL, 0 ; 5UL, 1 |]
                |> Array.map (fun (length, flags) ->
                    let inFd, system = make inKind "ksrc" (kindsSystem ())
                    let outFd, system = make outKind "kdst" system
                    copyFileRangeAnswer inFd outFd length flags system |> fst |> answerText
                )

            if actual <> measured then
                failwith $"%s{row}: the model answered %A{actual}"

    [<Test>]
    let ``FICLONE answers the measured matrix over every pair of descriptor kinds`` () : unit =
        let rows = kindsRows.Force () |> List.filter (fun line -> line.Contains " : ")

        rows.Length |> shouldEqual 10

        for row in rows do
            let inKind = row.Substring("in=".Length).Split(' ').[0]

            let cells =
                row.Substring(row.IndexOf " : " + 3).Split (' ', StringSplitOptions.RemoveEmptyEntries)

            for cell in cells do
                let outKind = cell.Substring (0, cell.IndexOf '=')
                let measured = cell.Substring (cell.IndexOf '=' + 1)
                let source, system = make inKind "ksrc" (kindsSystem ())
                let destination, system = make outKind "kdst" system

                match UnixDescriptor.fileClone destination source system with
                | Ok error ->
                    if $"%O{error}" <> measured then
                        failwith $"in=%s{inKind} out=%s{outKind}: measured %s{measured}, the model answered %O{error}"
                | Error refusal ->
                    failwith $"in=%s{inKind} out=%s{outKind}: refused, %s{FileCloneRefusal.describe refusal}"

    [<Test>]
    let ``copy_file_range and FICLONE are Linux calls, and refuse a mount they were not measured on`` () : unit =
        let darwin =
            systemOn
                SimulatedUnixPlatform.macOsArm64
                (Credentials.ofIds (uid 501u) (gid 20u) [])
                (filesystem [ "f", owner 501u 20u, 0o644, "hello" ; "g", owner 501u 20u, 0o644, "" ])

        let inFd, darwin = opened "f" FileAccessMode.ReadOnly darwin
        let outFd, darwin = opened "g" FileAccessMode.WriteOnly darwin

        UnixReadWrite.copyFileRange inFd outFd 5UL 0 darwin
        |> Result.map fst
        |> shouldEqual (Error (CopyFileRangeRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin))

        UnixDescriptor.fileClone outFd inFd darwin
        |> shouldEqual (Error (FileCloneRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin))

        let nfs =
            kindsSystemWith (
                UnixBootImage.withMount (Some EmulatedMount.Nfs)
                >> Configured.expectOk MountRefusal.describe
            )

        let inFd, nfs = opened "ksrc" FileAccessMode.ReadOnly nfs
        let outFd, nfs = opened "kdst" FileAccessMode.WriteOnly nfs

        UnixReadWrite.copyFileRange inFd outFd 5UL 0 nfs
        |> Result.map fst
        |> shouldEqual (Error (CopyFileRangeRefusal.UnmeasuredFileSystem EmulatedFileSystemType.Nfs))

        UnixDescriptor.fileClone outFd inFd nfs
        |> shouldEqual (Error (FileCloneRefusal.UnmeasuredFileSystem EmulatedFileSystemType.Nfs))

        // A refusal the descriptors earn first is still answered.
        UnixReadWrite.copyFileRange inFd 9999 5UL 0 nfs
        |> Result.map fst
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EBADF))

    // ------------------------------------------------------------- copy_file_range's arithmetic

    [<Test>]
    let ``copy_file_range copies the measured rows`` () : unit =
        let system =
            systemOn
                SimulatedUnixPlatform.linuxX64
                root
                (filesystem
                    [
                        "src", owner 0u 0u, 0o640, "hello world"
                        "dst", owner 0u 0u, 0o644, ""
                        "ten", owner 0u 0u, 0o640, "0123456789"
                        "abc", owner 0u 0u, 0o644, "abc"
                    ])

        let now = UnixMachineState.realtime system.Machine

        // The whole of a short file, from both offsets, then 0 at the end with
        // nothing moved.
        let inFd, s = opened "src" FileAccessMode.ReadOnly system
        let outFd, s = opened "dst" FileAccessMode.WriteOnly s
        let answer, s = copyFileRangeAnswer inFd outFd 11UL 0 s
        answer |> shouldEqual (SyscallAnswer.Completed 11L)
        contentsOf s "dst" |> shouldEqual ((bytes "hello world") |> Seq.toArray)
        offsetOf inFd s |> shouldEqual 11L
        offsetOf outFd s |> shouldEqual 11L
        (entryOf s "src").Times |> shouldEqual (entryOf system "src").Times

        (entryOf s "dst").Times
        |> shouldEqual
            { (entryOf system "dst").Times with
                Modification = now
                StatusChange = now
            }

        let s2 =
            { s with
                Machine = UnixMachineState.advanceClock 1_000_000_000L s.Machine
            }

        let answer, after = copyFileRangeAnswer inFd outFd 11UL 0 s2
        answer |> shouldEqual (SyscallAnswer.Completed 0L)
        after |> shouldEqual s2

        // More than remains, from the middle, into an offset past the end: the
        // gap is zeroes.
        let inFd, s = opened "ten" FileAccessMode.ReadOnly system
        let outFd, s = opened "abc" FileAccessMode.WriteOnly s
        let s = s |> seekTo inFd 4L |> seekTo outFd 6L
        let answer, s = copyFileRangeAnswer inFd outFd 100UL 0 s
        answer |> shouldEqual (SyscallAnswer.Completed 6L)

        contentsOf s "abc"
        |> shouldEqual
            [|
                0x61uy
                0x62uy
                0x63uy
                0uy
                0uy
                0uy
                0x34uy
                0x35uy
                0x36uy
                0x37uy
                0x38uy
                0x39uy
            |]

        offsetOf inFd s |> shouldEqual 10L
        offsetOf outFd s |> shouldEqual 12L

        // SSIZE_MAX and SIZE_MAX copy what remains.
        for length in [ uint64 Int64.MaxValue ; UInt64.MaxValue ] do
            let inFd, s = opened "src" FileAccessMode.ReadOnly system
            let outFd, s = opened "dst" FileAccessMode.WriteOnly s

            copyFileRangeAnswer inFd outFd length 0 s
            |> fst
            |> shouldEqual (SyscallAnswer.Completed 11L)

        // From past the source's end: 0.
        let inFd, s = opened "src" FileAccessMode.ReadOnly system
        let outFd, s = opened "dst" FileAccessMode.WriteOnly s
        let s = seekTo inFd (Int64.MaxValue - 2L) s
        let answer, after = copyFileRangeAnswer inFd outFd 5UL 0 s
        answer |> shouldEqual (SyscallAnswer.Completed 0L)
        after |> shouldEqual s

        // Into a destination at INT64_MAX: EFBIG, whatever the source and the
        // length, and nothing moves.
        for contents in [ "h" ; "hello" ; "" ] do
            for length in [ 1UL ; 5UL ; 0UL ] do
                let system =
                    systemOn
                        SimulatedUnixPlatform.linuxX64
                        root
                        (filesystem [ "s", owner 0u 0u, 0o644, contents ; "t", owner 0u 0u, 0o644, "" ])

                let inFd, s = opened "s" FileAccessMode.ReadOnly system
                let outFd, s = opened "t" FileAccessMode.WriteOnly s
                let s = seekTo outFd Int64.MaxValue s

                copyFileRangeAnswer inFd outFd length 0 s
                |> shouldEqual (SyscallAnswer.Failed UnixError.EFBIG, s)

        // Every flag bit alone is EINVAL.
        for bit in 0..31 do
            copyFileRangeAnswer inFd outFd 5UL (1 <<< bit) s
            |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL, s)

    [<Test>]
    let ``copy_file_range within one file is EINVAL exactly where the shortened ranges overlap`` () : unit =
        let system =
            systemOn SimulatedUnixPlatform.linuxX64 root (filesystem [ "same", owner 0u 0u, 0o644, "0123456789" ])

        let a, s = opened "same" FileAccessMode.ReadWrite system
        let b, s = opened "same" FileAccessMode.ReadWrite s

        let row (inFd : int) (inAt : int64) (outFd : int) (outAt : int64) (length : uint64) : string =
            let s = s |> seekTo a 0L |> seekTo b 0L |> seekTo inFd inAt |> seekTo outFd outAt
            copyFileRangeAnswer inFd outFd length 0 s |> fst |> answerText

        // The probe's rows, as it printed them.
        row a 0L a 0L 5UL |> shouldEqual "EINVAL"
        row a 0L b 3L 5UL |> shouldEqual "EINVAL"
        row a 0L b 5L 5UL |> shouldEqual "5"
        row a 0L b 5L 6UL |> shouldEqual "EINVAL"
        row a 8L b 0L 5UL |> shouldEqual "2"
        row a 2L b 0L 5UL |> shouldEqual "EINVAL"
        row a 0L b 12L 20UL |> shouldEqual "10"
        row a 0L a 0L 0UL |> shouldEqual "0"

    [<Test>]
    let ``copy_file_range strips set-ID bits from its destination as a write does`` () : unit =
        // The probe's rows: uid 1000 copying into its own file, and root.
        for credentials, rows in
            [
                u1000,
                [
                    0o6755, 0o0755
                    0o6745, 0o2745
                    0o4644, 0o0644
                    0o2755, 0o0755
                    0o2644, 0o2644
                    0o1644, 0o1644
                ]
                root,
                [
                    0o6755, 0o6755
                    0o6745, 0o6745
                    0o4644, 0o4644
                    0o2755, 0o2755
                    0o2644, 0o2644
                    0o1644, 0o1644
                ]
            ] do
            for before, expected in rows do
                let system =
                    systemOn
                        SimulatedUnixPlatform.linuxX64
                        credentials
                        (filesystem [ "src", owner 0u 0u, 0o644, "hello" ; "dst", owner 1000u 1000u, before, "" ])

                let inFd, s = opened "src" FileAccessMode.ReadOnly system
                let outFd, s = opened "dst" FileAccessMode.WriteOnly s
                let answer, s = copyFileRangeAnswer inFd outFd 5UL 0 s
                answer |> shouldEqual (SyscallAnswer.Completed 5L)

                let actual = modeOf s "dst"

                if actual <> expected then
                    failwith
                        $"%O{credentials.EffectiveUser}, dst 0o%o{before}: expected 0o%o{expected}, got 0o%o{actual}"

    /// One generated copy between regular files: what each file holds, where
    /// each description stands, whether the two are one file, the destination's
    /// mode and owner, who copies, and how much is asked for.
    type private CopyCase =
        {
            Source : string
            Destination : string
            SameFile : bool
            SourceOffset : int64
            DestinationOffset : int64
            Length : uint64
            DestinationMode : int
            DestinationOwner : uint32
            Caller : uint32
        }

    let private copyCaseGen : Gen<CopyCase> =
        gen {
            let! source = Gen.choose (0, 12) |> Gen.map (fun n -> String ('s', n))
            let! destination = Gen.choose (0, 12) |> Gen.map (fun n -> String ('d', n))
            let! sameFile = Gen.elements [ false ; true ]
            let! sourceOffset = Gen.choose (0, 16) |> Gen.map int64
            let! destinationOffset = Gen.choose (0, 16) |> Gen.map int64

            let! length =
                Gen.oneof
                    [
                        Gen.choose (0, 20) |> Gen.map uint64
                        Gen.elements [ uint64 Int64.MaxValue ; UInt64.MaxValue ]
                    ]

            let! destinationMode = Gen.choose (0, 0o7777)
            let! destinationOwner = Gen.elements [ 0u ; 1000u ; 2000u ]
            let! caller = Gen.elements [ 0u ; 1000u ]

            return
                {
                    Source = source
                    Destination = destination
                    SameFile = sameFile
                    SourceOffset = sourceOffset
                    DestinationOffset = destinationOffset
                    Length = length
                    DestinationMode = destinationMode
                    DestinationOwner = destinationOwner
                    Caller = caller
                }
        }

    [<Test>]
    let ``copy_file_range moves what a read then a pwrite would, or refuses an overlap`` () : unit =
        // The reference is naive on purpose: the bytes from the source's offset
        // to its end, cut to the length; an overlap within one file is EINVAL;
        // and the destination ends exactly as `pwrite` of those bytes leaves it,
        // `pwrite` being the measured write this copy is said to be.
        let property (case : CopyCase) : unit =
            let destinationName = if case.SameFile then "src" else "dst"

            let system =
                systemOn
                    SimulatedUnixPlatform.linuxX64
                    (if case.Caller = 0u then root else u1000)
                    (filesystem
                        [
                            "src", owner case.DestinationOwner case.DestinationOwner, case.DestinationMode, case.Source
                            "dst",
                            owner case.DestinationOwner case.DestinationOwner,
                            case.DestinationMode,
                            case.Destination
                        ])

            let inFd, system = opened "src" FileAccessMode.ReadOnly system
            let outFd, system = opened destinationName FileAccessMode.WriteOnly system

            let system =
                system |> seekTo inFd case.SourceOffset |> seekTo outFd case.DestinationOffset

            let sourceBytes = contentsOf system "src"

            let moved =
                if case.SourceOffset >= int64 sourceBytes.Length then
                    [||]
                else
                    let available = uint64 (int64 sourceBytes.Length - case.SourceOffset)
                    let count = int (min case.Length available)
                    Array.sub sourceBytes (int case.SourceOffset) count

            let overlaps =
                case.SameFile
                && moved.Length > 0
                && case.DestinationOffset < case.SourceOffset + int64 moved.Length
                && case.SourceOffset < case.DestinationOffset + int64 moved.Length

            let answer, after = copyFileRangeAnswer inFd outFd case.Length 0 system

            if overlaps then
                answer |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)
                after |> shouldEqual system
            elif moved.Length = 0 then
                answer |> shouldEqual (SyscallAnswer.Completed 0L)
                after |> shouldEqual system
            else
                answer |> shouldEqual (SyscallAnswer.Completed (int64 moved.Length))

                let expected =
                    match
                        UnixReadWrite.pwrite 0 outFd (ImmutableArray.CreateRange moved) case.DestinationOffset system
                    with
                    | Ok (WriteAnswer.Completed _, written) ->
                        written
                        |> seekTo inFd (case.SourceOffset + int64 moved.Length)
                        |> seekTo outFd (case.DestinationOffset + int64 moved.Length)
                    | other -> failwith $"pwrite: %A{other}"

                after |> shouldEqual expected

            UnixSystem.checkInvariants after |> shouldEqual []

        Check.One (config, Prop.forAll (Arb.fromGen copyCaseGen) property)

    [<Test>]
    let ``step answers copy_file_range and FICLONE as the functions do`` () : unit =
        let system = kindsSystem ()
        let inFd, system = opened "ksrc" FileAccessMode.ReadOnly system
        let outFd, system = opened "kdst" FileAccessMode.WriteOnly system

        match UnixSystem.step 0 (Syscall.CopyFileRange (inFd, outFd, 5UL, 0)) system with
        | Ok (SyscallOutcome.Answered answer, after) ->
            (answer, after) |> shouldEqual (copyFileRangeAnswer inFd outFd 5UL 0 system)
        | other -> failwith $"%A{other}"

        match UnixSystem.step 0 (Syscall.FileClone (outFd, inFd)) system with
        | Ok (SyscallOutcome.Answered answer, after) ->
            answer |> shouldEqual (SyscallAnswer.Failed UnixError.EOPNOTSUPP)
            after |> shouldEqual system
        | other -> failwith $"%A{other}"

    // ------------------------------------------------------------- futimens

    let private atime : UnixTimestamp =
        UnixTimestamp.createOrFail context 1_000_000_000L 111_111_111

    let private mtime : UnixTimestamp =
        UnixTimestamp.createOrFail context 900_000_000L 222_222_222

    let private fieldsOf (time : UnixTimestamp) : TimespecFields =
        {
            Seconds = UnixTimestamp.seconds time
            Nanoseconds = int64 (UnixTimestamp.nanoseconds time)
        }

    /// `futimens(2)` with two explicit times, which glibc makes
    /// `utimensat(fd, NULL, times, 0)`.
    let private futimens
        (fd : int)
        (access : UnixTimestamp)
        (modification : UnixTimestamp)
        (system : UnixSystem<int, string>)
        =
        UnixPathResolution.utimensat
            fd
            NullablePathArgument.Null
            (TimesArgument.Fields (fieldsOf access, fieldsOf modification))
            0
            system

    [<Test>]
    let ``futimens sets both times of a file or directory through any descriptor of its owner`` () : unit =
        for kind in [ "file-ro" ; "file-wo" ; "file-rw" ; "dir" ] do
            let system = kindsSystem ()
            let fd, system = make kind "ksrc" system

            let target =
                if kind = "dir" then
                    VirtualFileSystem.root system.Machine.FileSystem
                else
                    inodeAt system "ksrc"

            let before = (VirtualFileSystem.tryGet target system.Machine.FileSystem).Value

            match futimens fd atime mtime system with
            | Ok (SyscallAnswer.Completed 0L, after) ->
                let entry = (VirtualFileSystem.tryGet target after.Machine.FileSystem).Value

                entry
                |> shouldEqual
                    { before with
                        Times =
                            {
                                Access = atime
                                Modification = mtime
                                StatusChange = UnixMachineState.realtime system.Machine
                                Birth = before.Times.Birth
                            }
                    }

                // Nothing but that inode changed.
                { after with
                    Machine =
                        { after.Machine with
                            FileSystem = system.Machine.FileSystem
                        }
                }
                |> shouldEqual system
            | other -> failwith $"%s{kind}: %A{other}"

    [<Test>]
    let ``futimens answers EBADF for a descriptor not held, and as the probe saw for each other kind`` () : unit =
        for kind in [ "closed" ; "fd9999" ] do
            let fd, system = make kind "ksrc" (kindsSystem ())

            futimens fd atime mtime system
            |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EBADF, system))

        // Both ends of a pipe report the times set through either.
        match UnixPipe.pipe2 0 UserBuffer.Mapped (kindsSystem ()) with
        | Ok (Pipe2Answer.Created (readFd, writeFd), system) ->
            for fd in [ readFd ; writeFd ] do
                match futimens fd atime mtime system with
                | Ok (SyscallAnswer.Completed 0L, after) ->
                    for pipeEnd in [ readFd ; writeFd ] do
                        match UnixPathResolution.fstat pipeEnd after with
                        | Ok (FileStatusAnswer.Reported status) ->
                            (status.AccessTime, status.ModificationTime) |> shouldEqual (atime, mtime)
                        | other -> failwith $"fd %d{fd}: fstat(%d{pipeEnd}): %A{other}"
                | other -> failwith $"fd %d{fd}: %A{other}"
        | other -> failwith $"pipe2: %A{other}"

        let fd, system = make "port" "ksrc" (kindsSystem ())

        futimens fd atime mtime system
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EOPNOTSUPP, system))

        let fd, system = make "socket" "ksrc" (kindsSystem ())

        match futimens fd atime mtime system with
        | Error (UTimensAtRefusal.Socket _) -> ()
        | other -> failwith $"socket: %A{other}"

    [<Test>]
    let ``futimens sets explicit times only for the owner or a privileged caller`` () : unit =
        // The probe's rows: uid 1000 on root's 0666 file opened O_RDWR is EPERM;
        // on its own 0444 file opened O_RDONLY it succeeds; root on uid 1000's
        // 0600 file succeeds.
        let rows =
            [
                u1000, owner 0u 0u, 0o666, FileAccessMode.ReadWrite, false
                u1000, owner 1000u 1000u, 0o444, FileAccessMode.ReadOnly, true
                root, owner 1000u 1000u, 0o600, FileAccessMode.ReadOnly, true
                // A member of the file's group who does not own it.
                Credentials.ofIds (uid 2000u) (gid 1000u) [], owner 1000u 1000u, 0o666, FileAccessMode.ReadWrite, false
            ]

        for credentials, by, bits, access, permitted in rows do
            let system =
                systemOn SimulatedUnixPlatform.linuxX64 credentials (filesystem [ "t", by, bits, "hello" ])

            let fd, system = opened "t" access system

            match futimens fd atime mtime system, permitted with
            | Ok (SyscallAnswer.Completed 0L, after), true ->
                (entryOf after "t").Times.Modification |> shouldEqual mtime
            | Ok (SyscallAnswer.Failed UnixError.EPERM, after), false -> after |> shouldEqual system
            | other, _ -> failwith $"%O{credentials.EffectiveUser} on 0o%o{bits}: %A{other}"

    [<Test>]
    let ``futimens stores any time exactly and never moves the birth time`` () : unit =
        let timestampGen =
            gen {
                let! seconds =
                    Gen.oneof
                        [
                            Gen.choose (-2_000_000_000, 2_000_000_000) |> Gen.map int64
                            Gen.elements [ -1L ; 0L ; int64 Int32.MaxValue ; int64 Int32.MaxValue + 1L ; 253402300799L ]
                        ]

                let! nanoseconds = Gen.choose (0, 999_999_999)
                return UnixTimestamp.createOrFail context seconds nanoseconds
            }

        let property (access : UnixTimestamp, modification : UnixTimestamp) : unit =
            let system = kindsSystem ()
            let fd, system = opened "ksrc" FileAccessMode.ReadOnly system

            match futimens fd access modification system with
            | Ok (SyscallAnswer.Completed 0L, after) ->
                let times = (entryOf after "ksrc").Times
                times.Access |> shouldEqual access
                times.Modification |> shouldEqual modification
                times.Birth |> shouldEqual (entryOf system "ksrc").Times.Birth

                let call =
                    Syscall.UTimensAt (
                        fd,
                        NullablePathArgument.Null,
                        TimesArgument.Fields (fieldsOf access, fieldsOf modification),
                        0
                    )

                match UnixSystem.step 0 call system with
                | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), stepped) -> stepped |> shouldEqual after
                | other -> failwith $"step: %A{other}"
            | other -> failwith $"%A{other}"

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip timestampGen timestampGen)) property)

/// The copy that needs a file longer than one Linux call moves (0x7FFFF000
/// bytes): the source, the bytes moved and the destination's new contents are
/// two gigabytes each, so like `TestTransferCountsLarge` it is `[<Explicit>]`,
/// and CI runs it by category in a step of its own. See AGENTS.md.
[<TestFixture>]
[<Category("LargeMemory")>]
[<Explicit>]
[<NonParallelizable>]
module TestCopyFileRangeLarge =

    /// One call moves at most 0x7FFFF000 bytes of a longer source, whatever
    /// was asked for beyond that: measured on a 7 GiB tmpfs by
    /// `copy-file-range-cap.c`.
    [<Test>]
    [<NonParallelizable>]
    let ``copy_file_range moves at most one call's worth`` () : unit =
        let length = int TestTransferCounts.LinuxMaxTransfer + 16

        let content =
            ImmutableCollectionsMarshal.AsImmutableArray (Array.zeroCreate<byte> length)

        let source, system =
            TestTransferCounts.withFile content 0L (TestTransferCounts.systemOn (SimulatedUnixPlatform.linuxX64, None))

        // A second, empty file beside it, opened for writing.
        let destination, system =
            let inode, filesystem =
                match
                    VirtualFileSystem.createFile
                        (VirtualFileSystem.root system.Machine.FileSystem)
                        (DirectoryEntryName.parseOrFail "TestCopyFileRangeLarge" "g")
                        (PermissionBits.parseOrFail "TestCopyFileRangeLarge" 0o644)
                        (InodeOwner.ofProcess system.Process.Credentials)
                        TestTransferCounts.epoch
                        ImmutableArray.Empty
                        system.Machine.FileSystem
                with
                | Ok pair -> pair
                | Error error -> failwith $"could not seed the destination: %O{error}"

            let fd, registry =
                FileDescriptorRegistry.openFile inode FileAccessMode.WriteOnly (UnixSystemState.fileDescriptors system)

            fd,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }
            |> UnixSystemState.withFileDescriptors registry

        match UnixReadWrite.copyFileRange source destination (uint64 length) 0 system with
        | Ok (SyscallAnswer.Completed moved, after) ->
            moved |> shouldEqual (int64 TestTransferCounts.LinuxMaxTransfer)
            TestTransferCounts.positionOf source after |> shouldEqual moved
            TestTransferCounts.positionOf destination after |> shouldEqual moved
        | other -> failwith $"expected a completed copy, got %A{other}"

        GC.Collect ()
