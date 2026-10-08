namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A descriptor onto `/dev/null` or `/dev/urandom`, and what each syscall that
/// takes a descriptor answers through it.
///
/// The constants are the rows `devices.c` and `devices-l2.c` measured on Linux
/// 6.18.5 aarch64 (as root and as uid 1000, which agree on every row), in
/// `docs/plans/2026-08-23-posix-kernel-extraction/`. Only Linux holds a device:
/// Darwin's `/dev` is refused before a node is reached.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDeviceDescriptors =

    let private context : string = "TestDeviceDescriptors"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    /// The one task `UnixSystem.initial` registers.
    let private task : int = 0

    let private rootOwner : InodeOwner =
        {
            User = UserId.root
            Group = GroupId.parseOrFail context 0u
        }

    let private opening (access : FileAccessMode) : OpenFlags =
        {
            Access = access
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }


    /// A Linux machine whose root holds a regular file `f`, holding five bytes,
    /// and whose process is the flavour's default unprivileged user, or root.
    let private bootedAs (root : bool) : UnixSystem<int, string> =
        let seed =
            Map.ofList
                [
                    DirectoryEntryName.parseOrFail context "f",
                    SeedEntry.File (
                        ImmutableArray.Create<byte> [| 1uy ; 2uy ; 3uy ; 4uy ; 5uy |],
                        PermissionBits.parseOrFail context 0o666,
                        None
                    )
                ]

        let image : UnixBootImage<int, string> = UnixSystem.initial linux

        let configure =
            if root then
                Launched.credentials (Credentials.ofIds UserId.root (GroupId.parseOrFail context 0u) [])
            else
                id

        match UnixBootImage.withFileSystem (UnixTimestamp.ofSeconds 1_700_000_000L) rootOwner seed image with
        | Ok image -> Launched.bootWith configure UnixSystem.pipedStandardStreams 0 (CpuId 0) image
        | Error fault -> failwith $"booting failed: %A{fault}"

    let private booted : UnixSystem<int, string> = bootedAs false

    let private pathOf (device : CharacterDevice) : string =
        match device with
        | CharacterDevice.Null -> "/dev/null"
        | CharacterDevice.URandom -> "/dev/urandom"

    let private openWith
        (flags : OpenFlags)
        (path : string)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        match OpenFlagWords.openPath flags (PathArg.ofText path) 0o644 system with
        | Ok (SyscallAnswer.Completed fd, system) -> int fd, system
        | other -> failwith $"open(%s{path}, %A{flags}): expected a descriptor, got %A{other}"

    let private openDevice
        (device : CharacterDevice)
        (access : FileAccessMode)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        openWith (opening access) (pathOf device) system

    let private assertSound (system : UnixSystem<int, string>) : unit =
        UnixSystem.checkInvariants system |> shouldEqual []

    let private deviceGen : Gen<CharacterDevice> =
        Gen.elements [ CharacterDevice.Null ; CharacterDevice.URandom ]

    let private accessGen : Gen<FileAccessMode> =
        Gen.elements
            [
                FileAccessMode.ReadOnly
                FileAccessMode.WriteOnly
                FileAccessMode.ReadWrite
            ]

    /// A count of a few kilobytes at most: the size a caller reads a device in.
    let private smallCountGen : Gen<uint64> = Gen.choose (0, 9000) |> Gen.map uint64

    /// The most one `read` or `write` moves on Linux.
    let private maxTransfer : uint64 = 0x7FFFF000UL

    /// The bytes a read answered, or the error it failed with.
    let private readBytes (answer : ReadAnswer) : Result<ImmutableArray<byte>, UnixError> =
        match answer with
        | ReadAnswer.Completed bytes -> Ok bytes
        | ReadAnswer.Drawn draw -> Ok (EntropyDraw.bytes draw)
        | ReadAnswer.Failed error -> Error error

    let private readOf
        (fd : int)
        (buffer : UserBuffer)
        (count : uint64)
        (system : UnixSystem<int, string>)
        : ReadAnswer * UnixSystem<int, string>
        =
        match UnixReadWrite.read task fd buffer count system with
        | Ok (ReadOutcome.Answered answer, system) -> answer, system
        | other -> failwith $"read(%d{fd}, %A{buffer}, %d{count}): expected an answer, got %A{other}"

    let private getRandomOf
        (count : uint64)
        (system : UnixSystem<int, string>)
        : ImmutableArray<byte> * UnixSystem<int, string>
        =
        match UnixEntropy.getRandom 0 UserBuffer.Mapped count 0u system with
        | Ok (GetRandomAnswer.Completed draw, system) -> EntropyDraw.bytes draw, system
        | other -> failwith $"getrandom(%d{count}): expected bytes, got %A{other}"

    let private admitWriteOf
        (fd : int)
        (buffer : UserBuffer)
        (count : uint64)
        (system : UnixSystem<int, string>)
        : WriteAdmission * UnixSystem<int, string>
        =
        match UnixReadWrite.admitWrite task fd buffer count system with
        | Ok (WriteOutcome.Returns (admission, system)) -> admission, system
        | other -> failwith $"admitWrite(%d{fd}, %A{buffer}, %d{count}): expected an admission, got %A{other}"

    // ------------------------------------------------------------------ open

    [<Test>]
    let ``each device opens for every access mode, with O_TRUNC and O_CREAT changing nothing`` () : unit =
        let property
            (device : CharacterDevice)
            (access : FileAccessMode)
            (truncate : bool)
            (create : bool)
            (root : bool)
            =
            let system = bootedAs root

            let flags =
                { opening access with
                    Truncate = truncate
                    Create = create
                }

            let before =
                UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText (pathOf device)) system

            let fd, opened = openWith flags (pathOf device) system
            assertSound opened

            match FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors opened) with
            | Some description ->
                match description.Target with
                | OpenFileTarget.CharacterDevice (_, opened) -> opened |> shouldEqual device
                | other -> failwith $"expected a device's description, got %A{other}"

                description.AccessMode |> shouldEqual access
            | None -> failwith $"fd %d{fd} is not live"

            // Nothing about the node moved: not its times, not its size.
            UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText (pathOf device)) opened
            |> shouldEqual before

            UnixPathResolution.fstat fd opened
            |> Result.mapError (sprintf "%A")
            |> shouldEqual (before |> Result.mapError (sprintf "%A"))

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip
                        (Gen.zip3 deviceGen accessGen (Gen.elements [ true ; false ]))
                        (Gen.zip (Gen.elements [ true ; false ]) (Gen.elements [ true ; false ]))
                ))
                (fun ((device, access, truncate), (create, root)) -> property device access truncate create root)
        )

    [<Test>]
    let ``O_CREAT with O_EXCL is EEXIST and O_DIRECTORY is ENOTDIR`` () : unit =
        for device in CharacterDevice.all do
            let flags =
                { opening FileAccessMode.ReadOnly with
                    Create = true
                    Exclusive = true
                }

            match OpenFlagWords.openPath flags (PathArg.ofText (pathOf device)) 0o644 booted with
            | Ok (SyscallAnswer.Failed UnixError.EEXIST, _) -> ()
            | other -> failwith $"%O{device}: expected EEXIST, got %A{other}"

            let flags =
                { opening FileAccessMode.ReadOnly with
                    Directory = true
                }

            match OpenFlagWords.openPath flags (PathArg.ofText (pathOf device)) 0 booted with
            | Ok (SyscallAnswer.Failed UnixError.ENOTDIR, _) -> ()
            | other -> failwith $"%O{device}: expected ENOTDIR, got %A{other}"

    [<Test>]
    let ``a node's permission bits decide who may open it, as a file's do`` () : unit =
        // Root narrows `/dev/null` to 0o600; then the unprivileged user may not
        // open it for any access, and root still may.
        let narrowed =
            match UnixPathResolution.chmod (PathArg.ofText "/dev/null") 0o600 (bootedAs true) with
            | Ok (SyscallAnswer.Completed 0L, system) -> system
            | other -> failwith $"chmod as root: %A{other}"

        // Root then gives up its privilege for the default user's IDs.
        let unprivileged = Become.fully booted.Process.Credentials narrowed

        for access in
            [
                FileAccessMode.ReadOnly
                FileAccessMode.WriteOnly
                FileAccessMode.ReadWrite
            ] do
            match OpenFlagWords.openPath (opening access) (PathArg.ofText "/dev/null") 0 unprivileged with
            | Ok (SyscallAnswer.Failed UnixError.EACCES, _) -> ()
            | other -> failwith $"%A{access} as uid 1000: expected EACCES, got %A{other}"

            openDevice CharacterDevice.Null access narrowed
            |> ignore<int * UnixSystem<int, string>>

    [<Test>]
    let ``a description that names a node as something it is not breaks the invariants`` () : unit =
        let inodeOf (path : string) =
            match UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText path) booted with
            | Ok (FileStatusAnswer.Reported status) -> status.Inode
            | other -> failwith $"stat %s{path}: %A{other}"

        let withRegistry (registry : FileDescriptorRegistry) =
            UnixSystemState.withFileDescriptors registry booted

        let urandom = inodeOf "/dev/urandom"
        let file = inodeOf "/f"

        for registry in
            [
                FileDescriptorRegistry.openCharacterDevice
                    urandom
                    CharacterDevice.Null
                    FileAccessMode.ReadOnly
                    (UnixSystemState.fileDescriptors booted)
                FileDescriptorRegistry.openCharacterDevice
                    file
                    CharacterDevice.Null
                    FileAccessMode.ReadOnly
                    (UnixSystemState.fileDescriptors booted)
                FileDescriptorRegistry.openFile urandom FileAccessMode.ReadOnly (UnixSystemState.fileDescriptors booted)
            ] do
            match UnixSystem.checkInvariants (withRegistry (snd registry)) with
            | [ UnixSystemDefect.DescriptionKindMismatch _ ] -> ()
            | other -> failwith $"expected one kind mismatch, got %A{other}"

        FileDescriptorRegistry.openCharacterDevice
            urandom
            CharacterDevice.URandom
            FileAccessMode.ReadOnly
            (UnixSystemState.fileDescriptors booted)
        |> snd
        |> withRegistry
        |> assertSound

    [<Test>]
    let ``fstatfs through a device's descriptor is statfs of its path`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            UnixPathResolution.fstatfs fd system
            |> shouldEqual (Answered.statfs (PathArg.ofText (pathOf device)) system)

    // ------------------------------------------------------------------ read

    [<Test>]
    let ``a read of null answers 0 without touching the buffer`` () : unit =
        let property (count : uint64) (buffer : UserBuffer) (access : FileAccessMode) =
            let fd, system = openDevice CharacterDevice.Null access booted
            let answer, after = readOf fd buffer count system

            if not (FileAccessMode.permitsRead access) then
                answer |> shouldEqual (ReadAnswer.Failed UnixError.EBADF)
            else
                answer |> shouldEqual (ReadAnswer.Completed ImmutableArray.Empty)

            after |> shouldEqual system

        let bufferGen =
            Gen.elements
                [
                    UserBuffer.Mapped
                    UserBuffer.Unmapped 0UL
                    UserBuffer.Unmapped 16UL
                    UserBuffer.Opaque
                ]

        let countGen =
            Gen.oneof
                [
                    smallCountGen
                    Gen.elements [ maxTransfer ; maxTransfer + 1UL ; 0x1_0000_0000UL ]
                ]

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip3 countGen bufferGen accessGen)) (fun (c, b, a) -> property c b a)
        )

    [<Test>]
    let ``a read past the address space is EFAULT ahead of either device`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            for count in [ System.UInt64.MaxValue ; 0x7FFF_FFFF_FFFF_FFFFUL ] do
                let answer, after = readOf fd UserBuffer.Mapped count system
                answer |> shouldEqual (ReadAnswer.Failed UnixError.EFAULT)
                after |> shouldEqual system

    [<Test>]
    let ``urandom reads the bytes getrandom would have, from the same pool`` () : unit =
        // Measured: every count up to a whole call's worth is read in full
        // (`devices.c` READ rows, and `bigread.c`).
        let property (steps : (bool * uint64) list) =
            let fd, system = openDevice CharacterDevice.URandom FileAccessMode.ReadOnly booted

            let viaDevice, deviceSystem =
                ((ImmutableArray<byte>.Empty, system), steps)
                ||> List.fold (fun (sofar, system) (throughDevice, count) ->
                    if throughDevice then
                        let answer, system = readOf fd UserBuffer.Mapped count system

                        match readBytes answer with
                        | Ok bytes ->
                            bytes.Length |> shouldEqual (int count)
                            sofar.AddRange bytes, system
                        | Error error -> failwith $"read of %d{count}: %O{error}"
                    else
                        let bytes, system = getRandomOf count system
                        sofar.AddRange bytes, system
                )

            let viaGetRandom, _ =
                ((ImmutableArray<byte>.Empty, system), steps)
                ||> List.fold (fun (sofar, system) (_, count) ->
                    let bytes, system = getRandomOf count system
                    sofar.AddRange bytes, system
                )

            Seq.toList viaDevice |> shouldEqual (Seq.toList viaGetRandom)
            assertSound deviceSystem

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.listOf (Gen.zip (Gen.elements [ true ; false ]) smallCountGen))) property
        )

    [<Test>]
    let ``a urandom read longer than one call moves one call's worth`` () : unit =
        let fd, system = openDevice CharacterDevice.URandom FileAccessMode.ReadOnly booted

        for count in
            [
                maxTransfer
                maxTransfer + 1UL
                0x7FFF_FFFFUL
                0x8000_0000UL
                0x1_0000_0000UL
            ] do
            match readOf fd UserBuffer.Mapped count system with
            | ReadAnswer.Drawn draw, _ -> EntropyDraw.count draw |> shouldEqual (int maxTransfer)
            | other -> failwith $"read of %d{count}: expected a draw, got %A{other}"

    [<Test>]
    let ``a urandom read that cannot copy faults, and leaves the pool where it was`` () : unit =
        let fd, system = openDevice CharacterDevice.URandom FileAccessMode.ReadOnly booted

        let property (count : uint64) (address : uint64) =
            let answer, after = readOf fd (UserBuffer.Unmapped address) count system

            if count = 0UL then
                answer |> shouldEqual (ReadAnswer.Completed ImmutableArray.Empty)
            else
                answer |> shouldEqual (ReadAnswer.Failed UnixError.EFAULT)

            after |> shouldEqual system

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip smallCountGen (Gen.elements [ 0UL ; 16UL ; 0x1000UL ])))
                (fun (c, a) -> property c a)
        )

    [<Test>]
    let ``a urandom read into memory the caller cannot produce is refused, and an empty one is not`` () : unit =
        let fd, system = openDevice CharacterDevice.URandom FileAccessMode.ReadOnly booted

        match UnixReadWrite.read task fd UserBuffer.Opaque 16UL system with
        | Error (ReadRefusal.Buffer BufferRefusal.OpaqueAtTransfer) -> ()
        | other -> failwith $"expected a refusal, got %A{other}"

        readOf fd UserBuffer.Opaque 0UL system
        |> fst
        |> shouldEqual (ReadAnswer.Completed ImmutableArray.Empty)

    [<Test>]
    let ``a urandom read longer than a page is refused while the reader has a signal pending`` () : unit =
        // Linux's read copies a 64-byte block at a time and, at each page
        // boundary with bytes still to copy, stops if a signal is pending: so a
        // read of more than a page with one already pending answers a page,
        // short (`get_random_bytes_user`, drivers/char/random.c in 6.18). The
        // check counts bytes copied, so where the buffer starts does not
        // matter, and a read of exactly a page never reaches a check.
        let property (count : uint64) (pending : PendingSignal) (pread : bool) =
            let fd, system = openDevice CharacterDevice.URandom FileAccessMode.ReadOnly booted
            let system = PendingSignal.make task pending system

            let outcome =
                if pread then
                    match UnixReadWrite.pread task fd UserBuffer.Mapped count 0L system with
                    | Ok answer -> Ok answer
                    | Error (PReadRefusal.SignalAtPageBoundary count) -> Error count
                    | Error (PReadRefusal.Buffer refusal) -> failwith $"pread: %A{refusal}"
                else
                    match UnixReadWrite.read task fd UserBuffer.Mapped count system with
                    | Ok (ReadOutcome.Answered answer, system) -> Ok (answer, system)
                    | Error (ReadRefusal.SignalAtPageBoundary count) -> Error count
                    | other -> failwith $"read: %A{other}"

            match outcome with
            | Error refused ->
                refused |> shouldEqual (int count)

                (count > 4096UL && PendingSignal.transferrerHasSignal pending)
                |> shouldEqual true
            | Ok (answer, _) ->
                (count > 4096UL && PendingSignal.transferrerHasSignal pending)
                |> shouldEqual false

                match answer with
                | ReadAnswer.Drawn draw -> EntropyDraw.count draw |> shouldEqual (int count)
                | ReadAnswer.Completed bytes -> bytes.Length |> shouldEqual (int count)
                | ReadAnswer.Failed error -> failwith $"%O{error}"

        let countGen =
            Gen.oneof
                [
                    Gen.choose (0, 9000) |> Gen.map uint64
                    Gen.elements [ 4095UL ; 4096UL ; 4097UL ; 8192UL ; 8193UL ]
                ]

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 countGen PendingSignal.gen (Gen.elements [ true ; false ])))
                (fun (c, p, pr) -> property c p pr)
        )

    [<Test>]
    let ``a urandom write longer than a page is refused while the writer has a signal pending`` () : unit =
        // `write_pool_user` stops at a page boundary as the read does.
        let property (count : int) (pending : PendingSignal) (positioned : bool) =
            let fd, system = openDevice CharacterDevice.URandom FileAccessMode.WriteOnly booted
            let system = PendingSignal.make task pending system
            let refused = count > 4096 && PendingSignal.transferrerHasSignal pending
            let bytes = ImmutableArray.CreateRange (Seq.init count byte)

            if positioned then
                match UnixReadWrite.admitPWrite task fd UserBuffer.Mapped (uint64 count) 0L system with
                | Error (PWriteRefusal.SignalAtPageBoundary refusedCount) ->
                    refused |> shouldEqual true
                    refusedCount |> shouldEqual count
                | Ok admission ->
                    refused |> shouldEqual false

                    match admission with
                    | PWriteAdmission.Transfer transfer -> transfer |> shouldEqual count
                    | PWriteAdmission.Answered answer -> answer |> shouldEqual (WriteAnswer.Completed 0L)
                | Error other -> failwith $"admitPWrite: %A{other}"

                // The commit asks too, for a caller that skipped the admission.
                match UnixReadWrite.pwrite task fd bytes 0L system with
                | Error (PWriteRefusal.SignalAtPageBoundary _) -> refused |> shouldEqual true
                | Ok (answer, _) ->
                    refused |> shouldEqual false
                    answer |> shouldEqual (WriteAnswer.Completed (int64 count))
                | Error other -> failwith $"pwrite: %A{other}"
            else
                match UnixReadWrite.admitWrite task fd UserBuffer.Mapped (uint64 count) system with
                | Error (WriteRefusal.SignalAtPageBoundary refusedCount) ->
                    refused |> shouldEqual true
                    refusedCount |> shouldEqual count
                | Ok (WriteOutcome.Returns (admission, _)) ->
                    refused |> shouldEqual false

                    match admission with
                    | WriteAdmission.Transfer transfer -> transfer |> shouldEqual count
                    | WriteAdmission.Answered answer -> answer |> shouldEqual (WriteAnswer.Completed 0L)
                    | WriteAdmission.TransferThenSleep _ -> failwith "a device write never sleeps"
                | other -> failwith $"admitWrite: %A{other}"

                match UnixReadWrite.write task fd bytes system with
                | Error (WriteRefusal.SignalAtPageBoundary _) -> refused |> shouldEqual true
                | Ok (WriteOutcome.Returns (answer, _)) ->
                    refused |> shouldEqual false
                    answer |> shouldEqual (WriteAnswer.Completed (int64 count))
                | other -> failwith $"write: %A{other}"

        let countGen =
            Gen.oneof [ Gen.choose (0, 9000) ; Gen.elements [ 4095 ; 4096 ; 4097 ; 8192 ; 8193 ] ]

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 countGen PendingSignal.gen (Gen.elements [ true ; false ])))
                (fun (c, p, pw) -> property c p pw)
        )

    [<Test>]
    let ``a pending signal changes nothing a write to null answers`` () : unit =
        let fd, system = openDevice CharacterDevice.Null FileAccessMode.WriteOnly booted
        let system = PendingSignal.make task PendingSignal.CaughtForTransferrer system

        admitWriteOf fd UserBuffer.Mapped 65536UL system
        |> fst
        |> shouldEqual (WriteAdmission.Answered (WriteAnswer.Completed 65536L))

    [<Test>]
    let ``a pending signal changes nothing a read of null or a faulting read answers`` () : unit =
        let fd, system = openDevice CharacterDevice.Null FileAccessMode.ReadOnly booted
        let system = PendingSignal.make task PendingSignal.CaughtForTransferrer system

        readOf fd UserBuffer.Mapped 65536UL system
        |> fst
        |> shouldEqual (ReadAnswer.Completed ImmutableArray.Empty)

        let fd, system = openDevice CharacterDevice.URandom FileAccessMode.ReadOnly booted
        let system = PendingSignal.make task PendingSignal.CaughtForTransferrer system

        readOf fd (UserBuffer.Unmapped 0UL) 65536UL system
        |> fst
        |> shouldEqual (ReadAnswer.Failed UnixError.EFAULT)

    [<Test>]
    let ``a read through a descriptor opened for writing only is EBADF`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.WriteOnly booted
            let answer, after = readOf fd UserBuffer.Mapped 16UL system
            answer |> shouldEqual (ReadAnswer.Failed UnixError.EBADF)
            after |> shouldEqual system

    [<Test>]
    let ``O_NONBLOCK changes nothing a device read answers`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            let nonBlocking =
                match UnixDescriptor.setNonBlocking fd true system with
                | SetNonBlockingAnswer.Set, system -> system
                | other -> failwith $"F_SETFL O_NONBLOCK: %A{other}"

            fst (readOf fd UserBuffer.Mapped 64UL nonBlocking)
            |> readBytes
            |> shouldEqual (fst (readOf fd UserBuffer.Mapped 64UL system) |> readBytes)

    // ------------------------------------------------------------------ pread

    [<Test>]
    let ``pread checks its offset, then answers as read does`` () : unit =
        // Measured: 16@0 and 16@2^40 answer as a read does; 16@-1 and
        // 16@INT64_MAX are EINVAL.
        let property (device : CharacterDevice) (count : uint64) (offset : int64) =
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            match UnixReadWrite.pread task fd UserBuffer.Mapped count offset system with
            | Error refusal -> failwith $"pread refused: %A{refusal}"
            | Ok (answer, after) ->

            if not (answer.IsDrawn) then
                after |> shouldEqual system

            if offset < 0L || (count > 0UL && count > uint64 (System.Int64.MaxValue - offset)) then
                answer |> shouldEqual (ReadAnswer.Failed UnixError.EINVAL)
            else

            match device, readBytes answer with
            | CharacterDevice.Null, Ok bytes -> bytes.IsEmpty |> shouldEqual true
            | CharacterDevice.URandom, Ok bytes ->
                Seq.toList bytes |> shouldEqual (Seq.toList (fst (getRandomOf count system)))
            | _, Error error -> failwith $"pread %d{count}@%d{offset}: %O{error}"

        let offsetGen =
            Gen.oneof
                [
                    Gen.elements
                        [
                            0L
                            -1L
                            1L <<< 40
                            System.Int64.MaxValue
                            System.Int64.MaxValue - 16L
                            System.Int64.MinValue
                        ]
                    Gen.choose (-100, 100) |> Gen.map int64
                ]

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip3 deviceGen smallCountGen offsetGen)) (fun (d, c, o) -> property d c o)
        )

    [<Test>]
    let ``pread's negative offset is EINVAL ahead of the buffer screen`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            UnixReadWrite.pread task fd UserBuffer.Mapped System.UInt64.MaxValue -1L system
            |> shouldEqual (Ok (ReadAnswer.Failed UnixError.EINVAL, system))

            UnixReadWrite.pread task fd UserBuffer.Mapped System.UInt64.MaxValue 0L system
            |> shouldEqual (Ok (ReadAnswer.Failed UnixError.EFAULT, system))

    // ------------------------------------------------------------------ write

    [<Test>]
    let ``a write to null answers the count without reading the buffer`` () : unit =
        let property (count : uint64) (buffer : UserBuffer) (access : FileAccessMode) =
            let fd, system = openDevice CharacterDevice.Null access booted
            let admission, after = admitWriteOf fd buffer count system

            if not (FileAccessMode.permitsWrite access) then
                admission
                |> shouldEqual (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EBADF))
            else
                admission
                |> shouldEqual (WriteAdmission.Answered (WriteAnswer.Completed (int64 (min count maxTransfer))))

            after |> shouldEqual system

        let bufferGen =
            Gen.elements
                [
                    UserBuffer.Mapped
                    UserBuffer.Unmapped 0UL
                    UserBuffer.Unmapped 16UL
                    UserBuffer.Opaque
                ]

        let countGen =
            Gen.oneof
                [
                    smallCountGen
                    Gen.elements [ maxTransfer ; maxTransfer + 1UL ; 0x1_0000_0000UL ]
                ]

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip3 countGen bufferGen accessGen)) (fun (c, b, a) -> property c b a)
        )

    [<Test>]
    let ``a write to urandom reads the buffer, answers the count, and leaves the pool alone`` () : unit =
        let property (count : uint64) (buffer : UserBuffer) =
            let fd, system = openDevice CharacterDevice.URandom FileAccessMode.ReadWrite booted

            match UnixReadWrite.admitWrite task fd buffer count system with
            | Error (WriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer) ->
                buffer |> shouldEqual UserBuffer.Opaque
                count |> shouldNotEqual 0UL
            | Error refusal -> failwith $"admitWrite refused: %A{refusal}"
            | Ok (WriteOutcome.Returns (admission, after)) ->
                after |> shouldEqual system

                match admission with
                | WriteAdmission.Answered answer ->
                    match buffer, count with
                    | _, 0UL -> answer |> shouldEqual (WriteAnswer.Completed 0L)
                    | UserBuffer.Unmapped _, _ -> answer |> shouldEqual (WriteAnswer.Failed UnixError.EFAULT)
                    | _ -> failwith $"%A{buffer} of %d{count}: expected a transfer, got %A{answer}"
                | WriteAdmission.TransferThenSleep _ -> failwith "a device write never sleeps"
                | WriteAdmission.Transfer transfer ->
                    buffer |> shouldEqual UserBuffer.Mapped
                    transfer |> shouldEqual (int count)

                    let bytes = ImmutableArray.CreateRange (Seq.init transfer byte)

                    match UnixReadWrite.write task fd bytes after with
                    | Ok (WriteOutcome.Returns (answer, written)) ->
                        answer |> shouldEqual (WriteAnswer.Completed (int64 transfer))
                        // Bytes written to the device are not what it reads back:
                        // the next read is the one the write did not precede.
                        fst (readOf fd UserBuffer.Mapped 32UL written)
                        |> readBytes
                        |> shouldEqual (fst (readOf fd UserBuffer.Mapped 32UL system) |> readBytes)

                        assertSound written
                    | other -> failwith $"write: %A{other}"
            | Ok other -> failwith $"admitWrite: %A{other}"

        let bufferGen =
            Gen.elements
                [
                    UserBuffer.Mapped
                    UserBuffer.Unmapped 0UL
                    UserBuffer.Unmapped 16UL
                    UserBuffer.Opaque
                ]

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip smallCountGen bufferGen)) (fun (c, b) -> property c b))

    [<Test>]
    let ``a write past the address space is EFAULT ahead of either device`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.WriteOnly booted

            admitWriteOf fd UserBuffer.Mapped System.UInt64.MaxValue system
            |> fst
            |> shouldEqual (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))

    [<Test>]
    let ``pwrite checks its offset, then answers as write does`` () : unit =
        let property (device : CharacterDevice) (count : uint64) (offset : int64) =
            let fd, system = openDevice device FileAccessMode.WriteOnly booted

            match UnixReadWrite.admitPWrite 0 fd UserBuffer.Mapped count offset system with
            | Error refusal -> failwith $"admitPWrite refused: %A{refusal}"
            | Ok admission ->

            if offset < 0L || (count > 0UL && count > uint64 (System.Int64.MaxValue - offset)) then
                admission
                |> shouldEqual (PWriteAdmission.Answered (WriteAnswer.Failed UnixError.EINVAL))
            else

            match device, admission with
            | CharacterDevice.Null, _ ->
                admission
                |> shouldEqual (PWriteAdmission.Answered (WriteAnswer.Completed (int64 count)))
            | CharacterDevice.URandom, PWriteAdmission.Answered answer ->
                count |> shouldEqual 0UL
                answer |> shouldEqual (WriteAnswer.Completed 0L)
            | CharacterDevice.URandom, PWriteAdmission.Transfer transfer ->
                transfer |> shouldEqual (int count)
                let bytes = ImmutableArray.CreateRange (Seq.init transfer byte)

                match UnixReadWrite.pwrite 0 fd bytes offset system with
                | Ok (answer, after) ->
                    answer |> shouldEqual (WriteAnswer.Completed (int64 transfer))
                    after |> shouldEqual system
                | Error refusal -> failwith $"pwrite refused: %A{refusal}"

        let offsetGen =
            Gen.oneof
                [
                    Gen.elements
                        [
                            0L
                            -1L
                            1L <<< 40
                            System.Int64.MaxValue
                            System.Int64.MaxValue - 16L
                            System.Int64.MinValue
                        ]
                    Gen.choose (-100, 100) |> Gen.map int64
                ]

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.zip3 deviceGen smallCountGen offsetGen)) (fun (d, c, o) -> property d c o)
        )

    // ------------------------------------------------------------------ position and size

    [<Test>]
    let ``lseek answers 0 for whence 0 to 4 and EINVAL for any other, whatever the offset`` () : unit =
        let property (device : CharacterDevice) (whence : int) (offset : int64) =
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            match UnixDescriptor.lseek fd offset whence system with
            | Ok (answer, after) ->
                if whence >= 0 && whence <= 4 then
                    answer |> shouldEqual (SyscallAnswer.Completed 0L)
                else
                    answer |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

                after |> shouldEqual system
            | Error refusal -> failwith $"lseek refused: %A{refusal}"

        let offsetGen =
            Gen.oneof
                [
                    Gen.elements [ 0L ; -1L ; -5L ; 1L ; 100L ; System.Int64.MaxValue ; System.Int64.MinValue ]
                    Gen.choose (-1000, 1000) |> Gen.map int64
                ]

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 deviceGen (Gen.choose (-3, 10)) offsetGen))
                (fun (d, w, o) -> property d w o)
        )

    [<Test>]
    let ``ftruncate is EINVAL for every length and access mode`` () : unit =
        let property (device : CharacterDevice) (access : FileAccessMode) (length : int64) =
            let fd, system = openDevice device access booted

            match UnixDescriptor.ftruncate fd length system with
            | Ok (answer, after) ->
                answer |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)
                after |> shouldEqual system
            | Error refusal -> failwith $"ftruncate refused: %A{refusal}"

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 deviceGen accessGen (Gen.elements [ 0L ; 10L ; -1L ; System.Int64.MaxValue ])))
                (fun (d, a, l) -> property d a l)
        )

    [<Test>]
    let ``posix_fadvise accepts advice 0 to 5 and a non-negative length, at any offset`` () : unit =
        let property (device : CharacterDevice) (offset : int64) (length : int64) (advice : int) =
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            let expected =
                if length < 0L || advice < 0 || advice > 5 then
                    FileAdviceAnswer.Failed UnixError.EINVAL
                else
                    FileAdviceAnswer.Completed

            UnixDescriptor.posixFadvise fd offset length advice system
            |> shouldEqual (Ok expected)

        let int64Gen =
            Gen.elements [ 0L ; -1L ; 1L ; 4096L ; System.Int64.MaxValue ; System.Int64.MinValue ]

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip (Gen.zip deviceGen int64Gen) (Gen.zip int64Gen (Gen.choose (-2, 8)))))
                (fun ((d, o), (l, a)) -> property d o l a)
        )

    // ------------------------------------------------------------------ ioctl

    [<Test>]
    let ``FIONREAD and tcgetattr answer null's ENOTTY and urandom's EINVAL`` () : unit =
        for device, expected in
            [
                CharacterDevice.Null, UnixError.ENOTTY
                CharacterDevice.URandom, UnixError.EINVAL
            ] do
            for access in [ FileAccessMode.ReadOnly ; FileAccessMode.WriteOnly ] do
                let fd, system = openDevice device access booted

                for destination in [ UserBuffer.Mapped ; UserBuffer.Unmapped 0UL ; UserBuffer.Opaque ] do
                    UnixDescriptor.bytesAvailable fd destination system
                    |> shouldEqual (Ok (BytesAvailableAnswer.Failed expected))

                UnixDescriptor.terminalAttributes fd system
                |> shouldEqual (TerminalAttributesAnswer.NotATerminal expected)

    [<Test>]
    let ``FICLONE across the devtmpfs is EXDEV, and between devices EINVAL`` () : unit =
        let fileFd, system = openWith (opening FileAccessMode.ReadWrite) "/f" booted
        let nullFd, system = openDevice CharacterDevice.Null FileAccessMode.ReadWrite system

        let urandomFd, system =
            openDevice CharacterDevice.URandom FileAccessMode.ReadWrite system

        UnixDescriptor.fileClone nullFd fileFd system
        |> shouldEqual (Ok UnixError.EXDEV)

        UnixDescriptor.fileClone fileFd nullFd system
        |> shouldEqual (Ok UnixError.EXDEV)

        UnixDescriptor.fileClone urandomFd nullFd system
        |> shouldEqual (Ok UnixError.EINVAL)

        UnixDescriptor.fileClone nullFd nullFd system
        |> shouldEqual (Ok UnixError.EINVAL)

    [<Test>]
    let ``copy_file_range to, from or between devices is EINVAL`` () : unit =
        let fileFd, system = openWith (opening FileAccessMode.ReadWrite) "/f" booted
        let nullFd, system = openDevice CharacterDevice.Null FileAccessMode.ReadWrite system

        let urandomFd, system =
            openDevice CharacterDevice.URandom FileAccessMode.ReadWrite system

        for inFd, outFd in [ fileFd, nullFd ; nullFd, fileFd ; urandomFd, nullFd ; urandomFd, fileFd ] do
            match UnixReadWrite.copyFileRange inFd outFd 5UL 0 system with
            | Ok (answer, after) ->
                answer |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)
                after |> shouldEqual system
            | Error refusal -> failwith $"copy_file_range(%d{inFd}, %d{outFd}) refused: %A{refusal}"

    [<Test>]
    let ``getdents of a device is ENOTDIR`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            match UnixNamespace.readDirectoryEntry fd system with
            | Ok (ReadDirectoryAnswer.Failed UnixError.ENOTDIR, after) -> after |> shouldEqual system
            | other -> failwith $"%O{device}: expected ENOTDIR, got %A{other}"

    // ------------------------------------------------------------------ flock

    [<Test>]
    let ``two descriptions of one device contend under flock, and two devices do not`` () : unit =
        let lockExclusiveNonBlocking = 2 ||| 4

        let flock (fd : int) (system : UnixSystem<int, string>) =
            match UnixDescriptor.flock task fd lockExclusiveNonBlocking system with
            | Ok (SyscallOutcome.Answered answer, system) -> answer, system
            | other -> failwith $"flock(%d{fd}): %A{other}"

        let first, system = openDevice CharacterDevice.Null FileAccessMode.ReadOnly booted
        let second, system = openDevice CharacterDevice.Null FileAccessMode.WriteOnly system

        let other, system =
            openDevice CharacterDevice.URandom FileAccessMode.ReadOnly system

        let answer, system = flock first system
        answer |> shouldEqual (SyscallAnswer.Completed 0L)
        let answer, system = flock second system
        answer |> shouldEqual (SyscallAnswer.Failed UnixError.EAGAIN)
        let answer, system = flock other system
        answer |> shouldEqual (SyscallAnswer.Completed 0L)
        assertSound system

    // ------------------------------------------------------------------ readiness

    [<Test>]
    let ``poll reports IN, OUT, RDNORM and WRNORM, masked by the request`` () : unit =
        // Measured: revents 0x145 for every access mode, and nothing for a
        // request of 0.
        let level = 0x145

        let property (device : CharacterDevice) (access : FileAccessMode) (events : int16) =
            let fd, system = openDevice device access booted

            match
                UnixPoll.poll
                    task
                    [
                        {
                            Fd = fd
                            Events = events
                        }
                    ]
                    0
                    system
            with
            | Ok (PollOutcome.Answered (revents, count), after) ->
                let expected = int16 (level &&& (int events ||| 0x8 ||| 0x10))
                revents |> shouldEqual [ expected ]
                count |> shouldEqual (if expected = 0s then 0 else 1)
                after |> shouldEqual system
            | other -> failwith $"poll: %A{other}"

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 deviceGen accessGen (Gen.choose (0, 0x7FFF) |> Gen.map int16)))
                (fun (d, a, e) -> property d a e)
        )

    [<Test>]
    let ``epoll_ctl refuses a device as unpollable, whatever the operation`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.ReadOnly booted

            let queueFd, registry =
                FileDescriptorRegistry.createEpoll (UnixSystemState.fileDescriptors system)

            let system = UnixSystemState.withFileDescriptors registry system

            for op in [ 1 ; 2 ; 3 ] do
                match UnixPoll.epollCtl queueFd op fd (EpollEventArgument.Readable (1u, 42UL)) system with
                | Ok (EpollCtlAnswer.Failed EpollCtlError.TargetNotPollable, after) -> after |> shouldEqual system
                | other -> failwith $"%O{device}, op %d{op}: expected EPERM, got %A{other}"

    // ------------------------------------------------------------------ close

    [<Test>]
    let ``closing a device's descriptor leaves the node as it was`` () : unit =
        for device in CharacterDevice.all do
            let fd, system = openDevice device FileAccessMode.ReadWrite booted

            let copy, system =
                match Answered.dup fd system with
                | SyscallAnswer.Completed copy, system -> int copy, system
                | other -> failwith $"dup: %A{other}"

            let closed =
                match UnixDescriptor.close fd system with
                | Ok (SyscallAnswer.Completed _, system) -> system
                | other -> failwith $"close: %A{other}"

            assertSound closed

            fst (readOf copy UserBuffer.Mapped 8UL closed)
            |> readBytes
            |> Result.map (fun bytes -> bytes.Length)
            |> shouldEqual (Ok (if device = CharacterDevice.Null then 0 else 8))

            closed.Machine.FileSystem |> shouldEqual booted.Machine.FileSystem
