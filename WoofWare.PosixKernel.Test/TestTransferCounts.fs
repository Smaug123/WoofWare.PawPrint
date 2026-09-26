namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// What `read`, `pread`, `write` and `pwrite` do with the count they are given,
/// which is a `size_t`: the per-call limit, the buffer screen over the whole
/// count, and Linux's check of position + count. The rules are those measured by
/// docs/plans/2026-08-23-posix-kernel-extraction/transfer-counts.c and
/// transfer-counts-position.c, on Linux 6.18.5 aarch64 and Darwin 27.0.0 arm64.
///
/// The properties put an arbitrary-precision reference model of the measured
/// rule against the library over the whole `uint64` range of counts; the tables
/// carry the measured rows for the descriptors the reference does not model.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTransferCounts =

    let private context : string = "TestTransferCounts"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 3000

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private rootInode : InodeNumber = InodeNumber 1L

    /// `MAX_RW_COUNT` with 4 KiB pages, as measured.
    [<Literal>]
    let private LinuxMaxTransfer : uint64 = 0x7FFF_F000UL

    [<Literal>]
    let private IntMax : uint64 = 0x7FFF_FFFFUL

    // ------------------------------------------------------------ the platform

    [<Test>]
    let ``each platform's per-call limit is the measured one`` () : unit =
        SimulatedUnixPlatform.transferCountLimit SimulatedUnixPlatform.linuxX64
        |> shouldEqual (TransferCountLimit.Shortened 0x7FFF_F000)

        SimulatedUnixPlatform.transferCountLimit SimulatedUnixPlatform.linuxArm64
        |> shouldEqual (TransferCountLimit.Shortened 0x7FFF_F000)

        SimulatedUnixPlatform.transferCountLimit SimulatedUnixPlatform.macOsArm64
        |> shouldEqual (TransferCountLimit.Refused 0x7FFF_FFFF)

    [<Test>]
    let ``getrandom shortens to the same limit as read`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.linuxArm64 ] do
            UnixEntropy.getRandomMaxTransfer platform
            |> shouldEqual (uint64 (TransferCountLimit.maxTransfer (SimulatedUnixPlatform.transferCountLimit platform)))

    // ------------------------------------------------------- the machines

    /// Every flavour, and on Linux every address limit a machine has been
    /// observed to have.
    let private machines : (SimulatedUnixPlatform * uint64 option) list =
        [
            SimulatedUnixPlatform.linuxX64, Some ObservedUserAddressLimit.X64FourLevelPaging
            SimulatedUnixPlatform.linuxX64, Some ObservedUserAddressLimit.X64FiveLevelPaging
            SimulatedUnixPlatform.linuxArm64, Some ObservedUserAddressLimit.Arm64FortyEightBit
            SimulatedUnixPlatform.macOsArm64, None
        ]

    let private systemOn (platform : SimulatedUnixPlatform, limit : uint64 option) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        match limit with
        | None -> system
        | Some limit ->
            { system with
                Machine = UnixMachineState.withUserAddressLimit limit system.Machine
            }

    let private contentOf (length : int) : byte[] =
        Array.init length (fun i -> byte (i + 1))

    /// A system holding one regular file of `content`, and a read-write
    /// descriptor onto it at `position`.
    let private withFile
        (content : ImmutableArray<byte>)
        (position : int64)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let inode, filesystem =
            match
                VirtualFileSystem.createFile
                    rootInode
                    (DirectoryEntryName.parseOrFail context "f")
                    (PermissionBits.parseOrFail context 0o644)
                    epoch
                    content
                    system.Machine.FileSystem
            with
            | Ok pair -> pair
            | Error error -> failwith $"could not seed the file: %O{error}"

        let fd, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite system.Process.FileDescriptors

        fd,
        { system with
            Machine =
                { system.Machine with
                    FileSystem = filesystem
                }
            Process =
                { system.Process with
                    FileDescriptors = FileDescriptorRegistry.setOffset fd position registry
                }
        }

    let private positionOf (fd : int) (system : UnixSystem<int, string>) : int64 =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.File (_, offset)) -> offset
        | other -> failwith $"expected a file descriptor, got %O{other}"

    // ------------------------------------------------------- the reference

    /// Which rule the reference applied, so that a test can show the generator
    /// reaches every one of them.
    [<RequireQualifiedAccess>]
    type private Rule =
        | CountRefused
        | Screened
        | ScreenRefused
        | PositionOverflow
        | NegativeOffset
        | NothingToMove
        | FaultAtCopy
        | RefusedAtCopy
        | Moved of shortened : bool

    let private isLinux (platform : SimulatedUnixPlatform) : bool =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// The steps every call shares ahead of its object's own operation, stated
    /// in arbitrary precision so that the reference cannot share an overflow
    /// with the implementation. `Ok n` is the count the operation then sees.
    let private sharedSteps
        (platform : SimulatedUnixPlatform)
        (limit : uint64 option)
        (buffer : UserBuffer)
        (position : int64 option)
        (count : uint64)
        : Result<uint64, Rule * Result<UnixError, BufferRefusal>>
        =
        if not (isLinux platform) && count > IntMax then
            // Darwin: ahead of everything, the descriptor included.
            Error (Rule.CountRefused, Ok UnixError.EINVAL)
        else

        let screen =
            match limit, buffer with
            | None, _ -> Ok false
            | Some _, UserBuffer.Addressless -> Error BufferRefusal.AddresslessAtScreen
            | Some limit, UserBuffer.Unmapped address -> Ok (bigint address + bigint count > bigint limit)
            // Wherever real storage is, a range longer than the whole address
            // space does not fit.
            | Some limit, UserBuffer.Mapped
            | Some limit, UserBuffer.Opaque -> Ok (bigint count > bigint limit)

        match screen with
        | Error refusal -> Error (Rule.ScreenRefused, Error refusal)
        | Ok true -> Error (Rule.Screened, Ok UnixError.EFAULT)
        | Ok false ->

        match position with
        | Some position when isLinux platform && bigint position + bigint count > bigint Int64.MaxValue ->
            Error (Rule.PositionOverflow, Ok UnixError.EINVAL)
        | _ ->
            Ok (
                if isLinux platform then
                    min count LinuxMaxTransfer
                else
                    count
            )

    /// A read of `content` at `position`: the answer and where the position
    /// ends up.
    let private expectedRead
        (platform : SimulatedUnixPlatform)
        (limit : uint64 option)
        (content : byte[])
        (position : int64)
        (buffer : UserBuffer)
        (count : uint64)
        : Rule * Result<ReadAnswer * int64, BufferRefusal>
        =
        match sharedSteps platform limit buffer (Some position) count with
        | Error (rule, Ok error) -> rule, Ok (ReadAnswer.Failed error, position)
        | Error (rule, Error refusal) -> rule, Error refusal
        | Ok moved ->

        let available =
            if position >= int64 content.Length then
                0UL
            else
                uint64 content.Length - uint64 position

        let transfer = min moved available

        if transfer = 0UL then
            Rule.NothingToMove, Ok (ReadAnswer.Completed ImmutableArray.Empty, position)
        else

        match buffer with
        | UserBuffer.Unmapped _ -> Rule.FaultAtCopy, Ok (ReadAnswer.Failed UnixError.EFAULT, position)
        | UserBuffer.Opaque -> Rule.RefusedAtCopy, Error BufferRefusal.OpaqueAtTransfer
        | UserBuffer.Addressless -> Rule.RefusedAtCopy, Error BufferRefusal.AddresslessAtTransfer
        | UserBuffer.Mapped ->
            let bytes = ImmutableArray.Create (content, int position, int transfer)
            Rule.Moved (moved < count), Ok (ReadAnswer.Completed bytes, position + int64 transfer)

    /// The admission of a write of `count` bytes at `position` (the description's
    /// for `write`, the argument for `pwrite`) into a regular file open for
    /// writing.
    let private expectedAdmission
        (platform : SimulatedUnixPlatform)
        (limit : uint64 option)
        (position : int64)
        (buffer : UserBuffer)
        (count : uint64)
        : Rule * Result<WriteAdmission, BufferRefusal>
        =
        let failed (error : UnixError) =
            Ok (WriteAdmission.Answered (WriteAnswer.Failed error))

        match sharedSteps platform limit buffer (Some position) count with
        | Error (rule, Ok error) -> rule, failed error
        | Error (rule, Error refusal) -> rule, Error refusal
        | Ok moved ->

        if moved = 0UL then
            Rule.NothingToMove, Ok (WriteAdmission.Answered (WriteAnswer.Completed 0L))
        else

        match buffer with
        | UserBuffer.Unmapped _ -> Rule.FaultAtCopy, failed UnixError.EFAULT
        | UserBuffer.Opaque -> Rule.RefusedAtCopy, Error BufferRefusal.OpaqueAtTransfer
        | UserBuffer.Addressless -> Rule.RefusedAtCopy, Error BufferRefusal.AddresslessAtTransfer
        | UserBuffer.Mapped -> Rule.Moved (moved < count), Ok (WriteAdmission.Transfer (int moved))

    // ------------------------------------------------------- the generators

    let private anyUInt64 : Gen<uint64> =
        gen {
            let! hi = Gen.choose (Int32.MinValue, Int32.MaxValue)
            let! lo = Gen.choose (Int32.MinValue, Int32.MaxValue)
            return (uint64 (uint32 hi) <<< 32) ||| uint64 (uint32 lo)
        }

    /// Where a rule changes its answer: the per-call limits, each machine's
    /// address limit, and the edges of the signed and unsigned ranges.
    let private notableCounts : uint64 list =
        [
            0UL
            1UL
            2UL
            5UL
            6UL
            0x7FFF_EFFFUL
            LinuxMaxTransfer
            LinuxMaxTransfer + 1UL
            IntMax - 1UL
            IntMax
            IntMax + 1UL
            0xFFFF_FFFFUL
            0x1_0000_0000UL
            for limit in
                [
                    ObservedUserAddressLimit.X64FourLevelPaging
                    ObservedUserAddressLimit.X64FiveLevelPaging
                    ObservedUserAddressLimit.Arm64FortyEightBit
                ] do
                limit - 1UL
                limit
                limit + 1UL
            0x7FFF_FFFF_FFFF_FFFFUL
            0x8000_0000_0000_0000UL
            UInt64.MaxValue - 1UL
            UInt64.MaxValue
        ]

    let private counts : Gen<uint64> =
        Gen.oneof
            [
                Gen.elements notableCounts
                anyUInt64
                Gen.choose (0, 24) |> Gen.map uint64
            ]

    /// Positions at which a count can carry position + count past INT64_MAX,
    /// as well as ordinary ones.
    let private positions : Gen<int64> =
        Gen.oneof
            [
                Gen.choose (0, 24) |> Gen.map int64
                Gen.elements
                    [
                        1L <<< 62
                        Int64.MaxValue - int64 LinuxMaxTransfer - 1L
                        Int64.MaxValue - int64 LinuxMaxTransfer
                        Int64.MaxValue - int64 LinuxMaxTransfer + 1L
                        Int64.MaxValue - int64 IntMax
                        yield! [ 0L .. 12L ] |> List.map (fun k -> Int64.MaxValue - k)
                    ]
                anyUInt64 |> Gen.map (fun u -> int64 (u >>> 1))
            ]

    let private addresses : Gen<uint64> =
        Gen.oneof
            [
                Gen.elements
                    [
                        0UL
                        8UL
                        0x1000_0000UL
                        ObservedUserAddressLimit.X64FourLevelPaging - 5UL
                        ObservedUserAddressLimit.Arm64FortyEightBit - 5UL
                        ObservedUserAddressLimit.Arm64FortyEightBit
                        UInt64.MaxValue
                    ]
                anyUInt64
            ]

    let private buffers : Gen<UserBuffer> =
        Gen.frequency
            [
                4, Gen.constant UserBuffer.Mapped
                1, Gen.constant UserBuffer.Opaque
                1, Gen.constant UserBuffer.Addressless
                4, addresses |> Gen.map UserBuffer.Unmapped
            ]

    type private Case =
        {
            Machine : SimulatedUnixPlatform * uint64 option
            Content : byte[]
            Position : int64
            Buffer : UserBuffer
            Count : uint64
        }

    let private cases : Gen<Case> =
        gen {
            let! machine = Gen.elements machines
            let! length = Gen.choose (0, 16)
            let! position = positions
            let! buffer = buffers
            let! count = counts

            return
                {
                    Machine = machine
                    Content = contentOf length
                    Position = position
                    Buffer = buffer
                    Count = count
                }
        }

    // ------------------------------------------------------- the properties

    let private actualRead (case : Case) : Result<ReadAnswer * int64, BufferRefusal> =
        let fd, system =
            withFile (ImmutableArray.CreateRange case.Content) case.Position (systemOn case.Machine)

        match UnixReadWrite.read fd case.Buffer case.Count system with
        | Ok (answer, after) -> Ok (answer, positionOf fd after)
        | Error (ReadRefusal.Buffer refusal) -> Error refusal
        | Error other -> failwith $"a regular file's read was refused for something other than its buffer: %O{other}"

    let private sameRead
        (expected : Result<ReadAnswer * int64, BufferRefusal>)
        (actual : Result<ReadAnswer * int64, BufferRefusal>)
        : bool
        =
        // `ImmutableArray` compares by reference, so the bytes are compared as
        // lists.
        let normalise (result : Result<ReadAnswer * int64, BufferRefusal>) =
            match result with
            | Ok (ReadAnswer.Completed bytes, position) -> Ok (Ok (List.ofSeq bytes), position)
            | Ok (ReadAnswer.Failed error, position) -> Ok (Error error, position)
            | Error refusal -> Error refusal

        normalise expected = normalise actual

    [<Test>]
    let ``read answers every count as the measured rule does`` () : unit =
        let property (case : Case) : unit =
            let platform, limit = case.Machine

            let _, expected =
                expectedRead platform limit case.Content case.Position case.Buffer case.Count

            let actual = actualRead case

            if not (sameRead expected actual) then
                failwith $"%A{case}: expected %A{expected}, got %A{actual}"

        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    [<Test>]
    let ``pread answers every count and offset as the measured rule does`` () : unit =
        let offsets : Gen<int64> =
            Gen.oneof
                [
                    positions
                    Gen.elements [ -1L ; Int64.MinValue ]
                    anyUInt64 |> Gen.map int64
                ]

        let property (case : Case, offset : int64) : unit =
            let platform, limit = case.Machine
            // The description's own position plays no part in a `pread`, so it
            // is left at 0 and the case's position is ignored.
            let fd, system =
                withFile (ImmutableArray.CreateRange case.Content) 0L (systemOn case.Machine)

            let expected =
                if offset < 0L then
                    // Linux checks the offset before anything else; Darwin after
                    // its count and the descriptor, which here is a good one.
                    // Either way it is EINVAL, as is Darwin's count.
                    Ok (ReadAnswer.Failed UnixError.EINVAL, 0L)
                else
                    match expectedRead platform limit case.Content offset case.Buffer case.Count with
                    | _, Ok (answer, _) -> Ok (answer, 0L)
                    | _, Error refusal -> Error refusal

            let actual =
                match UnixReadWrite.pread fd case.Buffer case.Count offset system with
                | Ok answer -> Ok (answer, 0L)
                | Error refusal -> Error refusal

            if not (sameRead expected actual) then
                failwith $"%A{case} at offset %d{offset}: expected %A{expected}, got %A{actual}"

        let arb = Arb.fromGen (Gen.zip cases offsets)
        Check.One (config, Prop.forAll arb property)

    [<Test>]
    let ``write admits every count as the measured rule does`` () : unit =
        let property (case : Case) : unit =
            let platform, limit = case.Machine

            let fd, system =
                withFile (ImmutableArray.CreateRange case.Content) case.Position (systemOn case.Machine)

            let _, expected =
                expectedAdmission platform limit case.Position case.Buffer case.Count

            let actual =
                match UnixReadWrite.admitWrite fd case.Buffer case.Count system with
                | Ok admission -> Ok admission
                | Error (WriteRefusal.Buffer refusal) -> Error refusal
                | Error other ->
                    failwith $"a regular file's write was refused for something other than its buffer: %O{other}"

            if expected <> actual then
                failwith $"%A{case}: expected %A{expected}, got %A{actual}"

        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    [<Test>]
    let ``pwrite admits every count and offset as the measured rule does`` () : unit =
        let offsets : Gen<int64> =
            Gen.oneof
                [
                    positions
                    Gen.elements [ -1L ; Int64.MinValue ]
                    anyUInt64 |> Gen.map int64
                ]

        let property (case : Case, offset : int64) : unit =
            let platform, limit = case.Machine

            let fd, system =
                withFile (ImmutableArray.CreateRange case.Content) 0L (systemOn case.Machine)

            let expected =
                if offset < 0L then
                    // Ahead of the descriptor on both flavours, and Darwin's count
                    // refusal is the same errno.
                    Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EINVAL))
                else
                    snd (expectedAdmission platform limit offset case.Buffer case.Count)

            let actual =
                match UnixReadWrite.admitPWrite fd case.Buffer case.Count offset system with
                | Ok admission -> Ok admission
                | Error (PWriteRefusal.Buffer refusal) -> Error refusal
                | Error other ->
                    failwith $"a regular file's pwrite was refused for something other than its buffer: %O{other}"

            if expected <> actual then
                failwith $"%A{case} at offset %d{offset}: expected %A{expected}, got %A{actual}"

        let arb = Arb.fromGen (Gen.zip cases offsets)
        Check.One (config, Prop.forAll arb property)

    /// The properties above are only as good as the cases they draw: one that
    /// never reached, say, the position check would pass an implementation
    /// without it. So every rule the reference applies must turn up in a sample
    /// of the size each property draws, for reads and for writes alike.
    [<Test>]
    let ``the generated cases reach every rule`` () : unit =
        let sample = Gen.sampleWithSeed (Rnd 20260926UL) 100 3000 cases |> List.ofArray

        let reached (rules : Rule list) : Set<string> =
            rules
            |> List.map (fun rule ->
                match rule with
                | Rule.Moved shortened -> $"Moved %b{shortened}"
                | other -> $"%A{other}"
            )
            |> Set.ofList

        let everyRule =
            Set.ofList
                [
                    "CountRefused"
                    "Screened"
                    "ScreenRefused"
                    "PositionOverflow"
                    "NothingToMove"
                    "FaultAtCopy"
                    "RefusedAtCopy"
                    "Moved false"
                ]

        let readRules =
            sample
            |> List.map (fun case ->
                let platform, limit = case.Machine
                fst (expectedRead platform limit case.Content case.Position case.Buffer case.Count)
            )
            |> reached

        let writeRules =
            sample
            |> List.map (fun case ->
                let platform, limit = case.Machine
                fst (expectedAdmission platform limit case.Position case.Buffer case.Count)
            )
            |> reached

        Set.difference everyRule readRules |> shouldEqual Set.empty
        // A write is shortened whenever a mapped buffer asks for more than one
        // call moves on Linux, which a read of a sixteen-byte file never shows.
        Set.difference (Set.add "Moved true" everyRule) writeRules
        |> shouldEqual Set.empty

    /// `write` and `pwrite` answer the position check themselves, so a caller
    /// that skipped the admission still gets a kernel's answer: EINVAL on Linux
    /// once position + count passes INT64_MAX, and one byte fewer reaches the
    /// file, whose length this kernel cannot represent. Darwin has no such
    /// check, and every one of these reaches the file.
    [<Test>]
    let ``write and pwrite check the position themselves`` () : unit =
        let nearTop = Int64.MaxValue - 10L

        let outcome (result : Result<WriteAnswer * UnixSystem<int, string>, 'refusal>) : string =
            match result with
            | Ok (WriteAnswer.Failed error, _) -> $"%O{error}"
            | Ok (WriteAnswer.Completed written, _) -> $"moved %d{written}"
            | Error _ -> "unrepresentable"

        for machine, eleven in
            [
                (SimulatedUnixPlatform.linuxX64, None), "EINVAL"
                (SimulatedUnixPlatform.macOsArm64, None), "unrepresentable"
            ] do
            for length, expected in [ 11, eleven ; 10, "unrepresentable" ] do
                let bytes = ImmutableArray.CreateRange (contentOf length)
                let fd, system = withFile ImmutableArray.Empty nearTop (systemOn machine)

                (machine, length, "write", outcome (UnixReadWrite.write fd bytes system))
                |> shouldEqual (machine, length, "write", expected)

                (machine, length, "pwrite", outcome (UnixReadWrite.pwrite fd bytes nearTop system))
                |> shouldEqual (machine, length, "pwrite", expected)

    // ------------------------------------------------------- one call's limit

    /// A file longer than one Linux call moves, read in one call: 0x7FFFF000
    /// bytes on Linux, whatever was asked for beyond that, and everything asked
    /// for on Darwin. Measured with /dev/zero and a 3 GiB sparse file.
    ///
    /// Two gigabytes of file and as much again of answer, so this runs alone.
    [<Test>]
    [<NonParallelizable>]
    let ``a read moves at most one call's worth`` () : unit =
        let length = int LinuxMaxTransfer + 16

        let content =
            ImmutableCollectionsMarshal.AsImmutableArray (Array.zeroCreate<byte> length)

        let movedBy (machine : SimulatedUnixPlatform * uint64 option) (count : uint64) : int =
            let fd, system = withFile content 0L (systemOn machine)

            let moved =
                match UnixReadWrite.read fd UserBuffer.Mapped count system with
                | Ok (ReadAnswer.Completed bytes, after) ->
                    positionOf fd after |> shouldEqual (int64 bytes.Length)
                    bytes.Length
                | other -> failwith $"expected a completed read, got %A{other}"

            GC.Collect ()
            moved

        let linux =
            SimulatedUnixPlatform.linuxArm64, Some ObservedUserAddressLimit.Arm64FortyEightBit

        for count in
            [
                LinuxMaxTransfer + 1UL
                IntMax + 1UL
                ObservedUserAddressLimit.Arm64FortyEightBit
            ] do
            (count, movedBy linux count) |> shouldEqual (count, int LinuxMaxTransfer)

        (IntMax, movedBy (SimulatedUnixPlatform.macOsArm64, None) IntMax)
        |> shouldEqual (IntMax, length)

        GC.Collect ()

    /// `write` and `pwrite` take the bytes their admission said to extract. A
    /// caller that skipped the admission and hands over more than one call
    /// moves is refused loudly rather than answered.
    ///
    /// Two gigabytes of bytes, so this runs alone.
    [<Test>]
    [<NonParallelizable>]
    let ``write and pwrite refuse more than one call's worth`` () : unit =
        let bytes =
            ImmutableCollectionsMarshal.AsImmutableArray (Array.zeroCreate<byte> (int LinuxMaxTransfer + 1))

        let fd, system =
            withFile ImmutableArray.Empty 0L (systemOn (SimulatedUnixPlatform.linuxX64, None))

        let exn =
            Assert.Throws<Exception> (fun () -> UnixReadWrite.write fd bytes system |> ignore)

        exn.Message |> shouldContainText "one call moves"

        let exn =
            Assert.Throws<Exception> (fun () -> UnixReadWrite.pwrite fd bytes 0L system |> ignore)

        exn.Message |> shouldContainText "one call moves"

        // The same bytes are one call's worth on Darwin, whose limit is INT_MAX,
        // and move.
        match UnixReadWrite.write 1 bytes (systemOn (SimulatedUnixPlatform.macOsArm64, None)) with
        | Ok (WriteAnswer.Completed written, _) -> written |> shouldEqual (int64 bytes.Length)
        | other -> failwith $"expected a completed write, got %A{other}"

        GC.Collect ()

    // --------------------------------------- the descriptors, as measured

    /// What a row of the measured grid reads as.
    [<RequireQualifiedAccess>]
    type private Seen =
        | Errno of UnixError
        | Moved of int
        | Refused

    let private socketId : SocketId = SocketId 0L

    let private withDescriptor (kind : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let registry = system.Process.FileDescriptors

        let reopen (inode : InodeNumber) (mode : FileAccessMode) =
            let fd, registry = FileDescriptorRegistry.openFile inode mode registry

            fd,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        match kind with
        | "closed" -> 7, system
        | "stdin" -> 0, system
        | "stdout" -> 1, system
        | "dir" ->
            let fd, registry = FileDescriptorRegistry.openDirectory rootInode registry

            fd,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        | "port" ->
            let fd, registry = FileDescriptorRegistry.createSocketEventPort registry

            fd,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        | "socket" ->
            let fd, registry = FileDescriptorRegistry.createSocket socketId registry

            fd,
            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            Map.ofList
                                [
                                    socketId,
                                    {
                                        Domain = SocketDomain.Inet
                                        Kind = SocketKind.Stream
                                        Protocol = SocketProtocol.Tcp
                                        Binding = None
                                        Phase = SocketPhase.Idle
                                        ReuseAddress = false
                                    }
                                ]
                    }
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        | "readonly" ->
            let fd, system = withFile (ImmutableArray.CreateRange (contentOf 5)) 0L system

            let inode =
                match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                | Some (OpenFileTarget.File (inode, _)) -> inode
                | other -> failwith $"expected a file, got %O{other}"

            let fd, registry =
                FileDescriptorRegistry.openFile inode FileAccessMode.ReadOnly system.Process.FileDescriptors

            fd,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        | "writeonly" ->
            let fd, system = withFile (ImmutableArray.CreateRange (contentOf 5)) 0L system

            let inode =
                match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                | Some (OpenFileTarget.File (inode, _)) -> inode
                | other -> failwith $"expected a file, got %O{other}"

            let fd, registry =
                FileDescriptorRegistry.openFile inode FileAccessMode.WriteOnly system.Process.FileDescriptors

            fd,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        | other -> failwith $"no descriptor kind %s{other}"

    let private seen
        (op : string)
        (kind : string)
        (machine : SimulatedUnixPlatform * uint64 option)
        (count : uint64)
        : Seen
        =
        let fd, system = withDescriptor kind (systemOn machine)
        let buffer = UserBuffer.Mapped

        match op with
        | "read" ->
            match UnixReadWrite.read fd buffer count system with
            | Ok (ReadAnswer.Failed error, _) -> Seen.Errno error
            | Ok (ReadAnswer.Completed bytes, _) -> Seen.Moved bytes.Length
            | Error _ -> Seen.Refused
        | "pread" ->
            match UnixReadWrite.pread fd buffer count 0L system with
            | Ok (ReadAnswer.Failed error) -> Seen.Errno error
            | Ok (ReadAnswer.Completed bytes) -> Seen.Moved bytes.Length
            | Error _ -> Seen.Refused
        | "write" ->
            match UnixReadWrite.admitWrite fd buffer count system with
            | Ok (WriteAdmission.Answered (WriteAnswer.Failed error)) -> Seen.Errno error
            | Ok (WriteAdmission.Answered (WriteAnswer.Completed written)) -> Seen.Moved (int written)
            | Ok (WriteAdmission.Transfer count) -> Seen.Moved count
            | Error _ -> Seen.Refused
        | "pwrite" ->
            match UnixReadWrite.admitPWrite fd buffer count 0L system with
            | Ok (WriteAdmission.Answered (WriteAnswer.Failed error)) -> Seen.Errno error
            | Ok (WriteAdmission.Answered (WriteAnswer.Completed written)) -> Seen.Moved (int written)
            | Ok (WriteAdmission.Transfer count) -> Seen.Moved count
            | Error _ -> Seen.Refused
        | other -> failwith $"no op %s{other}"

    /// The measured grid, through a mapped buffer, at counts of 0, 5, 2^31 (past
    /// Darwin's limit and inside every address space) and SIZE_MAX (past every
    /// address space). Linux is the aarch64 machine it was measured on.
    ///
    /// Each row is the descriptor's answer on Linux and on Darwin. Refused means
    /// this library declines to answer (a socket's connection state); every
    /// errno is measured. An open file whose access mode forbids the call is
    /// "readonly" or "writeonly"; the standard streams are the two ends of a
    /// pipe, stdin the read end at end-of-file.
    [<Test>]
    let ``every descriptor answers a huge count as measured`` () : unit =
        let linux =
            SimulatedUnixPlatform.linuxArm64, Some ObservedUserAddressLimit.Arm64FortyEightBit

        let darwin = SimulatedUnixPlatform.macOsArm64, None
        let ebadf = Seen.Errno UnixError.EBADF
        let einval = Seen.Errno UnixError.EINVAL
        let efault = Seen.Errno UnixError.EFAULT
        let espipe = Seen.Errno UnixError.ESPIPE
        let eisdir = Seen.Errno UnixError.EISDIR
        let enxio = Seen.Errno UnixError.ENXIO
        let maxLinux = Seen.Moved (int LinuxMaxTransfer)
        let counts = [ 0UL ; 5UL ; IntMax + 1UL ; UInt64.MaxValue ]

        let rows : (string * string * Seen list * Seen list) list =
            [
                "read", "closed", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "read", "writeonly", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "read", "stdout", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "read", "port", [ einval ; einval ; einval ; einval ], [ enxio ; enxio ; einval ; einval ]
                "read", "dir", [ eisdir ; eisdir ; eisdir ; efault ], [ eisdir ; eisdir ; einval ; einval ]
                "read",
                "stdin",
                [ Seen.Moved 0 ; Seen.Moved 0 ; Seen.Moved 0 ; efault ],
                [ Seen.Moved 0 ; Seen.Moved 0 ; einval ; einval ]
                "read",
                "socket",
                [ Seen.Moved 0 ; Seen.Refused ; Seen.Refused ; efault ],
                [ Seen.Refused ; Seen.Refused ; einval ; einval ]

                "pread", "closed", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "pread", "writeonly", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "pread", "stdin", [ espipe ; espipe ; espipe ; espipe ], [ espipe ; espipe ; einval ; einval ]
                "pread", "stdout", [ espipe ; espipe ; espipe ; espipe ], [ ebadf ; ebadf ; einval ; einval ]
                "pread", "port", [ espipe ; espipe ; espipe ; espipe ], [ espipe ; espipe ; einval ; einval ]
                "pread", "socket", [ espipe ; espipe ; espipe ; espipe ], [ espipe ; espipe ; einval ; einval ]
                "pread", "dir", [ eisdir ; eisdir ; eisdir ; efault ], [ eisdir ; eisdir ; einval ; einval ]

                "write", "closed", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "write", "readonly", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "write", "stdin", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "write", "dir", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "write", "port", [ einval ; einval ; einval ; einval ], [ enxio ; enxio ; einval ; einval ]
                "write",
                "stdout",
                [ Seen.Moved 0 ; Seen.Moved 5 ; maxLinux ; efault ],
                [ Seen.Moved 0 ; Seen.Moved 5 ; einval ; einval ]
                "write",
                "socket",
                [ Seen.Refused ; Seen.Refused ; Seen.Refused ; efault ],
                [ Seen.Refused ; Seen.Refused ; einval ; einval ]

                "pwrite", "closed", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "pwrite", "readonly", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "pwrite", "dir", [ ebadf ; ebadf ; ebadf ; ebadf ], [ ebadf ; ebadf ; einval ; einval ]
                "pwrite", "stdin", [ espipe ; espipe ; espipe ; espipe ], [ ebadf ; ebadf ; einval ; einval ]
                "pwrite", "stdout", [ espipe ; espipe ; espipe ; espipe ], [ espipe ; espipe ; einval ; einval ]
                "pwrite", "port", [ espipe ; espipe ; espipe ; espipe ], [ espipe ; espipe ; einval ; einval ]
                "pwrite", "socket", [ espipe ; espipe ; espipe ; espipe ], [ espipe ; espipe ; einval ; einval ]
            ]

        for op, kind, onLinux, onDarwin in rows do
            (op, kind, "Linux", counts |> List.map (seen op kind linux))
            |> shouldEqual (op, kind, "Linux", onLinux)

            (op, kind, "Darwin", counts |> List.map (seen op kind darwin))
            |> shouldEqual (op, kind, "Darwin", onDarwin)

    /// A directory has a position too, and Linux checks position + count
    /// against it ahead of EISDIR. At its start the position is 0, and after an
    /// `lseek` it is the offset `lseek` set. Partway through a scan it is the
    /// filesystem's own: on tmpfs at most INT_MAX, measured
    /// (transfer-counts-directory.c), so no count the buffer screen admits can
    /// carry it past INT64_MAX; on NFS the server's cookie, which could be
    /// anything, so a nonzero count is refused. Darwin checks no position.
    [<Test>]
    let ``a directory read checks the position its description holds`` () : unit =
        let nearTop = Int64.MaxValue - 10L

        let scanned =
            DirectoryPosition.Cursor (DirectoryCursor.After (DirectoryEntryName.parseOrFail context "a"))

        let finished = DirectoryPosition.Cursor DirectoryCursor.ReturnedDot
        let start = DirectoryPosition.Cursor DirectoryCursor.Start
        let arm64Limit = ObservedUserAddressLimit.Arm64FortyEightBit

        let seenAt
            (platform : SimulatedUnixPlatform)
            (fileSystem : EmulatedFileSystemType)
            (position : DirectoryPosition)
            (count : uint64)
            : Seen
            =
            let system = systemOn (platform, None)

            let system =
                { system with
                    Machine = UnixMachineState.withFileSystemType (Some fileSystem) system.Machine
                }

            let fd, registry =
                FileDescriptorRegistry.openDirectory rootInode system.Process.FileDescriptors

            let system =
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors = FileDescriptorRegistry.setDirectoryPosition fd position registry
                        }
                }

            match UnixReadWrite.read fd UserBuffer.Mapped count system with
            | Ok (ReadAnswer.Failed error, _) -> Seen.Errno error
            | Ok (ReadAnswer.Completed bytes, _) -> Seen.Moved bytes.Length
            | Error (ReadRefusal.ScannedDirectoryPosition _) -> Seen.Refused
            | Error other -> failwith $"unexpected refusal %A{other}"

        let eisdir = Seen.Errno UnixError.EISDIR
        let einval = Seen.Errno UnixError.EINVAL
        let efault = Seen.Errno UnixError.EFAULT
        let linux = SimulatedUnixPlatform.linuxArm64
        let darwin = SimulatedUnixPlatform.macOsArm64

        let rows =
            [
                // Where `lseek` put it: ten bytes fit below INT64_MAX, eleven do not.
                linux, EmulatedFileSystemType.Tmpfs, DirectoryPosition.Unenumerable nearTop, 10UL, eisdir
                linux, EmulatedFileSystemType.Tmpfs, DirectoryPosition.Unenumerable nearTop, 11UL, einval
                linux, EmulatedFileSystemType.Nfs, DirectoryPosition.Unenumerable nearTop, 11UL, einval
                darwin, EmulatedFileSystemType.Apfs, DirectoryPosition.Unenumerable nearTop, 11UL, eisdir
                // At the start, position 0: nothing the screen admits overflows.
                linux, EmulatedFileSystemType.Nfs, start, 1UL, eisdir
                linux, EmulatedFileSystemType.Tmpfs, start, arm64Limit, eisdir
                linux, EmulatedFileSystemType.Tmpfs, start, arm64Limit + 1UL, efault
                // Partway through, and at the end, of a tmpfs scan: at most INT_MAX.
                linux, EmulatedFileSystemType.Tmpfs, scanned, 1UL, eisdir
                linux, EmulatedFileSystemType.Tmpfs, finished, arm64Limit, eisdir
                // ...of an NFS scan: the server's cookie, so only a count of zero
                // is decided.
                linux, EmulatedFileSystemType.Nfs, scanned, 0UL, eisdir
                linux, EmulatedFileSystemType.Nfs, scanned, 1UL, Seen.Refused
                linux, EmulatedFileSystemType.Nfs, finished, 1UL, Seen.Refused
                // The screen still comes first.
                linux, EmulatedFileSystemType.Nfs, finished, UInt64.MaxValue, efault
                // Darwin has no position check to depend on the cookie.
                darwin, EmulatedFileSystemType.Nfs, finished, 5UL, eisdir
                darwin, EmulatedFileSystemType.Apfs, scanned, IntMax, eisdir
            ]

        for platform, fileSystem, position, count, expected in rows do
            (platform, fileSystem, position, count, seenAt platform fileSystem position count)
            |> shouldEqual (platform, fileSystem, position, count, expected)

    // ---------------------------------------------------------------- getcwd

    /// `getcwd` has no limit of its own: every capacity from the path's length
    /// plus its terminator up to SIZE_MAX reports the path, measured on both.
    [<Test>]
    let ``getcwd reports the path at every capacity that holds it`` () : unit =
        let capacities : Gen<uint64> =
            Gen.oneof [ Gen.elements notableCounts ; anyUInt64 ; Gen.choose (0, 8) |> Gen.map uint64 ]

        let property (machine : SimulatedUnixPlatform * uint64 option, capacity : uint64) : unit =
            let system = systemOn machine
            // The root, whose path is "/" and so needs two bytes.
            let expected =
                if capacity = 0UL then
                    Ok (GetCwdAnswer.Failed UnixError.EINVAL)
                elif capacity < 2UL then
                    Ok (GetCwdAnswer.Failed UnixError.ERANGE)
                else
                    Ok (GetCwdAnswer.Reported (ImmutableArray.CreateRange [| byte '/' ; 0uy |]))

            let normalise (result : Result<GetCwdAnswer, GetCwdRefusal>) =
                match result with
                | Ok (GetCwdAnswer.Reported bytes) -> Ok (Ok (List.ofSeq bytes))
                | Ok (GetCwdAnswer.Failed error) -> Ok (Error error)
                | Error refusal -> Error refusal

            let actual = UnixPathResolution.getcwd UserBuffer.Mapped capacity system

            if normalise expected <> normalise actual then
                failwith $"%A{machine} at capacity %d{capacity}: expected %A{expected}, got %A{actual}"

        let arb = Arb.fromGen (Gen.zip (Gen.elements machines) capacities)
        Check.One (config, Prop.forAll arb property)
