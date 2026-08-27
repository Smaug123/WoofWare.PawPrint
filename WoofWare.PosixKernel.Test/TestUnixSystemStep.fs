namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The syscall surface, driven the way a client that is not PawPrint would
/// drive it.
///
/// This is the first fixture at this altitude, and it is here for the reachable
/// set rather than for the arithmetic: the guest tier runs one flavour, so the
/// Darwin arms of an ordering divergence are unreachable from a guest and
/// reachable from here by constructing a Darwin system directly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSystemStep =

    let private context : string = "TestUnixSystemStep"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    /// A machine with an empty filesystem, on the flavour asked for. Spelled out
    /// rather than built by a constructor because the library has none: see the
    /// note in the extraction plan.
    let private machineOn (platform : SimulatedUnixPlatform) : UnixMachineState =
        {
            Sockets = Map.empty
            Connections = Map.empty
            NextConnectionId = ConnectionId 0L
            NextSocketEventRegistrationOrdinal = 0L
            NextEphemeralPort = 32768us
            EphemeralPortRange = (32768us, 60999us)
            SoMaxConn = 4096
            LocalAddresses = []
            LocalRoutes = []
            NextSocketId = SocketId 0L
            VirtualClockTicks = 0L
            WallClockEpochMs = 0L
            NonCryptoRandomState = 1UL
            CryptoRandomState = 1UL
            ProcessorCount = 1
            UserAddressLimit = 0x7FFFFFFFFFFFUL
            UnixPlatform = platform
            FileSystem = VirtualFileSystem.empty epoch
            FileSystemType = EmulatedFileSystemType.Tmpfs
        }

    let private rootInode : InodeNumber = InodeNumber 1L

    let private processOn () : UnixProcessState<int, string> =
        {
            FileDescriptors = FileDescriptorRegistry.initial
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = Map.empty
            CurrentDirectory = AbsoluteUnixPath.parseOrFail context "/"
            CurrentDirectoryInode = rootInode
            ProcessPath = None
            DirectoryStreams = Map.empty
            NextDirectoryStreamId = DirectoryStreamId 0L
            UserId = 1000u
            GroupId = 1000u
            Umask = PermissionBits.parseOrFail context 0o022
            Signals = SignalState.empty
        }

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        {
            Machine = machineOn platform
            Process = processOn ()
            Tasks = Map.empty
        }

    let private linux : UnixSystem<int, string> =
        systemOn SimulatedUnixPlatform.linuxX64

    let private darwin : UnixSystem<int, string> =
        systemOn SimulatedUnixPlatform.macOsArm64

    /// A system holding one five-byte regular file, and the descriptor onto it.
    let private withOpenFile (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let inode, filesystem =
            match
                VirtualFileSystem.createFile
                    rootInode
                    (FileName.parseOrFail context "f")
                    (PermissionBits.parseOrFail context 0o644)
                    epoch
                    (ImmutableArray.CreateRange [ 1uy ; 2uy ; 3uy ; 4uy ; 5uy ])
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
                    FileDescriptors = registry
                }
        }

    /// A second descriptor onto the seeded file, opened with the access mode
    /// asked for. Separate from `withOpenFile` because the access mode is what
    /// several of the ordering rows below vary.
    let private withFileOpenedAs
        (accessMode : FileAccessMode)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, system = withOpenFile system

        let inode =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file descriptor, got %O{other}"

        let fd, registry =
            FileDescriptorRegistry.openFile inode accessMode system.Process.FileDescriptors

        fd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private answered (result : Result<SyscallAnswer * UnixSystem<int, string>, 'a>) : SyscallAnswer =
        match result with
        | Ok (answer, _) -> answer
        | Error e -> failwith $"expected an answer, got a refusal: %O{e}"

    // -------------------------------------------------------------------- read

    let private readBytes (result : Result<ReadAnswer * UnixSystem<int, string>, ReadRefusal>) : byte list =
        match result with
        | Ok (ReadAnswer.Completed bytes, _) -> List.ofSeq bytes
        | other -> failwith $"expected a completed read, got %A{other}"

    [<Test>]
    let ``read moves the window it says it moved, and advances the offset by it`` () : unit =
        let fd, system = withOpenFile linux

        // The seeded file is five bytes, so this is a short read: the offset
        // must land at the end rather than past it, which is what makes the
        // second read report end-of-file instead of a second short read.
        match UnixSystem.read fd UserBuffer.Mapped 8 system with
        | Ok (ReadAnswer.Completed bytes, after) ->
            List.ofSeq bytes |> shouldEqual [ 1uy ; 2uy ; 3uy ; 4uy ; 5uy ]

            match FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors with
            | Some (OpenFileTarget.File (_, offset)) -> offset |> shouldEqual 5L
            | other -> failwith $"expected a file descriptor, got %O{other}"

            UnixSystem.read fd UserBuffer.Mapped 8 after |> readBytes |> shouldEqual []
        | other -> failwith $"unexpected: %O{other}"

    [<Test>]
    let ``a read that moves nothing does not consult its buffer`` () : unit =
        // The measured rule: `read(f, NULL, 5)` at end-of-file is 0 rather than
        // EFAULT, and the same holds for every buffer that has no bytes to give.
        // A `Completed` with no bytes is how that reaches the caller, and a
        // caller that resolved its pointer first would turn each of these into a
        // fault or a crash.
        let fd, system = withOpenFile linux

        let system =
            match UnixSystem.read fd UserBuffer.Mapped 8 system with
            | Ok (_, system) -> system
            | other -> failwith $"could not exhaust the file: %A{other}"

        // Not `Addressless`, which never reaches the shortcut on this flavour:
        // see the row below, which is where that asymmetry is pinned.
        for buffer in [ UserBuffer.Unmapped 0UL ; UserBuffer.Opaque ; UserBuffer.Mapped ] do
            UnixSystem.read fd buffer 5 system |> readBytes |> shouldEqual []

    [<Test>]
    let ``a zero-length read does not consult its buffer either`` () : unit =
        // Distinct from the row above: there the *file* had nothing left, here
        // the *caller* asked for nothing, and only the second is reachable
        // without first exhausting the file.
        let fd, system = withOpenFile linux

        for buffer in [ UserBuffer.Unmapped 0UL ; UserBuffer.Opaque ; UserBuffer.Mapped ] do
            UnixSystem.read fd buffer 0 system |> readBytes |> shouldEqual []

    [<Test>]
    let ``an addressless buffer is refused before the shortcuts on a screening platform`` () : unit =
        // The one buffer whose answer depends on the flavour rather than on what
        // the read would have done. Linux screens the address before the
        // operation, and an addressless buffer cannot be screened — so it is
        // refused even for a read that would have moved nothing and never
        // touched it. Darwin screens nothing, so the same call reaches the
        // shortcut and answers 0.
        //
        // Both halves matter: the Linux one says the screen really does precede
        // the shortcuts, and the Darwin one says the refusal is the screen's
        // rather than a property of the buffer.
        let fd, system = withOpenFile linux

        UnixSystem.read fd UserBuffer.Addressless 0 system
        |> shouldEqual (Error (ReadRefusal.Buffer BufferRefusal.AddresslessAtScreen))

        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.read darwinFd UserBuffer.Addressless 0 darwinSystem
        |> readBytes
        |> shouldEqual []

    [<Test>]
    let ``a transfer through a buffer with no bytes is refused, not faulted`` () : unit =
        // EFAULT would be a wrong answer for an opaque address rather than an
        // approximate one; and an addressless buffer has nothing to fault about.
        let fd, system = withOpenFile linux

        UnixSystem.read fd UserBuffer.Opaque 5 system
        |> shouldEqual (Error (ReadRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

        // Under Darwin, which screens nothing up front, an addressless buffer
        // survives to the transfer; under Linux it is refused at the screen.
        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.read darwinFd UserBuffer.Addressless 5 darwinSystem
        |> shouldEqual (Error (ReadRefusal.Buffer BufferRefusal.AddresslessAtTransfer))

        UnixSystem.read fd UserBuffer.Addressless 5 system
        |> shouldEqual (Error (ReadRefusal.Buffer BufferRefusal.AddresslessAtScreen))

    [<Test>]
    let ``an unmapped buffer faults where the platform says it does`` () : unit =
        // Linux screens before the operation, so a wild address faults even
        // though the file has bytes to give; Darwin discovers it at the copy.
        // Either way the offset does not move.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue

        for flavour in [ linux ; darwin ] do
            let fd, system = withOpenFile flavour

            match UnixSystem.read fd wild 5 system with
            | Ok (ReadAnswer.Failed UnixError.EFAULT, after) ->
                match FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors with
                | Some (OpenFileTarget.File (_, offset)) -> offset |> shouldEqual 0L
                | other -> failwith $"expected a file descriptor, got %O{other}"
            | other -> failwith $"unexpected: %O{other}"

    [<Test>]
    let ``the screen answers where the transfer would not have`` () : unit =
        // The row that can tell "screened up front" from "faulted at the copy",
        // and the only shape that can: a wild address on an *exhausted* file.
        // With bytes left to move, both orders answer EFAULT and no input
        // separates them; with none left, Linux still faults because the screen
        // precedes the transfer window, and Darwin answers 0 because it screens
        // nothing and the shortcut is reached.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue

        let exhaust (flavour : UnixSystem<int, string>) : int * UnixSystem<int, string> =
            let fd, system = withOpenFile flavour

            match UnixSystem.read fd UserBuffer.Mapped 8 system with
            | Ok (_, system) -> fd, system
            | other -> failwith $"could not exhaust the file: %A{other}"

        let linuxFd, linuxSystem = exhaust linux

        UnixSystem.read linuxFd wild 5 linuxSystem
        |> shouldEqual (Ok (ReadAnswer.Failed UnixError.EFAULT, linuxSystem))

        let darwinFd, darwinSystem = exhaust darwin

        UnixSystem.read darwinFd wild 5 darwinSystem |> readBytes |> shouldEqual []

    let private socketDescription : SocketDescription =
        {
            Domain = SocketDomain.InterNetwork
            Kind = SocketKind.Stream
            Protocol = SocketProtocol.Tcp
            Binding = None
            Phase = SocketPhase.Idle
            ReuseAddress = false
        }

    let private socketZero : SocketId = SocketId 0L

    /// A descriptor onto one unbound, unconnected INET stream socket.
    let private withSocket (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.createSocket socketZero system.Process.FileDescriptors

        fd,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.ofList [ socketZero, socketDescription ]
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private socketRefused : Result<ReadAnswer * UnixSystem<int, string>, ReadRefusal> =
        Error (ReadRefusal.SocketConnectionState (socketZero, SocketDomain.InterNetwork, SocketKind.Stream))

    [<Test>]
    let ``a socket is refused, and the refusal names it`` () : unit =
        // `read(2)` on a socket is an answer about connection state, which this
        // kernel does not model; a constant here would become a lie the moment
        // it did. The refusal carries the socket's domain and kind because the
        // measured answers differ by both, and only the library can see them.
        let fd, system = withSocket linux

        UnixSystem.read fd UserBuffer.Mapped 5 system |> shouldEqual socketRefused

    [<Test>]
    let ``a screening platform answers a socket's bad address before the read`` () : unit =
        // Measured on both. Linux screens the address before the object's own
        // read operation, so `read(socket, (void*)-1, n)` is EFAULT for every `n`
        // including 0 — the socket is never consulted, and refusing would
        // decline a call a real kernel answers. Darwin screens nothing, so the
        // same call reaches the socket and earns a connection-state answer this
        // kernel cannot give.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue
        let linuxFd, linuxSystem = withSocket linux
        let darwinFd, darwinSystem = withSocket darwin

        for count in [ 0 ; 5 ] do
            UnixSystem.read linuxFd wild count linuxSystem
            |> shouldEqual (Ok (ReadAnswer.Failed UnixError.EFAULT, linuxSystem))

            UnixSystem.read darwinFd wild count darwinSystem
            |> shouldEqual (
                Error (ReadRefusal.SocketConnectionState (socketZero, SocketDomain.InterNetwork, SocketKind.Stream))
            )

    [<Test>]
    let ``a zero-length read of a socket is answered on Linux and refused on Darwin`` () : unit =
        // The one socket answer that needs no connection state, and it is a
        // flavour fact rather than a quirk of one socket kind: measured on
        // Linux, `read(sock, buf, 0)` is 0 for an INET stream, a UNIX-domain
        // stream and a datagram socket alike, while the same descriptors answer
        // ENOTCONN at length 1. Darwin has no such short-circuit — its stream
        // sockets answer ENOTCONN at length 0 too — so there the refusal stands.
        let linuxFd, linuxSystem = withSocket linux
        let darwinFd, darwinSystem = withSocket darwin

        UnixSystem.read linuxFd UserBuffer.Mapped 0 linuxSystem
        |> shouldEqual (Ok (ReadAnswer.Completed ImmutableArray.Empty, linuxSystem))

        UnixSystem.read darwinFd UserBuffer.Mapped 0 darwinSystem
        |> shouldEqual (
            Error (ReadRefusal.SocketConnectionState (socketZero, SocketDomain.InterNetwork, SocketKind.Stream))
        )

        // And the rule really is about the length rather than the socket: one
        // byte is refused on both.
        UnixSystem.read linuxFd UserBuffer.Mapped 1 linuxSystem
        |> shouldEqual socketRefused

    [<Test>]
    let ``Linux's zero-length socket answer does not depend on the phase`` () : unit =
        // The rule is keyed on the flavour alone, so it must hold for every
        // phase this kernel can put a socket in — measured on Linux for an idle,
        // a bound-not-listening, a listening and a connected socket, empty and
        // with a byte queued, and for one whose peer has closed. A rule drawn
        // from a single phase would be a rule about that phase.
        let phases =
            [
                SocketPhase.Idle
                SocketPhase.Listening
                    {
                        ListenState.Backlog = 4
                        ListenState.Queue = []
                    }
                SocketPhase.Established (ConnectionId 0L)
                SocketPhase.EstablishedPendingReport (ConnectionId 0L)
                SocketPhase.RefusedPendingDelivery
            ]

        for phase in phases do
            let fd, registry =
                FileDescriptorRegistry.createSocket socketZero linux.Process.FileDescriptors

            let system =
                { linux with
                    Machine =
                        { linux.Machine with
                            Sockets =
                                Map.ofList
                                    [
                                        socketZero,
                                        { socketDescription with
                                            Phase = phase
                                        }
                                    ]
                        }
                    Process =
                        { linux.Process with
                            FileDescriptors = registry
                        }
                }

            UnixSystem.read fd UserBuffer.Mapped 0 system
            |> shouldEqual (Ok (ReadAnswer.Completed ImmutableArray.Empty, system))

            // ...and one byte is still refused in every one of them, so the row
            // above is about the length rather than about the phase happening to
            // be an answerable one.
            match UnixSystem.read fd UserBuffer.Mapped 1 system with
            | Error (ReadRefusal.SocketConnectionState _) -> ()
            | other -> failwith $"expected a refusal for phase %O{phase}, got %A{other}"

    [<Test>]
    let ``read of a descriptor that is not open is EBADF whatever the buffer`` () : unit =
        // The descriptor precedes the buffer on both platforms, so even a buffer
        // that has no answer at all does not get one here.
        for buffer in
            [
                UserBuffer.Mapped
                UserBuffer.Unmapped 0UL
                UserBuffer.Opaque
                UserBuffer.Addressless
            ] do
            UnixSystem.read 7 buffer 5 linux
            |> shouldEqual (Ok (ReadAnswer.Failed UnixError.EBADF, linux))

    [<Test>]
    let ``a negative count is refused as the caller's own mistake`` () : unit =
        // Not an errno: a kernel never sees a negative count, because the
        // foreign-function layer that would produce one answers it first. A
        // library that returned EINVAL here would be inventing a kernel
        // behaviour to cover a client's bug.
        let fd, system = withOpenFile linux

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                UnixSystem.read fd UserBuffer.Mapped -1 system
                |> ignore<Result<ReadAnswer * UnixSystem<int, string>, ReadRefusal>>
            )

        exn.Message |> shouldContainText "UnixSystem.read"

    // ------------------------------------------------------------------- write

    let private admitted (result : Result<WriteAdmission, WriteRefusal>) : WriteAdmission =
        match result with
        | Ok admission -> admission
        | Error refusal -> failwith $"expected an admission, got %A{refusal}"

    /// A descriptor onto the seeded file, opened read-only.
    let private withReadOnlyFile (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        withFileOpenedAs FileAccessMode.ReadOnly system

    [<Test>]
    let ``the access mode precedes the buffer and the zero-length no-op`` () : unit =
        // Measured: `write(rdonlyFd, buf, 0)` is EBADF rather than 0, so neither
        // the screen nor the no-op can be reached from a descriptor that cannot
        // be written. Driven at count 0 *and* with a buffer that would otherwise
        // refuse, since either alone could pass against a wrong order.
        for flavour in [ linux ; darwin ] do
            let fd, system = withReadOnlyFile flavour

            for buffer in [ UserBuffer.Mapped ; UserBuffer.Addressless ; UserBuffer.Unmapped 0UL ] do
                for count in [ 0 ; 5 ] do
                    UnixSystem.admitWrite fd buffer count system
                    |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EBADF)))

            UnixSystem.admitWrite 7 UserBuffer.Mapped 0 system
            |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EBADF)))

    [<Test>]
    let ``the screen precedes the zero-length no-op, on the platform that screens`` () : unit =
        // The pair that says the order is the order. A wild address with nothing
        // to write is EFAULT on Linux, which screens before the operation, and 0
        // on Darwin, which screens nothing and reaches the no-op.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue

        let linuxFd, linuxSystem = withOpenFile linux

        UnixSystem.admitWrite linuxFd wild 0 linuxSystem
        |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT)))

        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.admitWrite darwinFd wild 0 darwinSystem
        |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Completed 0)))

    [<Test>]
    let ``a write that reaches the copy asks for exactly what was requested`` () : unit =
        let fd, system = withOpenFile linux

        UnixSystem.admitWrite fd UserBuffer.Mapped 3 system
        |> admitted
        |> shouldEqual (WriteAdmission.Transfer 3)

    [<Test>]
    let ``a buffer with no bytes is refused at the copy, not faulted`` () : unit =
        let fd, system = withOpenFile linux

        UnixSystem.admitWrite fd UserBuffer.Opaque 3 system
        |> shouldEqual (Error (WriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

        // An addressless buffer is refused at the *screen* under Linux and at
        // the copy under Darwin, which is the same asymmetry `read` has.
        UnixSystem.admitWrite fd UserBuffer.Addressless 3 system
        |> shouldEqual (Error (WriteRefusal.Buffer BufferRefusal.AddresslessAtScreen))

        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.admitWrite darwinFd UserBuffer.Addressless 3 darwinSystem
        |> shouldEqual (Error (WriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer))

    [<Test>]
    let ``admitting a write changes nothing`` () : unit =
        // Everything a write does before the copy is a question, so a caller may
        // ask it and then decline to write. If this ever stopped holding, the
        // two-call shape would be handing out a half-performed syscall.
        let fd, system = withOpenFile linux

        for buffer, count in [ UserBuffer.Mapped, 3 ; UserBuffer.Unmapped 0UL, 3 ; UserBuffer.Mapped, 0 ] do
            UnixSystem.admitWrite fd buffer count system
            |> ignore<Result<WriteAdmission, WriteRefusal>>

        UnixSystem.admitWrite fd UserBuffer.Mapped 3 system
        |> admitted
        |> shouldEqual (WriteAdmission.Transfer 3)

    [<Test>]
    let ``a write to a file lands, and advances the offset by what moved`` () : unit =
        let fd, system = withOpenFile linux

        match UnixSystem.write fd (ImmutableArray.CreateRange [ 9uy ; 9uy ]) system with
        | Ok (WriteAnswer.Completed written, after) ->
            written |> shouldEqual 2

            match FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, offset)) ->
                offset |> shouldEqual 2L

                match VirtualFileSystem.tryGetContent inode after.Machine.FileSystem with
                | Some (InodeContent.RegularFile (contents, _)) ->
                    List.ofSeq contents |> shouldEqual [ 9uy ; 9uy ; 3uy ; 4uy ; 5uy ]
                | other -> failwith $"expected a regular file, got %O{other}"
            | other -> failwith $"expected a file descriptor, got %O{other}"
        | other -> failwith $"unexpected: %A{other}"

    [<Test>]
    let ``a write to a standard stream is recorded rather than stored`` () : unit =
        let bytes = ImmutableArray.CreateRange [ 0x68uy ; 0x69uy ]

        match UnixSystem.write 1 bytes linux with
        | Ok (WriteAnswer.Completed written, after) ->
            written |> shouldEqual 2

            after.Process.OutputLog
            |> List.ofSeq
            |> shouldEqual
                [
                    {
                        OutputLogEntry.Role = FileDescriptorRole.StandardOutput
                        OutputLogEntry.Bytes = bytes
                    }
                ]
        | other -> failwith $"unexpected: %A{other}"

    [<Test>]
    let ``write answers the descriptor questions itself`` () : unit =
        // A caller that skipped the admission gets a kernel's answer rather than
        // an inconsistent one, which is what lets the second call take no buffer.
        UnixSystem.write 7 (ImmutableArray.CreateRange [ 1uy ]) linux
        |> shouldEqual (Ok (WriteAnswer.Failed UnixError.EBADF, linux))

        let fd, system = withReadOnlyFile linux

        UnixSystem.write fd (ImmutableArray.CreateRange [ 1uy ]) system
        |> shouldEqual (Ok (WriteAnswer.Failed UnixError.EBADF, system))

    [<Test>]
    let ``an empty write changes nothing, but only after the descriptor checks`` () : unit =
        // `admitWrite` answers this case too, so the arm is unreachable for a
        // caller that used the pair — but a caller that did not must get the
        // same answer. Measured: a zero-length write leaves `mtime` and `ctime`
        // where they were and does not extend the file, and
        // `VirtualFileSystem.writeFile` asserts a non-empty write for exactly
        // that reason.
        let fd, system = withOpenFile linux

        UnixSystem.write fd ImmutableArray<byte>.Empty system
        |> shouldEqual (Ok (WriteAnswer.Completed 0, system))

        // The standard-stream arm too, where the failure would be a phantom
        // entry in the output log rather than a restamped inode.
        UnixSystem.write 1 ImmutableArray<byte>.Empty linux
        |> shouldEqual (Ok (WriteAnswer.Completed 0, linux))

        // ...and it really is *after* the descriptor checks: measured,
        // `write(rdonlyFd, buf, 0)` is EBADF rather than 0.
        let readOnlyFd, readOnly = withReadOnlyFile linux

        UnixSystem.write readOnlyFd ImmutableArray<byte>.Empty readOnly
        |> shouldEqual (Ok (WriteAnswer.Failed UnixError.EBADF, readOnly))

    [<Test>]
    let ``a socket is refused by both halves of the write`` () : unit =
        // `write(2)` on a socket is an answer about connection state, which this
        // kernel does not model; EPIPE is the answer a reader of the Linux
        // measurement would reach for, and it is wrong on Darwin. Both calls
        // must refuse: the admission because a caller must not extract bytes for
        // a write that cannot happen, and `write` because a caller that skipped
        // the admission must not get a guess either.
        let socketId = SocketId 0L

        let socket : SocketDescription =
            {
                Domain = SocketDomain.InterNetwork
                Kind = SocketKind.Stream
                Protocol = SocketProtocol.Tcp
                Binding = None
                Phase = SocketPhase.Idle
                ReuseAddress = false
            }

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId linux.Process.FileDescriptors

        let system =
            { linux with
                Machine =
                    { linux.Machine with
                        Sockets = Map.ofList [ socketId, socket ]
                    }
                Process =
                    { linux.Process with
                        FileDescriptors = registry
                    }
            }

        let expected =
            Error (WriteRefusal.SocketConnectionState (socketId, SocketDomain.InterNetwork, SocketKind.Stream))

        UnixSystem.admitWrite fd UserBuffer.Mapped 5 system |> shouldEqual expected

        // Also at length zero, where a *file* would have been the no-op:
        // measured on both, `write(socket, buf, 0)` is the socket's own error.
        UnixSystem.admitWrite fd UserBuffer.Mapped 0 system |> shouldEqual expected

        UnixSystem.write fd (ImmutableArray.CreateRange [ 1uy ]) system
        |> shouldEqual expected

    [<Test>]
    let ``a screening platform answers a socket's bad address before the socket`` () : unit =
        // Measured on both. Linux screens the address before the object's own
        // write operation, so `write(socket, (void*)-1, n)` is EFAULT for every
        // `n` including 0 — the socket is never consulted, and refusing here
        // would abort a call a real kernel answers. Darwin screens nothing, so
        // the same call reaches the socket and earns a connection-state answer
        // this kernel cannot give.
        let socketId = SocketId 0L

        let socket : SocketDescription =
            {
                Domain = SocketDomain.InterNetwork
                Kind = SocketKind.Stream
                Protocol = SocketProtocol.Tcp
                Binding = None
                Phase = SocketPhase.Idle
                ReuseAddress = false
            }

        let withSocket (flavour : UnixSystem<int, string>) : int * UnixSystem<int, string> =
            let fd, registry =
                FileDescriptorRegistry.createSocket socketId flavour.Process.FileDescriptors

            fd,
            { flavour with
                Machine =
                    { flavour.Machine with
                        Sockets = Map.ofList [ socketId, socket ]
                    }
                Process =
                    { flavour.Process with
                        FileDescriptors = registry
                    }
            }

        let wild = UserBuffer.Unmapped System.UInt64.MaxValue
        let linuxFd, linuxSystem = withSocket linux
        let darwinFd, darwinSystem = withSocket darwin

        for count in [ 0 ; 5 ] do
            UnixSystem.admitWrite linuxFd wild count linuxSystem
            |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT)))

            UnixSystem.admitWrite darwinFd wild count darwinSystem
            |> shouldEqual (
                Error (WriteRefusal.SocketConnectionState (socketId, SocketDomain.InterNetwork, SocketKind.Stream))
            )

    [<Test>]
    let ``a defaulted byte array is rejected rather than written`` () : unit =
        let fd, system = withOpenFile linux

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                UnixSystem.write fd Unchecked.defaultof<ImmutableArray<byte>> system
                |> ignore<Result<WriteAnswer * UnixSystem<int, string>, WriteRefusal>>
            )

        exn.Message |> shouldContainText "ImmutableArray<byte>.Empty"

    // ------------------------------------------------------------------- pread

    let private preadBytes (result : Result<ReadAnswer, BufferRefusal>) : byte list =
        match result with
        | Ok (ReadAnswer.Completed bytes) -> List.ofSeq bytes
        | other -> failwith $"expected a completed pread, got %A{other}"

    let private failedWith (error : UnixError) : Result<ReadAnswer, BufferRefusal> = Ok (ReadAnswer.Failed error)

    /// A descriptor onto the seeded file, opened write-only: seekable, and not
    /// open for reading, which is the pair `pread`'s tie-break needs.
    let private withWriteOnlyFile (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        withFileOpenedAs FileAccessMode.WriteOnly system

    /// A system holding one directory, and the read-only descriptor onto it that
    /// `open` would give: the only access mode a directory can have.
    let private withOpenDirectory (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let inode, filesystem =
            match
                VirtualFileSystem.createDirectory
                    rootInode
                    (FileName.parseOrFail context "d")
                    (PermissionBits.parseOrFail context 0o755)
                    epoch
                    system.Machine.FileSystem
            with
            | Ok pair -> pair
            | Error error -> failwith $"could not seed the directory: %O{error}"

        let fd, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadOnly system.Process.FileDescriptors

        fd,
        { system with
            Machine =
                { system.Machine with
                    FileSystem = filesystem
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// A descriptor onto a socket event port.
    let private withSocketEventPort (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        fd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    [<Test>]
    let ``pread reads from the offset it is given, not from the description's`` () : unit =
        // The whole of what `pread` does differently, and the row a `pread`
        // implemented by delegating to `read` fails: the description's offset is
        // moved first, so reading from it would give different bytes.
        //
        // That the description's offset does not *move* needs no assertion: the
        // signature returns no system, so there is nothing that could have
        // moved it.
        for flavour in [ linux ; darwin ] do
            let fd, system = withOpenFile flavour

            let system =
                match UnixSystem.read fd UserBuffer.Mapped 2 system with
                | Ok (_, system) -> system
                | other -> failwith $"could not advance the offset: %A{other}"

            UnixSystem.pread fd UserBuffer.Mapped 2 0L system
            |> preadBytes
            |> shouldEqual [ 1uy ; 2uy ]

            // Short at the end of the file, from an offset the description never
            // held.
            UnixSystem.pread fd UserBuffer.Mapped 8 3L system
            |> preadBytes
            |> shouldEqual [ 4uy ; 5uy ]

    [<Test>]
    let ``a pread that moves nothing does not consult its buffer`` () : unit =
        // Two different ways to move nothing — the file is exhausted, and the
        // caller asked for nothing — and neither may touch the buffer: measured,
        // `pread(f, NULL, 5, atEof)` is 0 on both platforms rather than EFAULT.
        // A null pointer is an ordinary user address, so it passes the screen
        // and reaches the shortcut.
        let fd, system = withOpenFile linux

        for buffer in [ UserBuffer.Unmapped 0UL ; UserBuffer.Opaque ; UserBuffer.Mapped ] do
            UnixSystem.pread fd buffer 5 100L system |> preadBytes |> shouldEqual []
            UnixSystem.pread fd buffer 0 0L system |> preadBytes |> shouldEqual []

    [<Test>]
    let ``an addressless buffer is refused before pread's shortcuts on a screening platform`` () : unit =
        // The same asymmetry `read` has: Linux screens the address before the
        // operation, and an addressless buffer cannot be screened, so it is
        // refused even for a pread that would have moved nothing. Darwin screens
        // nothing, so the same call reaches the shortcut and answers 0.
        let fd, system = withOpenFile linux

        UnixSystem.pread fd UserBuffer.Addressless 0 0L system
        |> shouldEqual (Error BufferRefusal.AddresslessAtScreen)

        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.pread darwinFd UserBuffer.Addressless 0 0L darwinSystem
        |> preadBytes
        |> shouldEqual []

    [<Test>]
    let ``a pread transfer through a buffer with no bytes is refused, not faulted`` () : unit =
        let fd, system = withOpenFile linux
        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.pread fd UserBuffer.Opaque 5 0L system
        |> shouldEqual (Error BufferRefusal.OpaqueAtTransfer)

        UnixSystem.pread darwinFd UserBuffer.Addressless 5 0L darwinSystem
        |> shouldEqual (Error BufferRefusal.AddresslessAtTransfer)

        UnixSystem.pread fd UserBuffer.Addressless 5 0L system
        |> shouldEqual (Error BufferRefusal.AddresslessAtScreen)

    [<Test>]
    let ``pread's screen answers where its transfer would not have`` () : unit =
        // The pair that can tell "screened up front" from "faulted at the copy".
        // With bytes to move both orders fault and no input separates them; from
        // an offset past the end, Linux still faults because the screen precedes
        // the transfer window, and Darwin answers 0.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue
        let fd, system = withOpenFile linux
        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.pread fd wild 5 0L system
        |> shouldEqual (failedWith UnixError.EFAULT)

        UnixSystem.pread darwinFd wild 5 0L darwinSystem
        |> shouldEqual (failedWith UnixError.EFAULT)

        UnixSystem.pread fd wild 5 100L system
        |> shouldEqual (failedWith UnixError.EFAULT)

        UnixSystem.pread darwinFd wild 5 100L darwinSystem
        |> preadBytes
        |> shouldEqual []

    [<Test>]
    let ``a directory is EISDIR, behind the screen where there is one`` () : unit =
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue
        let fd, system = withOpenDirectory linux
        let darwinFd, darwinSystem = withOpenDirectory darwin

        UnixSystem.pread fd UserBuffer.Mapped 5 0L system
        |> shouldEqual (failedWith UnixError.EISDIR)

        UnixSystem.pread darwinFd UserBuffer.Mapped 5 0L darwinSystem
        |> shouldEqual (failedWith UnixError.EISDIR)

        // Measured: `pread(dir, (void*)-1, 5, 0)` is EFAULT under a screening
        // flavour and EISDIR under one that does not screen.
        UnixSystem.pread fd wild 5 0L system
        |> shouldEqual (failedWith UnixError.EFAULT)

        UnixSystem.pread darwinFd wild 5 0L darwinSystem
        |> shouldEqual (failedWith UnixError.EISDIR)

    [<Test>]
    let ``an unseekable descriptor is ESPIPE, and the flavours break the tie differently`` () : unit =
        // fd 0 is the read end of the pipe this kernel models standard input as;
        // fds 1 and 2 are write ends, and so fail two tests at once — neither
        // seekable nor open for reading. Measured:
        //
        //   descriptor                        Linux    Darwin
        //   pipe read end (unseekable)        ESPIPE   ESPIPE
        //   pipe write end (also unreadable)  ESPIPE   EBADF
        //   regular file O_WRONLY (seekable)  EBADF    EBADF
        UnixSystem.pread 0 UserBuffer.Mapped 5 0L linux
        |> shouldEqual (failedWith UnixError.ESPIPE)

        UnixSystem.pread 0 UserBuffer.Mapped 5 0L darwin
        |> shouldEqual (failedWith UnixError.ESPIPE)

        for fd in [ 1 ; 2 ] do
            UnixSystem.pread fd UserBuffer.Mapped 5 0L linux
            |> shouldEqual (failedWith UnixError.ESPIPE)

            UnixSystem.pread fd UserBuffer.Mapped 5 0L darwin
            |> shouldEqual (failedWith UnixError.EBADF)

        // The third row is the control: a *seekable* descriptor that is not open
        // for reading is EBADF on both, so the row above is about the tie rather
        // than about unreadability generally.
        for flavour in [ linux ; darwin ] do
            let fd, system = withWriteOnlyFile flavour

            UnixSystem.pread fd UserBuffer.Mapped 5 0L system
            |> shouldEqual (failedWith UnixError.EBADF)

    [<Test>]
    let ``a socket and a port are ESPIPE, ahead of the buffer screen`` () : unit =
        // Unseekable on both flavours, and measured to precede the screen:
        // `pread(port, (void*)-1, 8, 0)` is ESPIPE rather than EFAULT. Driven at
        // length 0 as well, because unseekability does not have the zero-length
        // shortcut a file's transfer window does.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue

        for flavour in [ linux ; darwin ] do
            let socketFd, socketSystem = withSocket flavour
            let portFd, portSystem = withSocketEventPort flavour

            for buffer in [ UserBuffer.Mapped ; wild ; UserBuffer.Opaque ; UserBuffer.Addressless ] do
                for count in [ 0 ; 5 ] do
                    UnixSystem.pread socketFd buffer count 0L socketSystem
                    |> shouldEqual (failedWith UnixError.ESPIPE)

                    UnixSystem.pread portFd buffer count 0L portSystem
                    |> shouldEqual (failedWith UnixError.ESPIPE)

        // This is where `pread` and `read` part company hardest, and why `pread`
        // needs no socket refusal: a socket's *read* operation is an answer about
        // connection state, which this kernel does not model, but its
        // seekability is not — every socket is unseekable whatever it is
        // connected to, so `pread` never reaches the read operation to ask.
        let fd, system = withSocket linux

        UnixSystem.read fd UserBuffer.Mapped 5 system |> shouldEqual socketRefused

    [<Test>]
    let ``pread of a descriptor that is not open is EBADF whatever the buffer`` () : unit =
        for buffer in
            [
                UserBuffer.Mapped
                UserBuffer.Unmapped 0UL
                UserBuffer.Opaque
                UserBuffer.Addressless
            ] do
            UnixSystem.pread 7 buffer 5 0L linux |> shouldEqual (failedWith UnixError.EBADF)

            UnixSystem.pread 7 buffer 5 0L darwin
            |> shouldEqual (failedWith UnixError.EBADF)

    [<Test>]
    let ``a negative offset is EINVAL, and the flavours answer it at different points`` () : unit =
        // The rows that pin the ordering. A *single*-fault input agrees on both
        // flavours, so only an input with two things wrong at once can tell them
        // apart:
        //
        //   input                        Linux    Darwin
        //   negative offset alone        EINVAL   EINVAL
        //   negative offset + bad fd     EINVAL   EBADF
        //   negative offset + pipe       EINVAL   ESPIPE
        //   negative offset + socket     EINVAL   ESPIPE
        //   negative offset + port       EINVAL   ESPIPE
        //   negative offset + O_WRONLY   EINVAL   EBADF
        //   negative offset + directory  EINVAL   EINVAL
        //
        // Linux validates the offset before it looks the descriptor up at all;
        // Darwin resolves the descriptor, its seekability and its access mode
        // first. The last row is the control: `EISDIR` follows the offset check
        // on both, so it does not move.
        for flavour in [ linux ; darwin ] do
            let fd, system = withOpenFile flavour

            UnixSystem.pread fd UserBuffer.Mapped 5 -1L system
            |> shouldEqual (failedWith UnixError.EINVAL)

        UnixSystem.pread 7 UserBuffer.Mapped 5 -1L linux
        |> shouldEqual (failedWith UnixError.EINVAL)

        UnixSystem.pread 7 UserBuffer.Mapped 5 -1L darwin
        |> shouldEqual (failedWith UnixError.EBADF)

        UnixSystem.pread 0 UserBuffer.Mapped 5 -1L linux
        |> shouldEqual (failedWith UnixError.EINVAL)

        UnixSystem.pread 0 UserBuffer.Mapped 5 -1L darwin
        |> shouldEqual (failedWith UnixError.ESPIPE)

        // The socket and the port are the rows that say the flag really is a
        // flag rather than a fact about pipes: their ESPIPE is unseekability,
        // exactly as the pipe's is, and Linux's offset check beats it too.
        let linuxSocket, linuxSocketSystem = withSocket linux

        UnixSystem.pread linuxSocket UserBuffer.Mapped 5 -1L linuxSocketSystem
        |> shouldEqual (failedWith UnixError.EINVAL)

        let darwinSocket, darwinSocketSystem = withSocket darwin

        UnixSystem.pread darwinSocket UserBuffer.Mapped 5 -1L darwinSocketSystem
        |> shouldEqual (failedWith UnixError.ESPIPE)

        let linuxPort, linuxPortSystem = withSocketEventPort linux

        UnixSystem.pread linuxPort UserBuffer.Mapped 5 -1L linuxPortSystem
        |> shouldEqual (failedWith UnixError.EINVAL)

        let darwinPort, darwinPortSystem = withSocketEventPort darwin

        UnixSystem.pread darwinPort UserBuffer.Mapped 5 -1L darwinPortSystem
        |> shouldEqual (failedWith UnixError.ESPIPE)

        let linuxWriteOnly, linuxSystem = withWriteOnlyFile linux

        UnixSystem.pread linuxWriteOnly UserBuffer.Mapped 5 -1L linuxSystem
        |> shouldEqual (failedWith UnixError.EINVAL)

        let darwinWriteOnly, darwinSystem = withWriteOnlyFile darwin

        UnixSystem.pread darwinWriteOnly UserBuffer.Mapped 5 -1L darwinSystem
        |> shouldEqual (failedWith UnixError.EBADF)

        for flavour in [ linux ; darwin ] do
            let fd, system = withOpenDirectory flavour

            UnixSystem.pread fd UserBuffer.Mapped 5 -1L system
            |> shouldEqual (failedWith UnixError.EINVAL)

    [<Test>]
    let ``the offset check precedes the buffer screen too`` () : unit =
        // Not implied by the rows above, which all pass a screenable buffer: the
        // screen sits between the descriptor steps and the operation, so an
        // unscreenable buffer with a negative offset says which of the two the
        // offset beats. It beats both, on both flavours — Linux because the
        // offset precedes everything, Darwin because it precedes the screen.
        let fd, system = withOpenFile linux
        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.pread fd UserBuffer.Addressless 5 -1L system
        |> shouldEqual (failedWith UnixError.EINVAL)

        UnixSystem.pread darwinFd UserBuffer.Addressless 5 -1L darwinSystem
        |> shouldEqual (failedWith UnixError.EINVAL)

    [<Test>]
    let ``a negative count is refused as pread's caller's own mistake`` () : unit =
        // Not an errno: a kernel never sees a negative count, and the two shims a
        // client might model do not agree on what to do with one — so answering
        // here would be inventing a kernel behaviour to cover a client's bug.
        let fd, system = withOpenFile linux

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                UnixSystem.pread fd UserBuffer.Mapped -1 0L system
                |> ignore<Result<ReadAnswer, BufferRefusal>>
            )

        exn.Message |> shouldContainText "UnixSystem.pread"

    // ------------------------------------------------------------------ pwrite

    let private pwriteAdmitted (result : Result<WriteAdmission, PWriteRefusal>) : WriteAdmission =
        match result with
        | Ok admission -> admission
        | Error refusal -> failwith $"expected an admission, got %A{refusal}"

    let private pwriteFailed (error : UnixError) : Result<WriteAdmission, PWriteRefusal> =
        Ok (WriteAdmission.Answered (WriteAnswer.Failed error))

    [<Test>]
    let ``pwrite writes at the offset it is given and leaves the description alone`` () : unit =
        // The whole of what `pwrite` does differently from `write`, and the row a
        // `pwrite` implemented by delegating to `write` fails: the description's
        // offset is moved first, so writing at it would land in the wrong place
        // and move it again.
        for flavour in [ linux ; darwin ] do
            let fd, system = withOpenFile flavour

            let system =
                match UnixSystem.read fd UserBuffer.Mapped 2 system with
                | Ok (_, system) -> system
                | other -> failwith $"could not advance the offset: %A{other}"

            match UnixSystem.pwrite fd (ImmutableArray.CreateRange [ 9uy ; 9uy ]) 3L system with
            | Ok (WriteAnswer.Completed written, after) ->
                written |> shouldEqual 2

                match FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors with
                | Some (OpenFileTarget.File (inode, offset)) ->
                    // Exactly where `read` left it, rather than at 5.
                    offset |> shouldEqual 2L

                    match VirtualFileSystem.tryGetContent inode after.Machine.FileSystem with
                    | Some (InodeContent.RegularFile (contents, _)) ->
                        List.ofSeq contents |> shouldEqual [ 1uy ; 2uy ; 3uy ; 9uy ; 9uy ]
                    | other -> failwith $"expected a regular file, got %O{other}"
                | other -> failwith $"expected a file descriptor, got %O{other}"
            | other -> failwith $"unexpected: %A{other}"

    [<Test>]
    let ``a pwrite past the end of the file extends it`` () : unit =
        let fd, system = withOpenFile linux

        match UnixSystem.pwrite fd (ImmutableArray.CreateRange [ 7uy ]) 7L system with
        | Ok (WriteAnswer.Completed written, after) ->
            written |> shouldEqual 1

            match FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, offset)) ->
                offset |> shouldEqual 0L

                match VirtualFileSystem.tryGetContent inode after.Machine.FileSystem with
                | Some (InodeContent.RegularFile (contents, _)) ->
                    List.ofSeq contents
                    |> shouldEqual [ 1uy ; 2uy ; 3uy ; 4uy ; 5uy ; 0uy ; 0uy ; 7uy ]
                | other -> failwith $"expected a regular file, got %O{other}"
            | other -> failwith $"expected a file descriptor, got %O{other}"
        | other -> failwith $"unexpected: %A{other}"

    [<Test>]
    let ``a negative offset beats every other fault, on both flavours`` () : unit =
        // Where `pwrite` differs from `pread`, and the reason it takes no
        // per-flavour flag. Measured, every second fault gives way to it on both:
        //
        //   negative offset with...    Linux    Darwin
        //   a bad descriptor           EINVAL   EINVAL
        //   a pipe's read end          EINVAL   EINVAL
        //   a pipe's write end         EINVAL   EINVAL
        //   a read-only file           EINVAL   EINVAL
        //   a directory                EINVAL   EINVAL
        //   a socket                   EINVAL   EINVAL
        //   a socket event port        EINVAL   EINVAL
        //   an unscreenable address    EINVAL   EINVAL
        //   a zero length              EINVAL   EINVAL
        //
        // `pread` answers the same shapes as EBADF, ESPIPE and EBADF on Darwin,
        // so a flag copied across from it would fail every row here.
        for flavour in [ linux ; darwin ] do
            let fd, system = withOpenFile flavour
            let readOnlyFd, readOnly = withReadOnlyFile flavour
            let dirFd, dirSystem = withOpenDirectory flavour
            let socketFd, socketSystem = withSocket flavour
            let portFd, portSystem = withSocketEventPort flavour

            for descriptor, holding in
                [
                    fd, system
                    7, system
                    0, system
                    1, system
                    readOnlyFd, readOnly
                    dirFd, dirSystem
                    socketFd, socketSystem
                    portFd, portSystem
                ] do
                UnixSystem.admitPWrite descriptor UserBuffer.Mapped 4 -1L holding
                |> shouldEqual (pwriteFailed UnixError.EINVAL)

            // And it beats the buffer screen and the no-op too, which the rows
            // above cannot say: both of those sit behind the descriptor steps,
            // so a buffer with no answer at all still earns EINVAL.
            UnixSystem.admitPWrite fd UserBuffer.Addressless 4 -1L system
            |> shouldEqual (pwriteFailed UnixError.EINVAL)

            UnixSystem.admitPWrite fd UserBuffer.Mapped 0 -1L system
            |> shouldEqual (pwriteFailed UnixError.EINVAL)

    [<Test>]
    let ``an unseekable descriptor is ESPIPE for pwrite, with the tie the other way up`` () : unit =
        // The mirror of `pread`'s tie: standard *input* is the one that fails two
        // tests at once, being neither seekable nor open for writing. Measured:
        //
        //   descriptor                        Linux    Darwin
        //   pipe write end (unseekable)       ESPIPE   ESPIPE
        //   pipe read end (also unwritable)   ESPIPE   EBADF
        //   regular file O_RDONLY (seekable)  EBADF    EBADF
        for fd in [ 1 ; 2 ] do
            UnixSystem.admitPWrite fd UserBuffer.Mapped 4 0L linux
            |> shouldEqual (pwriteFailed UnixError.ESPIPE)

            UnixSystem.admitPWrite fd UserBuffer.Mapped 4 0L darwin
            |> shouldEqual (pwriteFailed UnixError.ESPIPE)

        UnixSystem.admitPWrite 0 UserBuffer.Mapped 4 0L linux
        |> shouldEqual (pwriteFailed UnixError.ESPIPE)

        UnixSystem.admitPWrite 0 UserBuffer.Mapped 4 0L darwin
        |> shouldEqual (pwriteFailed UnixError.EBADF)

        // The control that says this is about the tie rather than about
        // unwritability generally.
        for flavour in [ linux ; darwin ] do
            let fd, system = withReadOnlyFile flavour

            UnixSystem.admitPWrite fd UserBuffer.Mapped 4 0L system
            |> shouldEqual (pwriteFailed UnixError.EBADF)

        // And seekability precedes the screen on both: measured,
        // `pwrite(pipeReadEnd, (void*)-1, 4, 0)` is ESPIPE on Linux and EBADF on
        // Darwin, not EFAULT.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue

        UnixSystem.admitPWrite 0 wild 4 0L linux
        |> shouldEqual (pwriteFailed UnixError.ESPIPE)

        UnixSystem.admitPWrite 0 wild 4 0L darwin
        |> shouldEqual (pwriteFailed UnixError.EBADF)

        UnixSystem.admitPWrite 1 wild 4 0L linux
        |> shouldEqual (pwriteFailed UnixError.ESPIPE)

    [<Test>]
    let ``a socket and a port are ESPIPE for pwrite, and so need no refusal`` () : unit =
        // Where `pwrite` and `write` part company hardest: `write` to a socket is
        // an answer about connection state, which this kernel cannot give, but
        // seekability is not — so `pwrite` never reaches the write operation to
        // ask, and `PWriteRefusal` has no socket case at all. Measured ESPIPE on
        // a TCP, a UDP and a Unix-domain socket alike, at every buffer and every
        // length.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue

        for flavour in [ linux ; darwin ] do
            let socketFd, socketSystem = withSocket flavour
            let portFd, portSystem = withSocketEventPort flavour

            for buffer in [ UserBuffer.Mapped ; wild ; UserBuffer.Opaque ; UserBuffer.Addressless ] do
                for count in [ 0 ; 4 ] do
                    UnixSystem.admitPWrite socketFd buffer count 0L socketSystem
                    |> shouldEqual (pwriteFailed UnixError.ESPIPE)

                    UnixSystem.admitPWrite portFd buffer count 0L portSystem
                    |> shouldEqual (pwriteFailed UnixError.ESPIPE)

        // The same socket refuses a `write`, and the port answers it with the
        // kind's own errno rather than with unseekability.
        let fd, system = withSocket linux

        UnixSystem.admitWrite fd UserBuffer.Mapped 4 system
        |> shouldEqual (
            Error (WriteRefusal.SocketConnectionState (socketZero, SocketDomain.InterNetwork, SocketKind.Stream))
        )

        let portFd, portSystem = withSocketEventPort linux

        UnixSystem.admitWrite portFd UserBuffer.Mapped 4 portSystem
        |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EINVAL)))

    [<Test>]
    let ``an unwritable file beats pwrite's screen and its no-op`` () : unit =
        // Measured: `pwrite(rdonlyFd, (void*)-1, 4, 0)` is EBADF rather than
        // EFAULT, and `pwrite(rdonlyFd, buf, 0, 0)` is EBADF rather than 0. Driven
        // at count 0 *and* with a buffer that would otherwise refuse, since either
        // alone could pass against a wrong order.
        for flavour in [ linux ; darwin ] do
            let fd, system = withReadOnlyFile flavour
            // A directory too, which is how it is unreachable as a *kind*: one
            // can only be opened for reading, so the access mode catches it and
            // `pwrite` never gets far enough to say EISDIR. Measured EBADF on
            // both, with a wild address as well as a good one.
            let dirFd, dirSystem = withOpenDirectory flavour

            for descriptor, holding in [ fd, system ; dirFd, dirSystem ] do
                for buffer in [ UserBuffer.Mapped ; UserBuffer.Addressless ; UserBuffer.Unmapped 0UL ] do
                    for count in [ 0 ; 4 ] do
                        UnixSystem.admitPWrite descriptor buffer count 0L holding
                        |> shouldEqual (pwriteFailed UnixError.EBADF)

    [<Test>]
    let ``pwrite's screen precedes its zero-length no-op, on the flavour that screens`` () : unit =
        // A wild address with nothing to write is EFAULT on Linux, which screens
        // before the operation, and 0 on Darwin, which screens nothing and reaches
        // the no-op.
        let wild = UserBuffer.Unmapped System.UInt64.MaxValue
        let fd, system = withOpenFile linux
        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.admitPWrite fd wild 0 0L system
        |> shouldEqual (pwriteFailed UnixError.EFAULT)

        UnixSystem.admitPWrite darwinFd wild 0 0L darwinSystem
        |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Completed 0)))

        // A *null* pointer passes the screen on both — it is an ordinary user
        // address — so it reaches the no-op at length 0 and faults at the copy
        // otherwise. Measured, `pwrite(f, NULL, 0, 0)` is 0 and
        // `pwrite(f, NULL, 4, 0)` is EFAULT, on both.
        for descriptor, holding in [ fd, system ; darwinFd, darwinSystem ] do
            UnixSystem.admitPWrite descriptor (UserBuffer.Unmapped 0UL) 0 0L holding
            |> shouldEqual (Ok (WriteAdmission.Answered (WriteAnswer.Completed 0)))

            UnixSystem.admitPWrite descriptor (UserBuffer.Unmapped 0UL) 4 0L holding
            |> shouldEqual (pwriteFailed UnixError.EFAULT)

    [<Test>]
    let ``a pwrite buffer with no bytes is refused at the copy, not faulted`` () : unit =
        let fd, system = withOpenFile linux

        UnixSystem.admitPWrite fd UserBuffer.Opaque 4 0L system
        |> shouldEqual (Error (PWriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

        UnixSystem.admitPWrite fd UserBuffer.Addressless 4 0L system
        |> shouldEqual (Error (PWriteRefusal.Buffer BufferRefusal.AddresslessAtScreen))

        let darwinFd, darwinSystem = withOpenFile darwin

        UnixSystem.admitPWrite darwinFd UserBuffer.Addressless 4 0L darwinSystem
        |> shouldEqual (Error (PWriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer))

    [<Test>]
    let ``a pwrite that reaches the copy asks for exactly what was requested`` () : unit =
        let fd, system = withOpenFile linux

        UnixSystem.admitPWrite fd UserBuffer.Mapped 3 9L system
        |> pwriteAdmitted
        |> shouldEqual (WriteAdmission.Transfer 3)

    [<Test>]
    let ``admitting a pwrite changes nothing`` () : unit =
        // Everything a write does before the copy is a question, so a caller may
        // ask and then decline. If this stopped holding, the two-call shape would
        // be handing out a half-performed syscall.
        let fd, system = withOpenFile linux

        for buffer, count in [ UserBuffer.Mapped, 3 ; UserBuffer.Unmapped 0UL, 3 ; UserBuffer.Mapped, 0 ] do
            UnixSystem.admitPWrite fd buffer count 0L system
            |> ignore<Result<WriteAdmission, PWriteRefusal>>

        UnixSystem.admitPWrite fd UserBuffer.Mapped 3 0L system
        |> pwriteAdmitted
        |> shouldEqual (WriteAdmission.Transfer 3)

    [<Test>]
    let ``pwrite answers the descriptor questions itself`` () : unit =
        // A caller that skipped the admission gets a kernel's answer rather than
        // an inconsistent one, which is what lets the second call take no buffer.
        let bytes = ImmutableArray.CreateRange [ 1uy ]

        UnixSystem.pwrite 7 bytes 0L linux
        |> shouldEqual (Ok (WriteAnswer.Failed UnixError.EBADF, linux))

        let fd, system = withReadOnlyFile linux

        UnixSystem.pwrite fd bytes 0L system
        |> shouldEqual (Ok (WriteAnswer.Failed UnixError.EBADF, system))

        // Including the negative offset, which precedes them all.
        let good, goodSystem = withOpenFile linux

        UnixSystem.pwrite good bytes -1L goodSystem
        |> shouldEqual (Ok (WriteAnswer.Failed UnixError.EINVAL, goodSystem))

    [<Test>]
    let ``an empty pwrite changes nothing, but only after the descriptor checks`` () : unit =
        let fd, system = withOpenFile linux

        // Including far past the end of the file, which does not extend it:
        // measured, `pwrite(f, buf, 0, 100000)` on a five-byte file leaves it five
        // bytes long.
        for offset in [ 0L ; 100000L ] do
            UnixSystem.pwrite fd ImmutableArray<byte>.Empty offset system
            |> shouldEqual (Ok (WriteAnswer.Completed 0, system))

        let readOnlyFd, readOnly = withReadOnlyFile linux

        UnixSystem.pwrite readOnlyFd ImmutableArray<byte>.Empty 0L readOnly
        |> shouldEqual (Ok (WriteAnswer.Failed UnixError.EBADF, readOnly))

    [<Test>]
    let ``a pwrite longer than the model can hold is refused, not answered`` () : unit =
        // A real filesystem answers this without difficulty, so an errno here
        // would be one no kernel produced. `pwrite` reaches it far more easily
        // than `write` does, its offset being an argument rather than a position
        // the file was walked to.
        let fd, system = withOpenFile linux
        let bytes = ImmutableArray.CreateRange [ 1uy ]

        match UnixSystem.pwrite fd bytes VirtualFileSystem.maxFileLength system with
        | Error (PWriteRefusal.ExceedsRepresentableLength (_, offset, count)) ->
            offset |> shouldEqual VirtualFileSystem.maxFileLength
            count |> shouldEqual 1

            PWriteRefusal.describe (PWriteRefusal.ExceedsRepresentableLength (rootInode, offset, count))
            |> shouldContainText "limit of the model"
        | other -> failwith $"expected a refusal, got %A{other}"

    [<Test>]
    let ``a defaulted byte array is rejected rather than pwritten`` () : unit =
        let fd, system = withOpenFile linux

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                UnixSystem.pwrite fd Unchecked.defaultof<ImmutableArray<byte>> 0L system
                |> ignore<Result<WriteAnswer * UnixSystem<int, string>, PWriteRefusal>>
            )

        exn.Message |> shouldContainText "ImmutableArray<byte>.Empty"

    [<Test>]
    let ``a negative count is refused as admitPWrite's caller's own mistake`` () : unit =
        let fd, system = withOpenFile linux

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                UnixSystem.admitPWrite fd UserBuffer.Mapped -1 0L system
                |> ignore<Result<WriteAdmission, PWriteRefusal>>
            )

        exn.Message |> shouldContainText "UnixSystem.admitPWrite"

    // ------------------------------------------------------------------- fstat

    let private reported (result : Result<FileStatusAnswer, FStatRefusal>) : FileStatus =
        match result with
        | Ok (FileStatusAnswer.Reported status) -> status
        | other -> failwith $"expected a reported status, got %A{other}"

    /// A system holding one symbolic link, and its inode. There is no descriptor
    /// onto one — `open` resolves a link — so only `statOf` can reach it.
    let private withSymlink (system : UnixSystem<int, string>) : InodeNumber * UnixSystem<int, string> =
        let inode, filesystem =
            match
                VirtualFileSystem.createSymlink
                    rootInode
                    (FileName.parseOrFail context "l")
                    epoch
                    (SymlinkTarget.parseOrFail context "abcdefg")
                    system.Machine.FileSystem
            with
            | Ok pair -> pair
            | Error error -> failwith $"could not seed the symlink: %O{error}"

        inode,
        { system with
            Machine =
                { system.Machine with
                    FileSystem = filesystem
                }
        }

    [<Test>]
    let ``fstat reports the fields a kernel knows about a regular file`` () : unit =
        let fd, system = withOpenFile linux
        let status = UnixSystem.fstat fd system |> reported

        // `S_IFREG ||| 0o644`, composed by the library so that the two bands are
        // assembled in one place rather than by every client.
        status.Mode |> shouldEqual 0o100644
        status.Size |> shouldEqual 5L
        // The literal, not `VirtualFileSystem.deviceId`: comparing the answer
        // against the constant it was read from is a row a zeroed constant would
        // pass. What matters to a guest is only that it is stable and non-zero —
        // a runtime compares `(st_dev, st_ino)` pairs and never interprets the
        // device — and zero is the one value that would be indistinguishable
        // from a field nobody wrote.
        status.DeviceId |> shouldEqual 0x1000001L

        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.File (inode, _)) -> status.Inode |> shouldEqual inode
        | other -> failwith $"expected a file descriptor, got %O{other}"

        // All three of the timestamps a freshly-created inode has, and `atime`
        // among them: nothing in this kernel moves it, so it is still the
        // creation time after a read.
        status.AccessTime |> shouldEqual epoch
        status.ModificationTime |> shouldEqual epoch
        status.StatusChangeTime |> shouldEqual epoch

    [<Test>]
    let ``a directory reports its type bits and the one invented field`` () : unit =
        let fd, system = withOpenDirectory linux
        let status = UnixSystem.fstat fd system |> reported

        status.Mode |> shouldEqual 0o40755
        // The only invented field in the whole record: this kernel has no block
        // allocator, so a directory has no natural size, and 4096 is what ext4
        // reports for a small one.
        status.Size |> shouldEqual 4096L

    [<Test>]
    let ``a symlink reports its target's byte length and the platform's own bits`` () : unit =
        // Reachable only through `statOf`: `open` resolves a link, so no
        // descriptor ever names one and `fstat` cannot see it.
        for flavour in [ linux ; darwin ] do
            let inode, system = withSymlink flavour

            let status =
                match UnixSystem.statOf inode system with
                | Some status -> status
                | None -> failwith "expected a status"

            // The target's length in bytes, which is what `readlink` would copy
            // out — not the length of anything the link points at.
            status.Size |> shouldEqual 7L

            let expected =
                SimulatedUnixPlatform.symlinkPermissions system.Machine.UnixPlatform
                |> PermissionBits.toInt

            status.Mode |> shouldEqual (0o120000 ||| expected)

        // And the two flavours disagree about those bits, so the row above is
        // reading the platform rather than a constant.
        (SimulatedUnixPlatform.symlinkPermissions SimulatedUnixPlatform.linuxX64
         |> PermissionBits.toInt)
        |> shouldNotEqual (
            SimulatedUnixPlatform.symlinkPermissions SimulatedUnixPlatform.macOsArm64
            |> PermissionBits.toInt
        )

    [<Test>]
    let ``a birth time is withheld on the flavour whose stat has no such field`` () : unit =
        // The inode knows when it was born either way; this decides whether a
        // guest is told. `None` rather than a zero, so that a client cannot read
        // "not reported" as "born at the epoch" — which, for an inode created at
        // the epoch, is a distinction no zeroed field could carry.
        let linuxFd, linuxSystem = withOpenFile linux
        (UnixSystem.fstat linuxFd linuxSystem |> reported).BirthTime |> shouldEqual None

        let darwinFd, darwinSystem = withOpenFile darwin

        (UnixSystem.fstat darwinFd darwinSystem |> reported).BirthTime
        |> shouldEqual (Some epoch)

    [<Test>]
    let ``ownership is the calling process's, not the inode's`` () : unit =
        // This kernel stores no per-inode ownership, so `fstat` reports whoever
        // is asking. Asserted by *changing* the asker: a hardcoded 1000/1000
        // would pass a row that only read the default.
        let fd, system = withOpenFile linux

        let system =
            { system with
                Process = UnixProcessState.withUserAndGroupId 41u 43u system.Process
            }

        let status = UnixSystem.fstat fd system |> reported
        status.UserId |> shouldEqual 41u
        status.GroupId |> shouldEqual 43u

    [<Test>]
    let ``fstat of a descriptor that is not open is EBADF`` () : unit =
        UnixSystem.fstat 7 linux
        |> shouldEqual (Ok (FileStatusAnswer.Failed UnixError.EBADF))

    [<Test>]
    let ``statOf an inode the filesystem does not hold is None`` () : unit =
        // `None` rather than a crash, because `statOf` is public and a caller
        // that got its inode from somewhere other than a live descriptor cannot
        // be assumed to have checked. `fstat` is the caller that *can* assume it,
        // and it crashes on `None` for that reason.
        UnixSystem.statOf (InodeNumber 99L) linux |> shouldEqual None

    [<Test>]
    let ``a descriptor with no inode is refused, and the refusal names which kind`` () : unit =
        // Three shapes of one refusal, distinguished because their measurements
        // are different: a real kernel answers all three, and this kernel has no
        // inode to answer them from.
        UnixSystem.fstat 0 linux
        |> shouldEqual (Error (FStatRefusal.StandardStream FileDescriptorRole.StandardInput))

        UnixSystem.fstat 1 linux
        |> shouldEqual (Error (FStatRefusal.StandardStream FileDescriptorRole.StandardOutput))

        let portFd, portSystem = withSocketEventPort linux

        UnixSystem.fstat portFd portSystem
        |> shouldEqual (Error FStatRefusal.SocketEventPort)

        let socketFd, socketSystem = withSocket linux

        UnixSystem.fstat socketFd socketSystem
        |> shouldEqual (Error (FStatRefusal.Socket socketZero))

    [<Test>]
    let ``each fstat refusal describes its own measurement`` () : unit =
        // One `describe` per shape rather than one for the genus: the reason a
        // pipe cannot be reported is not the reason a socket cannot, and a
        // client rendering either must not be handed the other's evidence.
        FStatRefusal.describe (FStatRefusal.StandardStream FileDescriptorRole.StandardInput)
        |> shouldContainText "pipe"

        FStatRefusal.describe FStatRefusal.SocketEventPort
        |> shouldContainText "anonymous kernel object"

        FStatRefusal.describe (FStatRefusal.Socket socketZero)
        |> shouldContainText "contention key"

        // And none of them names PawPrint: which client is asking, and what it
        // would have to build, is the client's half of the message.
        for refusal in
            [
                FStatRefusal.StandardStream FileDescriptorRole.StandardInput
                FStatRefusal.SocketEventPort
                FStatRefusal.Socket socketZero
            ] do
            FStatRefusal.describe refusal |> shouldNotContainText "SystemNative"

    // ------------------------------------------------------------ stat / lstat

    let private statPath (candidate : string) : UnixPath = UnixPath.parseOrFail context candidate

    /// A system holding `/d/inner`, a file `/d/inner/t`, and a symbolic link
    /// `/l -> /d/inner/t`. Enough to tell `stat` from `lstat`, and to move the
    /// current directory somewhere that is not the root.
    ///
    /// `/d/inner`'s own permissions are the caller's, because whether that
    /// directory is searchable is what one of the rows below varies.
    let private withTreeUnder
        (innerPermissions : PermissionBits)
        (system : UnixSystem<int, string>)
        : InodeNumber * InodeNumber * InodeNumber * UnixSystem<int, string>
        =
        let orFail (name : string) (result : Result<InodeNumber * VirtualFileSystem, UnixError>) =
            match result with
            | Ok pair -> pair
            | Error error -> failwith $"could not seed %s{name}: %O{error}"

        let dirPermissions = PermissionBits.parseOrFail context 0o755

        let d, vfs =
            VirtualFileSystem.createDirectory
                rootInode
                (FileName.parseOrFail context "d")
                dirPermissions
                epoch
                system.Machine.FileSystem
            |> orFail "/d"

        let inner, vfs =
            VirtualFileSystem.createDirectory d (FileName.parseOrFail context "inner") innerPermissions epoch vfs
            |> orFail "/d/inner"

        let target, vfs =
            VirtualFileSystem.createFile
                inner
                (FileName.parseOrFail context "t")
                (PermissionBits.parseOrFail context 0o600)
                epoch
                (ImmutableArray.CreateRange [ 1uy ; 2uy ; 3uy ])
                vfs
            |> orFail "/d/inner/t"

        let link, vfs =
            VirtualFileSystem.createSymlink
                rootInode
                (FileName.parseOrFail context "l")
                epoch
                (SymlinkTarget.parseOrFail context "/d/inner/t")
                vfs
            |> orFail "/l"

        // A link to a name nothing holds. Its whole purpose is to tell "the
        // final component is not dereferenced" from "it is": a link to an
        // *existing* file cannot, both readings landing on a name that exists.
        let _, vfs =
            VirtualFileSystem.createSymlink
                rootInode
                (FileName.parseOrFail context "dangling")
                epoch
                (SymlinkTarget.parseOrFail context "/d/inner/gone")
                vfs
            |> orFail "/dangling"

        inner,
        target,
        link,
        { system with
            Machine =
                { system.Machine with
                    FileSystem = vfs
                }
        }

    /// The tree with every directory searchable, which is what all but one row
    /// wants.
    let private withTree
        (system : UnixSystem<int, string>)
        : InodeNumber * InodeNumber * InodeNumber * UnixSystem<int, string>
        =
        withTreeUnder (PermissionBits.parseOrFail context 0o755) system

    [<Test>]
    let ``stat reports exactly what fstat reports for the same inode`` () : unit =
        // Two entry points onto one answer. If they could disagree, a guest that
        // opened a file and stat'd its path would see two different files.
        let _, target, _, system = withTree linux

        let fd, registry =
            FileDescriptorRegistry.openFile target FileAccessMode.ReadOnly system.Process.FileDescriptors

        let withDescriptor =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        UnixSystem.stat SymlinkPolicy.Follow (statPath "/d/inner/t") system
        |> shouldEqual (UnixSystem.fstat fd withDescriptor |> reported |> FileStatusAnswer.Reported)

    [<Test>]
    let ``following a link is the whole difference between stat and lstat`` () : unit =
        // The one thing the two syscalls do differently, and the row that fails
        // if a policy is dropped on the way through: the link is 10 bytes of
        // target text with the platform's own permission bits, and the file it
        // names is 3 bytes with 0o600.
        for flavour in [ linux ; darwin ] do
            let _, target, link, system = withTree flavour

            match UnixSystem.stat SymlinkPolicy.Follow (statPath "/l") system with
            | FileStatusAnswer.Reported status ->
                status.Inode |> shouldEqual target
                status.Size |> shouldEqual 3L
                status.Mode |> shouldEqual 0o100600
            | other -> failwith $"expected a status, got %A{other}"

            match UnixSystem.stat SymlinkPolicy.NoFollowFinal (statPath "/l") system with
            | FileStatusAnswer.Reported status ->
                status.Inode |> shouldEqual link
                // `/d/inner/t` is ten bytes, which is what `readlink` would copy.
                status.Size |> shouldEqual 10L
            | other -> failwith $"expected a status, got %A{other}"

    [<Test>]
    let ``a name nothing holds is ENOENT rather than a refusal`` () : unit =
        // `stat` has no refusal at all: every inode a path can resolve to is one
        // this filesystem holds, so the three descriptors `fstat` refuses for are
        // unreachable from a path.
        let _, _, _, system = withTree linux

        UnixSystem.stat SymlinkPolicy.Follow (statPath "/d/inner/nope") system
        |> shouldEqual (FileStatusAnswer.Failed UnixError.ENOENT)

    [<Test>]
    let ``a relative path starts at the current directory's inode`` () : unit =
        // The field this reads is `CurrentDirectoryInode`, not the recorded
        // current directory *path* — a real process reaches its cwd through a
        // reference it already holds. Asserted by moving the inode and leaving
        // the path alone: a resolver that re-walked the path would answer the
        // same for both systems below.
        let inner, target, _, system = withTree linux

        let at (directory : InodeNumber) : UnixSystem<int, string> =
            { system with
                Process =
                    { system.Process with
                        CurrentDirectoryInode = directory
                    }
            }

        UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "t") (at inner)
        |> shouldEqual (Ok target)

        UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "t") (at rootInode)
        |> shouldEqual (Error UnixError.ENOENT)

        // ...and a rooted path ignores it, which is what says the branch above is
        // a branch. Asked with the current directory at `inner` rather than at
        // the root: with it at the root the two arms agree, so that row could
        // not tell "rooted paths start at the root" from "every path starts at
        // the current directory".
        UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "/d/inner/t") (at inner)
        |> shouldEqual (Ok target)

    [<Test>]
    let ``a trailing separator demands a directory`` () : unit =
        // `resolvePath` fixes `TrailingSeparatorPolicy.Demand`, which is the
        // non-creating lookup's rule; a caller that needs the other axis uses
        // `resolvePathFull`. Without the demand reaching the walk, a trailing
        // separator on a regular file would resolve.
        let inner, target, _, system = withTree linux

        UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "/d/inner/t/") system
        |> shouldEqual (Error UnixError.ENOTDIR)

        UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "/d/inner/") system
        |> shouldEqual (Ok inner)

        UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "/d/inner/t") system
        |> shouldEqual (Ok target)

    [<Test>]
    let ``an unsearchable directory on the way is EACCES`` () : unit =
        // The privilege the walk uses is the calling process's, read from the
        // system rather than passed in — so dropping root changes the answer.
        let _, _, _, system = withTreeUnder (PermissionBits.parseOrFail context 0o600) linux

        UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "/d/inner/t") system
        |> shouldEqual (Error UnixError.EACCES)

        // uid 0 is exempt, which is what says the rule is being read from the
        // process rather than hardcoded.
        let asRoot =
            { system with
                Process = UnixProcessState.withUserAndGroupId 0u 0u system.Process
            }

        match UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "/d/inner/t") asRoot with
        | Ok _ -> ()
        | Error error -> failwith $"root should have been exempt, got %O{error}"

    // ------------------------------------------- mkdir / unlink / rmdir

    let private completed (answer : SyscallAnswer * UnixSystem<int, string>) : UnixSystem<int, string> =
        match answer with
        | SyscallAnswer.Completed 0L, system -> system
        | other -> failwith $"expected a success, got %A{other}"

    let private failedAs (error : UnixError) (answer : SyscallAnswer * UnixSystem<int, string>) : unit =
        match answer with
        | SyscallAnswer.Failed actual, _ -> actual |> shouldEqual error
        | other -> failwith $"expected %O{error}, got %A{other}"

    [<Test>]
    let ``mkdir binds a directory the umask has had its say over`` () : unit =
        // The mode is raw — the shim passes it straight through — so what the
        // directory actually gets is the kernel's business. Driven with a
        // distinctive umask rather than the default, since a `mkdir` that ignored
        // the umask entirely would pass a row that used 0.
        let system =
            { linux with
                Process =
                    { linux.Process with
                        Umask = PermissionBits.parseOrFail context 0o027
                    }
            }

        let after = UnixSystem.mkdir (statPath "/d") 0o777 system |> completed

        match UnixSystem.stat SymlinkPolicy.Follow (statPath "/d") after with
        | FileStatusAnswer.Reported status -> status.Mode |> shouldEqual 0o40750
        | other -> failwith $"expected a status, got %A{other}"

    [<Test>]
    let ``mkdir over a name something already holds is EEXIST`` () : unit =
        let _, _, _, system = withTree linux

        UnixSystem.mkdir (statPath "/d") 0o777 system |> failedAs UnixError.EEXIST

        // Including a symbolic link, which `mkdir` never dereferences — and a
        // *dangling* one is the case that says so: following it would find a free
        // name and bind a directory at the target, where the measured answer is
        // EEXIST at the link itself.
        UnixSystem.mkdir (statPath "/l") 0o777 system |> failedAs UnixError.EEXIST

        UnixSystem.mkdir (statPath "/dangling") 0o777 system
        |> failedAs UnixError.EEXIST

        UnixSystem.stat SymlinkPolicy.NoFollowFinal (statPath "/d/inner/gone") system
        |> shouldEqual (FileStatusAnswer.Failed UnixError.ENOENT)

    [<Test>]
    let ``unlink removes the name, and the inode with it when nothing holds it`` () : unit =
        let _, target, _, system = withTree linux

        let after = UnixSystem.unlink (statPath "/d/inner/t") system |> completed

        UnixSystem.stat SymlinkPolicy.Follow (statPath "/d/inner/t") after
        |> shouldEqual (FileStatusAnswer.Failed UnixError.ENOENT)

        // The inode is gone too, which is the part `unlink` adds over the
        // filesystem's own unbind.
        UnixSystem.statOf target after |> shouldEqual None

    [<Test>]
    let ``unlink of a file a descriptor holds leaves it readable through that descriptor`` () : unit =
        // The rule `forgetIfUnheld` exists for, and the one an unlink that simply
        // freed the inode would break: a real `unlink` of an open file leaves it
        // readable until the last descriptor closes.
        let _, target, _, system = withTree linux

        let fd, registry =
            FileDescriptorRegistry.openFile target FileAccessMode.ReadOnly system.Process.FileDescriptors

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        let after = UnixSystem.unlink (statPath "/d/inner/t") system |> completed

        // The name has gone...
        UnixSystem.stat SymlinkPolicy.Follow (statPath "/d/inner/t") after
        |> shouldEqual (FileStatusAnswer.Failed UnixError.ENOENT)

        // ...and the inode has not.
        UnixSystem.statOf target after |> shouldNotEqual None

        match UnixSystem.read fd UserBuffer.Mapped 8 after with
        | Ok (ReadAnswer.Completed bytes, _) -> List.ofSeq bytes |> shouldEqual [ 1uy ; 2uy ; 3uy ]
        | other -> failwith $"expected the contents, got %A{other}"

        // And closing that last descriptor is what finally reaps it.
        match UnixSystem.close fd after with
        | Ok (SyscallAnswer.Completed _, closed) -> UnixSystem.statOf target closed |> shouldEqual None
        | other -> failwith $"expected a close, got %A{other}"

    [<Test>]
    let ``rmdir refuses a directory that still holds something`` () : unit =
        let _, _, _, system = withTree linux

        UnixSystem.rmdir (statPath "/d") system |> failedAs UnixError.ENOTEMPTY

        // The leaf is empty, so it goes; and then its parent is empty too.
        let after =
            UnixSystem.unlink (statPath "/d/inner/t") system
            |> completed
            |> fun system -> UnixSystem.rmdir (statPath "/d/inner") system |> completed
            |> fun system -> UnixSystem.rmdir (statPath "/d") system |> completed

        UnixSystem.stat SymlinkPolicy.Follow (statPath "/d") after
        |> shouldEqual (FileStatusAnswer.Failed UnixError.ENOENT)

    [<Test>]
    let ``a removed directory's ctime is the flavour's own answer`` () : unit =
        // The one field `rmdir` moves that `unlink` does not decide the same way:
        // measured through a descriptor held across the call, Linux moves the
        // removed directory's `ctime` and Darwin leaves it. Only a *held*
        // descriptor can see it — an unheld inode is reaped and there is nothing
        // left to ask.
        for flavour, expected in [ linux, UnixTimestamp.ofMillisecondsSinceEpoch 5000L ; darwin, epoch ] do
            let fd, system = withOpenDirectory flavour

            let inode =
                match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                | Some (OpenFileTarget.File (inode, _)) -> inode
                | other -> failwith $"expected a directory descriptor, got %O{other}"

            // The clock has to have moved, or the two answers coincide.
            let system =
                { system with
                    Machine =
                        { system.Machine with
                            WallClockEpochMs = 5000L
                        }
                }

            let after = UnixSystem.rmdir (statPath "/d") system |> completed

            match UnixSystem.statOf inode after with
            | Some status -> status.StatusChangeTime |> shouldEqual expected
            | None -> failwith "the held descriptor should have kept the inode alive"

    [<Test>]
    let ``unlink and rmdir do not do each other's job`` () : unit =
        // Each refuses the other's target, which is what says the two entry
        // points are not one syscall with a flag.
        let _, _, _, system = withTree linux

        match UnixSystem.unlink (statPath "/d/inner") system with
        | SyscallAnswer.Failed _, _ -> ()
        | other -> failwith $"unlink should not remove a directory, got %A{other}"

        UnixSystem.rmdir (statPath "/d/inner/t") system |> failedAs UnixError.ENOTDIR

    [<Test>]
    let ``the three path syscalls through step agree with the primitives`` () : unit =
        // As for `close`: the dispatcher is sugar, and a client that logs and
        // replays through `step` must compute the same thing as one that calls
        // the primitive.
        let _, _, _, system = withTree linux

        for call, expected in
            [
                // A mode the default umask does *not* reduce to the same thing
                // as 0o777: with umask 0o022 both 0o755 and 0o777 become 0o755,
                // so a dispatcher that dropped the mode would agree.
                Syscall.MkDir (statPath "/new", 0o700), UnixSystem.mkdir (statPath "/new") 0o700 system
                Syscall.Unlink (statPath "/d/inner/t"), UnixSystem.unlink (statPath "/d/inner/t") system
                Syscall.RmDir (statPath "/d"), UnixSystem.rmdir (statPath "/d") system
            ] do
            UnixSystem.step call system |> shouldEqual (Ok expected)

    [<Test>]
    let ``close of a descriptor that is not open is EBADF and changes nothing`` () : unit =
        UnixSystem.close 7 linux
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EBADF, linux))

    [<Test>]
    let ``close drops the descriptor and answers zero`` () : unit =
        let fd, system = withOpenFile linux

        match UnixSystem.close fd system with
        | Ok (SyscallAnswer.Completed answer, after) ->
            // Zero rather than the descriptor number: `close(2)` reports success,
            // not what it closed.
            answer |> shouldEqual 0L

            FileDescriptorRegistry.tryFind fd after.Process.FileDescriptors
            |> shouldEqual None
        | other -> failwith $"unexpected: %O{other}"

    [<Test>]
    let ``close through step agrees with the primitive`` () : unit =
        // As for `geteuid` above: the dispatcher is sugar, and a client that logs
        // and replays through `step` must compute the same thing as one that
        // calls `close` directly.
        let fd, system = withOpenFile linux

        UnixSystem.step (Syscall.Close fd) system
        |> shouldEqual (UnixSystem.close fd system |> Result.mapError SyscallRefusal.Close)

    [<Test>]
    let ``close reaps the inode whose last name had already gone`` () : unit =
        // The rule `close` adds over `FileDescriptorRegistry.close`: the
        // descriptor was the last reference, so the inode goes with it. The two
        // `forgetIfUnheld` rows below are the same rule stated on its own; this
        // is the one that says `close` actually applies it.
        let fd, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file descriptor, got %O{other}"

        let unnamed =
            match
                VirtualFileSystem.unbind
                    UnbindTargetEffect.LostALink
                    rootInode
                    (FileName.parseOrFail context "f")
                    epoch
                    system.Machine.FileSystem
            with
            | Ok (_, filesystem) ->
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                }
            | Error error -> failwith $"could not unlink the file: %O{error}"

        // Still there while the descriptor holds it, which is what makes `read`
        // on an unlinked file work.
        (VirtualFileSystem.tryGet inode unnamed.Machine.FileSystem).IsSome
        |> shouldEqual true

        match UnixSystem.close fd unnamed with
        | Ok (SyscallAnswer.Completed _, after) ->
            (VirtualFileSystem.tryGet inode after.Machine.FileSystem).IsSome
            |> shouldEqual false
        | other -> failwith $"unexpected: %O{other}"

    [<Test>]
    let ``an unnamed inode a descriptor still holds is not freed`` () : unit =
        // The rule that makes `read` on an unlinked file keep working: the last
        // *name* has gone, but an open file description is still a reference.
        let fd, system = withOpenFile linux

        let unnamed =
            match
                VirtualFileSystem.unbind
                    UnbindTargetEffect.LostALink
                    rootInode
                    (FileName.parseOrFail context "f")
                    epoch
                    system.Machine.FileSystem
            with
            | Ok (_, filesystem) ->
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                }
            | Error error -> failwith $"could not unlink the file: %O{error}"

        let inode =
            match FileDescriptorRegistry.tryFindTarget fd unnamed.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file descriptor, got %O{other}"

        VirtualFileSystem.bindingCount inode unnamed.Machine.FileSystem |> shouldEqual 0
        UnixSystem.pinnedInodes unnamed |> Set.contains inode |> shouldEqual true

        let attempted = UnixSystem.forgetIfUnheld inode unnamed

        (VirtualFileSystem.tryGet inode attempted.Machine.FileSystem).IsSome
        |> shouldEqual true

    [<Test>]
    let ``an unnamed inode nothing holds is freed`` () : unit =
        // The other half of the same rule, and the pair is what makes the row
        // above load-bearing: without it, a `forgetIfUnheld` that never freed
        // anything would pass.
        let fd, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file descriptor, got %O{other}"

        let unnamed =
            match
                VirtualFileSystem.unbind
                    UnbindTargetEffect.LostALink
                    rootInode
                    (FileName.parseOrFail context "f")
                    epoch
                    system.Machine.FileSystem
            with
            | Ok (_, filesystem) -> filesystem
            | Error error -> failwith $"could not unlink the file: %O{error}"

        let released =
            match FileDescriptorRegistry.close fd system.Process.FileDescriptors with
            | Ok (registry, _) -> registry
            | Error error -> failwith $"could not close the descriptor: %O{error}"

        let orphaned =
            {
                Machine =
                    { system.Machine with
                        FileSystem = unnamed
                    }
                Process =
                    { system.Process with
                        FileDescriptors = released
                    }
                Tasks = system.Tasks
            }

        UnixSystem.pinnedInodes orphaned |> Set.contains inode |> shouldEqual false

        let reaped = UnixSystem.forgetIfUnheld inode orphaned

        (VirtualFileSystem.tryGet inode reaped.Machine.FileSystem).IsSome
        |> shouldEqual false

    [<Test>]
    let ``geteuid is total, and its type says so`` () : unit =
        // Not a `SyscallAnswer`: `geteuid(2)` cannot fail, so a shape that
        // admitted `Failed` would make an unreachable state representable. The
        // per-syscall function is the primitive for exactly this reason.
        UnixSystem.effectiveUserId linux |> shouldEqual 1000u

    [<Test>]
    let ``step agrees with the primitive it dispatches to`` () : unit =
        // The dispatcher is sugar. If the two ever disagree, the surface a client
        // logs and replays through is not the surface it computes through.
        match UnixSystem.step Syscall.GetEffectiveUserId linux with
        | Ok (SyscallAnswer.Completed answer, after) ->
            answer |> shouldEqual (int64 (UnixSystem.effectiveUserId linux))
            after |> shouldEqual linux
        | other -> failwith $"unexpected: %O{other}"

    [<Test>]
    let ``dup of a closed descriptor is EBADF and changes nothing`` () : unit =
        let answer, after = UnixSystem.dup 7 linux
        answer |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF)
        after |> shouldEqual linux

    [<Test>]
    let ``dup shares the description and takes the lowest free descriptor`` () : unit =
        let fd, system = withOpenFile linux
        let answer, after = UnixSystem.dup fd system

        let duplicated =
            match answer with
            | SyscallAnswer.Completed newFd -> int newFd
            | other -> failwith $"expected a descriptor, got %O{other}"

        duplicated |> shouldNotEqual fd

        // The same open file description, which is what makes the offset shared.
        FileDescriptorRegistry.tryFindTarget duplicated after.Process.FileDescriptors
        |> shouldEqual (FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors)

    [<Test>]
    let ``lseek on a closed descriptor is EBADF whatever the whence`` () : unit =
        // Measured ahead of everything on both platforms, including the whences
        // that are not portable at all.
        for system in [ linux ; darwin ] do
            for whence in [ 0 ; 1 ; 2 ; 3 ; 4 ; 99 ] do
                UnixSystem.lseek 7 0L whence system
                |> answered
                |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF)

    [<Test>]
    let ``an invalid whence and an unseekable descriptor are ordered by flavour`` () : unit =
        // The divergence this fixture exists for, and one a guest cannot reach:
        // CI's guests run Linux, so the Darwin row below has no other home.
        // Linux validates `whence` before asking whether the object is seekable;
        // Darwin the other way round. Standard input is a pipe, so it is
        // unseekable on both.
        let unseekableFd = 0

        UnixSystem.lseek unseekableFd 0L 99 linux
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

        UnixSystem.lseek unseekableFd 0L 99 darwin
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.ESPIPE)

    [<Test>]
    let ``a seek that lands moves the offset, and one that does not leaves it`` () : unit =
        let fd, system = withOpenFile linux

        let after =
            match UnixSystem.lseek fd 3L 0 system with
            | Ok (SyscallAnswer.Completed 3L, after) -> after
            | other -> failwith $"expected to land at 3, got %O{other}"

        // A failed seek does not move the description — measured.
        match UnixSystem.lseek fd -1L 0 after with
        | Ok (SyscallAnswer.Failed UnixError.EINVAL, unmoved) ->
            match UnixSystem.lseek fd 0L 1 unmoved with
            | Ok (SyscallAnswer.Completed position, _) -> position |> shouldEqual 3L
            | other -> failwith $"expected the offset to be where it was, got %O{other}"
        | other -> failwith $"expected EINVAL, got %O{other}"

    [<Test>]
    let ``seeking past the end of an int64 diverges in errno, not in ordering`` () : unit =
        // The one place the *errno* differs rather than the order: measured on a
        // tmpfs-backed file so the filesystem is held constant.
        for system, expected in [ linux, UnixError.EINVAL ; darwin, UnixError.EOVERFLOW ] do
            let fd, seeded = withOpenFile system

            UnixSystem.lseek fd (System.Int64.MaxValue - 4L) 2 seeded
            |> answered
            |> shouldEqual (SyscallAnswer.Failed expected)

    [<Test>]
    let ``the sparseness whences are refused, named for the simulated platform`` () : unit =
        // Both refusals, on both flavours: the raw number names a different
        // operation on each, which is half the reason there is no answer to give.
        for system, three, four in
            [
                linux, SeekExtension.SeekData, SeekExtension.SeekHole
                darwin, SeekExtension.SeekHole, SeekExtension.SeekData
            ] do
            let fd, seeded = withOpenFile system

            UnixSystem.lseek fd 0L 3 seeded
            |> shouldEqual (Error (LSeekRefusal.Sparseness (3, three)))

            UnixSystem.lseek fd 0L 4 seeded
            |> shouldEqual (Error (LSeekRefusal.Sparseness (4, four)))

    [<Test>]
    let ``a refusal describes what this kernel measured, and nothing about the caller`` () : unit =
        // The library's half of a diagnostic. It says why no answer exists; it
        // cannot say which entry point asked, because it never saw one.
        let described =
            LSeekRefusal.describe (LSeekRefusal.Sparseness (3, SeekExtension.SeekData))

        described |> shouldContainText "sparseness"
        described |> shouldContainText "SEEK_DATA"
        described |> shouldNotContainText "SystemNative"

    [<Test>]
    let ``step reports a refusal as an error carrying no system`` () : unit =
        // The shape's whole point: a refused call cannot hand back a state, so a
        // client that catches one still holds the system it passed in.
        let fd, seeded = withOpenFile linux

        UnixSystem.step (Syscall.LSeek (fd, 0L, 3)) seeded
        |> shouldEqual (Error (SyscallRefusal.LSeek (LSeekRefusal.Sparseness (3, SeekExtension.SeekData))))

    [<Test>]
    let ``ftruncate validates the length before the descriptor`` () : unit =
        // Measured on both: the same unknown fd is EBADF at length 0 and EINVAL
        // at length -1, so the length really is checked first rather than the two
        // faults merely sharing an errno. A row per fault alone could not tell.
        UnixSystem.ftruncate 99 0L linux
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF)

        UnixSystem.ftruncate 99 -1L linux
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

    [<Test>]
    let ``ftruncate of a read-only descriptor is EINVAL, not EBADF`` () : unit =
        // `ftruncate(2)` differs from `write(2)` here, measured on both. It is
        // also what makes a directory answer EINVAL without a type check, since
        // a directory can only ever be opened read-only.
        let fd, system = withOpenDirectory linux

        UnixSystem.ftruncate fd 0L system
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

    [<Test>]
    let ``ftruncate shortens the file and stamps it`` () : unit =
        let fd, system = withOpenFile linux

        let after =
            match UnixSystem.ftruncate fd 2L system with
            | Ok (SyscallAnswer.Completed 0L, after) -> after
            | other -> failwith $"expected success, got %O{other}"

        // Seeking to the end is how the length is read back without a `stat`.
        UnixSystem.lseek fd 0L 2 after
        |> answered
        |> shouldEqual (SyscallAnswer.Completed 2L)

    [<Test>]
    let ``an unlockable operation is EINVAL on Linux and refused on Darwin`` () : unit =
        // Linux validates strictly: exactly one of SH/EX/UN, optionally with NB.
        // Darwin is laxer *and* answers differently per input, which is why the
        // whole of it is refused rather than one row of it modelled.
        let shAndEx = 1 ||| 2

        UnixSystem.flock 0 shAndEx linux
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

        UnixSystem.flock 0 shAndEx darwin
        |> shouldEqual (Error (FLockRefusal.DarwinMalformedOperation shAndEx))

    [<Test>]
    let ``flock on a pipe is Linux's business and Darwin's refusal`` () : unit =
        // The standard streams are pipes here. Linux permits `flock` on one and
        // returns 0; Darwin answers ENOTSUP, and what that leaves the lock state
        // as is unmeasured.
        UnixSystem.flock 0 2 linux
        |> answered
        |> shouldEqual (SyscallAnswer.Completed 0L)

        match UnixSystem.flock 0 2 darwin with
        | Error (FLockRefusal.DarwinStandardStream _) -> ()
        | other -> failwith $"expected a Darwin standard-stream refusal, got %O{other}"

    [<Test>]
    let ``a contended blocking lock is refused, and a non-blocking one is EAGAIN`` () : unit =
        // Two descriptions of one file, so the second acquire genuinely
        // contends. The refusal must not quietly become the non-blocking answer:
        // that would hand a caller an EWOULDBLOCK no kernel would have produced.
        let first, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget first system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file, got %O{other}"

        let second, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite system.Process.FileDescriptors

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        let held =
            match UnixSystem.flock first 2 system with
            | Ok (SyscallAnswer.Completed 0L, held) -> held
            | other -> failwith $"expected the first lock to be granted, got %O{other}"

        UnixSystem.flock second (2 ||| 4) held
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EAGAIN)

        UnixSystem.flock second 2 held
        |> shouldEqual (Error (FLockRefusal.WouldBlockIndefinitely FlockMode.Exclusive))

        // And the refusal carries no system, so the caller still holds the one it
        // passed in. That is deliberate and it costs something real: a kernel
        // that could park would have dropped the caller's old lock before
        // sleeping, and this discards that. The alternative — a refusal that
        // carries a state — reintroduces exactly the "which state is this?"
        // ambiguity refusals exist to remove. When blocking gets an outcome of
        // its own, this row is what will have to change on purpose.
        match UnixSystem.flock second 2 held with
        | Error _ -> ()
        | Ok (answer, _) -> failwith $"expected a refusal carrying no system, got %O{answer}"

    [<Test>]
    let ``a failing flock still advances the descriptor table`` () : unit =
        // The design's most distinctive claim, and the reason state rides
        // alongside a `Failed` rather than being withheld: a conversion that
        // cannot be granted has already dropped the caller's old lock.
        let first, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget first system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file, got %O{other}"

        let second, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite system.Process.FileDescriptors

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        // `second` holds a shared lock; `first` takes one too, then tries to
        // convert to exclusive, which cannot be granted while `second` holds its.
        let held =
            match UnixSystem.flock second 1 system with
            | Ok (SyscallAnswer.Completed 0L, held) -> held
            | other -> failwith $"expected the shared lock to be granted, got %O{other}"

        let both =
            match UnixSystem.flock first 1 held with
            | Ok (SyscallAnswer.Completed 0L, both) -> both
            | other -> failwith $"expected the second shared lock to be granted, got %O{other}"

        let afterFailedConversion =
            match UnixSystem.flock first (2 ||| 4) both with
            | Ok (SyscallAnswer.Failed UnixError.EAGAIN, after) -> after
            | other -> failwith $"expected EAGAIN, got %O{other}"

        // The failure dropped `first`'s lock rather than leaving it: the table
        // the caller gets back is not the one it passed in.
        afterFailedConversion |> shouldNotEqual both

    [<Test>]
    let ``truncateAt refuses a negative length rather than emptying the file`` () : unit =
        // `truncateAt` is shared with `open`'s `O_TRUNC`, so unlike `ftruncate`
        // it has callers that have not screened the length. A negative one
        // reaches `Array.Take` as an empty prefix, which would silently empty the
        // file and stamp it — and the guard that stops it must not be a
        // `Debug.Assert`, which a Release build compiles out.
        let fd, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file, got %O{other}"

        let exn =
            Assert.Throws<exn> (fun () ->
                UnixSystem.truncateAt inode -1L system
                |> ignore<Result<UnixSystem<int, string>, TruncationRefusal>>
            )

        exn.Message |> shouldContainText "negative"

    // ---- `getcwd` ----------------------------------------------------------
    //
    // Every row below is measured on macOS 26.6/APFS and on Linux 6.x in a
    // container, one forked child per row so that a flavour which *dies* rather
    // than answering is observed rather than taking the probe with it.

    /// The tree with the current directory at `/d/inner`, whose path is eight
    /// bytes — so nine is an exact fit and eight is one byte short.
    let private atInner (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let inner, _, _, system = withTree system

        { system with
            Process =
                { system.Process with
                    CurrentDirectory = AbsoluteUnixPath.parseOrFail context "/d/inner"
                    CurrentDirectoryInode = inner
                }
        }

    /// The same, with `/d/inner` then removed: a current directory a real
    /// process keeps working relative to but which has no path any more.
    ///
    /// Orphaned through this library's own `rmdir` rather than by editing the
    /// filesystem, so that the state under test is one a guest could actually
    /// reach — `rmdir` is the only syscall that can orphan a directory.
    let private atOrphan (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let system = atInner system

        // `/d/inner/t` first: `rmdir` refuses a directory that still has names
        // in it, which is also what keeps an orphan empty for ever after.
        let system =
            match UnixSystem.unlink (statPath "/d/inner/t") system with
            | SyscallAnswer.Completed 0L, system -> system
            | other -> failwith $"could not empty /d/inner: %O{other}"

        match UnixSystem.rmdir (statPath "/d/inner") system with
        | SyscallAnswer.Completed 0L, system -> system
        | other -> failwith $"could not orphan the current directory: %O{other}"

    /// What a successful `getcwd` places: the path and the terminator that makes
    /// nine bytes an exact fit for an eight-byte path.
    let private cwdBytes : ImmutableArray<byte> =
        (AbsoluteUnixPath.toUtf8 (AbsoluteUnixPath.parseOrFail context "/d/inner")).Add 0uy

    [<Test>]
    let ``getcwd reports the path and its terminator, which is what makes the fit exact`` () : unit =
        for system in [ atInner linux ; atInner darwin ] do
            // Nine bytes: eight of path and the NUL. A caller sizing its buffer
            // by the path alone is one short, which is the next row.
            UnixSystem.getcwd UserBuffer.Mapped 9 system
            |> shouldEqual (Ok (GetCwdAnswer.Reported cwdBytes))

            UnixSystem.getcwd UserBuffer.Mapped 1000 system
            |> shouldEqual (Ok (GetCwdAnswer.Reported cwdBytes))

    [<Test>]
    let ``getcwd needs room for the terminator as well as the path`` () : unit =
        for system in [ atInner linux ; atInner darwin ] do
            UnixSystem.getcwd UserBuffer.Mapped 8 system
            |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ERANGE))

    [<Test>]
    let ``getcwd answers a zero capacity EINVAL, and that beats a removed directory`` () : unit =
        // POSIX: size 0 with a non-NULL buffer is EINVAL, *not* ERANGE — so a
        // caller must not treat it as "grow and retry". Measured against the
        // orphaned system too, which is what says this guard comes first: with
        // the current directory removed, `getcwd(buf, 0)` is still EINVAL.
        for system in [ atInner linux ; atInner darwin ; atOrphan linux ; atOrphan darwin ] do
            UnixSystem.getcwd UserBuffer.Mapped 0 system
            |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.EINVAL))

    [<Test>]
    let ``a kernel-copying getcwd answers ERANGE before it looks at the destination`` () : unit =
        // Measured: `getcwd((char*)123, 1)` is ERANGE, not EFAULT — the size
        // comparison comes first. Only asserted for the flavour that copies from
        // the kernel; the other one may already have stored by here, which the
        // next row is about.
        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 8 (atInner linux)
        |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ERANGE))

    [<Test>]
    let ``a user-space getcwd refuses an unwritable destination on every failing path too`` () : unit =
        // Darwin stores *before* it decides which answer to give, so a
        // destination it cannot write kills the process on calls that would
        // otherwise be ERANGE or ENOENT — not only on the success path. Whether
        // it has stored yet turns on the current directory's length against a
        // libc threshold that is not a kernel fact (measured: 1015 bytes ERANGEs
        // cleanly, 1016 bytes is a SIGSEGV, and PATH_MAX is 1024), so the
        // library refuses from capacity 2 up rather than pick a cell.
        //
        // Three states, because the refusal has to outrank three different
        // answers: a short path that would be ERANGE, a fitting path that would
        // succeed, and a removed directory that would be ENOENT.
        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 8 (atInner darwin)
        |> shouldEqual (Error GetCwdRefusal.FatalToTheProcess)

        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 1000 (atInner darwin)
        |> shouldEqual (Error GetCwdRefusal.FatalToTheProcess)

        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 1000 (atOrphan darwin)
        |> shouldEqual (Error GetCwdRefusal.FatalToTheProcess)

        // Capacity 2 exactly, which is where the refusal starts and therefore
        // the only capacity that can tell a floor of 2 from a floor of 3. It is
        // the removed-directory case that establishes it: measured, that flavour
        // stores its first byte at capacity 2 and dies for an unmapped
        // destination, where capacity 1 is a clean ERANGE.
        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 2 (atOrphan darwin)
        |> shouldEqual (Error GetCwdRefusal.FatalToTheProcess)

    [<Test>]
    let ``below capacity 2 even a user-space getcwd answers an unwritable destination`` () : unit =
        // The floor the refusal starts at, and it is measured rather than
        // assumed: at capacity 1 Darwin writes nothing, so it reports an errno
        // for a destination it could not have written — for a path of 1015 bytes
        // and for one of 1026 alike, either side of the threshold above. This is
        // what stops the refusal swallowing the whole entry point.
        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 1 (atInner darwin)
        |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ERANGE))

        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 1 (atOrphan darwin)
        |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ERANGE))

        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 0 (atInner darwin)
        |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.EINVAL))

    [<Test>]
    let ``an unwritable destination is EFAULT on Linux and fatal on Darwin`` () : unit =
        // The divergence this entry point exists to hold. Linux's `getcwd` is a
        // syscall whose `copy_to_user` reports a bad destination; Darwin's
        // assembles the path with stores in the caller's own context, so the
        // process dies instead. Measured against a `PROT_READ` page, which
        // discriminates the two mechanisms where an unmapped address cannot —
        // and `readlink` answers EFAULT on *both* in the same probe, so this is
        // `getcwd`'s own property rather than a general one.
        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 9 (atInner linux)
        |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.EFAULT))

        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 9 (atInner darwin)
        |> shouldEqual (Error GetCwdRefusal.FatalToTheProcess)

    [<Test>]
    let ``getcwd refuses a destination whose bytes the caller cannot place`` () : unit =
        for system in [ atInner linux ; atInner darwin ] do
            UnixSystem.getcwd UserBuffer.Opaque 9 system
            |> shouldEqual (Error (GetCwdRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

            // At the transfer rather than at a screen: neither flavour looks at
            // the destination's address before comparing sizes, so there is no
            // screen for an addressless buffer to reach.
            UnixSystem.getcwd UserBuffer.Addressless 9 system
            |> shouldEqual (Error (GetCwdRefusal.Buffer BufferRefusal.AddresslessAtTransfer))

    [<Test>]
    let ``getcwd refuses a negative capacity rather than answering one`` () : unit =
        // No `getcwd(3)` sees a negative size — its argument is a `size_t`. The
        // PAL shim rejects one before calling, and that guard stays with the
        // client whose signature admits it.
        let exn =
            Assert.Throws<exn> (fun () ->
                UnixSystem.getcwd UserBuffer.Mapped -1 (atInner linux)
                |> ignore<Result<GetCwdAnswer, GetCwdRefusal>>
            )

        exn.Message |> shouldContainText "negative"

    [<Test>]
    let ``a removed current directory outranks the size comparison on Linux only`` () : unit =
        // Linux's `sys_getcwd` builds the path, finds it disconnected, and never
        // reaches the length comparison: measured ENOENT at every size from 1
        // up. Darwin's climbs from the root downwards and so needs two bytes
        // before it can start, which makes size 1 ERANGE there.
        UnixSystem.getcwd UserBuffer.Mapped 1 (atOrphan linux)
        |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ENOENT))

        UnixSystem.getcwd UserBuffer.Mapped 1 (atOrphan darwin)
        |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ERANGE))

    [<Test>]
    let ``a removed current directory is ENOENT at every capacity that reaches it`` () : unit =
        // Swept rather than sampled, because the two flavours split on capacity
        // 1 alone and a single sample cannot see that. Darwin's `getcwd` does
        // write to the destination on these paths -- a NUL at the last byte, and
        // the stale path too once the buffer reaches PATH_MAX -- and this
        // library deliberately reports none of it: see
        // `GetCwdOrphanAnswer.ShortestPathFirst` and docs/divergences.md.
        for capacity in [ 2 ; 3 ; 8 ; 64 ; 1023 ; 1024 ; 1025 ; 4096 ] do
            UnixSystem.getcwd UserBuffer.Mapped capacity (atOrphan darwin)
            |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ENOENT))

            UnixSystem.getcwd UserBuffer.Mapped capacity (atOrphan linux)
            |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ENOENT))

    [<Test>]
    let ``a removed directory outranks EFAULT on the kernel-copying flavour`` () : unit =
        // Linux never reaches a copy once it knows the directory is detached, so
        // an unmapped destination there is ENOENT and not EFAULT. That is the
        // pairing for the Darwin row above: the same call, the same destination,
        // and the two flavours differ in whether the destination matters at all.
        UnixSystem.getcwd (UserBuffer.Unmapped 123UL) 1000 (atOrphan linux)
        |> shouldEqual (Ok (GetCwdAnswer.Failed UnixError.ENOENT))

    // ---- `open` ------------------------------------------------------------

    /// Read-only, no flags set: the shape every row below varies one field of.
    let private plainOpen : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
        }

    let private openedFd (answer : SyscallAnswer * UnixSystem<int, string>) : int =
        match answer with
        | SyscallAnswer.Completed fd, _ -> int fd
        | other -> failwith $"expected a descriptor, got %O{other}"

    let private openFailed (answer : SyscallAnswer * UnixSystem<int, string>) : UnixError =
        match answer with
        | SyscallAnswer.Failed error, _ -> error
        | other -> failwith $"expected a failure, got %O{other}"

    [<Test>]
    let ``O_EXCL does nothing without O_CREAT`` () : unit =
        // The rule the record's shape exists to keep testable. `Exclusive` is
        // passed through as the caller set it rather than pre-combined with
        // `Create`, so this row can ask the question at all: measured on both,
        // `open(existing, O_WRONLY|O_EXCL)` succeeds and
        // `open(missing, O_WRONLY|O_EXCL)` is ENOENT, exactly as without it.
        //
        // A client that had ANDed the two before calling would make this row
        // vacuous — it would be asserting its own arithmetic, not this kernel's
        // rule.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            let exclusiveOnly =
                { plainOpen with
                    Exclusive = true
                }

            UnixSystem.openPath exclusiveOnly (statPath "/d/inner/t") 0o666 system
            |> openedFd
            |> shouldBeGreaterThan 2

            UnixSystem.openPath exclusiveOnly (statPath "/d/inner/nope") 0o666 system
            |> openFailed
            |> shouldEqual UnixError.ENOENT

            // On a *symbolic link*, which is the row that makes the rule
            // load-bearing rather than merely stated. `O_EXCL` with `O_CREAT`
            // stops the walk at the link (the next test), so an implementation
            // that read `Exclusive` without `Create` would answer ELOOP here.
            // Measured on both: `open(link, O_RDONLY|O_EXCL)` follows to the
            // file and succeeds, exactly as without the flag.
            UnixSystem.openPath exclusiveOnly (statPath "/l") 0o666 system
            |> openedFd
            |> shouldBeGreaterThan 2

            // And with `Create` it bites, which is what says the field is read
            // at all.
            UnixSystem.openPath
                { exclusiveOnly with
                    Create = true
                }
                (statPath "/d/inner/t")
                0o666
                system
            |> openFailed
            |> shouldEqual UnixError.EEXIST

    [<Test>]
    let ``O_CREAT|O_EXCL does not follow a final symlink`` () : unit =
        // Measured unanimously: an existing link is EEXIST whether it dangles or
        // points at a file, and nothing is created. Following it would create the
        // *target* of `/dangling` instead, so the row asserts the target is still
        // absent afterwards rather than only reading the errno.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            let creatingExclusive =
                { plainOpen with
                    Create = true
                    Exclusive = true
                }

            let answer, after =
                UnixSystem.openPath creatingExclusive (statPath "/dangling") 0o666 system

            answer |> shouldEqual (SyscallAnswer.Failed UnixError.EEXIST)

            UnixSystem.stat SymlinkPolicy.Follow (statPath "/d/inner/gone") after
            |> shouldEqual (FileStatusAnswer.Failed UnixError.ENOENT)

    [<Test>]
    let ``a directory opens for reading but not for writing`` () : unit =
        // CoreLib depends on both halves: `SafeFileHandle.Init` skips its own
        // directory check when write access was asked for, on the strength of
        // "open will have failed with EISDIR", and it opens-then-fstats to raise
        // `UnauthorizedAccessException` for a read.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            UnixSystem.openPath plainOpen (statPath "/d/inner") 0o666 system
            |> openedFd
            |> shouldBeGreaterThan 2

            UnixSystem.openPath
                { plainOpen with
                    Access = FileAccessMode.WriteOnly
                }
                (statPath "/d/inner")
                0o666
                system
            |> openFailed
            |> shouldEqual UnixError.EISDIR

    [<Test>]
    let ``O_TRUNC refuses a directory even opened read-only`` () : unit =
        // The one row where the directory arm fires for a *read-only* open:
        // measured, `open(d, O_RDONLY | O_TRUNC)` is EISDIR on both.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            UnixSystem.openPath
                { plainOpen with
                    Truncate = true
                }
                (statPath "/d/inner")
                0o666
                system
            |> openFailed
            |> shouldEqual UnixError.EISDIR

    [<Test>]
    let ``O_TRUNC empties a regular file opened read-only`` () : unit =
        // `O_TRUNC` is not confined to a write access mode: measured on both,
        // `open(f, O_RDONLY | O_TRUNC)` on a writable file succeeds and empties
        // it. The file is seeded with three bytes, so an implementation that
        // skipped the truncation would leave them.
        for flavour in [ linux ; darwin ] do
            let _, target, _, system = withTree flavour

            let _, after =
                UnixSystem.openPath
                    { plainOpen with
                        Truncate = true
                    }
                    (statPath "/d/inner/t")
                    0o666
                    system

            match UnixSystem.statOf target after with
            | Some status -> status.Size |> shouldEqual 0L
            | None -> failwith "the file vanished"

    [<Test>]
    let ``O_TRUNC demands the write bit that the access mode did not`` () : unit =
        // Measured at uid 1000 on both: `0400` with `RDONLY|TRUNC` is EACCES,
        // where plain `RDONLY` succeeds. Two rows rather than one, because the
        // EACCES alone cannot tell "TRUNC demands write" from "this file was
        // unopenable anyway".
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            let vfs =
                match
                    VirtualFileSystem.createFile
                        (VirtualFileSystem.root system.Machine.FileSystem)
                        (FileName.parseOrFail context "readable")
                        (PermissionBits.parseOrFail context 0o400)
                        epoch
                        (ImmutableArray.CreateRange [ 1uy ])
                        system.Machine.FileSystem
                with
                | Ok (_, vfs) -> vfs
                | Error error -> failwith $"could not seed: %O{error}"

            let system =
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = vfs
                        }
                }

            UnixSystem.openPath plainOpen (statPath "/readable") 0o666 system
            |> openedFd
            |> shouldBeGreaterThan 2

            UnixSystem.openPath
                { plainOpen with
                    Truncate = true
                }
                (statPath "/readable")
                0o666
                system
            |> openFailed
            |> shouldEqual UnixError.EACCES

    [<Test>]
    let ``O_NOFOLLOW makes opening a symbolic link ELOOP`` () : unit =
        // What `SafeFileHandle.OpenNoFollowSymlink` reads back to decide a path
        // was a symlink without racing. Paired with the same open *without* the
        // flag, which follows to the file — otherwise the row could not tell
        // ELOOP from "this link was broken".
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            UnixSystem.openPath plainOpen (statPath "/l") 0o666 system
            |> openedFd
            |> shouldBeGreaterThan 2

            UnixSystem.openPath
                { plainOpen with
                    NoFollow = true
                }
                (statPath "/l")
                0o666
                system
            |> openFailed
            |> shouldEqual UnixError.ELOOP

    [<Test>]
    let ``O_CLOEXEC and O_SYNC are accepted and change nothing`` () : unit =
        // They are in the record so a caller can say they were asked for rather
        // than drop them silently; this kernel models neither `exec` nor
        // durability. Asserted as "the same answer as without them" rather than
        // as "no error", which would pass for an implementation that rejected
        // every open.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            let plain = UnixSystem.openPath plainOpen (statPath "/d/inner/t") 0o666 system

            let withBoth =
                UnixSystem.openPath
                    { plainOpen with
                        CloseOnExec = true
                        Synchronous = true
                    }
                    (statPath "/d/inner/t")
                    0o666
                    system

            withBoth |> shouldEqual plain

    [<Test>]
    let ``a created file takes its permissions from mode and the umask`` () : unit =
        // `mode` crosses raw and unvalidated. Asserted at 0o777 against the
        // fixture's umask of 0o022, which is the pair that discriminates: 0o666
        // and 0o777 both land on 0o644 and 0o755 respectively, but a mode the
        // umask does not touch would agree with an implementation that ignored
        // the umask entirely.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            let _, after =
                UnixSystem.openPath
                    { plainOpen with
                        Create = true
                        Access = FileAccessMode.WriteOnly
                    }
                    (statPath "/made")
                    0o777
                    system

            match UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "/made") after with
            | Ok inode ->
                match UnixSystem.statOf inode after with
                | Some status -> status.Mode &&& 0o7777 |> shouldEqual 0o755
                | None -> failwith "the created file has no status"
            | Error error -> failwith $"the created file is unreachable: %O{error}"

    [<Test>]
    let ``a nonzero mode without O_CREAT is not rejected`` () : unit =
        // `SafeFileHandle.OpenReadOnly` passes 0666 even for a read-only open of
        // an existing file, so a kernel that validated `mode` here would refuse
        // the BCL's own read path.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            UnixSystem.openPath plainOpen (statPath "/d/inner/t") 0o666 system
            |> openedFd
            |> shouldBeGreaterThan 2

    [<Test>]
    let ``a creating open picks its own trailing-separator rule, and the flavours differ`` () : unit =
        // The cell that discriminates is a *free* name carrying a trailing
        // separator: an existing directory is EISDIR on both, so it cannot tell
        // the two rules apart. Measured, `open("new/", O_CREAT|O_WRONLY)` is
        // EISDIR on Linux and ENOENT on Darwin, and neither creates anything.
        //
        // A non-creating open demands a directory on both, which is why the
        // policy is chosen from `Create` at all rather than being one constant.
        let creating =
            { plainOpen with
                Create = true
                Access = FileAccessMode.WriteOnly
            }

        for flavour, expected in [ linux, UnixError.EISDIR ; darwin, UnixError.ENOENT ] do
            let _, _, _, system = withTree flavour

            let answer, after = UnixSystem.openPath creating (statPath "/new/") 0o666 system

            answer |> shouldEqual (SyscallAnswer.Failed expected)

            // Nothing was created under either rule, which is the half an errno
            // alone does not assert.
            UnixSystem.resolvePath SymlinkPolicy.Follow (statPath "/new") after
            |> shouldEqual (Error UnixError.ENOENT)

            // The same name *without* the separator creates, so the row above is
            // about the separator rather than about the name being unreachable.
            UnixSystem.openPath creating (statPath "/new") 0o666 system
            |> openedFd
            |> shouldBeGreaterThan 2

    [<Test>]
    let ``the descriptor carries the access mode that was asked for`` () : unit =
        // Every other row reads the *number* the open returned, which cannot
        // tell one access mode from another. What can is using the descriptor:
        // a write-only description refuses `read` with EBADF and a read-only one
        // refuses `write`, so the two directions pin the field in both
        // directions rather than only asserting it is not read-only.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            let writeOnlyFd, afterWriteOnly =
                match
                    UnixSystem.openPath
                        { plainOpen with
                            Access = FileAccessMode.WriteOnly
                        }
                        (statPath "/d/inner/t")
                        0o666
                        system
                with
                | SyscallAnswer.Completed fd, after -> int fd, after
                | other -> failwith $"expected a descriptor, got %O{other}"

            match UnixSystem.read writeOnlyFd UserBuffer.Mapped 8 afterWriteOnly with
            | Ok (ReadAnswer.Failed UnixError.EBADF, _) -> ()
            | other -> failwith $"a write-only descriptor should refuse read: %O{other}"

            let readOnlyFd, afterReadOnly =
                match UnixSystem.openPath plainOpen (statPath "/d/inner/t") 0o666 system with
                | SyscallAnswer.Completed fd, after -> int fd, after
                | other -> failwith $"expected a descriptor, got %O{other}"

            match UnixSystem.admitWrite readOnlyFd UserBuffer.Mapped 3 afterReadOnly with
            | Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EBADF)) -> ()
            | other -> failwith $"a read-only descriptor should refuse write: %O{other}"

    // ---- `opendir` / `readdir` ---------------------------------------------

    let private openedStream
        (system : UnixSystem<int, string>)
        (path : string)
        : DirectoryStreamId * UnixSystem<int, string>
        =
        match UnixSystem.opendir (statPath path) system with
        | OpenDirAnswer.Opened id, after -> id, after
        | other -> failwith $"expected a stream, got %O{other}"

    /// Everything the stream yields, as (name, kind) pairs in the order it
    /// yielded them.
    let private drain
        (id : DirectoryStreamId)
        (system : UnixSystem<int, string>)
        : (string * DirectoryEntryKind) list * UnixSystem<int, string>
        =
        let rec go fuel acc system =
            if fuel <= 0 then
                failwith
                    $"readdir yielded %d{List.length acc} entries without reaching end-of-stream; the cursor is not advancing."
            else

            match UnixSystem.readdir id system with
            | ReadDirAnswer.EndOfStream, system -> List.rev acc, system
            | ReadDirAnswer.Entry (name, kind), system ->
                let text = System.Text.Encoding.UTF8.GetString (name.AsSpan ())
                go (fuel - 1) ((text, kind) :: acc) system

        // Bounded rather than "until end-of-stream": a `readdir` that fails to
        // advance its cursor is a real thing to get wrong, and an unbounded loop
        // would hang the suite rather than report it. No fixture here holds more
        // than a handful of entries.
        go 64 [] system

    [<Test>]
    let ``a stream yields every binding, then dotdot, then dot`` () : unit =
        // The whole order in one row, because the cursor's states are only
        // meaningful as a sequence: names in ascending order, then `..`, then
        // `.`, then end-of-stream. Asserting a set would pass for a cursor that
        // never advanced past the first name.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour
            let id, system = openedStream system "/d/inner"

            drain id system
            |> fst
            |> shouldEqual
                [
                    "t", DirectoryEntryKind.RegularFile
                    "..", DirectoryEntryKind.Directory
                    ".", DirectoryEntryKind.Directory
                ]

    [<Test>]
    let ``the kind reported is the entry's own, not the stream's`` () : unit =
        // A directory holding one of each, so a `readdir` that reported the
        // *directory's* kind for every entry — or the first entry's for all of
        // them — is distinguishable. `/l` is a symbolic link and is reported as
        // one: `readdir` does not follow it.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour
            let id, system = openedStream system "/"

            let entries, _ = drain id system

            entries
            |> List.filter (fun (name, _) -> name <> "." && name <> "..")
            |> shouldEqual
                [
                    "d", DirectoryEntryKind.Directory
                    "dangling", DirectoryEntryKind.Symlink
                    "l", DirectoryEntryKind.Symlink
                ]

    [<Test>]
    let ``two streams over one directory advance independently`` () : unit =
        // The cursor belongs to the stream rather than to the directory, which
        // a single-stream test cannot tell. Draining one leaves the other at the
        // start.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour
            let first, system = openedStream system "/d/inner"
            let second, system = openedStream system "/d/inner"

            let drained, system = drain first system
            drained |> List.length |> shouldEqual 3

            drain second system |> fst |> List.length |> shouldEqual 3

    [<Test>]
    let ``opendir follows a final symlink and demands a directory`` () : unit =
        // Measured on both: `opendir` follows a final symbolic link, and a
        // trailing separator changes no row — "ld" and "ld/" both succeed, "f"
        // and "f/" are both ENOTDIR.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            match UnixSystem.opendir (statPath "/l") system with
            | OpenDirAnswer.Failed UnixError.ENOTDIR, _ -> ()
            | other -> failwith $"a link to a regular file should be ENOTDIR: %O{other}"

            match UnixSystem.opendir (statPath "/d/inner/t") system with
            | OpenDirAnswer.Failed UnixError.ENOTDIR, _ -> ()
            | other -> failwith $"a regular file should be ENOTDIR: %O{other}"

            match UnixSystem.opendir (statPath "/d/inner/nope") system with
            | OpenDirAnswer.Failed UnixError.ENOENT, _ -> ()
            | other -> failwith $"a free name should be ENOENT: %O{other}"

    [<Test>]
    let ``opendir consumes a descriptor that pins the directory`` () : unit =
        // A real `opendir` takes a file descriptor, and `dirfd(3)` hands it
        // back. Nothing in the PAL calls `dirfd`, so the only way a guest sees
        // it is in the *numbering* of a later open — which is what this asserts,
        // rather than reading the stream's own field.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour

            let withoutStream =
                match UnixSystem.openPath plainOpen (statPath "/d/inner/t") 0o666 system with
                | SyscallAnswer.Completed fd, _ -> int fd
                | other -> failwith $"expected a descriptor, got %O{other}"

            let _, afterStream = openedStream system "/d/inner"

            let withStream =
                match UnixSystem.openPath plainOpen (statPath "/d/inner/t") 0o666 afterStream with
                | SyscallAnswer.Completed fd, _ -> int fd
                | other -> failwith $"expected a descriptor, got %O{other}"

            withStream |> shouldEqual (withoutStream + 1)

    [<Test>]
    let ``a stream over a removed directory is at end-of-stream at once`` () : unit =
        // Dots included, which is the whole of the choice recorded on
        // `nextDirectoryEntry`: probed on both kernels, `opendir` then `rmdir`
        // then `readdir` answers NULL without yielding either dot.
        for flavour in [ linux ; darwin ] do
            let _, _, _, system = withTree flavour
            let id, system = openedStream system "/d/inner"

            let system =
                match UnixSystem.unlink (statPath "/d/inner/t") system with
                | SyscallAnswer.Completed 0L, system -> system
                | other -> failwith $"could not empty the directory: %O{other}"

            let system =
                match UnixSystem.rmdir (statPath "/d/inner") system with
                | SyscallAnswer.Completed 0L, system -> system
                | other -> failwith $"could not remove the directory: %O{other}"

            drain id system |> fst |> shouldEqual []

    [<Test>]
    let ``readdir refuses a stream this kernel never issued`` () : unit =
        // A real libc calls this undefined behaviour rather than reporting an
        // errno, so there is nothing to answer and inventing EBADF would be a
        // plausible wrong answer.
        let _, _, _, system = withTree linux

        let exn =
            Assert.Throws<exn> (fun () ->
                UnixSystem.readdir (DirectoryStreamId 99L) system
                |> ignore<ReadDirAnswer * UnixSystem<int, string>>
            )

        exn.Message |> shouldContainText "not a directory stream this kernel issued"
