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
        let fd, system = withOpenFile system

        let readOnly =
            FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors
            |> function
                | Some (OpenFileTarget.File (inode, _)) -> inode
                | other -> failwith $"expected a file descriptor, got %O{other}"

        let fd, registry =
            FileDescriptorRegistry.openFile readOnly FileAccessMode.ReadOnly system.Process.FileDescriptors

        fd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

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
