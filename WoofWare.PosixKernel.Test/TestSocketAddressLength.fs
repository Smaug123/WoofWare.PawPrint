namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The 32-bit address length `bind`, `connect`, `getsockname` and `accept`
/// take, over its whole range on both flavours: Linux reads the word as an
/// `int`, and Darwin as the `socklen_t` it is.
///
/// Every expectation here is a row of
/// `docs/plans/2026-08-23-posix-kernel-extraction/socket-address-length.c`,
/// measured on Linux 6.18.5 aarch64 and Darwin 27.0.0 arm64 over every word in
/// [-300, 300], every +/-2^k and +/-2^k +/- 1, INT32_MIN and INT32_MAX; its
/// outputs are checked in beside it. The properties draw from those words and
/// from the whole `uint32` range, and every one of them runs over the probe's
/// words exhaustively as well.
///
/// Every system is built through the syscalls themselves, so a queued
/// connection is one `connect` put there.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketAddressLength =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private listenerPort : uint16 = 5000us

    /// The words the probe swept, in the same construction.
    let private probeWords : uint32 list =
        [
            yield! [ -300 .. 300 ] |> List.map uint32
            for k in 0..31 do
                let p = 1L <<< k

                for v in [ p ; p - 1L ; p + 1L ; -p ; -p - 1L ; -p + 1L ] do
                    yield uint32 (int32 v)
            yield uint32 System.Int32.MinValue
            yield uint32 System.Int32.MaxValue
        ]
        |> List.distinct

    let private lengthGen : Gen<uint32> =
        Gen.oneof [ Gen.elements probeWords ; ArbMap.defaults |> ArbMap.generate<uint32> ]

    /// Check `property` on random words, then on every word the probe swept.
    let private everyLength (property : uint32 -> unit) : unit =
        Check.One (propertyConfig, Prop.forAll (Arb.fromGen lengthGen) property)

        for word in probeWords do
            property word

    let private isLinux (platform : SimulatedUnixPlatform) : bool =
        SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

    /// A fresh process on `platform` whose leader is task 0, as every call here
    /// is made by.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot

    let private streamSocket (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

    let private bindAt (endpoint : InternetEndpoint) (fd : int) (system : UnixSystem<int, string>) =
        match CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet system.Machine.UnixPlatform endpoint) system with
        | Ok (BindAnswer.Bound _, system) -> system
        | other -> failwith $"binding fd %d{fd} at %O{endpoint}: %A{other}"

    /// A listener at loopback:`listenerPort` on `platform`, and its descriptor.
    let private withListener (platform : SimulatedUnixPlatform) : int * UnixSystem<int, string> =
        let fd, system = streamSocket (systemOn platform)
        let system = bindAt (loopback listenerPort) fd system

        match UnixSocket.listen fd 8 system with
        | Ok (ListenAnswer.Listening _, system) -> fd, system
        | other -> failwith $"listening: %A{other}"

    /// A new client connected to the listener, and its descriptor.
    let private connectClient (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system = streamSocket system

        match
            CopyIn.connect
                fd
                UserBuffer.Mapped
                16u
                (CopyIn.inet system.Machine.UnixPlatform (loopback listenerPort))
                system
        with
        | Ok (ConnectOutcome.Completed, system) -> fd, system
        | other -> failwith $"connecting a client: %A{other}"

    let private queueOf (fd : int) (system : UnixSystem<int, string>) : ConnectionId list =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) ->
            match (UnixMachineState.socket socketId system.Machine).Phase with
            | SocketPhase.Listening listenState -> listenState.Queue
            | phase -> failwith $"fd %d{fd} is %A{phase}, not listening"
        | other -> failwith $"fd %d{fd} names %A{other}, not a socket"

    let private localAddress (fd : int) (system : UnixSystem<int, string>) : InternetEndpoint =
        match UnixSocket.getsockname fd UserBuffer.Mapped 16u system with
        | Ok (GetSockNameAnswer.Reported (copiedOut, _)) -> CopyOut.endpoint copiedOut
        | other -> failwith $"getsockname of fd %d{fd}: %A{other}"

    let private admit
        (syscall : SockaddrCopySyscall)
        (fd : int)
        (destination : UserBuffer)
        (word : uint32)
        (system : UnixSystem<int, string>)
        : SockaddrCopyAdmission
        =
        match UnixSocket.admitSockaddrCopy syscall fd destination word system with
        | Ok admission -> admission
        | Error refusal -> failwith $"word 0x%08x{word}: refused: %s{SockaddrCopyRefusal.describe refusal}"

    /// The measured answer of `bind` and of `connect` through a well-formed
    /// `sockaddr_in` in real storage, where the call can otherwise succeed:
    /// `None` for success.
    let private measuredCopyIn (platform : SimulatedUnixPlatform) (word : uint32) : UnixError option =
        if isLinux platform then
            let length = int word

            if 16 <= length && length <= 128 then
                None
            else
                Some UnixError.EINVAL
        elif word = 16u then
            None
        elif word <= 255u then
            Some UnixError.EINVAL
        else
            Some UnixError.ENAMETOOLONG

    [<Test>]
    let ``bind answers the measured length table at every length`` () : unit =
        for platform in platforms do
            everyLength (fun word ->
                let fd, system = streamSocket (systemOn platform)
                let target = loopback 0us

                let actual =
                    match CopyIn.bind fd UserBuffer.Mapped word (CopyIn.inet platform target) system with
                    | Ok (BindAnswer.Bound _, _) -> None
                    | Ok (BindAnswer.Failed error, _) -> Some error
                    | Error refusal -> failwith $"word 0x%08x{word}: %s{BindRefusal.describe refusal}"

                if actual <> measuredCopyIn platform word then
                    failwith
                        $"%O{platform} bind at 0x%08x{word}: expected %A{measuredCopyIn platform word}, got %A{actual}"
            )

    [<Test>]
    let ``connect answers the measured length table at every length`` () : unit =
        for platform in platforms do
            everyLength (fun word ->
                let _, system = withListener platform
                let fd, system = streamSocket system
                let target = loopback listenerPort

                let actual =
                    match CopyIn.connect fd UserBuffer.Mapped word (CopyIn.inet platform target) system with
                    | Ok (ConnectOutcome.Completed, _) -> None
                    | Ok (ConnectOutcome.Failed error, _) -> Some error
                    | Error refusal -> failwith $"word 0x%08x{word}: %s{ConnectRefusal.describe refusal}"

                if actual <> measuredCopyIn platform word then
                    failwith
                        $"%O{platform} connect at 0x%08x{word}: expected %A{measuredCopyIn platform word}, got %A{actual}"
            )

    /// Through real storage and through an unmapped buffer: Linux answers EINVAL
    /// for a negative length before it touches either the buffer or the length
    /// cell; Darwin bounds the copy by the length, so no length is an error.
    [<Test>]
    let ``getsockname answers the measured length table at every length`` () : unit =
        for platform in platforms do
            everyLength (fun word ->
                let fd, system = streamSocket (systemOn platform)
                let system = bindAt (loopback 6000us) fd system

                for destination in [ UserBuffer.Mapped ; UserBuffer.Unmapped 4096UL ] do
                    let expected =
                        if isLinux platform && int word < 0 then
                            GetSockNameAnswer.Failed (UnixError.EINVAL, None)
                        else
                            match destination with
                            | UserBuffer.Unmapped _ when word <> 0u ->
                                GetSockNameAnswer.Failed (
                                    UnixError.EFAULT,
                                    (if isLinux platform then Some 16 else None)
                                )
                            | _ -> GetSockNameAnswer.Reported (CopyOut.expected platform (loopback 6000us) word, 16)

                    match UnixSocket.getsockname fd destination word system with
                    | Ok actual when actual = expected -> ()
                    | actual ->
                        failwith
                            $"%O{platform} getsockname through %A{destination} at 0x%08x{word}: expected %A{expected}, got %A{actual}"
            )

    /// Linux's `accept` takes the connection off the queue before it reads the
    /// length, so a negative one loses it: the queue is empty afterwards, no
    /// descriptor is left open, and the client sees the server end close
    /// (`POLLIN|POLLRDHUP`, as the probe measured). Darwin accepts at every
    /// length.
    [<Test>]
    let ``accept answers the measured length table at every length`` () : unit =
        for platform in platforms do
            everyLength (fun word ->
                let listenerFd, system = withListener platform
                let clientFd, system = connectClient system
                let client = localAddress clientFd system
                let fdsBefore = FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors system)

                let destinations =
                    if isLinux platform && int word < 0 then
                        // The buffer is never looked at, so an unmapped one
                        // answers the same (measured).
                        [ UserBuffer.Mapped ; UserBuffer.Unmapped 4096UL ; UserBuffer.Opaque ]
                    else
                        [ UserBuffer.Mapped ]

                for destination in destinations do
                    match UnixConnection.accept 0 listenerFd destination word system with
                    | Ok (AcceptOutcome.DroppedConnection UnixError.EINVAL, after) when
                        isLinux platform && int word < 0
                        ->
                        queueOf listenerFd after |> shouldEqual []

                        FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors after)
                        |> shouldEqual fdsBefore

                        UnixSystem.checkInvariants after |> shouldEqual []

                        match
                            UnixPoll.poll
                                0
                                [
                                    {
                                        Fd = clientFd
                                        Events = 0x2001s
                                    }
                                ]
                                0
                                after
                        with
                        | Ok (PollOutcome.Answered ([ revents ], 1), _) -> revents |> shouldEqual 0x2001s
                        | other -> failwith $"polling the client after the drop: %A{other}"
                    | Ok (AcceptOutcome.Accepted (acceptedFd, copiedOut, reportedLength), after) when
                        not (isLinux platform && int word < 0)
                        ->
                        copiedOut |> shouldEqual (CopyOut.expected platform client word)
                        reportedLength |> shouldEqual 16
                        Map.containsKey acceptedFd fdsBefore |> shouldEqual false
                        queueOf listenerFd after |> shouldEqual []
                    | other -> failwith $"%O{platform} accept at 0x%08x{word} through %A{destination}: got %A{other}"
            )

    /// Linux reads no length when the destination is NULL, so every length
    /// succeeds there (measured). This library does not model a NULL
    /// destination for `accept`, so it refuses rather than answering EINVAL;
    /// and a destination the client has no address for might be NULL.
    [<Test>]
    let ``a negative length through a null destination is not answered on Linux`` () : unit =
        let listenerFd, system = withListener SimulatedUnixPlatform.linuxX64
        let _, system = connectClient system

        for word in [ 0xFFFF_FFFFu ; 0x8000_0000u ] do
            match UnixConnection.accept 0 listenerFd (UserBuffer.Unmapped 0UL) word system with
            | Error (AcceptRefusal.UnmeasuredCopyOutFault _) -> ()
            | other -> failwith $"word 0x%08x{word}: expected a refusal, got %A{other}"

            match UnixConnection.accept 0 listenerFd UserBuffer.Addressless word system with
            | Error (AcceptRefusal.Buffer BufferRefusal.AddresslessAtScreen) -> ()
            | other -> failwith $"word 0x%08x{word}: expected a refusal, got %A{other}"

    /// A dropped connection is exactly what accepting it and then closing the
    /// accepted descriptor would leave, bar the identities the accept would
    /// have spent: the queue loses its head, the client sees the close and its
    /// registrations are signalled as for a close, and a connection whose
    /// client has already gone leaves the table.
    [<Test>]
    let ``a dropped connection is an accept followed by a close`` () : unit =
        let gen =
            gen {
                let! clients = Gen.choose (1, 3)
                let! closedClients = Gen.listOfLength clients (ArbMap.defaults |> ArbMap.generate<bool>)
                // A non-blocking client's connect answers EINPROGRESS and leaves
                // it awaiting the report of its establishment.
                let! nonBlockingClients = Gen.listOfLength clients (ArbMap.defaults |> ArbMap.generate<bool>)

                let! watched =
                    Gen.listOfLength
                        clients
                        (Gen.elements [ 0u ; 0x80000001u ; 0x80002001u ; 0x80000004u ; 0x80002015u ])

                let! word = lengthGen |> Gen.filter (fun word -> int word < 0)
                return List.zip3 closedClients nonBlockingClients watched, word
            }

        let property (clients : (bool * bool * uint32) list, word : uint32) : unit =
            let listenerFd, system = withListener SimulatedUnixPlatform.linuxX64

            let queueFd, system =
                match UnixPoll.epollCreate1 0 system with
                | Ok (Ok created) -> created
                | other -> failwith $"epoll_create1: %A{other}"

            let system =
                (system, clients)
                ||> List.fold (fun system (closed, nonBlocking, events) ->
                    let clientFd, system =
                        if nonBlocking then
                            let fd, system = streamSocket system
                            let system = UnixDescriptor.setNonBlocking fd true system |> snd

                            match
                                CopyIn.connect
                                    fd
                                    UserBuffer.Mapped
                                    16u
                                    (CopyIn.inet system.Machine.UnixPlatform (loopback listenerPort))
                                    system
                            with
                            | Ok (ConnectOutcome.Failed UnixError.EINPROGRESS, system) -> fd, system
                            | other -> failwith $"connecting a non-blocking client: %A{other}"
                        else
                            connectClient system

                    let system =
                        if events = 0u then
                            system
                        else
                            match
                                UnixPoll.epollCtl queueFd 1 clientFd (EpollEventArgument.Readable (events, 7UL)) system
                            with
                            | Ok (EpollCtlAnswer.Changed, system) -> system
                            | other -> failwith $"epoll_ctl ADD: %A{other}"

                    if closed then
                        match UnixDescriptor.close clientFd system with
                        | Ok (SyscallAnswer.Completed _, system) -> system
                        | other -> failwith $"closing a client: %A{other}"
                    else
                        system
                )

            let dropped =
                match UnixConnection.accept 0 listenerFd UserBuffer.Mapped word system with
                | Ok (AcceptOutcome.DroppedConnection UnixError.EINVAL, after) -> after
                | other -> failwith $"word 0x%08x{word}: expected a dropped connection, got %A{other}"

            let acceptedThenClosed =
                match UnixConnection.accept 0 listenerFd UserBuffer.Mapped 16u system with
                | Ok (AcceptOutcome.Accepted (acceptedFd, _, _), after) ->
                    match UnixDescriptor.close acceptedFd after with
                    | Ok (SyscallAnswer.Completed _, after) -> after
                    | other -> failwith $"closing the accepted descriptor: %A{other}"
                | other -> failwith $"expected an accept, got %A{other}"

            UnixSystem.checkInvariants dropped |> shouldEqual []

            FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors dropped)
            |> shouldEqual (FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors acceptedThenClosed))

            OpenFileTable.descriptions dropped.Machine.OpenFiles
            |> shouldEqual (OpenFileTable.descriptions acceptedThenClosed.Machine.OpenFiles)

            // The socket identity and the description identity the accept
            // minted are the only differences.
            (UnixSystemState.withFileDescriptors
                ((UnixSystemState.fileDescriptors dropped))
                { acceptedThenClosed with
                    Machine =
                        { acceptedThenClosed.Machine with
                            NextSocketId = dropped.Machine.NextSocketId
                        }
                })
            |> shouldEqual dropped

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    /// A blocking accept reads the length only once a connection has arrived
    /// and been taken, so on Linux a negative one sleeps, then drops the
    /// connection that wakes it (measured: EINVAL after the connect, and the
    /// queue empty).
    [<Test>]
    let ``a blocking accept with a negative length sleeps, then drops the connection that wakes it`` () : unit =
        for platform in platforms do
            for word in [ 0xFFFF_FFFFu ; 0x8000_0000u ; 16u ] do
                let listenerFd, system = withListener platform
                let system = Tasks.ensure 1 system

                let system =
                    match UnixConnection.accept 1 listenerFd UserBuffer.Mapped word system with
                    | Ok (AcceptOutcome.WouldBlock _, system) -> system
                    | other -> failwith $"%O{platform} 0x%08x{word}: expected a park, got %A{other}"

                let _, system = connectClient system

                match UnixConnection.finishAccept 1 system, isLinux platform && int word < 0 with
                | Ok (AcceptOutcome.DroppedConnection UnixError.EINVAL, after), true ->
                    queueOf listenerFd after |> shouldEqual []
                    UnixTaskTable.parkedFor 1 after.Tasks |> shouldEqual None
                    UnixSystem.checkInvariants after |> shouldEqual []
                | Ok (AcceptOutcome.Accepted _, _), false -> ()
                | other, _ -> failwith $"%O{platform} 0x%08x{word}: got %A{other}"

    /// The descriptor is classified before the length on both flavours: a
    /// closed one is EBADF at every length, whatever the call.
    [<Test>]
    let ``a closed descriptor is EBADF at every length`` () : unit =
        for platform in platforms do
            everyLength (fun word ->
                let system = systemOn platform
                let closedFd = 99

                for syscall in [ SockaddrCopySyscall.Bind ; SockaddrCopySyscall.Connect ] do
                    admit syscall closedFd (UserBuffer.Unmapped 4096UL) word system
                    |> shouldEqual (SockaddrCopyAdmission.Answered UnixError.EBADF)

                UnixSocket.getsockname closedFd UserBuffer.Mapped word system
                |> shouldEqual (Ok (GetSockNameAnswer.Failed (UnixError.EBADF, None)))

                match UnixConnection.accept 0 closedFd UserBuffer.Mapped word system with
                | Ok (AcceptOutcome.Failed UnixError.EBADF, _) -> ()
                | other -> failwith $"%O{platform} accept at 0x%08x{word}: %A{other}"
            )

    /// Linux's `connect` copies the sockaddr in before it asks whether the
    /// descriptor is a socket, so on a pipe, a file or an epoll descriptor the
    /// length's EINVAL and the copy's EFAULT come before ENOTSOCK. Its `bind`,
    /// and both of Darwin's calls, answer ENOTSOCK at every length and through
    /// every buffer. A buffer whose bytes the client cannot produce is still
    /// copied without fault, and so answers ENOTSOCK too; one whose address the
    /// client has no number for has no answer where the copy happens.
    [<Test>]
    let ``a descriptor that is not a socket answers in the measured order`` () : unit =
        for platform in platforms do
            let system = systemOn platform

            let fileFd, registry =
                FileDescriptorRegistry.openFile
                    (InodeNumber 1L)
                    FileAccessMode.ReadOnly
                    (UnixSystemState.fileDescriptors system)

            let createEventQueue =
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> FileDescriptorRegistry.createEpoll
                | SimulatedUnixFlavour.Darwin -> FileDescriptorRegistry.createKqueue

            let queueFd, registry = createEventQueue registry

            let system = UnixSystemState.withFileDescriptors registry system

            everyLength (fun word ->
                for fd in [ 0 ; fileFd ; queueFd ] do
                    for destination in
                        [
                            UserBuffer.Mapped
                            UserBuffer.Unmapped 4096UL
                            UserBuffer.Opaque
                            UserBuffer.Addressless
                        ] do
                        UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Bind fd destination word system
                        |> shouldEqual (Ok (SockaddrCopyAdmission.Answered UnixError.ENOTSOCK))

                        let length = int word

                        let expected =
                            if not (isLinux platform) then
                                Ok (SockaddrCopyAdmission.Answered UnixError.ENOTSOCK)
                            elif length < 0 || length > 128 then
                                Ok (SockaddrCopyAdmission.Answered UnixError.EINVAL)
                            elif length = 0 then
                                Ok (SockaddrCopyAdmission.Answered UnixError.ENOTSOCK)
                            else
                                match destination with
                                | UserBuffer.Unmapped _ -> Ok (SockaddrCopyAdmission.Answered UnixError.EFAULT)
                                | UserBuffer.Addressless ->
                                    Error (SockaddrCopyRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
                                | UserBuffer.Mapped
                                | UserBuffer.Opaque -> Ok (SockaddrCopyAdmission.Answered UnixError.ENOTSOCK)

                        let actual =
                            UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Connect fd destination word system

                        if actual <> expected then
                            failwith
                                $"%O{platform} connect on fd %d{fd} through %A{destination} at 0x%08x{word}: expected %A{expected}, got %A{actual}"
            )

    /// The same order on a socket whose domain this library does not model:
    /// Linux's `connect` answers the length and the copy before anything about
    /// the socket (measured for `AF_UNIX` and `AF_INET6`), and only a copy that
    /// succeeds reaches the domain, which is refused. Darwin's, and `bind` on
    /// both, look at the socket first, so refuse at every length.
    [<Test>]
    let ``a socket in an unmodelled domain answers the copy-in's errors on Linux connect`` () : unit =
        for platform in platforms do
            for domain in [ SocketDomain.Unix ; SocketDomain.Inet6 ] do
                let fd, system =
                    NewSocket.create domain SocketKind.Stream SocketProtocol.Default (systemOn platform)

                let socketId =
                    match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
                    | Some (OpenFileTarget.Socket socketId) -> socketId
                    | other -> failwith $"%A{other}"

                let refused = Error (SockaddrCopyRefusal.UnmodelledDomain (socketId, domain))

                everyLength (fun word ->
                    for destination in [ UserBuffer.Mapped ; UserBuffer.Unmapped 4096UL ] do
                        UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Bind fd destination word system
                        |> shouldEqual refused

                        let length = int word

                        let expected =
                            if not (isLinux platform) then
                                refused
                            elif length < 0 || length > 128 then
                                Ok (SockaddrCopyAdmission.Answered UnixError.EINVAL)
                            elif length > 0 && destination <> UserBuffer.Mapped then
                                Ok (SockaddrCopyAdmission.Answered UnixError.EFAULT)
                            else
                                refused

                        let actual =
                            UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Connect fd destination word system

                        if actual <> expected then
                            failwith
                                $"%O{platform} connect on %O{domain} through %A{destination} at 0x%08x{word}: expected %A{expected}, got %A{actual}"
                )
