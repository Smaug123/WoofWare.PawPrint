namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `getsockopt(SOL_SOCKET, SO_ERROR)`: what it reads in each socket phase, when
/// a read takes a pending refusal, and what a refused socket answers once its
/// error has been taken.
///
/// The rows are literals of the measurements, per flavour: Linux 6.18.5 (arm64,
/// under Apple's `container`) and Darwin 27.0.0, with `soerror.c`,
/// `nullcell.c` and `consumed-epoll.c` in `docs/probes/so-error/`. `TestSocketErrorAgainstHost`
/// puts the same questions to the kernel running the suite.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketErrorOption =

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64
    let private platforms : SimulatedUnixPlatform list = [ linux ; darwin ]

    let private flavourColumn (platform : SimulatedUnixPlatform) (onLinux : 'a) (onDarwin : 'a) : 'a =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> onLinux
        | SimulatedUnixFlavour.Darwin -> onDarwin

    /// ECONNREFUSED as each kernel numbers it, measured: what an `SO_ERROR`
    /// read of a pending refusal writes.
    let private refusedErrno (platform : SimulatedUnixPlatform) : int = flavourColumn platform 111 61

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private wildcard (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    /// Nothing listens or is bound here in any system these tests build.
    let private nobody : InternetEndpoint = loopback 5999us

    let private listenerPort : uint16 = 5000us

    let private unmapped : UserBuffer = UserBuffer.Unmapped 0x1000UL
    let private null' : UserBuffer = UserBuffer.Unmapped 0UL

    /// `(socklen_t)-1`.
    let private minus1 : uint32 = System.UInt32.MaxValue

    let private socketIdOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) -> socketId
        | other -> failwith $"fd %d{fd} names %A{other}, not a socket"

    let private phaseOf (fd : int) (system : UnixSystem<int, string>) : SocketPhase =
        (UnixMachineState.socket (socketIdOf fd system) system.Machine).Phase

    let private endpointOf (fd : int) (system : UnixSystem<int, string>) : InternetEndpoint =
        match (UnixMachineState.socket (socketIdOf fd system) system.Machine).Binding with
        | Some binding -> binding.Endpoint
        | None -> failwith $"fd %d{fd} is not bound"

    let private stream (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

    let private datagram (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        NewSocket.create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

    let private nonBlocking (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.setNonBlocking fd true system with
        | SetNonBlockingAnswer.Set, system -> system
        | other, _ -> failwith $"setting O_NONBLOCK on fd %d{fd} answered %A{other}"

    let private bindAt (fd : int) (endpoint : InternetEndpoint) (system : UnixSystem<int, string>) =
        match CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet (UnixSystem.platform system) endpoint) system with
        | Ok (BindAnswer.Bound _, system) -> system
        | other -> failwith $"binding fd %d{fd} to %O{endpoint} answered %A{other}"

    let private connect
        (fd : int)
        (destination : InternetEndpoint)
        (system : UnixSystem<int, string>)
        : ConnectOutcome * UnixSystem<int, string>
        =
        match CopyIn.connect fd UserBuffer.Mapped 16u (CopyIn.inet (UnixSystem.platform system) destination) system with
        | Ok answer -> answer
        | Error refusal -> failwith $"connect refused: %s{ConnectRefusal.describe refusal}"

    let private listening (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system = stream system
        let system = bindAt fd (loopback listenerPort) system

        match UnixSocket.listen fd 8 system with
        | Ok (ListenAnswer.Listening _, system) -> fd, system
        | Ok (answer, _) -> failwith $"listen answered %A{answer}"
        | Error refusal -> failwith $"listen refused: %s{ListenRefusal.describe refusal}"

    let private fresh (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot

    /// A stream socket whose non-blocking connect to `nobody` was refused, with
    /// the refusal still pending; `prepare` runs on the socket first.
    let private refusedWith
        (platform : SimulatedUnixPlatform)
        (prepare : int -> UnixSystem<int, string> -> UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, system = stream (fresh platform)
        let system = nonBlocking fd system |> prepare fd
        let outcome, system = connect fd nobody system
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)
        phaseOf fd system |> shouldEqual (SocketPhase.Refused RefusalError.Pending)
        fd, system

    let private refused (platform : SimulatedUnixPlatform) : int * UnixSystem<int, string> =
        refusedWith platform (fun _ system -> system)

    /// The whole `getsockopt(SO_ERROR)` a client makes: ask the admission, read
    /// the length cell only if the kernel would, then make the call.
    let private readError
        (fd : int)
        (value : UserBuffer)
        (length : UserBuffer)
        (declaredLength : uint32)
        (system : UnixSystem<int, string>)
        : GetSockOptAnswer * UnixSystem<int, string>
        =
        let platform = system.Machine.UnixPlatform
        let level = SimulatedUnixPlatform.socketOptionLevel platform
        let optionName = SimulatedUnixPlatform.socketErrorOption platform

        let read =
            match UnixSocket.admitGetSockOpt fd level optionName value length system with
            | Ok GetSockOptAdmission.ReadLength -> Some declaredLength
            | Ok GetSockOptAdmission.SkipLength
            | Ok (GetSockOptAdmission.Answered _)
            | Error _ -> None

        match UnixSocket.getsockopt fd level optionName value length read system with
        | Ok answer -> answer
        | Error refusal -> failwith $"getsockopt refused: %s{SocketOptionRefusal.describe refusal}"

    let private readErrorPlainly (fd : int) (system : UnixSystem<int, string>) =
        readError fd UserBuffer.Mapped UserBuffer.Mapped 4u system

    // ------------------------------------------------------------------
    // What a read reports, and what it takes
    // ------------------------------------------------------------------

    /// The refusal is reported by the first read and by no later one, and the
    /// read leaves the socket refused with nothing pending. Measured on both.
    [<Test>]
    let ``a read reports a pending refusal once, and takes it`` () : unit =
        for platform in platforms do
            let fd, system = refused platform

            let answer, system = readErrorPlainly fd system
            answer |> shouldEqual (GetSockOptAnswer.Reported (refusedErrno platform, 4u))
            phaseOf fd system |> shouldEqual (SocketPhase.Refused RefusalError.Reported)

            let answer, after = readErrorPlainly fd system
            answer |> shouldEqual (GetSockOptAnswer.Reported (0, 4u))
            after |> shouldEqual system

    /// Every phase holding no pending refusal reads 0, and the read changes
    /// nothing: measured on a fresh and a bound socket, a listener with and
    /// without a queued connection, both ends of an established connection, a
    /// Linux connection whose completion no connect has reported yet, a fresh
    /// and a connected datagram socket, and a socket refused by a blocking
    /// connect.
    [<Test>]
    let ``a read outside a pending refusal reports zero and changes nothing`` () : unit =
        for platform in platforms do
            let cases : (string * int * UnixSystem<int, string>) list =
                [
                    yield
                        (let fd, system = stream (fresh platform)
                         "fresh", fd, system)
                    yield
                        (let fd, system = stream (fresh platform)
                         "bound", fd, bindAt fd (loopback 6000us) system)
                    yield
                        (let fd, system = listening (fresh platform)
                         "listening, queue empty", fd, system)
                    yield
                        (let listener, system = listening (fresh platform)
                         let client, system = stream system
                         let _, system = connect client (loopback listenerPort) system
                         "listening, queue holds one", listener, system)
                    yield
                        (let _, system = listening (fresh platform)
                         let client, system = stream system
                         let _, system = connect client (loopback listenerPort) system
                         "blocking-connected client", client, system)
                    yield
                        (let listener, system = listening (fresh platform)
                         let client, system = stream system
                         let _, system = connect client (loopback listenerPort) system

                         let server, _, system =
                             UnixConnection.acceptConnection (socketIdOf listener system) system

                         "accepted server end", server, system)
                    yield
                        (let fd, system = datagram (fresh platform)
                         "fresh datagram", fd, system)
                    yield
                        (let fd, system = datagram (fresh platform)
                         let _, system = connect fd nobody system
                         "datagram with a peer", fd, system)
                    yield
                        (let fd, system = stream (fresh platform)
                         let outcome, system = connect fd nobody system
                         outcome |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)
                         "refused by a blocking connect", fd, system)
                    if SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux then
                        yield
                            (let _, system = listening (fresh platform)
                             let client, system = stream system
                             let system = nonBlocking client system
                             let _, system = connect client (loopback listenerPort) system

                             phaseOf client system
                             |> function
                                 | SocketPhase.EstablishedPendingReport _ -> ()
                                 | other -> failwith $"expected a completion to report, got %A{other}"

                             "completed, not yet reported", client, system)
                ]

            for label, fd, system in cases do
                let answer, after = readErrorPlainly fd system

                if answer <> GetSockOptAnswer.Reported (0, 4u) then
                    failwith $"%O{platform} %s{label}: expected zero, got %A{answer}"

                if after <> system then
                    failwith $"%O{platform} %s{label}: the read changed the system"

    type private ReadRow =
        {
            Label : string
            Value : UserBuffer
            LengthCell : UserBuffer
            Length : uint32
            /// The answer, and whether the refusal is still pending afterwards.
            Linux : GetSockOptAnswer * bool
            Darwin : GetSockOptAnswer * bool
        }

    /// Whether a read takes the refusal is not whether it succeeds: both
    /// kernels take it as soon as the call has read the option, whatever the
    /// copy-out then does. Linux reads the length cell, and refuses a negative
    /// length, before that; Darwin reads the cell first only when the value
    /// buffer is not null. Every row measured on a freshly refused socket.
    let private readRows : ReadRow list =
        let reported (platform : SimulatedUnixPlatform) (length : uint32) =
            GetSockOptAnswer.Reported (refusedErrno platform, length)

        let efault = GetSockOptAnswer.Failed UnixError.EFAULT
        let taken, kept = false, true
        let real = UserBuffer.Mapped

        let row label value lengthCell length onLinux onDarwin =
            {
                Label = label
                Value = value
                LengthCell = lengthCell
                Length = length
                Linux = onLinux
                Darwin = onDarwin
            }

        [
            row "length 4" real real 4u (reported linux 4u, taken) (reported darwin 4u, taken)
            row "length 0" real real 0u (reported linux 0u, taken) (reported darwin 0u, taken)
            row "length 2" real real 2u (reported linux 2u, taken) (reported darwin 2u, taken)
            row "length 8" real real 8u (reported linux 4u, taken) (reported darwin 4u, taken)
            row
                "length -1"
                real
                real
                minus1
                (GetSockOptAnswer.Failed UnixError.EINVAL, kept)
                (reported darwin 4u, taken)
            row "null value, length 4" null' real 4u (efault, taken) (reported darwin 0u, taken)
            row "unmapped value, length 4" unmapped real 4u (efault, taken) (efault, taken)
            row "unmapped length cell" real unmapped 4u (efault, kept) (efault, kept)
            row "null length cell" real null' 4u (efault, kept) (efault, kept)
            row "unmapped value, null length cell" unmapped null' 4u (efault, kept) (efault, kept)
            // Darwin reads the length cell only for a non-null value buffer:
            // with a null one it reads the option, then faults writing the
            // length back (`nullcell.c`).
            row "null value, null length cell" null' null' 4u (efault, kept) (efault, taken)
            row "null value, unmapped length cell" null' unmapped 4u (efault, kept) (efault, taken)
            row
                "null value, length -1"
                null'
                real
                minus1
                (GetSockOptAnswer.Failed UnixError.EINVAL, kept)
                (reported darwin 0u, taken)
        ]

    [<Test>]
    let ``a read takes the refusal exactly when it gets past the length`` () : unit =
        for platform in platforms do
            for row in readRows do
                let fd, system = refused platform
                let answer, system = readError fd row.Value row.LengthCell row.Length system

                let pending =
                    match phaseOf fd system with
                    | SocketPhase.Refused RefusalError.Pending -> true
                    | SocketPhase.Refused RefusalError.Reported -> false
                    | other -> failwith $"%O{platform} %s{row.Label}: the read left phase %A{other}"

                let expected = flavourColumn platform row.Linux row.Darwin

                if (answer, pending) <> expected then
                    failwith $"%O{platform} %s{row.Label}: expected %A{expected}, got %A{(answer, pending)}"

    // ------------------------------------------------------------------
    // A refused socket once its error is taken
    // ------------------------------------------------------------------

    /// On Linux the next connect answers ECONNABORTED wherever it is aimed, and
    /// resets the socket exactly as delivering the refusal would: the socket is
    /// idle, its binding keeps the port and reverts the address to whatever
    /// `bind(2)` locked, and the connect after that is a fresh attempt.
    /// Measured from an implicit binding, and from `bind(2)` to 127.0.0.1 and
    /// to 0.0.0.0.
    [<Test>]
    let ``Linux: a connect after the read answers ECONNABORTED and resets the socket`` () : unit =
        let provenances : (string * (int -> UnixSystem<int, string> -> UnixSystem<int, string>) * uint32) list =
            [
                "implicit", (fun _ system -> system), InternetEndpoint.WildcardAddress
                "bound 127.0.0.1", (fun fd system -> bindAt fd (loopback 0us) system), InternetEndpoint.LoopbackAddress
                "bound 0.0.0.0", (fun fd system -> bindAt fd (wildcard 0us) system), InternetEndpoint.WildcardAddress
            ]

        for label, prepare, addressAfter in provenances do
            for destination in [ nobody ; loopback listenerPort ] do
                let fd, system = refusedWith linux prepare
                let _, system = listening system
                let pendingEndpoint = endpointOf fd system
                pendingEndpoint.Address |> shouldEqual InternetEndpoint.LoopbackAddress

                let _, system = readErrorPlainly fd system
                endpointOf fd system |> shouldEqual pendingEndpoint

                let outcome, system = connect fd destination system

                if outcome <> ConnectOutcome.Failed UnixError.ECONNABORTED then
                    failwith $"%s{label}, to %O{destination}: expected ECONNABORTED, got %A{outcome}"

                phaseOf fd system |> shouldEqual SocketPhase.Idle

                endpointOf fd system
                |> shouldEqual (InternetEndpoint.ofParts addressAfter pendingEndpoint.Port)

                UnixSystem.checkInvariants system |> shouldEqual []

        // ...and the attempt after the reset is refused afresh, with the
        // refusal pending again.
        let fd, system = refused linux
        let _, system = readErrorPlainly fd system
        let _, system = connect fd nobody system
        let outcome, system = connect fd nobody system
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)
        phaseOf fd system |> shouldEqual (SocketPhase.Refused RefusalError.Pending)

    /// Darwin's connect never reports a refusal, taken or not: EISCONN wherever
    /// it is aimed, and the socket stays refused. Measured.
    [<Test>]
    let ``Darwin: a connect after the read answers EISCONN and changes nothing`` () : unit =
        for destination in [ nobody ; loopback listenerPort ] do
            let fd, system = refused darwin
            let _, system = listening system
            let _, system = readErrorPlainly fd system
            let outcome, after = connect fd destination system
            outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EISCONN)
            after |> shouldEqual system

    /// The read drops ERR from the level and nothing else: Linux measured
    /// IN|OUT|RDHUP|HUP through level-triggered `epoll_wait`, and IN|OUT|HUP
    /// through a `poll(2)` that did not ask for RDHUP.
    [<Test>]
    let ``Linux: a taken refusal's level is the pending one without ERR`` () : unit =
        let fd, system = refused linux
        let _, system = readErrorPlainly fd system

        UnixMachineState.socketReadinessLevel (socketIdOf fd system) system.Machine
        |> shouldEqual
            {
                In = true
                Out = true
                RdHup = true
                Hup = true
                Err = false
            }
