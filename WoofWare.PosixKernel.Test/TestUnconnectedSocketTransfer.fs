namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `read(2)` and `write(2)` on a socket with no peer, held to what each kernel
/// answered: the sweep `socket-unconnected-transfer.c` took on Darwin 27.0 and
/// Linux 6.18.5, embedded from beside the probe, and the host the suite runs on.
///
/// Three layers, because each is blind to something the others see. The rules
/// are held to every measured row, including the phases this kernel cannot
/// build (a bound Unix-domain socket, say), where they are a claim about a
/// socket the library may one day make. `UnixReadWrite` is held to every
/// measured row whose socket the kernel *can* build, which is what sees the
/// wiring: the phase, the description's `O_NONBLOCK`, and the signal's
/// receiver. The host is held to the same rows as the kernel, live, so a
/// measurement taken on one machine is checked on another — Linux's column on
/// CI's x86-64, Darwin's here.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnconnectedSocketTransfer =

    [<RequireQualifiedAccess>]
    type private Phase =
        | Idle
        | Bound
        | ListeningUnbound
        | ListeningBound

    [<RequireQualifiedAccess>]
    type private Call =
        | Read of count : uint64
        | Write of count : uint64

    /// What the probe saw. A write's signal is reported only as it was seen:
    /// the handler ran once, on the writing thread, before the write returned.
    [<RequireQualifiedAccess>]
    type private Seen =
        | ReturnedZero
        | Failed of UnixError
        | FailedRaisingSigPipe of UnixError
        | Sleeps

    type private Row =
        {
            Flavour : SimulatedUnixFlavour
            Domain : SocketDomain
            Kind : SocketKind
            Phase : Phase
            NonBlocking : bool
            Call : Call
            Seen : Seen
        }

    let private errorNamed (name : string) : UnixError =
        match name with
        | "ENOTCONN" -> UnixError.ENOTCONN
        | "EPIPE" -> UnixError.EPIPE
        | "EINVAL" -> UnixError.EINVAL
        | "EDESTADDRREQ" -> UnixError.EDESTADDRREQ
        | "EAGAIN" -> UnixError.EAGAIN
        | "EMSGSIZE" -> UnixError.EMSGSIZE
        | other -> failwith $"the probe reported %s{other}, which this test does not know"

    let private rowPattern =
        Regex (
            @"^(INET6?|UNIX)\s+(STREAM|DGRAM|SEQPACKET)\s+(\S+)\s+(block|nonblock)\s+(read|write)\s+(\d+):\s*(.*?)\s*$"
        )

    /// Every row of one flavour's sweep whose socket the kernel under the probe
    /// could make; a row whose `socket`, `bind` or `listen` failed is dropped,
    /// since there was no socket to ask.
    let private rowsOf (flavour : SimulatedUnixFlavour) : Row list =
        let resource =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "WoofWare.PosixKernel.Test.socketUnconnectedTransfer.linux.txt"
            | SimulatedUnixFlavour.Darwin -> "WoofWare.PosixKernel.Test.socketUnconnectedTransfer.darwin.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n')
        |> Array.toList
        |> List.choose (fun line ->
            let m = rowPattern.Match line

            if not m.Success then
                None
            elif m.Groups.[7].Value.StartsWith ("no such phase", StringComparison.Ordinal) then
                None
            else

            let domain =
                match m.Groups.[1].Value with
                | "INET" -> SocketDomain.Inet
                | "INET6" -> SocketDomain.Inet6
                | _ -> SocketDomain.Unix

            let kind =
                match m.Groups.[2].Value with
                | "STREAM" -> SocketKind.Stream
                | "DGRAM" -> SocketKind.Datagram
                | _ -> SocketKind.SeqPacket

            let phase =
                match m.Groups.[3].Value with
                | "idle" -> Phase.Idle
                | "bound" -> Phase.Bound
                | "listening-unbound" -> Phase.ListeningUnbound
                | "listening-bound" -> Phase.ListeningBound
                | other -> failwith $"unknown phase %s{other} in: %s{line}"

            let count = UInt64.Parse m.Groups.[6].Value
            let answer = m.Groups.[7].Value

            let call, seen =
                match m.Groups.[5].Value with
                | "read" ->
                    let seen =
                        if answer = "-> 0" then
                            Seen.ReturnedZero
                        elif answer = "sleeps (alarm fired)" then
                            Seen.Sleeps
                        elif answer.StartsWith ("-> -1 ", StringComparison.Ordinal) then
                            Seen.Failed (errorNamed (answer.Substring 6))
                        else
                            failwith $"unparsed read answer in: %s{line}"

                    Call.Read count, seen
                | _ ->
                    let failed =
                        Regex.Match (answer, @"^-> -1 (\S+); SIGPIPE handler ran (\d) time\(s\)(.*)$")

                    if not failed.Success then
                        failwith $"unparsed write answer in: %s{line}"

                    let error = errorNamed failed.Groups.[1].Value

                    let seen =
                        match failed.Groups.[2].Value, failed.Groups.[3].Value with
                        | "0", "" -> Seen.Failed error
                        | "1", ", on the writing thread, before the write returned" -> Seen.FailedRaisingSigPipe error
                        | _ -> failwith $"a SIGPIPE this test does not model in: %s{line}"

                    Call.Write count, seen

            Some
                {
                    Flavour = flavour
                    Domain = domain
                    Kind = kind
                    Phase = phase
                    NonBlocking = m.Groups.[4].Value = "nonblock"
                    Call = call
                    Seen = seen
                }
        )

    let private allRows : Lazy<Row list> =
        lazy (rowsOf SimulatedUnixFlavour.Linux @ rowsOf SimulatedUnixFlavour.Darwin)

    let private platformOf (flavour : SimulatedUnixFlavour) : SimulatedUnixPlatform =
        match flavour with
        | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64
        | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64

    /// Whether the rules' answer is the one the probe saw. A write whose answer
    /// depends on the send buffer agrees with either of the two errnos it can
    /// give.
    let private rulesAgree (row : Row) : bool =
        match row.Call with
        | Call.Read count ->
            match UnconnectedSocketRules.read row.Flavour row.Domain row.Kind row.NonBlocking count, row.Seen with
            | UnconnectedSocketRead.Empty, Seen.ReturnedZero -> true
            | UnconnectedSocketRead.Fails error, Seen.Failed seen -> error = seen
            | UnconnectedSocketRead.Sleeps, Seen.Sleeps -> true
            | _ -> false
        | Call.Write count ->
            match UnconnectedSocketRules.write row.Flavour row.Domain row.Kind count, row.Seen with
            | UnconnectedSocketWrite.Fails error, Seen.Failed seen -> error = seen
            | UnconnectedSocketWrite.Breaks, Seen.FailedRaisingSigPipe UnixError.EPIPE -> true
            | UnconnectedSocketWrite.DependsOnSendBuffer, Seen.Failed UnixError.ENOTCONN
            | UnconnectedSocketWrite.DependsOnSendBuffer, Seen.Failed UnixError.EMSGSIZE -> true
            | _ -> false

    [<Test>]
    let ``the sweep covers every domain and kind each flavour can make, in each phase`` () : unit =
        // So that a probe edit which drops a row family cannot leave the tests
        // below green over nothing.
        let shapes =
            allRows.Value
            |> List.map (fun row -> row.Flavour, row.Domain, row.Kind, row.Phase)
            |> Set.ofList

        let expected =
            [
                for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
                    for domain in [ SocketDomain.Inet ; SocketDomain.Inet6 ; SocketDomain.Unix ] do
                        yield flavour, domain, SocketKind.Stream, Phase.Idle
                        yield flavour, domain, SocketKind.Stream, Phase.Bound
                        yield flavour, domain, SocketKind.Stream, Phase.ListeningBound
                        yield flavour, domain, SocketKind.Datagram, Phase.Idle
                        yield flavour, domain, SocketKind.Datagram, Phase.Bound

                    for domain in [ SocketDomain.Inet ; SocketDomain.Inet6 ] do
                        yield flavour, domain, SocketKind.Stream, Phase.ListeningUnbound

                yield SimulatedUnixFlavour.Linux, SocketDomain.Unix, SocketKind.SeqPacket, Phase.Idle
                yield SimulatedUnixFlavour.Linux, SocketDomain.Unix, SocketKind.SeqPacket, Phase.ListeningBound
            ]
            |> Set.ofList

        Set.difference expected shapes |> shouldEqual Set.empty

    [<Test>]
    let ``the rules answer every measured row as its kernel did`` () : unit =
        let disagreeing =
            allRows.Value
            |> List.filter (rulesAgree >> not)
            |> List.map (fun row -> $"%A{row}")

        disagreeing |> shouldEqual []

    /// The rules' answer for a socket the kernel under the probe gave EMSGSIZE
    /// above a size limit: Linux checks a UDP datagram's 16-bit length before
    /// its destination, so the boundary is between 65535 and 65536.
    [<Test>]
    let ``a Linux UDP write is EMSGSIZE exactly above 65535 bytes`` () : unit =
        let write (count : uint64) =
            UnconnectedSocketRules.write SimulatedUnixFlavour.Linux SocketDomain.Inet SocketKind.Datagram count

        write 65535UL
        |> shouldEqual (UnconnectedSocketWrite.Fails UnixError.EDESTADDRREQ)

        write 65536UL |> shouldEqual (UnconnectedSocketWrite.Fails UnixError.EMSGSIZE)

        write UInt64.MaxValue
        |> shouldEqual (UnconnectedSocketWrite.Fails UnixError.EMSGSIZE)

        UnconnectedSocketRules.write SimulatedUnixFlavour.Darwin SocketDomain.Inet SocketKind.Datagram 65536UL
        |> shouldEqual (UnconnectedSocketWrite.Fails UnixError.EDESTADDRREQ)

    /// Which sockets a failed write left bound, measured by
    /// socket-unconnected-autobind.c: getsockname(2) reported port 0 before a
    /// write of 0, 1 or 65536 bytes and an ephemeral port after it, for these
    /// and no others.
    [<Test>]
    let ``a write binds first exactly where the probe saw a port appear`` () : unit =
        let measured =
            [
                for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
                    for domain in [ SocketDomain.Inet ; SocketDomain.Inet6 ; SocketDomain.Unix ] do
                        for kind in [ SocketKind.Stream ; SocketKind.Datagram ] do
                            let bound =
                                flavour = SimulatedUnixFlavour.Linux
                                && kind = SocketKind.Datagram
                                && domain <> SocketDomain.Unix

                            yield (flavour, domain, kind), bound
            ]

        measured
        |> List.filter (fun ((flavour, domain, kind), bound) ->
            UnconnectedSocketRules.writeBindsFirst flavour domain kind <> bound
        )
        |> shouldEqual []

    /// A Linux UDP socket bound to `127.0.0.1:0`, connected, and dissolved by an
    /// `AF_UNSPEC` connect keeps its address and drops its port; a write then
    /// gives it a port and keeps the address, failing as it would have
    /// (socket-unconnected-autobind.c, the half-bound row).
    [<Test>]
    let ``a failed write gives a half-bound Linux UDP socket a port and keeps its address`` () : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

        let connectTo (endpoint : InternetEndpoint) (family : int) (system : UnixSystem<int, string>) =
            match
                CopyIn.connect fd UserBuffer.Mapped 16u (CopyIn.blob system.Machine.UnixPlatform family endpoint) system
            with
            | Ok (ConnectOutcome.Completed, system) -> system
            | other -> failwith $"connect did not complete: %A{other}"

        let loopbackAt (port : uint16) =
            InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

        let system =
            match
                CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet system.Machine.UnixPlatform (loopbackAt 0us)) system
            with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"bind failed: %A{other}"
            |> connectTo (loopbackAt 9us) SimulatedUnixPlatform.internetAddressFamily

        let socketId =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"not a socket: %A{other}"

        // The `AF_UNSPEC` connect, whose sockaddr carries a family and no
        // endpoint.
        let system =
            match
                UnixConnection.connectSocket
                    socketId
                    false
                    16u
                    (CopyIn.mapped
                        (UnixSystem.platform system)
                        16u
                        (CopyIn.blob (UnixSystem.platform system) 0 (InternetEndpoint.ofParts 0u 0us)))
                    system
            with
            | Ok (ConnectOutcome.Completed, system) -> system
            | other -> failwith $"the dissolve did not complete: %A{other}"

        let before = system.Machine.Sockets.[socketId]
        before.Phase |> shouldEqual SocketPhase.Idle
        before.Binding |> Option.map _.Endpoint |> shouldEqual (Some (loopbackAt 0us))

        match UnixReadWrite.admitWrite system.Leader fd UserBuffer.Mapped 1UL system with
        | Ok (WriteOutcome.Returns (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EDESTADDRREQ), after)) ->
            let low, high = system.Machine.EphemeralPortRange

            match after.Machine.Sockets.[socketId].Binding with
            | Some binding ->
                binding.Endpoint.Address |> shouldEqual InternetEndpoint.LoopbackAddress

                (binding.Endpoint.Port >= low && binding.Endpoint.Port <= high)
                |> shouldEqual true

                binding.LockedAddress |> shouldEqual before.Binding.Value.LockedAddress
                binding.LockedPort |> shouldEqual before.Binding.Value.LockedPort
            | None -> failwith "the write unbound the socket"
        | other -> failwith $"expected EDESTADDRREQ, got %A{other}"

    /// The process the kernel-side rows run in: a leader, 0, and a worker, 1,
    /// which makes every call, as the probe made its writes on a worker; with
    /// `SIGPIPE` caught, so that a write raising it returns.
    let private processOn (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> =
            UnixSystem.initial (platformOf flavour) UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot
            |> Tasks.spawn 1

        { system with
            Process =
                { system.Process with
                    Signals =
                        SignalState.setDisposition
                            Signal.SIGPIPE
                            (SignalDisposition.Catch (SignalCatch.ofHandler "on SIGPIPE"))
                            system.Process.Signals
                }
        }

    let private loopbackAnyPort : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 0us

    /// The socket a row describes, made in the kernel, or `None` where the kernel
    /// cannot make one in that phase.
    let private build (row : Row) : (int * UnixSystem<int, string>) option =
        let fd, system =
            NewSocket.create row.Domain row.Kind SocketProtocol.Default (processOn row.Flavour)

        let bound (system : UnixSystem<int, string>) =
            match row.Domain with
            | SocketDomain.Inet6
            | SocketDomain.Unix -> None
            | SocketDomain.Inet ->
                match
                    CopyIn.bind
                        fd
                        UserBuffer.Mapped
                        16u
                        (CopyIn.inet system.Machine.UnixPlatform loopbackAnyPort)
                        system
                with
                | Ok (BindAnswer.Bound _, system) -> Some system
                | other -> failwith $"the kernel would not bind %A{row}: %A{other}"

        let listening (system : UnixSystem<int, string>) =
            match UnixSocket.listen fd 4 system with
            | Ok (ListenAnswer.Listening _, system) -> Some system
            | Ok (ListenAnswer.Failed _, _)
            | Error _ -> None

        let built =
            match row.Phase with
            | Phase.Idle -> Some system
            | Phase.Bound -> bound system
            | Phase.ListeningUnbound -> listening system
            | Phase.ListeningBound -> bound system |> Option.bind listening

        built
        |> Option.map (fun system ->
            if row.NonBlocking then
                match UnixDescriptor.setNonBlocking fd true system with
                | SetNonBlockingAnswer.Set, system -> fd, system
                | other, _ -> failwith $"the kernel would not set O_NONBLOCK: %A{other}"
            else
                fd, system
        )

    let private sigPipeAtWorker : PendingSignal<int> =
        {
            Signal = Signal.SIGPIPE
            Target = ValueSome 1
        }

    /// Whether `after` is `before` with nothing changed but what a write that
    /// binds first changes: the socket on `fd` bound, if it was not, to the
    /// wildcard and a port from the ephemeral range, locking nothing.
    let private boundAsTheRulesSay
        (row : Row)
        (fd : int)
        (before : UnixSystem<int, string>)
        (after : UnixSystem<int, string>)
        : bool
        =
        let socketId =
            match FileDescriptorRegistry.tryFindTarget fd before.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"fd %d{fd} is not a socket: %A{other}"

        let socketBefore = before.Machine.Sockets.[socketId]
        let socketAfter = after.Machine.Sockets.[socketId]

        if
            socketBefore.Binding.IsNone
            && UnconnectedSocketRules.writeBindsFirst row.Flavour row.Domain row.Kind
        then
            let low, high = before.Machine.EphemeralPortRange

            match socketAfter.Binding with
            | Some binding ->
                binding.Endpoint.Address = InternetEndpoint.WildcardAddress
                && binding.Endpoint.Port >= low
                && binding.Endpoint.Port <= high
                && binding.LockedAddress = None
                && not binding.LockedPort
                && { after with
                       Machine =
                           { after.Machine with
                               NextEphemeralPort = before.Machine.NextEphemeralPort
                               Sockets = before.Machine.Sockets
                           }
                   } = before
            | None -> false
        else
            after = before

    /// Whether the kernel's answer for `row`'s call on `fd` is the one the probe
    /// saw, or the refusal the rules call for where the answer is a sleep, a
    /// binding the kernel cannot record, or depends on the send buffer.
    let private kernelAgrees (row : Row) (fd : int) (system : UnixSystem<int, string>) : bool =
        match row.Call with
        | Call.Read count ->
            match UnixReadWrite.read 1 fd UserBuffer.Mapped count system, row.Seen with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after), Seen.ReturnedZero ->
                bytes.IsEmpty && after = system
            | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), after), Seen.Failed seen ->
                error = seen && after = system
            | Error (ReadRefusal.DatagramSleep _), Seen.Sleeps -> true
            | _ -> false
        | Call.Write count ->
            let admitted = UnixReadWrite.admitWrite 1 fd UserBuffer.Mapped count system

            // `write` without the admission, as a caller holding the bytes may
            // make it, answers the same, but only for a count the caller could
            // hold in one call.
            let written =
                if count <= uint64 (1 <<< 20) then
                    Some (UnixReadWrite.write 1 fd (ImmutableArray.Create<byte> (Array.zeroCreate (int count))) system)
                else
                    None

            let admissionAgrees =
                match admitted, row.Seen with
                | Ok (WriteOutcome.Returns (WriteAdmission.Answered (WriteAnswer.Failed error), after)),
                  Seen.Failed seen -> error = seen && boundAsTheRulesSay row fd system after
                | Error (WriteRefusal.Inet6Binding _), Seen.Failed _ ->
                    row.Domain = SocketDomain.Inet6
                    && row.Phase = Phase.Idle
                    && UnconnectedSocketRules.writeBindsFirst row.Flavour row.Domain row.Kind
                | Ok (WriteOutcome.ReturnsRaising (WriteAdmission.Answered (WriteAnswer.Failed error), signal, after)),
                  Seen.FailedRaisingSigPipe seen ->
                    error = seen
                    && signal = sigPipeAtWorker
                    && SignalState.pending after.Process.Signals = [ sigPipeAtWorker ]
                | Error (WriteRefusal.SendBuffer _), Seen.Failed UnixError.ENOTCONN
                | Error (WriteRefusal.SendBuffer _), Seen.Failed UnixError.EMSGSIZE ->
                    row.Flavour = SimulatedUnixFlavour.Linux
                    && row.Domain = SocketDomain.Unix
                    && row.Kind = SocketKind.Datagram
                | _ -> false

            let writeAgrees =
                match written, admitted with
                | None, _ -> true
                | Some written, admitted ->
                    match written, admitted with
                    | Ok (WriteOutcome.Returns (answer, after)),
                      Ok (WriteOutcome.Returns (WriteAdmission.Answered admittedAnswer, admittedAfter)) ->
                        answer = admittedAnswer && after = admittedAfter
                    | Ok (WriteOutcome.ReturnsRaising (answer, signal, after)),
                      Ok (WriteOutcome.ReturnsRaising (WriteAdmission.Answered admittedAnswer,
                                                       admittedSignal,
                                                       admittedAfter)) ->
                        answer = admittedAnswer && signal = admittedSignal && after = admittedAfter
                    | Error refusal, Error admittedRefusal -> refusal = admittedRefusal
                    | _ -> false

            admissionAgrees && writeAgrees

    [<Test>]
    let ``the kernel answers every measured row it can build as its kernel did`` () : unit =
        let built =
            allRows.Value
            |> List.choose (fun row -> build row |> Option.map (fun built -> row, built))

        // The kernel binds and listens only in AF_INET, so these are what it can
        // build; that it builds all of them is part of the claim.
        let shapes =
            built
            |> List.map (fun (row, _) -> row.Flavour, row.Domain, row.Kind, row.Phase)
            |> Set.ofList

        let expected =
            [
                for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
                    for domain in [ SocketDomain.Inet ; SocketDomain.Inet6 ; SocketDomain.Unix ] do
                        yield flavour, domain, SocketKind.Stream, Phase.Idle
                        yield flavour, domain, SocketKind.Datagram, Phase.Idle

                    yield flavour, SocketDomain.Inet, SocketKind.Stream, Phase.Bound
                    yield flavour, SocketDomain.Inet, SocketKind.Stream, Phase.ListeningUnbound
                    yield flavour, SocketDomain.Inet, SocketKind.Stream, Phase.ListeningBound
                    yield flavour, SocketDomain.Inet, SocketKind.Datagram, Phase.Bound

                yield SimulatedUnixFlavour.Linux, SocketDomain.Unix, SocketKind.SeqPacket, Phase.Idle
            ]
            |> Set.ofList

        Set.difference expected shapes |> shouldEqual Set.empty

        built
        |> List.filter (fun (row, (fd, system)) -> not (kernelAgrees row fd system))
        |> List.map (fun (row, _) -> $"%A{row}")
        |> shouldEqual []

    // ------------------------------------------------------------------
    // The host the suite runs on
    // ------------------------------------------------------------------

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "bind", SetLastError = true)>]
    extern int private hostBind(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "listen", SetLastError = true)>]
    extern int private hostListen(int fd, int backlog)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostRead(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWrite(int fd, byte[] buffer, unativeint count)

    // `fcntl(2)` is variadic, which a P/Invoke cannot call portably, so this goes
    // through the runtime's own fixed-arity wrapper of it.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")>]
    extern int private hostSetNonBlocking(nativeint fd, int isNonBlocking)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "getsockname", SetLastError = true)>]
    extern int private hostGetSockName(int fd, byte[] address, uint32& length)

    [<Test>]
    let ``this host's failed write binds an unbound socket exactly when the rules say`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour

            let disagreeing =
                [
                    for domain in [ SocketDomain.Inet ; SocketDomain.Inet6 ] do
                        for kind in [ SocketKind.Stream ; SocketKind.Datagram ] do
                            let rawDomain, rawKind, protocol =
                                NewSocket.arguments platform domain kind SocketProtocol.Default

                            let fd = hostSocket (rawDomain, rawKind, protocol)

                            if fd >= 0 then
                                try
                                    hostWrite (fd, [| 0uy |], 1un) |> ignore<nativeint>
                                    // sin_port and sin6_port are both at offset 2, in
                                    // network order, on both flavours.
                                    let name = Array.zeroCreate<byte> 128
                                    let mutable length = 128u

                                    if hostGetSockName (fd, name, &length) <> 0 then
                                        failwith $"getsockname failed with errno %d{Marshal.GetLastPInvokeError ()}"

                                    let bound = name.[2] <> 0uy || name.[3] <> 0uy

                                    if bound <> UnconnectedSocketRules.writeBindsFirst flavour domain kind then
                                        yield $"%O{domain} %O{kind}: this host bound it %b{bound}"
                                finally
                                    hostClose fd |> ignore<int>
                ]

            disagreeing |> shouldEqual []
        )

    /// What the host answered: the return value, and the errno if it was -1;
    /// or `None` for an IPv6 row on a host with no IPv6, which is a fact about
    /// the machine rather than its kernel.
    let private onHost (platform : SimulatedUnixPlatform) (row : Row) : (int64 * int) option =
        let domain, kind, protocol =
            NewSocket.arguments platform row.Domain row.Kind SocketProtocol.Default

        let fd = hostSocket (domain, kind, protocol)
        let socketErrno = Marshal.GetLastPInvokeError ()

        let noIpv6 =
            UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) UnixError.EAFNOSUPPORT

        if fd < 0 && row.Domain = SocketDomain.Inet6 && socketErrno = noIpv6 then
            None
        elif fd < 0 then
            failwith $"the host would not make the socket of %A{row}: errno %d{socketErrno}"
        else

        try
            let address = SimulatedUnixPlatform.encodeInternetSockaddr platform loopbackAnyPort

            let bind () =
                if hostBind (fd, address, uint32 address.Length) <> 0 then
                    failwith $"the host would not bind %A{row}: errno %d{Marshal.GetLastPInvokeError ()}"

            let listen () =
                if hostListen (fd, 4) <> 0 then
                    failwith $"the host would not listen on %A{row}: errno %d{Marshal.GetLastPInvokeError ()}"

            match row.Phase with
            | Phase.Idle -> ()
            | Phase.Bound -> bind ()
            | Phase.ListeningUnbound -> listen ()
            | Phase.ListeningBound ->
                bind ()
                listen ()

            if row.NonBlocking && hostSetNonBlocking (nativeint fd, 1) <> 0 then
                failwith $"the host would not set O_NONBLOCK on %A{row}"

            Marshal.SetLastPInvokeError 0

            let returned =
                match row.Call with
                | Call.Read count -> hostRead (fd, Array.zeroCreate (int count), unativeint count)
                | Call.Write count -> hostWrite (fd, Array.zeroCreate (int count), unativeint count)

            let errno = if returned = -1n then Marshal.GetLastPInvokeError () else 0

            Some (int64 returned, errno)
        finally
            hostClose fd |> ignore<int>

    [<Test>]
    let ``this host answers every row the kernel can build as the kernel does`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering platform

            let errno (error : UnixError) : int =
                UnixError.toRawErrnoUnder numbering error

            // The rows of the other flavour's sweep, run here: the measurement
            // says what that kernel answered, and the host says what this one
            // does, so the kernel's rows for this flavour must agree with both.
            let rows =
                allRows.Value
                |> List.filter (fun row -> row.Flavour = flavour)
                |> List.filter (fun row -> (build row).IsSome)
                // A blocking read of an unconnected datagram socket sleeps for
                // ever on the host, and the kernel refuses it.
                |> List.filter (fun row ->
                    match row.Call, row.Kind, row.NonBlocking with
                    | Call.Read count, SocketKind.Datagram, false -> count = 0UL
                    | _ -> true
                )

            // The host's runtime ignores SIGPIPE, so a write that raises it
            // reports only its errno here; the signal is the probe's to report.
            let disagreeing =
                rows
                |> List.choose (fun row ->
                    let expected =
                        match row.Call with
                        | Call.Read count ->
                            match UnconnectedSocketRules.read flavour row.Domain row.Kind row.NonBlocking count with
                            | UnconnectedSocketRead.Empty -> Some (0L, 0)
                            | UnconnectedSocketRead.Fails error -> Some (-1L, errno error)
                            | UnconnectedSocketRead.Sleeps -> None
                        | Call.Write count ->
                            match UnconnectedSocketRules.write flavour row.Domain row.Kind count with
                            | UnconnectedSocketWrite.Fails error -> Some (-1L, errno error)
                            | UnconnectedSocketWrite.Breaks -> Some (-1L, errno UnixError.EPIPE)
                            | UnconnectedSocketWrite.DependsOnSendBuffer -> None

                    match expected with
                    | None -> None
                    | Some expected ->
                        match onHost platform row with
                        | None -> None
                        | Some actual when actual = expected -> None
                        | Some actual -> Some $"%A{row}: the rules say %A{expected}, this host answered %A{actual}"
                )

            disagreeing |> shouldEqual []
        )
