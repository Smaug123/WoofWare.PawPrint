namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `shutdown(2)` on connected loopback TCP sockets, and what each end then
/// sees: sections S, T, R, P, E and F, and the connected rows of U, of
/// `tcp-shutdown.c` (docs/plans/2026-10-08-tcp-shutdown-linger), and the
/// binds of an endpoint an end still holds, by `bind-held-endpoint.c` beside
/// it, measured on Linux 6.18.5 aarch64 and Darwin 27.0, replayed against the
/// host the suite runs on and against this kernel's syscalls, each driven as
/// the probe drives its pair (`ShutdownProbe`). Each host falsifies its own
/// column: Darwin's here, Linux's on CI's x86-64. The kernel is held to both
/// flavours' rows on every host.
///
/// A line marked `~timing` is not compared, and a `~counts` line is compared
/// without its counts. The host ignores `SIGPIPE`, so its writes report no
/// signal, and the probe's `+SIGPIPE` is not compared against it. The listener
/// rows of E are stage 5's, and so are not replayed. This kernel forgets a
/// connection once neither socket refers to it, so the `TIME_WAIT` an end
/// whose FIN was made first leaves once both sockets have closed is
/// unmodelled, and F's lines that bind after both have closed are the
/// host's alone.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestShutdownAgainstHost =

    let private c : ConnectionEnd = ConnectionEnd.Client
    let private p : ConnectionEnd = ConnectionEnd.Server

    // ------------------------------------------------------------------
    // The host
    // ------------------------------------------------------------------

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "bind", SetLastError = true)>]
    extern int private hostBind(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "listen", SetLastError = true)>]
    extern int private hostListen(int fd, int backlog)

    [<DllImport("libc", EntryPoint = "getsockname", SetLastError = true)>]
    extern int private hostGetSockName(int fd, byte[] address, uint32& length)

    [<DllImport("libc", EntryPoint = "getpeername", SetLastError = true)>]
    extern int private hostGetPeerName(int fd, byte[] address, uint32& length)

    [<DllImport("libc", EntryPoint = "connect", SetLastError = true)>]
    extern int private hostConnect(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "accept", SetLastError = true)>]
    extern int private hostAccept(int fd, nativeint address, nativeint length)

    [<DllImport("libc", EntryPoint = "shutdown", SetLastError = true)>]
    extern int private hostShutdown(int fd, int how)

    [<DllImport("libc", EntryPoint = "setsockopt", SetLastError = true)>]
    extern int private hostSetSockOpt(int fd, int level, int optionName, byte[] value, uint32 length)

    [<DllImport("libc", EntryPoint = "getsockopt", SetLastError = true)>]
    extern int private hostGetSockOpt(int fd, int level, int optionName, int& value, uint32& length)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostRead(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWrite(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "poll", SetLastError = true)>]
    extern int private hostPoll(int64[] fds, unativeint count, int timeoutMs)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "pipe", SetLastError = true)>]
    extern int private hostPipe(int[] fds)

    [<DllImport("libc", EntryPoint = "epoll_create1", SetLastError = true)>]
    extern int private hostEpollCreate1(int flags)

    [<DllImport("libc", EntryPoint = "epoll_ctl", SetLastError = true)>]
    extern int private hostEpollCtl(int epfd, int op, int fd, byte[] event)

    [<DllImport("libc", EntryPoint = "epoll_wait", SetLastError = true)>]
    extern int private hostEpollWait(int epfd, byte[] events, int maxEvents, int timeoutMs)

    [<DllImport("libc", EntryPoint = "kqueue", SetLastError = true)>]
    extern int private hostKqueue()

    [<DllImport("libc", EntryPoint = "kevent", SetLastError = true)>]
    extern int private hostKevent(int kq, byte[] changes, int nchanges, byte[] events, int nevents, int64[] timeout)

    // `fcntl(2)` and `ioctl(2)` are variadic, which a P/Invoke cannot call
    // portably, so these go through .NET's own fixed-arity wrappers.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")>]
    extern int private hostSetNonBlocking(nativeint fd, int isNonBlocking)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetBytesAvailable")>]
    extern int private hostBytesAvailable(nativeint fd, int& available)

    [<Literal>]
    let private AF_INET = 2

    [<Literal>]
    let private SOCK_STREAM = 1

    let private lingerOption (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 13
        | SimulatedUnixFlavour.Darwin -> 0x80

    /// What the probe's `rdy` asks `poll` for: IN|OUT|PRI, and RDHUP on Linux.
    let private rdyEvents (flavour : SimulatedUnixFlavour) : int16 =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x2007s
        | SimulatedUnixFlavour.Darwin -> 0x7s

    let private hostBuffer : byte[] = Array.zeroCreate (1 <<< 20)

    /// The size of `struct epoll_event`: packed on x86-64, padded elsewhere.
    let private epollEventSize : int =
        if RuntimeInformation.ProcessArchitecture = Architecture.X64 then
            12
        else
            16

    let private epollEvent (events : uint32) (data : uint64) : byte[] =
        let bytes = Array.zeroCreate<byte> epollEventSize
        BitConverter.GetBytes(events).CopyTo (bytes, 0)
        BitConverter.GetBytes(data).CopyTo (bytes, epollEventSize - 8)
        bytes

    /// The `struct kevent` registering `fd`'s `filter` with `flags`.
    let private keventChange (fd : int) (filter : int16) (flags : uint16) : byte[] =
        let bytes = Array.zeroCreate<byte> 32
        BitConverter.GetBytes(uint64 fd).CopyTo (bytes, 0)
        BitConverter.GetBytes(filter).CopyTo (bytes, 8)
        BitConverter.GetBytes(flags).CopyTo (bytes, 10)
        bytes

    /// Each event in a returned `struct kevent` array: ident, filter, flags,
    /// fflags, data.
    let private keventsOf (bytes : byte[]) (count : int) : (int * int16 * uint16 * uint32 * int64) list =
        [
            for k in 0 .. count - 1 do
                let at = 32 * k

                int (BitConverter.ToUInt64 (bytes, at)),
                BitConverter.ToInt16 (bytes, at + 8),
                BitConverter.ToUInt16 (bytes, at + 10),
                BitConverter.ToUInt32 (bytes, at + 12),
                BitConverter.ToInt64 (bytes, at + 16)
        ]

    let private renderKevent (data : int64) (flags : uint16) (fflags : uint32) : string =
        let eof = if flags &&& KeventFlags.Eof <> 0us then "/EOF" else ""
        $"%d{data}%s{eof}/%d{fflags}"

    /// Each TCP socket in this Linux host's network namespace, whichever process
    /// holds it, as `/proc/net/tcp` and `/proc/net/tcp6` list it: its local
    /// port, its remote port, and its line.
    let private linuxTcpSockets () : (int * int * string) list =
        let portOf (field : string) : int =
            Convert.ToInt32 (field.Substring (field.LastIndexOf ':' + 1), 16)

        [
            for path in [ "/proc/net/tcp" ; "/proc/net/tcp6" ] do
                // A host without IPv6 has no tcp6, and so no IPv6 sockets.
                if path = "/proc/net/tcp" || IO.File.Exists path then
                    for line in IO.File.ReadAllLines path |> Array.skip 1 do
                        let fields = line.Split (' ', StringSplitOptions.RemoveEmptyEntries)
                        portOf fields.[1], portOf fields.[2], $"%s{path}: %s{line.Trim ()}"
        ]

    /// A pair on the host, as the probe's `pair_locked` makes one.
    type private HostPair (flavour : SimulatedUnixFlavour, shape : ShutdownPairShape) =
        let platform = HostPlatform.platformOf flavour

        let check (what : string) (result : int) : unit =
            if result < 0 then
                failwith $"%s{what} failed with errno %d{Marshal.GetLastPInvokeError ()}"

        let loopbackAt (port : int) : byte[] =
            SimulatedUnixPlatform.encodeInternetSockaddr
                platform
                (Ipv4Endpoint.ofParts Ipv4Address.loopback (uint16 port))

        let portOf (fd : int) : int =
            let name = Array.zeroCreate<byte> 16
            let mutable length = 16u
            check "getsockname" (hostGetSockName (fd, name, &length))
            (int name.[2] <<< 8) ||| int name.[3]

        let freePort () : int =
            let t = hostSocket (AF_INET, SOCK_STREAM, 0)
            check "socket" t
            let address = loopbackAt 0
            check "bind" (hostBind (t, address, uint32 address.Length))
            let port = portOf t
            hostClose t |> ignore<int>
            port

        let errorName (errno : int) : string =
            match UnixError.ofRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) errno with
            | Some error -> $"%A{error}"
            | None -> $"errno%d{errno}"

        let ans (result : int64) (errno : int) : string =
            if result < 0L then
                $"-1 %s{errorName errno}"
            else
                string result

        let settle () : unit = Thread.Sleep 30

        let call (f : unit -> int64) : int64 * int =
            Marshal.SetLastPInvokeError 0
            let result = f ()
            result, (if result < 0L then Marshal.GetLastPInvokeError () else 0)

        let setReuseAddress (fd : int) : unit =
            check
                "setsockopt(SO_REUSEADDR)"
                (hostSetSockOpt (
                    fd,
                    SimulatedUnixPlatform.socketOptionLevel platform,
                    SimulatedUnixPlatform.reuseAddressOption platform,
                    BitConverter.GetBytes 1,
                    4u
                ))

        let mutable fds : Map<ConnectionEnd, int> = Map.empty
        let mutable edgePort : int option = None
        // c's port and p's, read once the pair has connected.
        let mutable connection : int * int = 0, 0
        let mutable foreignHolders : string list = []

        // Linux's `connect` chooses a local port that a socket outside the
        // pair already holds, provided their remote endpoints differ, and such
        // a socket refuses a bind of that port whatever the pair's own sockets
        // do. Darwin's chooses only a free port. Read while the refused
        // candidate is still open, so that the holder cannot yet have gone.
        let noteRefusal (port : int) : unit =
            match flavour with
            | SimulatedUnixFlavour.Darwin -> ()
            | SimulatedUnixFlavour.Linux ->
                let cPort, pPort = connection

                let foreign =
                    linuxTcpSockets ()
                    |> List.filter (fun (local, remote, _) ->
                        local = port
                        && not ((local = cPort && remote = pPort) || (local = pPort && remote = cPort))
                    )
                    |> List.map (fun (_, _, line) -> line)

                foreignHolders <- foreignHolders @ foreign

        do
            let listener = hostSocket (AF_INET, SOCK_STREAM, 0)
            check "socket" listener

            let address =
                loopbackAt (
                    match shape with
                    | ShutdownPairShape.ListenerLocked -> freePort ()
                    | ShutdownPairShape.Plain
                    | ShutdownPairShape.ClientLocked
                    | ShutdownPairShape.ClientReuseAddress
                    | ShutdownPairShape.ListenerReuseAddress -> 0
                )

            match shape with
            | ShutdownPairShape.ListenerReuseAddress -> setReuseAddress listener
            | ShutdownPairShape.Plain
            | ShutdownPairShape.ClientLocked
            | ShutdownPairShape.ListenerLocked
            | ShutdownPairShape.ClientReuseAddress -> ()

            check "bind" (hostBind (listener, address, uint32 address.Length))
            check "listen" (hostListen (listener, 4))
            let destination = loopbackAt (portOf listener)
            let client = hostSocket (AF_INET, SOCK_STREAM, 0)
            check "socket" client

            match shape with
            | ShutdownPairShape.ClientLocked ->
                let own = loopbackAt (freePort ())
                check "bind" (hostBind (client, own, uint32 own.Length))
            | ShutdownPairShape.ClientReuseAddress -> setReuseAddress client
            | ShutdownPairShape.Plain
            | ShutdownPairShape.ListenerLocked
            | ShutdownPairShape.ListenerReuseAddress -> ()

            check "connect" (hostConnect (client, destination, uint32 destination.Length))
            settle ()
            let server = hostAccept (listener, 0n, 0n)
            check "accept" server
            hostClose listener |> ignore<int>

            for fd in [ client ; server ] do
                if hostSetNonBlocking (nativeint fd, 1) <> 0 then
                    failwith $"could not make fd %d{fd} non-blocking"

            fds <- Map.ofList [ c, client ; p, server ]
            connection <- portOf client, portOf server

        /// Each socket outside this pair that was found holding a port whose
        /// bind was refused with `EADDRINUSE`, as `/proc/net/tcp` listed it;
        /// empty on Darwin, whose `connect` never shares a port.
        member _.ForeignHolders : string list = foreignHolders

        member _.Fd (e : ConnectionEnd) : int =
            match Map.tryFind e fds with
            | Some fd -> fd
            | None -> failwith $"the %A{e} end has closed"

        member this.Read (e : ConnectionEnd) (count : int) : string =
            let result, errno =
                call (fun () -> int64 (hostRead (this.Fd e, hostBuffer, unativeint count)))

            settle ()
            ans result errno

        member this.WriteAnswer (e : ConnectionEnd) (count : int) : Result<int, string> =
            let result, errno =
                call (fun () -> int64 (hostWrite (this.Fd e, hostBuffer, unativeint count)))

            settle ()

            if result < 0L then
                Error (ans result errno)
            else
                Ok (int result)

        /// Close what is still open, with `SO_LINGER` {1, 0}, as the probe's
        /// `discard` does, and the edge registration's port.
        member _.Discard () : unit =
            let linger = Array.append (BitConverter.GetBytes 1) (BitConverter.GetBytes 0)

            for fd in Map.values fds do
                hostSetSockOpt (fd, SimulatedUnixPlatform.socketOptionLevel platform, lingerOption flavour, linger, 8u)
                |> ignore<int>

                hostClose fd |> ignore<int>

            fds <- Map.empty
            edgePort |> Option.iter (hostClose >> ignore<int>)
            edgePort <- None

        interface IShutdownPair with
            member _.Refused = false
            member this.Read e count = this.Read e count
            member this.WriteAnswer e count = this.WriteAnswer e count

            member this.SoError e =
                let mutable value = 0
                let mutable length = 4u

                check
                    "getsockopt(SO_ERROR)"
                    (hostGetSockOpt (
                        this.Fd e,
                        SimulatedUnixPlatform.socketOptionLevel platform,
                        SimulatedUnixPlatform.socketErrorOption platform,
                        &value,
                        &length
                    ))

                if value = 0 then "0" else errorName value

            member this.Shutdown e how =
                let raw =
                    match how with
                    | TcpShutdownHow.Read -> 0
                    | TcpShutdownHow.Write -> 1
                    | TcpShutdownHow.Both -> 2

                (this :> IShutdownPair).ShutdownRaw e raw

            member this.ShutdownRaw e how =
                let result, errno = call (fun () -> int64 (hostShutdown (this.Fd e, how)))
                settle ()
                ans result errno

            member this.Close e =
                let result, errno = call (fun () -> int64 (hostClose (this.Fd e)))
                fds <- Map.remove e fds
                settle ()
                ans result errno

            member this.Abort e =
                let linger = Array.append (BitConverter.GetBytes 1) (BitConverter.GetBytes 0)

                hostSetSockOpt (
                    this.Fd e,
                    SimulatedUnixPlatform.socketOptionLevel platform,
                    lingerOption flavour,
                    linger,
                    8u
                )
                |> ignore<int>

                (this :> IShutdownPair).Close e

            member this.Fionread e =
                let mutable available = 0

                if hostBytesAvailable (nativeint (this.Fd e), &available) <> 0 then
                    -1
                else
                    available

            member this.Rdy e =
                let fd = this.Fd e
                let entry = [| int64 (uint32 fd) ||| (int64 (uint16 (rdyEvents flavour)) <<< 32) |]
                check "poll" (hostPoll (entry, 1un, 0))
                let revents = int (uint16 (entry.[0] >>> 48))

                let waiters =
                    match flavour with
                    | SimulatedUnixFlavour.Linux ->
                        let ep = hostEpollCreate1 0
                        check "epoll_create1" ep

                        check
                            "epoll_ctl"
                            (hostEpollCtl (ep, 1, fd, epollEvent (uint32 (rdyEvents flavour)) (uint64 fd)))

                        let out = Array.zeroCreate<byte> epollEventSize
                        let n = hostEpollWait (ep, out, 1, 0)
                        hostClose ep |> ignore<int>
                        let events = if n > 0 then BitConverter.ToUInt32 (out, 0) else 0u
                        $"epoll=0x%x{events}"
                    | SimulatedUnixFlavour.Darwin ->
                        let kq = hostKqueue ()
                        check "kqueue" kq

                        let changes =
                            Array.append
                                (keventChange fd KeventFilter.Read KeventFlags.Add)
                                (keventChange fd KeventFilter.Write KeventFlags.Add)

                        check "kevent" (hostKevent (kq, changes, 2, null, 0, null))
                        let out = Array.zeroCreate<byte> 64
                        let n = hostKevent (kq, null, 0, out, 2, [| 0L ; 0L |])
                        hostClose kq |> ignore<int>
                        let events = keventsOf out (max n 0)

                        let render (filter : int16) =
                            events
                            |> List.tryFind (fun (_, f, _, _, _) -> f = filter)
                            |> Option.map (fun (_, _, flags, fflags, data) -> renderKevent data flags fflags)
                            |> Option.defaultValue "-"

                        $"kq-read=%s{render KeventFilter.Read} kq-write=%s{render KeventFilter.Write}"

                $"poll=0x%x{revents} %s{waiters} fionread=%d{(this :> IShutdownPair).Fionread e}"

            member this.Fill e =
                let rec go (total : int64) (dry : int) =
                    if dry >= 3 || total >= (64L <<< 20) then
                        total
                    else
                        let result, errno =
                            call (fun () -> int64 (hostWrite (this.Fd e, hostBuffer, 1048576un)))

                        if result > 0L then
                            go (total + result) 0
                        elif
                            errno = UnixError.toRawErrnoUnder
                                (SimulatedUnixPlatform.rawErrnoNumbering platform)
                                UnixError.EAGAIN
                        then
                            settle ()
                            go total (dry + 1)
                        else
                            total

                go 0L 0

            member this.Drain e =
                let eagain =
                    UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) UnixError.EAGAIN

                let rec go (total : int64) (dry : int) =
                    let result, errno =
                        call (fun () -> int64 (hostRead (this.Fd e, hostBuffer, 1048576un)))

                    if result > 0L then
                        go (total + result) 0
                    elif result < 0L && errno = eagain && dry < 3 then
                        settle ()
                        go total (dry + 1)
                    else
                        settle ()
                        total, ans result errno

                go 0L 0

            member _.Settle () = settle ()

            member this.RegisterEdges ends =
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    let ep = hostEpollCreate1 0
                    check "epoll_create1" ep

                    for e in ends do
                        let fd = this.Fd e
                        check "epoll_ctl" (hostEpollCtl (ep, 1, fd, epollEvent 0x80002005u (uint64 fd)))

                    edgePort <- Some ep
                | SimulatedUnixFlavour.Darwin ->
                    let kq = hostKqueue ()
                    check "kqueue" kq
                    let flags = KeventFlags.Add ||| KeventFlags.Clear

                    let changes =
                        ends
                        |> List.collect (fun e ->
                            [
                                keventChange (this.Fd e) KeventFilter.Read flags
                                keventChange (this.Fd e) KeventFilter.Write flags
                            ]
                        )

                    check "kevent" (hostKevent (kq, Array.concat changes, changes.Length, null, 0, null))
                    edgePort <- Some kq

            member _.Edges () =
                let port =
                    match edgePort with
                    | Some port -> port
                    | None -> failwith "no edge registration"

                let cFd = Map.tryFind c fds |> Option.defaultValue -1

                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    let out = Array.zeroCreate<byte> (4 * epollEventSize)
                    let n = hostEpollWait (port, out, 4, 0)

                    let events =
                        [
                            for k in 0 .. n - 1 do
                                let at = k * epollEventSize

                                int (BitConverter.ToUInt64 (out, at + epollEventSize - 8)),
                                BitConverter.ToUInt32 (out, at)
                        ]

                    let of' (isC : bool) =
                        events
                        |> List.tryFind (fun (fd, _) -> (fd = cFd) = isC)
                        |> Option.map (fun (_, e) -> $"0x%x{e}")
                        |> Option.defaultValue "-"

                    $"c=%s{of' true} p=%s{of' false}"
                | SimulatedUnixFlavour.Darwin ->
                    let out = Array.zeroCreate<byte> (4 * 32)
                    let n = hostKevent (port, null, 0, out, 4, [| 0L ; 0L |])

                    match keventsOf out (max n 0) with
                    | [] -> "-"
                    | events ->
                        events
                        |> List.map (fun (fd, filter, flags, fflags, data) ->
                            let who = if fd = cFd then "c" else "p"
                            let what = if filter = KeventFilter.Read then "read" else "write"
                            $"%s{who}-%s{what}=%s{renderKevent data flags fflags}"
                        )
                        |> String.concat " "

            member this.PortOf e = portOf (this.Fd e)

            member _.BindFree port =
                let n = hostSocket (AF_INET, SOCK_STREAM, 0)
                check "socket" n
                let address = loopbackAt port

                let result, errno =
                    call (fun () -> int64 (hostBind (n, address, uint32 address.Length)))

                if result < 0L && errorName errno = "EADDRINUSE" then
                    noteRefusal port

                hostClose n |> ignore<int>

                if result = 0L then "bind-ok"
                elif errorName errno = "EADDRINUSE" then "bind-EADDRINUSE"
                else ans result errno

            member _.Candidate reuse wildcard port =
                let n = hostSocket (AF_INET, SOCK_STREAM, 0)
                check "socket" n

                if reuse then
                    setReuseAddress n

                let address =
                    SimulatedUnixPlatform.encodeInternetSockaddr
                        platform
                        (Ipv4Endpoint.ofParts (if wildcard then Ipv4Address.any else Ipv4Address.loopback) (uint16 port))

                let bound, bindErrno =
                    call (fun () -> int64 (hostBind (n, address, uint32 address.Length)))

                if bound < 0L && errorName bindErrno = "EADDRINUSE" then
                    noteRefusal port

                let answer =
                    if bound < 0L then
                        $"bind-%s{errorName bindErrno}"
                    else
                        let listened, listenErrno = call (fun () -> int64 (hostListen (n, 4)))

                        if listened < 0L then
                            $"bind-ok listen-%s{errorName listenErrno}"
                        else
                            "bind-ok listen-ok"

                hostClose n |> ignore<int>
                answer

            member this.ReuseAddress e =
                let mutable value = 0
                let mutable length = 4u

                check
                    "getsockopt(SO_REUSEADDR)"
                    (hostGetSockOpt (
                        this.Fd e,
                        SimulatedUnixPlatform.socketOptionLevel platform,
                        SimulatedUnixPlatform.reuseAddressOption platform,
                        &value,
                        &length
                    ))

                if value <> 0 then 1 else 0

            member this.PeerName e =
                let name = Array.zeroCreate<byte> 16
                let mutable length = 16u
                Marshal.SetLastPInvokeError 0
                let result = hostGetPeerName (this.Fd e, name, &length)

                if result = 0 then
                    "ok"
                else
                    errorName (Marshal.GetLastPInvokeError ())

            member _.KeepsTimeWaitOnceBothClosed = true

    /// How many times `hostReplay` runs a scenario whose bind another socket
    /// refused before it fails.
    let private hostScenarioAttempts : int = 5

    /// Each scenario on a pair of its own, `concurrently` or one at a time; their
    /// lines in order. A scenario in which a pair's bind was refused while a
    /// socket outside that pair held the port (`HostPair.ForeignHolders`) is run
    /// again on fresh pairs, since that refusal is the other socket's and not
    /// the answer the scenario measures; this fails once
    /// `hostScenarioAttempts` runs have each been refused so. A foreign
    /// socket can only refuse a bind that would otherwise have succeeded, so
    /// a line that survives is the pairs' own answer, except where a foreign
    /// holder came and went in the moment between the refusal and the
    /// listing of sockets, which leaves the refusal in place.
    let private hostReplay
        (flavour : SimulatedUnixFlavour)
        (concurrently : bool)
        (scenarios : ShutdownProbe.Scenario list)
        : ShutdownLine list
        =
        let rec run (attempt : int) (scenario : ShutdownProbe.Scenario) : ShutdownLine list =
            let mutable made : HostPair list = []

            let pairOf (shape : ShutdownPairShape) : IShutdownPair =
                let pair = new HostPair (flavour, shape)
                made <- pair :: made
                pair :> IShutdownPair

            let lines =
                try
                    scenario pairOf
                finally
                    for pair in made do
                        pair.Discard ()

            match made |> List.collect (fun pair -> pair.ForeignHolders) with
            | [] -> lines
            | _ when attempt < hostScenarioAttempts -> run (attempt + 1) scenario
            | holders ->
                let listed = String.concat "\n" holders

                failwith
                    $"in each of %d{hostScenarioAttempts} runs of a scenario, another socket held a port whose bind was refused; in the last:\n%s{listed}"

        let scenarios = List.toArray scenarios

        (if concurrently then
             Array.Parallel.map (run 1) scenarios
         else
             Array.map (run 1) scenarios)
        |> List.concat

    let private withoutSigPipe (text : string) : string = text.Replace ("+SIGPIPE", "")

    let private hostDisagreementsOf
        (probe : ShutdownProbe.Probe)
        (section : string)
        (concurrently : bool)
        (scenarios : ShutdownProbe.Scenario list)
        : string list
        =
        let mutable found = []

        HostPlatform.onUnixHost (fun flavour ->
            found <-
                hostReplay flavour concurrently scenarios
                |> ShutdownProbe.disagreements probe flavour section withoutSigPipe
        )

        found

    let private hostDisagreements = hostDisagreementsOf ShutdownProbe.Probe.Shutdown

    [<Test>]
    let ``this host answers shutdown, and what each end then sees, as its flavour was measured (section S)`` () =
        hostDisagreements "S" true ShutdownProbe.sectionS |> shouldEqual []

    [<Test>]
    let ``this host answers shutdown twice, and after the peer's FIN or reset, as measured (section T)`` () =
        hostDisagreements "T" true ShutdownProbe.sectionT |> shouldEqual []

    [<Test>]
    let ``this host answers bytes arriving after the receive side is shut as measured (section R)`` () =
        hostDisagreements "R" true ShutdownProbe.sectionR |> shouldEqual []

    [<Test>]
    let ``this host answers a close after shutdown as measured (section P)`` () =
        hostDisagreements "P" true ShutdownProbe.sectionP |> shouldEqual []

    [<Test>]
    let ``this host reports edges after shutdown as measured (section E)`` () =
        hostDisagreements "E" true ShutdownProbe.sectionE |> shouldEqual []

    // One scenario at a time: a scenario frees an endpoint and then binds it,
    // which another's connect could take in between.
    [<Test>]
    let ``this host frees an endpoint by which FIN came first, as measured (section F)`` () =
        hostDisagreements "F" false ShutdownProbe.sectionF |> shouldEqual []

    // One scenario at a time, as for section F.
    [<Test>]
    let ``this host answers a bind of an endpoint an end still holds as measured (bind-held-endpoint.c)`` () =
        hostDisagreementsOf ShutdownProbe.Probe.HeldEndpoint "H" false ShutdownProbe.sectionH
        |> shouldEqual []

    /// Section H's `c released closed` row, reduced to one candidate: `c`
    /// releases its endpoint, and a fresh socket binds it. On the attempts
    /// `foreignOn` names, a listener outside the pair holds the port across
    /// that bind, as another test's socket can. Each run of the scenario is
    /// counted in `attempts`.
    let private releasedThenBound
        (flavour : SimulatedUnixFlavour)
        (foreignOn : int -> bool)
        (attempts : int ref)
        : ShutdownProbe.Scenario
        =
        fun pairOf ->
            attempts.Value <- attempts.Value + 1
            let s = ShutdownRecorder (pairOf ShutdownPairShape.Plain)
            let port = s.PortOf c
            s.Shutdown p TcpShutdownHow.Write |> ignore<string>
            s.Close c |> ignore<string>

            let foreign =
                if foreignOn attempts.Value then
                    let fd = hostSocket (AF_INET, SOCK_STREAM, 0)

                    let address =
                        SimulatedUnixPlatform.encodeInternetSockaddr
                            (HostPlatform.platformOf flavour)
                            (Ipv4Endpoint.ofParts Ipv4Address.loopback (uint16 port))

                    if
                        fd < 0
                        || hostBind (fd, address, uint32 address.Length) <> 0
                        || hostListen (fd, 4) <> 0
                    then
                        failwith $"the foreign listener could not take port %d{port}"

                    Some fd
                else
                    None

            s.Emit (s.Candidate false false port)
            foreign |> Option.iter (hostClose >> ignore<int>)
            s.Lines

    /// Only Linux's `connect` chooses a local port that another socket holds,
    /// so only there is a foreign holder looked for.
    let private onLinuxHost (action : unit -> unit) : unit =
        HostPlatform.onUnixHost (fun flavour ->
            match flavour with
            | SimulatedUnixFlavour.Linux -> action ()
            | SimulatedUnixFlavour.Darwin -> Assert.Ignore "a foreign holder is looked for on Linux only"
        )

    [<Test>]
    let ``a host scenario whose bind another socket refused is run again`` () =
        onLinuxHost (fun () ->
            let attempts = ref 0

            hostReplay
                SimulatedUnixFlavour.Linux
                false
                [ releasedThenBound SimulatedUnixFlavour.Linux ((=) 1) attempts ]
            |> shouldEqual [ ShutdownLine.Line "bind-ok listen-ok" ]

            attempts.Value |> shouldEqual 2
        )

    [<Test>]
    let ``a host scenario whose binds another socket always refuses fails the replay`` () =
        onLinuxHost (fun () ->
            let attempts = ref 0

            let error =
                Assert.Throws<exn> (fun () ->
                    hostReplay
                        SimulatedUnixFlavour.Linux
                        false
                        [ releasedThenBound SimulatedUnixFlavour.Linux (fun _ -> true) attempts ]
                    |> ignore<ShutdownLine list>
                )

            error.Message |> shouldContainText "another socket"
            attempts.Value |> shouldEqual hostScenarioAttempts
        )

    // `c` holds its endpoint in TIME_WAIT, so the bind's refusal is the
    // pair's own, and the scenario runs once.
    [<Test>]
    let ``a host scenario whose bind the pair itself refused is not run again`` () =
        onLinuxHost (fun () ->
            let attempts = ref 0

            let scenario : ShutdownProbe.Scenario =
                fun pairOf ->
                    attempts.Value <- attempts.Value + 1
                    let s = ShutdownRecorder (pairOf ShutdownPairShape.Plain)
                    let port = s.PortOf c
                    s.Close c |> ignore<string>
                    s.Shutdown p TcpShutdownHow.Write |> ignore<string>
                    s.Emit (s.Candidate false false port)
                    s.Lines

            hostReplay SimulatedUnixFlavour.Linux false [ scenario ]
            |> shouldEqual [ ShutdownLine.Line "bind-EADDRINUSE" ]

            attempts.Value |> shouldEqual 1
        )

    // ------------------------------------------------------------------
    // Section U: sockets without a connection, and a `how` out of range
    // ------------------------------------------------------------------

    /// The calls section U makes, on one side or the other. Every answer is
    /// rendered as the probe prints it; `Refused` stands for a call the side
    /// will not answer.
    type private UWorld =
        {
            /// A fresh stream socket.
            Stream : unit -> int
            Datagram : unit -> int
            /// A descriptor that names no socket.
            NotSocket : unit -> int
            /// `bind` to the loopback address and port 0.
            BindAny : int -> unit
            Shutdown : int -> int -> ShutdownLine
            Close : int -> unit
            /// A stream socket whose blocking connect was refused, and that
            /// connect's answer.
            Refused : unit -> int * string
            /// The section's `connect-unreported` row for `how` (`None` for
            /// the control, which makes no shutdown).
            Unreported : int option -> ShutdownLine
            /// The section's `fresh-then` and `udp-then` rows for `how`.
            Then : int -> ShutdownLine list
            /// `shutdown` of a connected socket with `how`.
            ConnectedShutdown : int -> ShutdownLine
        }

    let private rawHowName (how : int) : string =
        match how with
        | 0 -> "RD"
        | 1 -> "WR"
        | 2 -> "RDWR"
        | other -> $"how=%d{other}"

    let private prefixed (prefix : string) (line : ShutdownLine) : ShutdownLine =
        match line with
        | ShutdownLine.Line answer -> ShutdownLine.Line (prefix + answer)
        | other -> other

    /// Every line of section U, as the probe prints them; the listener's rows
    /// are stage 5's.
    let private sectionU (w : UWorld) : ShutdownLine list =
        [
            for how in [ 0 ; 1 ; 2 ] do
                let hn = rawHowName how
                let f = w.Stream ()
                prefixed $"U\tfresh\t%s{hn}\t" (w.Shutdown f how)
                w.Close f
                let f = w.Stream ()
                w.BindAny f
                prefixed $"U\tbound\t%s{hn}\t" (w.Shutdown f how)
                w.Close f
                let u = w.Datagram ()
                prefixed $"U\tudp-unconnected\t%s{hn}\t" (w.Shutdown u how)
                w.Close u
                let n = w.NotSocket ()
                prefixed $"U\tnot-socket\t%s{hn}\t" (w.Shutdown n how)
                w.Close n
                prefixed $"U\tclosed-fd\t%s{hn}\t" (w.Shutdown n how)
            for _ in 1 .. 3 * 4 do
                ShutdownLine.Elsewhere
            for how in [ 0 ; 1 ; 2 ] do
                let f, answer = w.Refused ()
                prefixed $"U\trefused(%s{answer})\t%s{rawHowName how}\t" (w.Shutdown f how)
                w.Close f
            for how in [ 3 ; -1 ] do
                let f = w.Stream ()
                prefixed $"U\tfresh\thow=%d{how}\t" (w.Shutdown f how)
                w.Close f
                let n = w.NotSocket ()
                prefixed $"U\tnot-socket\thow=%d{how}\t" (w.Shutdown n how)
                w.Close n
            w.Unreported None
            for how in [ 0 ; 1 ; 2 ] do
                w.Unreported (Some how)
            for how in [ 0 ; 1 ; 2 ] do
                yield! w.Then how
            for how in [ 3 ; -1 ] do
                prefixed $"U\tconnected\thow=%d{how}\t" (w.ConnectedShutdown how)
        ]

    /// Section U's calls on the host.
    let private hostU (flavour : SimulatedUnixFlavour) : UWorld =
        let platform = HostPlatform.platformOf flavour

        let check (what : string) (result : int) : unit =
            if result < 0 then
                failwith $"%s{what} failed with errno %d{Marshal.GetLastPInvokeError ()}"

        let errorName (errno : int) : string =
            match UnixError.ofRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) errno with
            | Some error -> $"%A{error}"
            | None -> $"errno%d{errno}"

        let call (f : unit -> int) : string =
            Marshal.SetLastPInvokeError 0
            let result = f ()
            let errno = Marshal.GetLastPInvokeError ()
            Thread.Sleep 30

            if result < 0 then
                $"-1 %s{errorName errno}"
            else
                string result

        let loopbackAt (port : int) : byte[] =
            SimulatedUnixPlatform.encodeInternetSockaddr
                platform
                (Ipv4Endpoint.ofParts Ipv4Address.loopback (uint16 port))

        let stream () =
            let fd = hostSocket (AF_INET, SOCK_STREAM, 0)
            check "socket" fd
            fd

        let listening () : int * byte[] =
            let l = stream ()
            let any = loopbackAt 0
            check "bind" (hostBind (l, any, 16u))
            check "listen" (hostListen (l, 4))
            let name = Array.zeroCreate<byte> 16
            let mutable length = 16u
            check "getsockname" (hostGetSockName (l, name, &length))
            l, name

        let rdy (fd : int) : string =
            let entry = [| int64 (uint32 fd) ||| (int64 (uint16 (rdyEvents flavour)) <<< 32) |]
            check "poll" (hostPoll (entry, 1un, 0))
            let revents = int (uint16 (entry.[0] >>> 48))

            let waiters =
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    let ep = hostEpollCreate1 0
                    check "epoll_create1" ep
                    check "epoll_ctl" (hostEpollCtl (ep, 1, fd, epollEvent (uint32 (rdyEvents flavour)) (uint64 fd)))
                    let out = Array.zeroCreate<byte> epollEventSize
                    let n = hostEpollWait (ep, out, 1, 0)
                    hostClose ep |> ignore<int>
                    $"epoll=0x%x{(if n > 0 then BitConverter.ToUInt32 (out, 0) else 0u)}"
                | SimulatedUnixFlavour.Darwin ->
                    let kq = hostKqueue ()
                    check "kqueue" kq

                    let changes =
                        Array.append
                            (keventChange fd KeventFilter.Read KeventFlags.Add)
                            (keventChange fd KeventFilter.Write KeventFlags.Add)

                    check "kevent" (hostKevent (kq, changes, 2, null, 0, null))
                    let out = Array.zeroCreate<byte> 64
                    let n = hostKevent (kq, null, 0, out, 2, [| 0L ; 0L |])
                    hostClose kq |> ignore<int>
                    let events = keventsOf out (max n 0)

                    let render (filter : int16) =
                        events
                        |> List.tryFind (fun (_, f, _, _, _) -> f = filter)
                        |> Option.map (fun (_, _, flags, fflags, data) -> renderKevent data flags fflags)
                        |> Option.defaultValue "-"

                    $"kq-read=%s{render KeventFilter.Read} kq-write=%s{render KeventFilter.Write}"

            let mutable available = 0
            hostBytesAvailable (nativeint fd, &available) |> ignore<int>
            $"poll=0x%x{revents} %s{waiters} fionread=%d{available}"

        let shutdownOf (fd : int) (how : int) : string = call (fun () -> hostShutdown (fd, how))

        {
            Stream = stream
            Datagram =
                fun () ->
                    let fd = hostSocket (AF_INET, 2, 0)
                    check "socket" fd
                    fd
            NotSocket =
                fun () ->
                    let fds = Array.zeroCreate<int> 2

                    match hostPipe fds with
                    | 0 ->
                        hostClose fds.[1] |> ignore<int>
                        fds.[0]
                    | _ -> failwith "pipe failed"
            BindAny =
                fun fd ->
                    let any = loopbackAt 0
                    check "bind" (hostBind (fd, any, 16u))
            Shutdown = fun fd how -> ShutdownLine.Line (shutdownOf fd how)
            Close = fun fd -> hostClose fd |> ignore<int>
            Refused =
                fun () ->
                    let l, name = listening ()
                    hostClose l |> ignore<int>
                    let f = stream ()
                    f, call (fun () -> hostConnect (f, name, 16u))
            Unreported =
                fun how ->
                    let l, name = listening ()
                    let f = stream ()
                    hostSetNonBlocking (nativeint f, 1) |> ignore<int>
                    let first = call (fun () -> hostConnect (f, name, 16u))

                    let sh =
                        match how with
                        | None -> "-"
                        | Some how -> shutdownOf f how

                    let again = call (fun () -> hostConnect (f, name, 16u))
                    hostClose f |> ignore<int>
                    hostClose l |> ignore<int>

                    let name =
                        match how with
                        | None -> "none"
                        | Some how -> rawHowName how

                    ShutdownLine.Line $"U\tconnect-unreported(%s{first})\t%s{name}\tshutdown=%s{sh} connect=%s{again}"
            Then =
                fun how ->
                    let hn = rawHowName how
                    let l, name = listening ()
                    let f = stream ()
                    hostSetNonBlocking (nativeint f, 1) |> ignore<int>
                    let before = rdy f
                    let sh = shutdownOf f how
                    let after = rdy f
                    hostSetNonBlocking (nativeint f, 0) |> ignore<int>
                    let connected = call (fun () -> hostConnect (f, name, 16u))
                    hostSetNonBlocking (nativeint f, 1) |> ignore<int>
                    let peer = hostAccept (l, 0n, 0n)
                    check "accept" peer
                    let written = call (fun () -> int (hostWrite (f, hostBuffer, 1un)))
                    hostWrite (peer, hostBuffer, 1un) |> ignore<nativeint>
                    Thread.Sleep 30
                    let read = call (fun () -> int (hostRead (f, hostBuffer, 4096un)))
                    let rc = rdy f
                    hostClose f |> ignore<int>
                    hostClose peer |> ignore<int>
                    hostClose l |> ignore<int>
                    let u = hostSocket (AF_INET, 2, 0)
                    check "socket" u
                    let ub = rdy u
                    let us = shutdownOf u how
                    let ua = rdy u
                    hostClose u |> ignore<int>

                    [
                        ShutdownLine.Line
                            $"U\tfresh-then\t%s{hn}\tshutdown=%s{sh}\trdy before\t%s{before}\trdy after\t%s{after}"
                        ShutdownLine.Line
                            $"U\tfresh-then\t%s{hn}\tconnect=%s{connected} write1=%s{written} read(after p wrote 1)=%s{read}\trdy\t%s{rc}"
                        ShutdownLine.Line $"U\tudp-then\t%s{hn}\tshutdown=%s{us}\trdy before\t%s{ub}\trdy after\t%s{ua}"
                    ]
            ConnectedShutdown =
                fun how ->
                    let pair = new HostPair (flavour, ShutdownPairShape.Plain)

                    try
                        ShutdownLine.Line ((pair :> IShutdownPair).ShutdownRaw ConnectionEnd.Client how)
                    finally
                        pair.Discard ()
        }

    [<Test>]
    let ``this host answers shutdown on sockets without a connection, and a bad how, as measured (section U)`` () =
        HostPlatform.onUnixHost (fun flavour ->
            sectionU (hostU flavour)
            |> ShutdownProbe.disagreements ShutdownProbe.Probe.Shutdown flavour "U" withoutSigPipe
            |> shouldEqual []
        )

    // ------------------------------------------------------------------
    // The kernel
    // ------------------------------------------------------------------

    let private payload : byte[] = Array.init (1 <<< 20) (fun i -> byte (i * 31 + 7))

    /// A booted system of `platform`, with task 1 for `accept` and `SIGPIPE`
    /// ignored, as the test host and the .NET runtime ignore it.
    let private kernelSystem (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system =
            UnixSystem.initial<int, string> platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> Tasks.ensure 1

        { system with
            Process =
                { system.Process with
                    Signals = SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore system.Process.Signals
                }
        }

    let private kernelClose (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.tryClose fd system with
        | Ok (SyscallAnswer.Completed 0L, system) -> system
        | other -> failwith $"closing fd %d{fd}: %A{other}"

    let private loopback (platform : SimulatedUnixPlatform) (port : int) : byte[] =
        CopyIn.inet platform (Ipv4Endpoint.ofParts Ipv4Address.loopback (uint16 port))

    let private kernelBind (fd : int) (port : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match CopyIn.bind fd UserBuffer.Mapped 16u (loopback system.Machine.UnixPlatform port) system with
        | Ok (BindAnswer.Bound _, system) -> system
        | other -> failwith $"bind: %A{other}"

    let private kernelSocketOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) -> socketId
        | other -> failwith $"fd %d{fd} names %A{other}"

    /// The port a socket the kernel made is bound to.
    let private kernelPortOf (fd : int) (system : UnixSystem<int, string>) : int =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) ->
            match (UnixMachineState.socket socketId system.Machine).Ipv4Binding with
            | Some binding -> int binding.Endpoint.Port
            | None -> failwith $"fd %d{fd} is unbound"
        | other -> failwith $"fd %d{fd} names %A{other}"

    /// The explicit ports `ShutdownPairShape.ClientLocked` and
    /// `ListenerLocked` bind, outside the ephemeral range.
    let private lockedPort : int = 5100

    /// A pair in the kernel, as `HostPair` makes one on the host.
    type private KernelPair (platform : SimulatedUnixPlatform, shape : ShutdownPairShape) =
        let flavour = SimulatedUnixPlatform.flavour platform
        let mutable system = kernelSystem platform
        let mutable refused = false
        let mutable fds : Map<ConnectionEnd, int> = Map.empty
        let mutable edgePort : int option = None

        let rendered (error : UnixError) : string = $"-1 %A{error}"

        do
            let listener, s =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let s =
                match shape with
                | ShutdownPairShape.ListenerReuseAddress -> ReuseAddress.set true listener s
                | ShutdownPairShape.Plain
                | ShutdownPairShape.ClientLocked
                | ShutdownPairShape.ListenerLocked
                | ShutdownPairShape.ClientReuseAddress -> s

            let s =
                kernelBind
                    listener
                    (match shape with
                     | ShutdownPairShape.ListenerLocked -> lockedPort
                     | ShutdownPairShape.Plain
                     | ShutdownPairShape.ClientLocked
                     | ShutdownPairShape.ClientReuseAddress
                     | ShutdownPairShape.ListenerReuseAddress -> 0)
                    s

            let s =
                match UnixSocket.tryListen listener 4 s with
                | Ok (ListenAnswer.Listening _, s) -> s
                | other -> failwith $"listen: %A{other}"

            let port = kernelPortOf listener s

            let client, s =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp s

            let s =
                match shape with
                | ShutdownPairShape.ClientLocked -> kernelBind client lockedPort s
                | ShutdownPairShape.ClientReuseAddress -> ReuseAddress.set true client s
                | ShutdownPairShape.Plain
                | ShutdownPairShape.ListenerLocked
                | ShutdownPairShape.ListenerReuseAddress -> s

            let s =
                match CopyIn.connect client UserBuffer.Mapped 16u (loopback platform port) s with
                | Ok (ConnectOutcome.Completed, s) -> s
                | other -> failwith $"connect: %A{other}"

            let server, s =
                match UnixConnection.tryAccept 1 listener UserBuffer.Mapped 16u s with
                | Ok (AcceptOutcome.Accepted (accepted, _, _), s) -> accepted, s
                | other -> failwith $"accept: %A{other}"

            let s = kernelClose listener s
            let _, s = UnixDescriptor.setNonBlocking client true s
            let _, s = UnixDescriptor.setNonBlocking server true s
            system <- s
            fds <- Map.ofList [ c, client ; p, server ]

        member _.System : UnixSystem<int, string> = system

        member _.Fd (e : ConnectionEnd) : int =
            match Map.tryFind e fds with
            | Some fd -> fd
            | None -> failwith $"the %A{e} end has closed"

        member this.Read (e : ConnectionEnd) (count : int) : string =
            match ReadOutcomes.read (this.Fd e) UserBuffer.Mapped (uint64 count) system with
            | Ok (ReadAnswer.Completed bytes, after) ->
                system <- after
                string bytes.Length
            | Ok (ReadAnswer.Failed error, after) ->
                system <- after
                rendered error
            | other -> failwith $"a read of %d{count}: %A{other}"

        member this.WriteAnswer (e : ConnectionEnd) (count : int) : Result<int, string> =
            let bytes = ImmutableArray.Create (payload, 0, count)

            match WriteOutcomes.admitThenWrite system.Leader (this.Fd e) UserBuffer.Mapped bytes system with
            | Ok (WriteOutcome.Returns (WriteAnswer.Completed written, after)) ->
                system <- after
                Ok (int written)
            | Ok (WriteOutcome.Returns (WriteAnswer.Failed error, after)) ->
                system <- after
                Error (rendered error)
            | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed error, entry, after)) when
                entry.Signal = Signal.SIGPIPE
                ->
                system <- after
                Error (rendered error + "+SIGPIPE")
            | other -> failwith $"a write of %d{count}: %A{other}"

        interface IShutdownPair with
            member _.Refused = refused
            member this.Read e count = this.Read e count
            member this.WriteAnswer e count = this.WriteAnswer e count

            member this.SoError e =
                let level = SimulatedUnixPlatform.socketOptionLevel platform
                let optionName = SimulatedUnixPlatform.socketErrorOption platform

                match
                    UnixSocket.tryGetsockopt
                        (this.Fd e)
                        level
                        optionName
                        UserBuffer.Mapped
                        UserBuffer.Mapped
                        (Some 4u)
                        system
                with
                | Ok (GetSockOptAnswer.Reported (OptionValue.Int 0), after) ->
                    system <- after
                    "0"
                | Ok (GetSockOptAnswer.Reported (OptionValue.Int raw), after) ->
                    system <- after

                    match UnixError.ofRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) raw with
                    | Some error -> $"%A{error}"
                    | None -> failwith $"SO_ERROR reported %d{raw}"
                | other -> failwith $"getsockopt(SO_ERROR): %A{other}"

            member this.Shutdown e how =
                let raw =
                    match how with
                    | TcpShutdownHow.Read -> 0
                    | TcpShutdownHow.Write -> 1
                    | TcpShutdownHow.Both -> 2

                (this :> IShutdownPair).ShutdownRaw e raw

            member this.ShutdownRaw e how =
                match UnixConnection.tryShutdown (this.Fd e) how system with
                | Ok (answer, after) ->
                    UnixSystem.checkInvariants after |> shouldEqual []
                    system <- after

                    match answer with
                    | ShutdownAnswer.Shut -> "0"
                    | ShutdownAnswer.Failed error -> rendered error
                | Error (ShutdownRefusal.ReceiveShutBeforeUnsentBytes _) ->
                    refused <- true
                    ""
                | Error refusal -> failwith $"shutdown refused: %s{ShutdownRefusal.describe refusal}"

            member this.Close e =
                match UnixDescriptor.tryClose (this.Fd e) system with
                | Ok (SyscallAnswer.Completed 0L, after) ->
                    system <- after
                    fds <- Map.remove e fds
                    "0"
                | Error (CloseRefusal.Release (DescriptionReleaseRefusal.DarwinCloseBehindQueuedFin _)) ->
                    refused <- true
                    ""
                | other -> failwith $"close: %A{other}"

            member _.Abort _ =
                failwith "this kernel models no abortive close, so it does not replay a close under SO_LINGER {1, 0}"

            member this.Fionread e =
                match UnixDescriptor.tryBytesAvailable (this.Fd e) UserBuffer.Mapped system with
                | Ok (BytesAvailableAnswer.Reported count) -> count
                | other -> failwith $"FIONREAD: %A{other}"

            member this.Rdy e =
                let fd = this.Fd e

                let revents, after =
                    match
                        UnixPoll.tryPoll
                            system.Leader
                            [
                                {
                                    Fd = fd
                                    Events = rdyEvents flavour
                                }
                            ]
                            0
                            system
                    with
                    | Ok (PollOutcome.Answered ([ revents ], _), after) -> int (uint16 revents), after
                    | other -> failwith $"poll: %A{other}"

                system <- after

                let waiters =
                    match flavour with
                    | SimulatedUnixFlavour.Linux ->
                        // This kernel models no level-triggered registration;
                        // what one would report is the socket's level, as
                        // asked for, with ERR and HUP, which are never masked.
                        let description =
                            match FileDescriptorRegistry.tryFindId fd (UnixSystemState.fileDescriptors system) with
                            | Some description -> description
                            | None -> failwith $"fd %d{fd} names no description"

                        let level = LinuxReadiness.ofDescription description system
                        $"epoll=0x%x{level &&& (uint32 (rdyEvents flavour) ||| 0x18u)}"
                    | SimulatedUnixFlavour.Darwin ->
                        let kq, s = KeventWorld.kqueue system

                        let changes =
                            [
                                KeventWorld.change fd KeventFilter.Read KeventFlags.Add 0UL
                                KeventWorld.change fd KeventFilter.Write KeventFlags.Add 0UL
                            ]

                        let events, s =
                            match
                                UnixKqueue.tryKevent
                                    s.Leader
                                    kq
                                    2
                                    changes
                                    2
                                    UserBuffer.Mapped
                                    (KeventTimeout.Readable (0L, 0L))
                                    s
                            with
                            | Ok (KeventOutcome.Answered events, s) -> events, s
                            | other -> failwith $"kevent: %A{other}"

                        system <- kernelClose kq s

                        let render (filter : int16) =
                            events
                            |> List.tryFind (fun event -> event.Filter = filter)
                            |> Option.map (fun event -> renderKevent event.Data event.Flags event.FilterFlags)
                            |> Option.defaultValue "-"

                        $"kq-read=%s{render KeventFilter.Read} kq-write=%s{render KeventFilter.Write}"

                $"poll=0x%x{revents} %s{waiters} fionread=%d{(this :> IShutdownPair).Fionread e}"

            member this.Fill e =
                let rec go (total : int64) =
                    match this.WriteAnswer e (1 <<< 20) with
                    | Ok n -> go (total + int64 n)
                    | Error _ -> total

                go 0L

            member this.Drain e =
                let rec go (total : int64) =
                    match this.Read e (1 <<< 20) with
                    | answer when answer.StartsWith ("-", StringComparison.Ordinal) || answer = "0" -> total, answer
                    | count -> go (total + int64 count)

                go 0L

            member _.Settle () = ()

            member this.RegisterEdges ends =
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    let ep, s =
                        match UnixPoll.tryEpollCreate1 0 system with
                        | Ok (Ok created) -> created
                        | other -> failwith $"epoll_create1: %A{other}"

                    let s =
                        (s, ends)
                        ||> List.fold (fun s e ->
                            let fd = this.Fd e

                            match
                                UnixPoll.tryEpollCtl ep 1 fd (EpollEventArgument.Readable (0x80002005u, uint64 fd)) s
                            with
                            | Ok (EpollCtlAnswer.Changed, s) -> s
                            | other -> failwith $"epoll_ctl: %A{other}"
                        )

                    system <- s
                    edgePort <- Some ep
                | SimulatedUnixFlavour.Darwin ->
                    let kq, s = KeventWorld.kqueue system
                    let flags = KeventFlags.Add ||| KeventFlags.Clear

                    let changes =
                        ends
                        |> List.collect (fun e ->
                            [
                                KeventWorld.change (this.Fd e) KeventFilter.Read flags 0UL
                                KeventWorld.change (this.Fd e) KeventFilter.Write flags 0UL
                            ]
                        )

                    let s =
                        match
                            UnixKqueue.tryKevent
                                s.Leader
                                kq
                                changes.Length
                                changes
                                0
                                UserBuffer.Mapped
                                (KeventTimeout.Readable (0L, 0L))
                                s
                        with
                        | Ok (KeventOutcome.Answered [], s) -> s
                        | other -> failwith $"kevent: %A{other}"

                    system <- s
                    edgePort <- Some kq

            member _.Edges () =
                let port =
                    match edgePort with
                    | Some port -> port
                    | None -> failwith "no edge registration"

                let cFd = Map.tryFind c fds |> Option.defaultValue -1

                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    match UnixPoll.tryEpollWait system.Leader port 4 UserBuffer.Mapped 0 system with
                    | Ok (EpollWaitOutcome.Answered events, after) ->
                        system <- after

                        let of' (isC : bool) =
                            events
                            |> List.tryFind (fun (data, _) -> (int data = cFd) = isC)
                            |> Option.map (fun (_, e) -> $"0x%x{e}")
                            |> Option.defaultValue "-"

                        $"c=%s{of' true} p=%s{of' false}"
                    | other -> failwith $"epoll_wait: %A{other}"
                | SimulatedUnixFlavour.Darwin ->
                    match
                        UnixKqueue.tryKevent
                            system.Leader
                            port
                            0
                            []
                            4
                            UserBuffer.Mapped
                            (KeventTimeout.Readable (0L, 0L))
                            system
                    with
                    | Ok (KeventOutcome.Answered [], after) ->
                        system <- after
                        "-"
                    | Ok (KeventOutcome.Answered events, after) ->
                        system <- after

                        events
                        |> List.map (fun event ->
                            let who = if int event.Ident = cFd then "c" else "p"
                            let what = if event.Filter = KeventFilter.Read then "read" else "write"
                            $"%s{who}-%s{what}=%s{renderKevent event.Data event.Flags event.FilterFlags}"
                        )
                        |> String.concat " "
                    | other -> failwith $"kevent: %A{other}"

            member this.PortOf e = kernelPortOf (this.Fd e) system

            member _.BindFree port =
                let fresh, s =
                    NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

                let answer, s =
                    match CopyIn.bind fresh UserBuffer.Mapped 16u (loopback platform port) s with
                    | Ok (BindAnswer.Bound _, s) -> "bind-ok", s
                    | Ok (BindAnswer.Failed UnixError.EADDRINUSE, s) -> "bind-EADDRINUSE", s
                    | Ok (BindAnswer.Failed error, s) -> rendered error, s
                    | Error refusal -> failwith $"bind refused: %A{refusal}"

                system <- kernelClose fresh s
                answer

            member _.Candidate reuse wildcard port =
                let fresh, s =
                    NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

                let s = if reuse then ReuseAddress.set true fresh s else s

                let address =
                    CopyIn.inet
                        platform
                        (Ipv4Endpoint.ofParts (if wildcard then Ipv4Address.any else Ipv4Address.loopback) (uint16 port))

                let answer, s =
                    match CopyIn.bind fresh UserBuffer.Mapped 16u address s with
                    | Ok (BindAnswer.Bound _, s) ->
                        match UnixSocket.tryListen fresh 4 s with
                        | Ok (ListenAnswer.Listening _, s) -> "bind-ok listen-ok", s
                        | Ok (ListenAnswer.Failed error, s) -> $"bind-ok listen-%A{error}", s
                        | Error refusal -> failwith $"listen refused: %A{refusal}"
                    | Ok (BindAnswer.Failed error, s) -> $"bind-%A{error}", s
                    | Error refusal -> failwith $"bind refused: %A{refusal}"

                system <- kernelClose fresh s
                answer

            member this.ReuseAddress e =
                let level = SimulatedUnixPlatform.socketOptionLevel platform
                let name = SimulatedUnixPlatform.reuseAddressOption platform

                match
                    UnixSocket.tryGetsockopt (this.Fd e) level name UserBuffer.Mapped UserBuffer.Mapped (Some 4u) system
                with
                | Ok (answer, after) ->
                    system <- after
                    if OptionValue.readInt answer <> 0 then 1 else 0
                | Error refusal ->
                    failwith $"getsockopt(SO_REUSEADDR) refused: %s{SocketOptionRefusal.describe refusal}"

            member this.PeerName e =
                match UnixSocket.tryGetpeername (this.Fd e) UserBuffer.Mapped 16u system with
                | Ok (GetSockNameAnswer.Reported _) -> "ok"
                | Ok (GetSockNameAnswer.Failed (error, _)) -> $"%A{error}"
                | Error refusal -> failwith $"getpeername refused: %s{GetSockNameRefusal.describe refusal}"

            member _.KeepsTimeWaitOnceBothClosed = false

    let private kernelReplay (flavour : SimulatedUnixFlavour) (scenarios : ShutdownProbe.Scenario list) =
        let platform = HostPlatform.platformOf flavour

        scenarios
        |> List.collect (fun scenario ->
            let mutable made : KernelPair list = []

            let pairOf (shape : ShutdownPairShape) : IShutdownPair =
                let pair = KernelPair (platform, shape)
                made <- pair :: made
                pair :> IShutdownPair

            let lines = scenario pairOf

            for pair in made do
                UnixSystem.checkInvariants pair.System |> shouldEqual []

            lines
        )


    /// Section U's calls in the kernel, on one system threaded through them.
    /// What a shutdown that answers ENOTCONN leaves behind (the `fresh-then`
    /// and `udp-then` rows) is held to the kernel by
    /// ``an unconnected socket's shutdown changes nothing on Darwin and is refused on Linux``.
    let private kernelU (platform : SimulatedUnixPlatform) : UWorld =
        let mutable system = kernelSystem platform

        let created (kind : SocketKind) (protocol : SocketProtocol) : int =
            let fd, after = NewSocket.create SocketDomain.Inet kind protocol system
            system <- after
            fd

        let connectAnswer (fd : int) (port : int) : string =
            match CopyIn.connect fd UserBuffer.Mapped 16u (loopback platform port) system with
            | Ok (ConnectOutcome.Completed, after) ->
                system <- after
                "0"
            | Ok (ConnectOutcome.Failed error, after) ->
                system <- after
                $"-1 %A{error}"
            | Error refusal -> failwith $"connect refused: %s{ConnectRefusal.describe refusal}"

        let shutdownOf (fd : int) (how : int) : ShutdownLine =
            match UnixConnection.tryShutdown fd how system with
            | Ok (answer, after) ->
                system <- after

                match answer with
                | ShutdownAnswer.Shut -> ShutdownLine.Line "0"
                | ShutdownAnswer.Failed error -> ShutdownLine.Line $"-1 %A{error}"
            | Error (ShutdownRefusal.LinuxUnconnected _) -> ShutdownLine.Refused
            | Error refusal -> failwith $"shutdown refused: %s{ShutdownRefusal.describe refusal}"

        let listening () : int * int =
            let l = created SocketKind.Stream SocketProtocol.Tcp
            system <- kernelBind l 0 system

            match UnixSocket.tryListen l 4 system with
            | Ok (ListenAnswer.Listening _, after) -> system <- after
            | other -> failwith $"listen: %A{other}"

            l, kernelPortOf l system

        {
            Stream = fun () -> created SocketKind.Stream SocketProtocol.Tcp
            Datagram = fun () -> created SocketKind.Datagram SocketProtocol.Udp
            NotSocket =
                fun () ->
                    match UnixPipe.tryPipe2 0 UserBuffer.Mapped system with
                    | Ok (Pipe2Answer.Created (readFd, writeFd), after) ->
                        system <- kernelClose writeFd after
                        readFd
                    | other -> failwith $"pipe2: %A{other}"
            BindAny = fun fd -> system <- kernelBind fd 0 system
            Shutdown = shutdownOf
            Close = fun fd -> system <- kernelClose fd system
            Refused =
                fun () ->
                    let l, port = listening ()
                    system <- kernelClose l system
                    let f = created SocketKind.Stream SocketProtocol.Tcp
                    f, connectAnswer f port
            Unreported =
                fun how ->
                    let l, port = listening ()
                    let f = created SocketKind.Stream SocketProtocol.Tcp
                    let _, after = UnixDescriptor.setNonBlocking f true system
                    system <- after
                    let first = connectAnswer f port

                    let sh =
                        match how with
                        | None -> ShutdownLine.Line "-"
                        | Some how -> shutdownOf f how

                    let again = connectAnswer f port
                    system <- kernelClose f system |> kernelClose l

                    let name =
                        match how with
                        | None -> "none"
                        | Some how -> rawHowName how

                    match sh with
                    | ShutdownLine.Line answer ->
                        ShutdownLine.Line
                            $"U\tconnect-unreported(%s{first})\t%s{name}\tshutdown=%s{answer} connect=%s{again}"
                    | other -> other
            Then = fun _ -> List.replicate 3 ShutdownLine.Elsewhere
            ConnectedShutdown =
                fun how ->
                    let pair = KernelPair (platform, ShutdownPairShape.Plain)
                    ShutdownLine.Line ((pair :> IShutdownPair).ShutdownRaw ConnectionEnd.Client how)
        }

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private kernelDisagreementsOf
        (probe : ShutdownProbe.Probe)
        (section : string)
        (scenarios : ShutdownProbe.Scenario list)
        : string list
        =
        flavours
        |> List.collect (fun flavour ->
            kernelReplay flavour scenarios
            |> ShutdownProbe.disagreements probe flavour section id
        )

    let private kernelDisagreements (section : string) (scenarios : ShutdownProbe.Scenario list) : string list =
        kernelDisagreementsOf ShutdownProbe.Probe.Shutdown section scenarios

    [<Test>]
    let ``the kernel answers shutdown, and what each end then sees, as each flavour measured (section S)`` () =
        kernelDisagreements "S" ShutdownProbe.sectionS |> shouldEqual []

    [<Test>]
    let ``the kernel answers shutdown twice, and after the peer's FIN or reset, as measured (section T)`` () =
        kernelDisagreements "T" ShutdownProbe.sectionT |> shouldEqual []

    [<Test>]
    let ``the kernel answers bytes arriving after the receive side is shut as measured (section R)`` () =
        kernelDisagreements "R" ShutdownProbe.sectionR |> shouldEqual []

    [<Test>]
    let ``the kernel answers a close after shutdown as measured (section P)`` () =
        kernelDisagreements "P" ShutdownProbe.sectionP |> shouldEqual []

    [<Test>]
    let ``the kernel reports edges after shutdown as measured (section E)`` () =
        kernelDisagreements "E" ShutdownProbe.sectionE |> shouldEqual []

    [<Test>]
    let ``the kernel frees an end's endpoint, open or closed, and answers getpeername as measured (section F)`` () =
        kernelDisagreements "F" ShutdownProbe.sectionF |> shouldEqual []

    [<Test>]
    let ``the kernel answers a bind of an endpoint an end still holds as measured (bind-held-endpoint.c)`` () =
        kernelDisagreementsOf ShutdownProbe.Probe.HeldEndpoint "H" ShutdownProbe.sectionH
        |> shouldEqual []

    [<Test>]
    let ``the kernel refuses only where the probe marked the outcome as waiting on a timer`` () =
        // Darwin's SHUT_RD before the peer's unsent bytes (R, p-unsent).
        [
            for flavour in flavours do
                let measured = ShutdownProbe.measured ShutdownProbe.Probe.Shutdown flavour "R"
                let replayed = kernelReplay flavour ShutdownProbe.sectionR

                for line, modelled in List.zip measured replayed do
                    if modelled = ShutdownLine.Refused then
                        line.Mark |> shouldEqual ShutdownMark.Timing

                flavour, replayed |> List.filter ((=) ShutdownLine.Refused) |> List.length
        ]
        |> shouldEqual [ SimulatedUnixFlavour.Linux, 0 ; SimulatedUnixFlavour.Darwin, 4 ]

    [<Test>]
    let ``the kernel answers shutdown on sockets without a connection, and a bad how, as measured (section U)`` () =
        for flavour in flavours do
            let replayed = sectionU (kernelU (HostPlatform.platformOf flavour))
            let measured = ShutdownProbe.measured ShutdownProbe.Probe.Shutdown flavour "U"

            // Linux's ENOTCONN to a socket without a connection still shuts its
            // sides, which this kernel does not keep, so it refuses each of
            // those rows; Darwin answers every one.
            let refusedRows =
                List.zip measured replayed
                |> List.filter (fun (_, line) -> line = ShutdownLine.Refused)
                |> List.map (fun (line, _) -> (line.Text.Split '\t').[1])
                |> List.countBy (fun row ->
                    if row.StartsWith ("refused", StringComparison.Ordinal) then
                        "refused"
                    else
                        row
                )

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                refusedRows
                |> shouldEqual [ "fresh", 3 ; "bound", 3 ; "udp-unconnected", 3 ; "refused", 3 ]
            | SimulatedUnixFlavour.Darwin -> refusedRows |> shouldEqual []

            replayed
            |> List.map (fun line ->
                match line with
                | ShutdownLine.Refused -> ShutdownLine.Elsewhere
                | other -> other
            )
            |> ShutdownProbe.disagreements ShutdownProbe.Probe.Shutdown flavour "U" id
            |> shouldEqual []

    /// A socket without a connection: Darwin answers ENOTCONN and changes
    /// nothing at all (`tcp-shutdown.c`, section U, `fresh-then` and
    /// `udp-then`); Linux's ENOTCONN still shuts the sides, which this kernel
    /// does not keep on such a socket, so it refuses the call. A listener is
    /// refused on both, until its `shutdown` is modelled.
    [<Test>]
    let ``an unconnected socket's shutdown changes nothing on Darwin and is refused on Linux`` () =
        for flavour in flavours do
            let platform = HostPlatform.platformOf flavour

            for kind, protocol in
                [
                    SocketKind.Stream, SocketProtocol.Tcp
                    SocketKind.Datagram, SocketProtocol.Udp
                ] do
                for bound in [ false ; true ] do
                    for how in [ 0 ; 1 ; 2 ] do
                        let fd, system =
                            NewSocket.create SocketDomain.Inet kind protocol (kernelSystem platform)

                        let system = if bound then kernelBind fd 0 system else system
                        let socket = kernelSocketOf fd system

                        match flavour, UnixConnection.tryShutdown fd how system with
                        | SimulatedUnixFlavour.Darwin, Ok (ShutdownAnswer.Failed UnixError.ENOTCONN, after) ->
                            after |> shouldEqual system
                        | SimulatedUnixFlavour.Linux, Error (ShutdownRefusal.LinuxUnconnected refused) ->
                            refused |> shouldEqual socket
                        | _, other -> failwith $"%A{flavour} %A{kind} bound %b{bound} how %d{how}: %A{other}"

            let listener, system =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp (kernelSystem platform)

            let system = kernelBind listener 0 system

            let system =
                match UnixSocket.tryListen listener 4 system with
                | Ok (ListenAnswer.Listening _, system) -> system
                | other -> failwith $"listen: %A{other}"

            for how in [ 0 ; 1 ; 2 ] do
                UnixConnection.tryShutdown listener how system
                |> Result.map fst
                |> shouldEqual (Error (ShutdownRefusal.Listener (kernelSocketOf listener system)))

            // A `how` out of range is screened before the state.
            UnixConnection.tryShutdown listener 3 system
            |> Result.map fst
            |> shouldEqual (Ok (ShutdownAnswer.Failed UnixError.EINVAL))
