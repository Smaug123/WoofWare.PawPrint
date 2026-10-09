namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `read(2)`, `recv(2)`, `write(2)` and `send(2)` on a connected loopback TCP
/// socket, and what `poll(2)` reports of it, in each state a connection passes
/// through: the S section of `tcp-transfer.c`
/// (docs/plans/2026-10-07-tcp-byte-transfer), measured on Linux 6.18.5
/// aarch64 and Darwin 27.0 and embedded from beside the probe, held to the
/// host the suite runs on and to the kernel. Each host falsifies its own
/// column: Darwin's here, Linux's on CI's x86-64. The kernel is held to both
/// flavours' rows on every host, for the calls it has (`read` and `write`,
/// not `recv` and `send`) and the states it can reach (not `SO_LINGER`'s), and
/// to what `epoll` and the kqueue filters reported as well as `poll`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConnectedTransferAgainstHost =

    /// Where the observed socket `s` and its accepted peer `p` stand; the
    /// probe's header describes how each is reached.
    [<RequireQualifiedAccess>]
    type private State =
        | Idle
        | DataIn
        | SendFull
        | Fin
        | FinData
        | FinDrained
        | Reset
        | ResetData
        | LingerZero
        | FinWritten
        | SendFullReset

    [<RequireQualifiedAccess>]
    type private Operation =
        /// Readiness alone, then `getsockopt(SO_ERROR)`.
        | Readiness
        | Read
        | ReadZero
        | Recv
        | RecvZero
        /// `recv(MSG_PEEK)` twice, then `read`.
        | Peek
        | Write
        | WriteZero
        | Send
        | SendZero
        | SendNoSignal
        /// With `O_NONBLOCK` cleared, `recv(MSG_DONTWAIT)` then
        /// `send(MSG_DONTWAIT)` of 65536 bytes.
        | DontWait

    /// One call's answer, as the probe saw it. The probe caught `SIGPIPE`; the
    /// test host ignores it, so here a write that raised it reports only its
    /// errno.
    [<RequireQualifiedAccess>]
    type private Seen =
        | Returned of count : int64
        | Failed of error : UnixError * raisedSigPipe : bool
        /// What `getsockopt(SO_ERROR)` read: `None` for 0.
        | PendingError of error : UnixError option

    /// `poll(2)`'s `revents` when asked for every event the flavour has, and
    /// `FIONREAD`.
    type private Readiness =
        {
            Revents : int
            Readable : int
        }

    type private Row =
        {
            Flavour : SimulatedUnixFlavour
            State : State
            Operation : Operation
            Before : Readiness
            Seen : Seen list
            After : Readiness
            /// What the probe recorded besides `poll` and `FIONREAD`: Linux's
            /// level-triggered `epoll` mask, and Darwin's two kqueue filters,
            /// as `waiters` renders them.
            BeforeWaiters : string
            AfterWaiters : string
        }

    let private stateNamed (name : string) : State =
        match name with
        | "IDLE" -> State.Idle
        | "DATA_IN" -> State.DataIn
        | "SNDFULL" -> State.SendFull
        | "FIN" -> State.Fin
        | "FIN_DATA" -> State.FinData
        | "FIN_DRAINED" -> State.FinDrained
        | "RST" -> State.Reset
        | "RST_DATA" -> State.ResetData
        | "LINGER0" -> State.LingerZero
        | "FIN_WRITTEN" -> State.FinWritten
        | "SNDFULL_RST" -> State.SendFullReset
        | other -> failwith $"the probe reported state %s{other}, which this test does not know"

    let private operationNamed (name : string) : Operation =
        match name with
        | "none" -> Operation.Readiness
        | "read" -> Operation.Read
        | "read0" -> Operation.ReadZero
        | "recv" -> Operation.Recv
        | "recv0" -> Operation.RecvZero
        | "peek" -> Operation.Peek
        | "write" -> Operation.Write
        | "write0" -> Operation.WriteZero
        | "send" -> Operation.Send
        | "send0" -> Operation.SendZero
        | "sendnosig" -> Operation.SendNoSignal
        | "dontwait" -> Operation.DontWait
        | other -> failwith $"the probe reported operation %s{other}, which this test does not know"

    let private errorNamed (name : string) : UnixError =
        match name with
        | "EAGAIN" -> UnixError.EAGAIN
        | "EPIPE" -> UnixError.EPIPE
        | "ECONNRESET" -> UnixError.ECONNRESET
        | other -> failwith $"the probe reported %s{other}, which this test does not know"

    let private readinessPattern : Regex =
        Regex @"poll=0x([0-9a-f]+) .* fionread=(-?\d+)"

    let private waitersPattern : Regex = Regex @"poll=0x[0-9a-f]+ (.*) fionread="

    let private waitersOf (text : string) : string =
        let m = waitersPattern.Match text

        if not m.Success then
            failwith $"no waiters in %s{text}"

        m.Groups.[1].Value

    let private readinessOf (text : string) : Readiness =
        let m = readinessPattern.Match text

        if not m.Success then
            failwith $"no readiness in %s{text}"

        {
            Revents = Convert.ToInt32 (m.Groups.[1].Value, 16)
            Readable = int m.Groups.[2].Value
        }

    let private seenOf (text : string) : Seen =
        if text.StartsWith ("SO_ERROR=", StringComparison.Ordinal) then
            match text.Substring 9 with
            | "0" -> Seen.PendingError None
            | name -> Seen.PendingError (Some (errorNamed name))
        else
            match text.Split ' ' |> Array.toList with
            | [ "-1" ; name ] -> Seen.Failed (errorNamed name, false)
            | [ "-1" ; name ; "SIGPIPE" ] -> Seen.Failed (errorNamed name, true)
            | [ count ] -> Seen.Returned (int64 count)
            | _ -> failwith $"the probe reported %s{text}, which this test cannot read"

    /// Every S row of one flavour's run.
    let private rowsOf (flavour : SimulatedUnixFlavour) : Row list =
        let resource =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "WoofWare.PosixKernel.Test.tcpTransfer.linux.txt"
            | SimulatedUnixFlavour.Darwin -> "WoofWare.PosixKernel.Test.tcpTransfer.darwin.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split '\n'
        |> Array.toList
        |> List.choose (fun line ->
            match line.TrimEnd('\r').Split '\t' |> Array.toList with
            | [ "S" ; state ; operation ; before ; seen ; after ] ->
                Some
                    {
                        Flavour = flavour
                        State = stateNamed state
                        Operation = operationNamed operation
                        Before = readinessOf before
                        Seen = seen.Split " ; " |> Array.toList |> List.map seenOf
                        After = readinessOf after
                        BeforeWaiters = waitersOf before
                        AfterWaiters = waitersOf after
                    }
            | _ -> None
        )

    [<Test>]
    let ``each flavour's run holds every state and operation`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let rows = rowsOf flavour
            rows.Length |> shouldEqual (11 * 12)

            rows
            |> List.map (fun row -> row.State, row.Operation)
            |> List.distinct
            |> List.length
            |> shouldEqual (11 * 12)

    // The host the suite runs on

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "bind", SetLastError = true)>]
    extern int private hostBind(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "listen", SetLastError = true)>]
    extern int private hostListen(int fd, int backlog)

    [<DllImport("libc", EntryPoint = "getsockname", SetLastError = true)>]
    extern int private hostGetSockName(int fd, byte[] address, uint32& length)

    [<DllImport("libc", EntryPoint = "connect", SetLastError = true)>]
    extern int private hostConnect(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "accept", SetLastError = true)>]
    extern int private hostAccept(int fd, nativeint address, nativeint length)

    [<DllImport("libc", EntryPoint = "setsockopt", SetLastError = true)>]
    extern int private hostSetSockOpt(int fd, int level, int optionName, byte[] value, uint32 length)

    [<DllImport("libc", EntryPoint = "getsockopt", SetLastError = true)>]
    extern int private hostGetSockOpt(int fd, int level, int optionName, int& value, uint32& length)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostRead(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWrite(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "recv", SetLastError = true)>]
    extern nativeint private hostRecv(int fd, byte[] buffer, unativeint count, int flags)

    [<DllImport("libc", EntryPoint = "send", SetLastError = true)>]
    extern nativeint private hostSend(int fd, byte[] buffer, unativeint count, int flags)

    [<DllImport("libc", EntryPoint = "poll", SetLastError = true)>]
    extern int private hostPoll(int64[] fds, unativeint count, int timeoutMs)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    // `fcntl(2)` and `ioctl(2)` are variadic, which a P/Invoke cannot call
    // portably (Apple's arm64 ABI passes variadic arguments on the stack), so
    // these go through .NET's own fixed-arity wrappers in `System.Native`.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")>]
    extern int private hostSetNonBlocking(nativeint fd, int isNonBlocking)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetBytesAvailable")>]
    extern int private hostBytesAvailable(nativeint fd, int& available)

    [<Literal>]
    let private AF_INET = 2

    [<Literal>]
    let private SOCK_STREAM = 1

    [<Literal>]
    let private MSG_PEEK = 0x2

    let private dontWait (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x40
        | SimulatedUnixFlavour.Darwin -> 0x80

    let private noSignal (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x4000
        | SimulatedUnixFlavour.Darwin -> 0x80000

    let private lingerOption (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 13
        | SimulatedUnixFlavour.Darwin -> 0x80

    /// Every event the flavour's `<poll.h>` has but `POLLERR`, `POLLHUP` and
    /// `POLLNVAL`, which `poll` reports unasked: what the probe asked for.
    let private everyEvent (flavour : SimulatedUnixFlavour) : int16 =
        match flavour with
        // IN PRI OUT RDNORM RDBAND WRNORM WRBAND MSG RDHUP
        | SimulatedUnixFlavour.Linux -> 0x27c7s
        // IN PRI OUT RDNORM RDBAND WRBAND (WRNORM is OUT)
        | SimulatedUnixFlavour.Darwin -> 0x1c7s

    /// The probe's pause after each step, for what a step sets in motion to
    /// arrive: Darwin's loopback delivers a segment on another thread.
    let private settle () : unit = Thread.Sleep 20

    let private payload : byte[] = Array.init 65536 (fun i -> byte (i * 31 + 7))

    let private hostReadiness (flavour : SimulatedUnixFlavour) (fd : int) : Readiness =
        // struct pollfd { int fd; short events; short revents; }, little-endian.
        let entry = [| int64 (uint32 fd) ||| (int64 (uint16 (everyEvent flavour)) <<< 32) |]

        if hostPoll (entry, 1un, 0) < 0 then
            failwith $"poll failed with errno %d{Marshal.GetLastPInvokeError ()}"

        let mutable available = 0

        if hostBytesAvailable (nativeint fd, &available) <> 0 then
            failwith $"FIONREAD failed on fd %d{fd}"

        {
            Revents = int (uint16 (entry.[0] >>> 48))
            Readable = available
        }

    /// A connected loopback pair, both ends non-blocking: the connecting
    /// socket, then the accepted one.
    let private hostPair (platform : SimulatedUnixPlatform) : int * int =
        let check (what : string) (result : int) : unit =
            if result < 0 then
                failwith $"%s{what} failed with errno %d{Marshal.GetLastPInvokeError ()}"

        let listener = hostSocket (AF_INET, SOCK_STREAM, 0)
        check "socket" listener

        let address =
            SimulatedUnixPlatform.encodeInternetSockaddr
                platform
                (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 0us)

        check "bind" (hostBind (listener, address, uint32 address.Length))
        check "listen" (hostListen (listener, 8))
        let name = Array.zeroCreate<byte> address.Length
        let mutable length = uint32 name.Length
        check "getsockname" (hostGetSockName (listener, name, &length))
        let client = hostSocket (AF_INET, SOCK_STREAM, 0)
        check "socket" client
        check "connect" (hostConnect (client, name, length))
        let server = hostAccept (listener, 0n, 0n)
        check "accept" server
        hostClose listener |> ignore<int>

        for fd in [ client ; server ] do
            if hostSetNonBlocking (nativeint fd, 1) <> 0 then
                failwith $"could not make fd %d{fd} non-blocking"

        client, server

    let private writeExactly (fd : int) (count : int) : unit =
        let written = hostWrite (fd, payload, unativeint count)

        if written <> nativeint count then
            failwith $"a write of %d{count} took %d{written} (errno %d{Marshal.GetLastPInvokeError ()})"

    /// Bring a fresh pair to `state`, as the probe's `build` does: the
    /// observed socket, and its peer unless the peer has closed.
    let private hostBuild (platform : SimulatedUnixPlatform) (state : State) : int * int option =
        let flavour = SimulatedUnixPlatform.flavour platform
        let s, p = hostPair platform

        let fill () : unit =
            while hostWrite (s, payload, 65536un) > 0n do
                ()

        let closed () : int option =
            hostClose p |> ignore<int>
            None

        let peer =
            match state with
            | State.Idle -> Some p
            | State.DataIn ->
                writeExactly p 1000
                Some p
            | State.SendFull ->
                fill ()
                Some p
            | State.SendFullReset ->
                fill ()
                settle ()
                closed ()
            | State.Fin -> closed ()
            | State.FinData ->
                writeExactly p 1000
                closed ()
            | State.FinDrained ->
                writeExactly p 1000
                let peer = closed ()
                settle ()
                let got = hostRead (s, Array.zeroCreate 4096, 4096un)

                if got <> 1000n then
                    failwith $"FIN_DRAINED: the drain read %d{got}"

                peer
            | State.Reset ->
                writeExactly s 1000
                settle ()
                closed ()
            | State.ResetData ->
                writeExactly p 1000
                writeExactly s 1000
                settle ()
                closed ()
            | State.LingerZero ->
                let linger = Array.append (BitConverter.GetBytes 1) (BitConverter.GetBytes 0)

                if
                    hostSetSockOpt (
                        p,
                        SimulatedUnixPlatform.socketOptionLevel platform,
                        lingerOption flavour,
                        linger,
                        uint32 linger.Length
                    )
                    <> 0
                then
                    failwith $"SO_LINGER failed with errno %d{Marshal.GetLastPInvokeError ()}"

                closed ()
            | State.FinWritten ->
                let peer = closed ()
                settle ()
                writeExactly s 100
                peer

        settle ()
        s, peer

    /// One call on the host: what it returned, and the errno if it was -1.
    let private call (f : unit -> nativeint) : int64 * int =
        Marshal.SetLastPInvokeError 0
        let result = int64 (f ())
        result, (if result < 0L then Marshal.GetLastPInvokeError () else 0)

    /// What the host answered to `operation`, one entry per call, as the probe
    /// made them.
    let private hostCalls (platform : SimulatedUnixPlatform) (s : int) (operation : Operation) : (int64 * int) list =
        let flavour = SimulatedUnixPlatform.flavour platform
        let buffer = Array.zeroCreate<byte> 4096

        let thrice (f : int -> nativeint) : (int64 * int) list =
            [ 0..2 ]
            |> List.map (fun i ->
                let answer = call (fun () -> f i)
                settle ()
                answer
            )

        match operation with
        | Operation.Readiness ->
            let mutable value = 0
            let mutable length = 4u

            if
                hostGetSockOpt (
                    s,
                    SimulatedUnixPlatform.socketOptionLevel platform,
                    SimulatedUnixPlatform.socketErrorOption platform,
                    &value,
                    &length
                )
                <> 0
            then
                failwith $"getsockopt(SO_ERROR) failed with errno %d{Marshal.GetLastPInvokeError ()}"

            [ 0L, value ]
        | Operation.Read -> thrice (fun _ -> hostRead (s, buffer, 4096un))
        | Operation.ReadZero -> thrice (fun _ -> hostRead (s, buffer, 0un))
        | Operation.Recv -> thrice (fun _ -> hostRecv (s, buffer, 4096un, 0))
        | Operation.RecvZero -> thrice (fun _ -> hostRecv (s, buffer, 0un, 0))
        | Operation.Peek ->
            thrice (fun i ->
                if i < 2 then
                    hostRecv (s, buffer, 4096un, MSG_PEEK)
                else
                    hostRead (s, buffer, 4096un)
            )
        | Operation.Write -> thrice (fun _ -> hostWrite (s, payload, 100un))
        | Operation.WriteZero -> thrice (fun _ -> hostWrite (s, payload, 0un))
        | Operation.Send -> thrice (fun _ -> hostSend (s, payload, 100un, 0))
        | Operation.SendZero -> thrice (fun _ -> hostSend (s, payload, 0un, 0))
        | Operation.SendNoSignal -> thrice (fun _ -> hostSend (s, payload, 100un, noSignal flavour))
        | Operation.DontWait ->
            hostSetNonBlocking (nativeint s, 0) |> ignore<int>
            let received = call (fun () -> hostRecv (s, buffer, 4096un, dontWait flavour))
            let sent = call (fun () -> hostSend (s, payload, 65536un, dontWait flavour))
            hostSetNonBlocking (nativeint s, 1) |> ignore<int>
            [ received ; sent ]

    /// What the row says the host's calls answer, in the host's numbering.
    let private expectedCalls (platform : SimulatedUnixPlatform) (seen : Seen list) : (int64 * int) list =
        let errno (error : UnixError) : int =
            UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error

        seen
        |> List.map (fun seen ->
            match seen with
            | Seen.Returned count -> count, 0
            | Seen.Failed (error, _) -> -1L, errno error
            | Seen.PendingError None -> 0L, 0
            | Seen.PendingError (Some error) -> 0L, errno error
        )

    [<Test>]
    let ``this host answers every state and operation as its flavour was measured to`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour

            // A send buffer filled to EAGAIN is not a state either kernel holds
            // still: Darwin's loopback drains it on another thread, and Linux
            // frees space on a timer (the C section of the probe), so the rows
            // after it depend on how long the test host took.
            let rows = rowsOf flavour |> List.filter (fun row -> row.State <> State.SendFull)

            let disagreements =
                rows
                |> List.toArray
                |> Array.Parallel.map (fun row ->
                    let s, peer = hostBuild platform row.State

                    try
                        let before = hostReadiness flavour s
                        let answered = hostCalls platform s row.Operation
                        let after = hostReadiness flavour s
                        let expected = expectedCalls platform row.Seen

                        [
                            if before <> row.Before then
                                $"%A{row.State} %A{row.Operation}: before the calls this host reports %A{before}, measured %A{row.Before}"
                            if answered <> expected then
                                $"%A{row.State} %A{row.Operation}: this host answered %A{answered}, measured %A{expected}"
                            if after <> row.After then
                                $"%A{row.State} %A{row.Operation}: after the calls this host reports %A{after}, measured %A{row.After}"
                        ]
                    finally
                        hostClose s |> ignore<int>
                        peer |> Option.iter (hostClose >> ignore<int>)
                )
                |> Seq.concat
                |> Seq.toList

            disagreements |> shouldEqual []
        )

    // The kernel

    /// Whether the kernel can be held to `row`: a call it has, in a state it
    /// can reach and holds still. `recv` and `send` are not modelled yet, nor
    /// `SO_LINGER`. A Linux send buffer the timer frees space in is the
    /// kernel's stated non-reproduction (it frees space only as the peer
    /// reads), so a write to a full one answers `EAGAIN` for ever where Linux
    /// took 100 bytes 20 ms on; and Darwin's full send buffer drains on another
    /// thread, so its rows after a fill are not repeatable at all.
    let private kernelHolds (row : Row) : bool =
        let operation =
            match row.Operation with
            | Operation.Readiness
            | Operation.Read
            | Operation.ReadZero
            | Operation.Write
            | Operation.WriteZero -> true
            | Operation.Recv
            | Operation.RecvZero
            | Operation.Peek
            | Operation.Send
            | Operation.SendZero
            | Operation.SendNoSignal
            | Operation.DontWait -> false

        let state =
            match row.Flavour, row.State with
            | _, State.LingerZero -> false
            | SimulatedUnixFlavour.Darwin, State.SendFull
            | SimulatedUnixFlavour.Darwin, State.SendFullReset -> false
            | SimulatedUnixFlavour.Linux, State.SendFull -> row.Operation <> Operation.Write
            | _, _ -> true

        operation && state

    let private kernelPort : uint16 = 5000us

    let private kernelLoopback (platform : SimulatedUnixPlatform) : byte[] =
        CopyIn.inet platform (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress kernelPort)

    let private kernelClose (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed 0L, system) -> system
        | other -> failwith $"closing fd %d{fd}: %A{other}"

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

    /// A connected loopback pair in the kernel, both ends non-blocking, as
    /// `hostPair` makes one: the connecting socket, then the accepted one.
    let private kernelPair (platform : SimulatedUnixPlatform) : int * int * UnixSystem<int, string> =
        let system = kernelSystem platform

        let listener, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match CopyIn.bind listener UserBuffer.Mapped 16u (kernelLoopback platform) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"bind: %A{other}"

        let system =
            match UnixSocket.listen listener 8 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"listen: %A{other}"

        let client, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match CopyIn.connect client UserBuffer.Mapped 16u (kernelLoopback platform) system with
            | Ok (ConnectOutcome.Completed, system) -> system
            | other -> failwith $"connect: %A{other}"

        let server, system =
            match UnixConnection.accept 1 listener UserBuffer.Mapped 16u system with
            | Ok (AcceptOutcome.Accepted (accepted, _, _), system) -> accepted, system
            | other -> failwith $"accept: %A{other}"

        let system = kernelClose listener system
        let _, system = UnixDescriptor.setNonBlocking client true system
        let _, system = UnixDescriptor.setNonBlocking server true system
        client, server, system

    /// A write of `count` bytes of the payload through `fd`: what it answered,
    /// whether it raised `SIGPIPE`, and the system after.
    let private kernelWrite
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : Seen * UnixSystem<int, string>
        =
        let bytes = System.Collections.Immutable.ImmutableArray.Create (payload, 0, count)

        match WriteOutcomes.admitThenWrite system.Leader fd UserBuffer.Mapped bytes system with
        | Ok (WriteOutcome.Returns (WriteAnswer.Completed written, system)) -> Seen.Returned written, system
        | Ok (WriteOutcome.Returns (WriteAnswer.Failed error, system)) -> Seen.Failed (error, false), system
        | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed error, entry, system)) when entry.Signal = Signal.SIGPIPE ->
            Seen.Failed (error, true), system
        | other -> failwith $"a write of %d{count} through fd %d{fd}: %A{other}"

    let private kernelRead
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : Seen * UnixSystem<int, string>
        =
        match ReadOutcomes.read fd UserBuffer.Mapped (uint64 count) system with
        | Ok (ReadAnswer.Completed bytes, system) -> Seen.Returned (int64 bytes.Length), system
        | Ok (ReadAnswer.Failed error, system) -> Seen.Failed (error, false), system
        | other -> failwith $"a read of %d{count} through fd %d{fd}: %A{other}"

    let private kernelWriteExactly
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match kernelWrite fd count system with
        | Seen.Returned written, system when written = int64 count -> system
        | other, _ -> failwith $"a write of %d{count} answered %A{other}"

    /// Bring a fresh pair in the kernel to `state`, as `hostBuild` does on the
    /// host: the observed socket, and the system.
    let private kernelBuild (platform : SimulatedUnixPlatform) (state : State) : int * UnixSystem<int, string> =
        let s, p, system = kernelPair platform

        let rec fill (system : UnixSystem<int, string>) : UnixSystem<int, string> =
            match kernelWrite s 65536 system with
            | Seen.Returned _, system -> fill system
            | Seen.Failed (UnixError.EAGAIN, false), system -> system
            | other, _ -> failwith $"filling: %A{other}"

        let system =
            match state with
            | State.Idle -> system
            | State.DataIn -> kernelWriteExactly p 1000 system
            | State.SendFull -> fill system
            | State.SendFullReset -> fill system |> kernelClose p
            | State.Fin -> kernelClose p system
            | State.FinData -> kernelWriteExactly p 1000 system |> kernelClose p
            | State.FinDrained ->
                let system = kernelWriteExactly p 1000 system |> kernelClose p

                match kernelRead s 4096 system with
                | Seen.Returned 1000L, system -> system
                | other, _ -> failwith $"FIN_DRAINED: the drain read %A{other}"
            | State.Reset -> kernelWriteExactly s 1000 system |> kernelClose p
            | State.ResetData -> kernelWriteExactly p 1000 system |> kernelWriteExactly s 1000 |> kernelClose p
            | State.LingerZero -> failwith "SO_LINGER is not modelled, so the kernel cannot reach LINGER0"
            | State.FinWritten -> kernelClose p system |> kernelWriteExactly s 100

        s, system

    /// What `fd` presents in the kernel: `poll`'s revents for every event the
    /// flavour has (a poll, which a Linux socket's send-space wake can notice),
    /// `FIONREAD`, and the waiters as the probe printed them.
    let private kernelReadiness
        (fd : int)
        (system : UnixSystem<int, string>)
        : Readiness * string * UnixSystem<int, string>
        =
        let platform = system.Machine.UnixPlatform
        let flavour = SimulatedUnixPlatform.flavour platform

        let revents, system =
            match
                UnixPoll.poll
                    system.Leader
                    [
                        {
                            Fd = fd
                            Events = everyEvent flavour
                        }
                    ]
                    0
                    system
            with
            | Ok (PollOutcome.Answered ([ revents ], _), system) -> int (uint16 revents), system
            | other -> failwith $"poll: %A{other}"

        let readable =
            match UnixDescriptor.bytesAvailable fd UserBuffer.Mapped system with
            | Ok (BytesAvailableAnswer.Reported count) -> count
            | other -> failwith $"FIONREAD: %A{other}"

        let socketId =
            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"fd %d{fd} names %A{other}"

        let waiters =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                let description =
                    match FileDescriptorRegistry.tryFindId fd (UnixSystemState.fileDescriptors system) with
                    | Some description -> description
                    | None -> failwith $"fd %d{fd} names no description"

                $"epoll=0x%x{LinuxReadiness.ofDescription description system}"
            | SimulatedUnixFlavour.Darwin ->
                let fflags (error : UnixError option) : int =
                    match error with
                    | None -> 0
                    | Some error -> UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error

                let render (name : string) (filter : KqueueFilter) : string =
                    match DarwinReadiness.ofSocket filter socketId system.Machine with
                    | None -> $"%s{name}(-)"
                    | Some (KqueueFilterReport.Ready data) -> $"%s{name}(data=%d{data} fflags=0)"
                    | Some (KqueueFilterReport.EndOfFile (data, error)) ->
                        $"%s{name}(data=%d{data} EOF fflags=%d{fflags error})"

                let read = render "READ" KqueueFilter.Read
                let write = render "WRITE" KqueueFilter.Write
                $"%s{read} %s{write}"

        {
            Revents = revents
            Readable = readable
        },
        waiters,
        system

    /// What the kernel answered to `operation`, one entry per call.
    let private kernelCalls
        (s : int)
        (operation : Operation)
        (system : UnixSystem<int, string>)
        : Seen list * UnixSystem<int, string>
        =
        let thrice
            (call : UnixSystem<int, string> -> Seen * UnixSystem<int, string>)
            : Seen list * UnixSystem<int, string>
            =
            (([], system), [ 1..3 ])
            ||> List.fold (fun (seen, system) _ ->
                let answer, system = call system
                seen @ [ answer ], system
            )

        match operation with
        | Operation.Readiness ->
            let platform = system.Machine.UnixPlatform

            let level = SimulatedUnixPlatform.socketOptionLevel platform
            let optionName = SimulatedUnixPlatform.socketErrorOption platform

            match UnixSocket.getsockopt s level optionName UserBuffer.Mapped UserBuffer.Mapped (Some 4u) system with
            | Ok (GetSockOptAnswer.Reported (OptionValue.Int 0), system) -> [ Seen.PendingError None ], system
            | Ok (GetSockOptAnswer.Reported (OptionValue.Int raw), system) ->
                [
                    Seen.PendingError (UnixError.ofRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) raw)
                ],
                system
            | Ok (GetSockOptAnswer.Reported copied, _) ->
                failwith $"getsockopt(SO_ERROR) copied out %A{copied}, not an int"
            | Ok (GetSockOptAnswer.Failed (error, _), _) -> failwith $"getsockopt(SO_ERROR): %O{error}"
            | Error refusal -> failwith $"getsockopt(SO_ERROR): %s{SocketOptionRefusal.describe refusal}"
        | Operation.Read -> thrice (kernelRead s 4096)
        | Operation.ReadZero -> thrice (kernelRead s 0)
        | Operation.Write -> thrice (kernelWrite s 100)
        | Operation.WriteZero -> thrice (kernelWrite s 0)
        | other -> failwith $"the kernel has no %A{other}"

    [<Test>]
    let ``the kernel answers every state and operation it can reach as each flavour was measured to`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let platform = HostPlatform.platformOf flavour
            let rows = rowsOf flavour |> List.filter kernelHolds

            // Every reachable state with each of the five calls the kernel has,
            // bar what `kernelHolds` leaves out.
            rows.Length
            |> shouldEqual (
                match flavour with
                | SimulatedUnixFlavour.Linux -> 10 * 5 - 1
                | SimulatedUnixFlavour.Darwin -> 8 * 5
            )

            let disagreements =
                rows
                |> List.collect (fun row ->
                    let s, system = kernelBuild platform row.State
                    let before, beforeWaiters, system = kernelReadiness s system
                    let answered, system = kernelCalls s row.Operation system
                    let after, afterWaiters, system = kernelReadiness s system

                    UnixSystem.checkInvariants system |> shouldEqual []

                    [
                        if before <> row.Before || beforeWaiters <> row.BeforeWaiters then
                            $"%A{flavour} %A{row.State} %A{row.Operation}: before the calls the kernel reports %A{before} %s{beforeWaiters}, measured %A{row.Before} %s{row.BeforeWaiters}"
                        if answered <> row.Seen then
                            $"%A{flavour} %A{row.State} %A{row.Operation}: the kernel answered %A{answered}, measured %A{row.Seen}"
                        if after <> row.After || afterWaiters <> row.AfterWaiters then
                            $"%A{flavour} %A{row.State} %A{row.Operation}: after the calls the kernel reports %A{after} %s{afterWaiters}, measured %A{row.After} %s{row.AfterWaiters}"
                    ]
                )

            disagreements |> shouldEqual []
