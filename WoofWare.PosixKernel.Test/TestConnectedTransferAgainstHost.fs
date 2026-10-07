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
/// host the suite runs on. Each host falsifies its own column: Darwin's here,
/// Linux's on CI's x86-64.
///
/// The kernel does not yet transfer bytes between sockets, so these rows are
/// the measurement the model will be held to, not yet a check of it.
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
