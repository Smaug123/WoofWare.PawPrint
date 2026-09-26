namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixPoll.poll`, driven directly on a constructed system.
///
/// The tier that reaches what `sourcesPure/SocketPoll.cs` cannot: every bit of
/// Linux's own `<poll.h>` alphabet (a guest reaches `poll` through the shim,
/// which asks for six bits and hands back six), the Darwin refusal (a guest
/// runs one flavour, and PawPrint's guests run Linux), the socket-event-port
/// entry (no managed caller polls one), and the park refusal (a guest that
/// reached it would abort the interpreter rather than report).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPoll =

    // Linux's `<poll.h>`, as the probe printed it (Linux 6.18.5 aarch64, glibc
    // 2.41, 2026-09-23; docs/plans/2026-08-23-posix-kernel-extraction/poll-alphabet.c).
    // Stated here rather than read from the library, so that a library which
    // renumbered a bit disagrees with the table instead of with itself.
    let private pollIn : int16 = 0x0001s
    let private pollPri : int16 = 0x0002s
    let private pollOut : int16 = 0x0004s
    let private pollErr : int16 = 0x0008s
    let private pollHup : int16 = 0x0010s
    let private pollNval : int16 = 0x0020s
    let private pollRdNorm : int16 = 0x0040s
    let private pollRdBand : int16 = 0x0080s
    let private pollWrNorm : int16 = 0x0100s
    let private pollWrBand : int16 = 0x0200s
    let private pollMsg : int16 = 0x0400s
    let private pollRdHup : int16 = 0x2000s

    /// Every bit, so that a report is compared against a level rather than
    /// against the request that produced it.
    let private everything : int16 = -1s

    /// A simulated process on the flavour asked for, before anything has
    /// happened to it.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        { system with
            Machine =
                { system.Machine with
                    LocalRoutes = []
                }
        }

    let private linux : UnixSystem<int, string> =
        systemOn SimulatedUnixPlatform.linuxX64

    let private entry (fd : int) (events : int16) : PollEntry =
        {
            Fd = fd
            Events = events
        }

    let private pollOrFail
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<int, string>)
        : int16 list * int
        =
        match UnixPoll.poll entries milliseconds system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{PollRefusal.describe refusal}"

    /// Give `system` a socket in `phase` and a descriptor onto it.
    let private withSocket
        (domain : SocketDomain)
        (kind : SocketKind)
        (phase : SocketPhase)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let socketId = system.Machine.NextSocketId

        let (SocketId raw) = socketId

        let socket =
            {
                Domain = domain
                Kind = kind
                Protocol =
                    match domain, kind with
                    | SocketDomain.Unix, _ -> SocketProtocol.Default
                    | _, SocketKind.Stream -> SocketProtocol.Tcp
                    | _, _ -> SocketProtocol.Udp
                Binding = None
                ReuseAddress = false
                Phase = phase
            }

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        fd,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.add socketId socket system.Machine.Sockets
                    NextSocketId = SocketId (raw + 1L)
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// An idle IPv4 stream socket.
    let private idleSocket (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        withSocket SocketDomain.Inet SocketKind.Stream SocketPhase.Idle system

    let private withFile
        (accessMode : FileAccessMode)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) accessMode system.Process.FileDescriptors

        fd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private peer : InternetEndpoint =
        {
            Address = 0x7F000001u
            Port = 5555us
        }

    /// One descriptor onto each object and state this kernel models `poll` for,
    /// with the level a real Linux kernel presents for it: the `revents` it
    /// answers to `events = 0xFFFF`.
    ///
    /// Every level is measured, by `poll-alphabet.c` on Linux 6.18.5 aarch64
    /// (2026-09-23), which polled each object with all 65536 request masks at
    /// timeout 0. On every one of them, every one of the 65536 answers was
    /// `level & (events | POLLERR | POLLHUP)`, with `rv` counting exactly the
    /// entries whose `revents` was non-zero: that rule and this column together
    /// *are* the measured table, and the test below checks the library against
    /// both.
    let private measuredLevels : (string * int * int16) list * UnixSystem<int, string> =
        let connection = ConnectionId 7L
        let orphan = ConnectionId 8L
        let queued = ConnectionId 9L

        let rows, system =
            [
                // The launch shape: stdin is the read end of a pipe whose writer
                // the launcher closed, and the output streams are write ends with
                // space and a live reader.
                "stdin", (fun system -> 0, system), 0x0010s
                "stdout", (fun system -> 1, system), 0x0104s
                "stderr", (fun system -> 2, system), 0x0104s
                // Measured under every access mode, at EOF and at offset 0, empty
                // and not, and for a directory: the VFS default mask.
                "regular file, read-only", withFile FileAccessMode.ReadOnly, 0x0145s
                "regular file, write-only", withFile FileAccessMode.WriteOnly, 0x0145s
                "regular file, read-write", withFile FileAccessMode.ReadWrite, 0x0145s
                // TCP: never WRBAND, in any phase.
                "IPv4 TCP, idle", withSocket SocketDomain.Inet SocketKind.Stream SocketPhase.Idle, 0x0114s
                "IPv6 TCP, idle", withSocket SocketDomain.Inet6 SocketKind.Stream SocketPhase.Idle, 0x0114s
                "IPv4 TCP, listening, queue empty",
                withSocket
                    SocketDomain.Inet
                    SocketKind.Stream
                    (SocketPhase.Listening
                        {
                            Backlog = 8
                            Queue = []
                        }),
                0x0000s
                "IPv4 TCP, listening, queue nonempty",
                withSocket
                    SocketDomain.Inet
                    SocketKind.Stream
                    (SocketPhase.Listening
                        {
                            Backlog = 8
                            Queue = [ queued ]
                        }),
                0x0041s
                "IPv4 TCP, established, peer alive",
                withSocket SocketDomain.Inet SocketKind.Stream (SocketPhase.Established connection),
                0x0104s
                "IPv4 TCP, established pending report, peer alive",
                withSocket SocketDomain.Inet SocketKind.Stream (SocketPhase.EstablishedPendingReport connection),
                0x0104s
                "IPv4 TCP, established, peer closed",
                withSocket SocketDomain.Inet SocketKind.Stream (SocketPhase.Established orphan),
                0x2145s
                "IPv4 TCP, refused, pending delivery",
                withSocket SocketDomain.Inet SocketKind.Stream SocketPhase.RefusedPendingDelivery,
                0x215ds
                // UDP: WRBAND alongside OUT, connected or not.
                "IPv4 UDP, idle", withSocket SocketDomain.Inet SocketKind.Datagram SocketPhase.Idle, 0x0304s
                "IPv6 UDP, idle", withSocket SocketDomain.Inet6 SocketKind.Datagram SocketPhase.Idle, 0x0304s
                "IPv4 UDP, peer set",
                withSocket SocketDomain.Inet SocketKind.Datagram (SocketPhase.DatagramPeer peer),
                0x0304s
                "IPv6 UDP, peer set",
                withSocket SocketDomain.Inet6 SocketKind.Datagram (SocketPhase.DatagramPeer peer),
                0x0304s
                // AF_UNIX: WRBAND alongside OUT for a stream socket too, which is
                // where it parts from TCP.
                "Unix stream, idle", withSocket SocketDomain.Unix SocketKind.Stream SocketPhase.Idle, 0x0314s
                "Unix datagram, idle", withSocket SocketDomain.Unix SocketKind.Datagram SocketPhase.Idle, 0x0304s
            ]
            |> List.fold
                (fun (rows, system) (name, add, level) ->
                    let fd, system = add system
                    (name, fd, level) :: rows, system
                )
                ([], linux)

        // The peer of the "peer alive" rows: a second end on the same
        // connection. Not itself a row, because it duplicates one.
        let _, system =
            withSocket SocketDomain.Inet SocketKind.Stream (SocketPhase.Established connection) system

        List.rev rows, system

    /// The heart of the matter: every one of the 65536 request masks, asked of
    /// every modelled object at once, answered exactly as the measured table
    /// says — each named bit, the four Linux ignores (`POLLREMOVE`, the
    /// unassigned 0x0800, and the kernel-internal 0x4000 and 0x8000), and every
    /// combination of them.
    [<Test>]
    let ``every request mask is answered as the measured Linux table says`` () : unit =
        let rows, system = measuredLevels

        let mismatches =
            [
                for mask in 0..0xFFFF do
                    let events = int16 (uint16 mask)
                    let entries = rows |> List.map (fun (_, fd, _) -> entry fd events)

                    let expected =
                        rows
                        |> List.map (fun (_, _, level) -> level &&& (events ||| pollErr ||| pollHup))

                    let expectedCount = expected |> List.filter (fun r -> r <> 0s) |> List.length

                    match UnixPoll.poll entries 0 system with
                    | Error refusal -> yield $"events 0x%04x{mask}: refused: %s{PollRefusal.describe refusal}"
                    | Ok (reported, count) ->
                        for (name, fd, _), expected, reported in List.zip3 rows expected reported do
                            if expected <> reported then
                                yield
                                    $"events 0x%04x{mask}, %s{name} (fd %d{fd}): expected 0x%04x{uint16 expected}, got 0x%04x{uint16 reported}"

                        if count <> expectedCount then
                            yield $"events 0x%04x{mask}: expected count %d{expectedCount}, got %d{count}"
            ]

        mismatches |> List.truncate 20 |> shouldEqual []

    // ------------------------------------------------------------------
    // Per-entry reports, one rule at a time
    // ------------------------------------------------------------------

    /// The launch shape this kernel models: stdin is the read end of a pipe
    /// whose write end the launcher closed, so it presents `HUP`; the output
    /// streams are write ends with space and a live reader, so they present
    /// `OUT|WRNORM`. Reported here whether or not they were asked for, which
    /// is what makes the `events = 0` column worth having.
    [<Test>]
    let ``the standard streams report the launch shape`` () : unit =
        for fd, level in [ 0, pollHup ; 1, pollOut ||| pollWrNorm ; 2, pollOut ||| pollWrNorm ] do
            pollOrFail [ entry fd everything ] 0 linux |> shouldEqual ([ level ], 1)

            // `HUP` is output-only and `OUT` is not: asking for nothing still
            // reports stdin's hangup, and reports nothing for the write ends.
            let unrequested = level &&& pollHup
            let count = if unrequested = 0s then 0 else 1

            pollOrFail [ entry fd 0s ] 0 linux |> shouldEqual ([ unrequested ], count)

    [<Test>]
    let ``an idle stream socket reports OUT, WRNORM and HUP`` () : unit =
        let fd, system = idleSocket linux

        pollOrFail [ entry fd everything ] 0 system
        |> shouldEqual ([ pollOut ||| pollWrNorm ||| pollHup ], 1)

    /// `POLLRDNORM` and `POLLWRNORM` are separate bits on Linux, reported with
    /// `IN` and `OUT` respectively but only when asked for themselves: asking
    /// for one does not report the other.
    [<Test>]
    let ``RDNORM and WRNORM answer only for themselves`` () : unit =
        let listenerFd, system =
            withSocket
                SocketDomain.Inet
                SocketKind.Stream
                (SocketPhase.Listening
                    {
                        Backlog = 1
                        Queue = [ ConnectionId 0L ]
                    })
                linux

        pollOrFail [ entry listenerFd pollRdNorm ] 0 system
        |> shouldEqual ([ pollRdNorm ], 1)

        pollOrFail [ entry listenerFd pollIn ] 0 system |> shouldEqual ([ pollIn ], 1)

        pollOrFail [ entry 1 pollWrNorm ] 0 system |> shouldEqual ([ pollWrNorm ], 1)
        pollOrFail [ entry 1 pollOut ] 0 system |> shouldEqual ([ pollOut ], 1)

    /// `POLLWRBAND` rides with `OUT` on a datagram socket and on every Unix-domain
    /// socket, and never on TCP: measured, and the one bit whose answer depends
    /// on more than the epoll level.
    [<Test>]
    let ``WRBAND is reported for datagram and Unix-domain sockets but never for TCP`` () : unit =
        let tcp, system = idleSocket linux

        let udp, system =
            withSocket SocketDomain.Inet SocketKind.Datagram SocketPhase.Idle system

        let unixStream, system =
            withSocket SocketDomain.Unix SocketKind.Stream SocketPhase.Idle system

        pollOrFail [ entry tcp pollWrBand ; entry udp pollWrBand ; entry unixStream pollWrBand ] 0 system
        |> shouldEqual ([ pollHup ; pollWrBand ; pollWrBand ||| pollHup ], 3)

    /// `POLLRDHUP` is reported when asked for, which the shim never does: a
    /// connection whose peer has gone presents it.
    [<Test>]
    let ``RDHUP is reported when asked for`` () : unit =
        let fd, system =
            withSocket SocketDomain.Inet SocketKind.Stream (SocketPhase.Established (ConnectionId 3L)) linux

        pollOrFail [ entry fd pollRdHup ] 0 system |> shouldEqual ([ pollRdHup ], 1)
        pollOrFail [ entry fd pollIn ] 0 system |> shouldEqual ([ pollIn ], 1)

    /// Measured: a regular file answers `IN|OUT|RDNORM|WRNORM` at every offset
    /// and under every access mode, and a directory answers the same. Files have
    /// no `->poll` handler, so the VFS default reports them always-ready --
    /// which is why nothing here varies with the file's contents or the
    /// description's position.
    [<Test>]
    let ``a regular file polls IN, OUT, RDNORM and WRNORM`` () : unit =
        for accessMode in
            [
                FileAccessMode.ReadOnly
                FileAccessMode.WriteOnly
                FileAccessMode.ReadWrite
            ] do
            let fd, system = withFile accessMode linux

            pollOrFail [ entry fd everything ] 0 system
            |> shouldEqual ([ pollIn ||| pollOut ||| pollRdNorm ||| pollWrNorm ], 1)

    /// Measured on both kernels: a negative descriptor is ignored, reports
    /// nothing, and does not count towards the return value. It is not an error
    /// and not NVAL, which is the distinction this row exists to pin.
    [<Test>]
    let ``a negative descriptor reports nothing and is not NVAL`` () : unit =
        for fd in [ -1 ; -2 ; System.Int32.MinValue ] do
            pollOrFail [ entry fd everything ; entry 0 everything ] 0 linux
            |> shouldEqual ([ 0s ; pollHup ], 1)

    /// POLLNVAL is a statement about the entry rather than a readiness level, so
    /// it is reported whatever was asked for, and nothing else is.
    [<Test>]
    let ``a descriptor that is not open is NVAL, whatever was asked`` () : unit =
        for events in [ 0s ; pollIn ; everything ; pollNval ; 0x0800s ] do
            pollOrFail [ entry 99 events ] 0 linux |> shouldEqual ([ pollNval ], 1)

    /// `IN` and `OUT` only when asked for; `ERR` and `HUP` unconditionally. The
    /// row that separates the two rules is a socket presenting both.
    [<Test>]
    let ``IN and OUT are masked by the request and HUP is not`` () : unit =
        let fd, system = idleSocket linux

        pollOrFail [ entry fd pollIn ] 0 system |> shouldEqual ([ pollHup ], 1)

    /// `ERR` is output-only too, and a pending refusal is the only modelled
    /// phase whose level carries it.
    [<Test>]
    let ``ERR is reported whatever was asked`` () : unit =
        let fd, system =
            withSocket SocketDomain.Inet SocketKind.Stream SocketPhase.RefusedPendingDelivery linux

        pollOrFail [ entry fd 0s ] 0 system |> shouldEqual ([ pollErr ||| pollHup ], 1)

    /// `PRI`, `RDBAND` and `MSG` are askable and never reported: no modelled
    /// object presents urgent data, priority-band data or a message.
    [<Test>]
    let ``PRI, RDBAND and MSG are never reported`` () : unit =
        let rows, system = measuredLevels
        let unreported = pollPri ||| pollRdBand ||| pollMsg

        let reported, _ =
            pollOrFail (rows |> List.map (fun (_, fd, _) -> entry fd unreported)) 0 system

        List.zip rows reported
        |> List.filter (fun (_, r) -> r &&& unreported <> 0s)
        |> shouldEqual []

    // ------------------------------------------------------------------
    // The count
    // ------------------------------------------------------------------

    /// What `poll(2)` returns is the number of entries carrying anything: not
    /// the number of entries, and not the number of conditions. The three
    /// numbers differ in this one call, which is what makes it a test.
    [<Test>]
    let ``the count is entries carrying anything`` () : unit =
        let fd, system = idleSocket linux

        // stdin (HUP), the socket (OUT|WRNORM|HUP, three conditions), an
        // ignored negative descriptor, and stdout with nothing asked for and
        // nothing to report.
        let entries =
            [ entry 0 everything ; entry fd everything ; entry -1 everything ; entry 1 0s ]

        let reported, triggered = pollOrFail entries 0 system

        List.length reported |> shouldEqual 4
        triggered |> shouldEqual 2

    [<Test>]
    let ``a poll of no entries answers zero`` () : unit =
        pollOrFail [] 0 linux |> shouldEqual ([], 0)

    // ------------------------------------------------------------------
    // Refusals
    // ------------------------------------------------------------------

    /// Ahead of the entries, and so of an empty entry list too, and whatever
    /// the request: Darwin's answer depends on which of its kqueue filters the
    /// requested bits select, so no request mask is answerable without that
    /// model.
    [<Test>]
    let ``a Darwin-flavoured kernel refuses every poll, whatever it asks`` () : unit =
        let darwin = systemOn SimulatedUnixPlatform.macOsArm64
        let expected = Error (PollRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)

        UnixPoll.poll [] 0 darwin |> shouldEqual expected

        for mask in 0..0xFFFF do
            let events = int16 (uint16 mask)

            UnixPoll.poll [ entry 0 events ; entry 99 events ] 0 darwin
            |> shouldEqual expected

    /// A guest can reach this where it cannot reach epoll's equivalent:
    /// `epoll_ctl` screens the targets it accepts, and `poll(2)` accepts any
    /// descriptor.
    [<Test>]
    let ``an entry naming a socket event port is refused`` () : unit =
        let portFd, registry =
            FileDescriptorRegistry.createSocketEventPort linux.Process.FileDescriptors

        let system =
            { linux with
                Process =
                    { linux.Process with
                        FileDescriptors = registry
                    }
            }

        UnixPoll.poll [ entry portFd everything ] 0 system
        |> shouldEqual (Error (PollRefusal.UnmodelledTarget portFd))

        // ...and it is refused from anywhere in the list, not only at the head:
        // the entries are all decoded before the answer, exactly as the caller
        // fills its whole array before the syscall.
        UnixPoll.poll [ entry 0 everything ; entry portFd everything ] 0 system
        |> shouldEqual (Error (PollRefusal.UnmodelledTarget portFd))

    /// A real `poll` inspects its entries in order, so the entry a refusal
    /// names is the first one it could not answer: a client bisecting its
    /// array is told the right one whichever way round it filled it.
    [<Test>]
    let ``the refusal names the first unmeasured entry in list order`` () : unit =
        let firstPort, registry =
            FileDescriptorRegistry.createSocketEventPort linux.Process.FileDescriptors

        let secondPort, registry = FileDescriptorRegistry.createSocketEventPort registry

        let system =
            { linux with
                Process =
                    { linux.Process with
                        FileDescriptors = registry
                    }
            }

        UnixPoll.poll [ entry firstPort everything ; entry secondPort everything ] 0 system
        |> shouldEqual (Error (PollRefusal.UnmodelledTarget firstPort))

        UnixPoll.poll
            [
                entry secondPort everything
                entry 0 everything
                entry firstPort everything
            ]
            0
            system
        |> shouldEqual (Error (PollRefusal.UnmodelledTarget secondPort))

    /// Nothing ready and a non-zero timeout is the only case that needs a park.
    /// A timeout of zero is answerable, which is what stops this from being "any
    /// poll that reports nothing".
    [<Test>]
    let ``nothing ready and a non-zero timeout is refused`` () : unit =
        // stdout asked only for bits it does not present: open, live, and
        // carrying nothing.
        let entries = [ entry 1 (pollIn ||| pollRdHup ||| 0x0800s) ]

        for timeout in [ -1 ; 1 ; 5000 ] do
            UnixPoll.poll entries timeout linux
            |> shouldEqual (Error (PollRefusal.WouldPark timeout))

        pollOrFail entries 0 linux |> shouldEqual ([ 0s ], 0)

    /// An entry carrying anything at all makes a real poll return immediately at
    /// any timeout, which is measured rather than assumed -- so a *ready* poll
    /// is answered at the same timeouts the row above refuses.
    [<Test>]
    let ``anything ready is answered at every timeout`` () : unit =
        for timeout in [ -1 ; 0 ; 1 ; 5000 ] do
            pollOrFail [ entry 0 everything ; entry 1 0s ] timeout linux
            |> snd
            |> shouldEqual 1

    /// An empty poll reports nothing, so it parks like any other poll that
    /// reports nothing -- which is the row that shows the refusal is about the
    /// *count*, not about having entries.
    [<Test>]
    let ``an empty poll with a timeout is refused`` () : unit =
        UnixPoll.poll [] -1 linux |> shouldEqual (Error (PollRefusal.WouldPark -1))
