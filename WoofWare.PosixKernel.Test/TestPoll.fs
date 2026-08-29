namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.poll`, driven directly on a constructed system.
///
/// The tier that reaches what `sourcesPure/SocketPoll.cs` cannot: the Darwin
/// refusal (a guest runs one flavour, and PawPrint's guests run Linux), the
/// socket-event-port entry (no managed caller polls one), and the park refusal
/// (a guest that reached it would abort the interpreter rather than report).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPoll =

    let private context : string = "TestPoll"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

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

    /// Everything, so that a report is compared against a mask rather than
    /// against the request that produced it.
    let private all : PollEvents =
        {
            In = true
            Pri = true
            Out = true
            Err = true
            Hup = true
            Nval = true
        }

    let private entry (fd : int) (requested : PollEvents) : PollEntry =
        {
            Fd = fd
            Requested = requested
        }

    let private pollOrFail
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<int, string>)
        : PollEvents list * int
        =
        match UnixSystem.poll entries milliseconds system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{PollRefusal.describe refusal}"

    /// An idle stream socket, which on Linux presents `OUT|HUP`.
    let private idleSocket (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let socket =
            {
                Domain = SocketDomain.InterNetwork
                Kind = SocketKind.Stream
                Protocol = SocketProtocol.Tcp
                Binding = None
                ReuseAddress = false
                Phase = SocketPhase.Idle
            }

        let fd, registry =
            FileDescriptorRegistry.createSocket (SocketId 0L) system.Process.FileDescriptors

        fd,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.add (SocketId 0L) socket system.Machine.Sockets
                    NextSocketId = SocketId 1L
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    // ------------------------------------------------------------------
    // Per-entry reports
    // ------------------------------------------------------------------

    /// The launch shape this kernel models: stdin is the read end of a pipe
    /// whose write end the launcher closed, so it presents `HUP`; the output
    /// streams are write ends with space and a live reader, so they present
    /// `OUT`. Reported here whether or not they were asked for, which is what
    /// makes the `events = 0` column worth having.
    [<Test>]
    let ``the standard streams report the launch shape`` () : unit =
        let rows =
            [
                0,
                { PollEvents.none with
                    Hup = true
                }
                1,
                { PollEvents.none with
                    Out = true
                }
                2,
                { PollEvents.none with
                    Out = true
                }
            ]

        for fd, expected in rows do
            pollOrFail [ entry fd all ] 0 linux |> shouldEqual ([ expected ], 1)

            // `HUP` is output-only and `OUT` is not: asking for nothing still
            // reports stdin's hangup, and reports nothing for the write ends.
            let unrequested = if expected.Hup then expected else PollEvents.none
            let count = if expected.Hup then 1 else 0

            pollOrFail [ entry fd PollEvents.none ] 0 linux
            |> shouldEqual ([ unrequested ], count)

    [<Test>]
    let ``an idle stream socket reports OUT and HUP`` () : unit =
        let fd, system = idleSocket linux

        pollOrFail [ entry fd all ] 0 system
        |> shouldEqual (
            [
                { PollEvents.none with
                    Out = true
                    Hup = true
                }
            ],
            1
        )

    /// Measured (`pollgaps.c`): a regular file answers `IN|OUT` at every offset
    /// and under `O_RDONLY` as much as `O_RDWR`, and a directory answers the
    /// same. Files have no `->poll` handler, so the VFS default reports them
    /// always-ready -- which is why nothing here varies with the file's
    /// contents or the description's position.
    [<Test>]
    let ``a regular file polls IN and OUT`` () : unit =
        for accessMode in
            [
                FileAccessMode.ReadOnly
                FileAccessMode.WriteOnly
                FileAccessMode.ReadWrite
            ] do
            let fd, registry =
                FileDescriptorRegistry.openFile (InodeNumber 1L) accessMode linux.Process.FileDescriptors

            let system =
                { linux with
                    Process =
                        { linux.Process with
                            FileDescriptors = registry
                        }
                }

            pollOrFail [ entry fd all ] 0 system
            |> shouldEqual (
                [
                    { PollEvents.none with
                        In = true
                        Out = true
                    }
                ],
                1
            )

    /// Measured on both kernels: a negative descriptor is ignored, reports
    /// nothing, and does not count towards the return value. It is not an error
    /// and not NVAL, which is the distinction this row exists to pin.
    [<Test>]
    let ``a negative descriptor reports nothing and is not NVAL`` () : unit =
        for fd in [ -1 ; -2 ; System.Int32.MinValue ] do
            pollOrFail [ entry fd all ; entry 0 all ] 0 linux
            |> shouldEqual (
                [
                    PollEvents.none
                    { PollEvents.none with
                        Hup = true
                    }
                ],
                1
            )

    /// POLLNVAL is a statement about the entry rather than a readiness level, so
    /// it is reported whether or not anything was asked for.
    [<Test>]
    let ``a descriptor that is not open is NVAL, requested or not`` () : unit =
        let expected =
            [
                { PollEvents.none with
                    Nval = true
                }
            ],
            1

        pollOrFail [ entry 99 all ] 0 linux |> shouldEqual expected
        pollOrFail [ entry 99 PollEvents.none ] 0 linux |> shouldEqual expected

    /// `IN` and `OUT` only when asked for; `ERR` and `HUP` unconditionally. The
    /// row that separates the two rules is a socket presenting both.
    [<Test>]
    let ``IN and OUT are masked by the request and HUP is not`` () : unit =
        let fd, system = idleSocket linux

        pollOrFail
            [
                entry
                    fd
                    { PollEvents.none with
                        In = true
                    }
            ]
            0
            system
        |> shouldEqual (
            [
                { PollEvents.none with
                    Hup = true
                }
            ],
            1
        )

    /// `PRI` is askable and never reported: `ReadinessLevel` has no urgent-data
    /// condition to project, and no modelled Linux phase sets it.
    [<Test>]
    let ``PRI is never reported`` () : unit =
        let fd, system = idleSocket linux

        let reported, _ =
            pollOrFail
                [
                    entry
                        fd
                        { all with
                            Pri = true
                        }
                ]
                0
                system

        reported |> List.exists (fun events -> events.Pri) |> shouldEqual false

    // ------------------------------------------------------------------
    // The count
    // ------------------------------------------------------------------

    /// What `poll(2)` returns is the number of entries carrying anything: not
    /// the number of entries, and not the number of conditions. The three
    /// numbers differ in this one call, which is what makes it a test.
    [<Test>]
    let ``the count is entries carrying anything`` () : unit =
        let fd, system = idleSocket linux

        // stdin (HUP), the socket (OUT|HUP, two conditions), an ignored negative
        // descriptor, and stdout with nothing asked for and nothing to report.
        let entries =
            [ entry 0 all ; entry fd all ; entry -1 all ; entry 1 PollEvents.none ]

        let reported, triggered = pollOrFail entries 0 system

        List.length reported |> shouldEqual 4
        triggered |> shouldEqual 2

    [<Test>]
    let ``a poll of no entries answers zero`` () : unit =
        pollOrFail [] 0 linux |> shouldEqual ([], 0)

    // ------------------------------------------------------------------
    // Refusals
    // ------------------------------------------------------------------

    /// Ahead of the entries, and so of an empty entry list too: the coarseness
    /// is deliberate, because a zero-entry Darwin poll would otherwise be the
    /// one answerable row of a flavour whose every other row refuses.
    [<Test>]
    let ``a Darwin-flavoured kernel refuses every poll`` () : unit =
        let darwin = systemOn SimulatedUnixPlatform.macOsArm64
        let expected = Error (PollRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)

        UnixSystem.poll [] 0 darwin |> shouldEqual expected
        UnixSystem.poll [ entry 0 all ] 0 darwin |> shouldEqual expected
        UnixSystem.poll [ entry 99 all ] 0 darwin |> shouldEqual expected

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

        UnixSystem.poll [ entry portFd all ] 0 system
        |> shouldEqual (Error (PollRefusal.UnmeasuredTarget portFd))

        // ...and it is refused from anywhere in the list, not only at the head:
        // the entries are all decoded before the answer, exactly as the caller
        // fills its whole array before the syscall.
        UnixSystem.poll [ entry 0 all ; entry portFd all ] 0 system
        |> shouldEqual (Error (PollRefusal.UnmeasuredTarget portFd))

    /// Nothing ready and a non-zero timeout is the only case that needs a park.
    /// A timeout of zero is answerable, which is what stops this from being "any
    /// poll that reports nothing".
    [<Test>]
    let ``nothing ready and a non-zero timeout is refused`` () : unit =
        // stdout with nothing asked for: open, live, and carrying nothing.
        let entries = [ entry 1 PollEvents.none ]

        for timeout in [ -1 ; 1 ; 5000 ] do
            UnixSystem.poll entries timeout linux
            |> shouldEqual (Error (PollRefusal.WouldPark timeout))

        pollOrFail entries 0 linux |> shouldEqual ([ PollEvents.none ], 0)

    /// An entry carrying anything at all makes a real poll return immediately at
    /// any timeout, which is measured rather than assumed -- so a *ready* poll
    /// is answered at the same timeouts the row above refuses.
    [<Test>]
    let ``anything ready is answered at every timeout`` () : unit =
        for timeout in [ -1 ; 0 ; 1 ; 5000 ] do
            pollOrFail [ entry 0 all ; entry 1 PollEvents.none ] timeout linux
            |> snd
            |> shouldEqual 1

    /// An empty poll reports nothing, so it parks like any other poll that
    /// reports nothing -- which is the row that shows the refusal is about the
    /// *count*, not about having entries.
    [<Test>]
    let ``an empty poll with a timeout is refused`` () : unit =
        UnixSystem.poll [] -1 linux |> shouldEqual (Error (PollRefusal.WouldPark -1))

    /// `pollReadinessOfDescription` refuses the two entries `poll` screens
    /// first, and says so rather than inventing a level.
    [<Test>]
    let ``the level function refuses what poll screens`` () : unit =
        let e =
            Assert.Throws<exn> (fun () ->
                UnixSystem.pollReadinessOfDescription (OpenFileDescriptionId 99L) linux
                |> ignore<_>
            )

        e.Message |> shouldContainText "names no live open file description"
