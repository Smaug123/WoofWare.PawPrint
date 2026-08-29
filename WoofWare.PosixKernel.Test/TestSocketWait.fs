namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.admitSocketWait`: the screens `epoll_wait(2)` and `kevent(2)`
/// apply before either can deliver or sleep.
///
/// Five of the eight measured rows differ between the flavours, and a guest runs
/// one — so half of this table has never had a test that could reach it. The
/// orderings are what the fixture is really for: each adjacent pair below is
/// separated by an input that provokes exactly one of the two, which is how they
/// were measured in the first place.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketWait =

    let private context : string = "TestSocketWait"

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


    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    /// A system with an event port open, and the descriptor onto it.
    let private withPort (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        fd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// A system with a socket open, and the descriptor onto it. The "wrong kind
    /// of object" the two flavours answer differently about.
    let private withSocket (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
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

    let private admit
        (fd : int)
        (maxEvents : int)
        (buffer : UserBuffer)
        (system : UnixSystem<int, string>)
        : SocketWaitAdmission
        =
        match UnixSystem.admitSocketWait fd maxEvents buffer system with
        | Ok admission -> admission
        | Error refusal -> failwith $"expected an admission, got a refusal: %s{SocketWaitRefusal.describe refusal}"

    /// An address the four-level-paging limit rejects: `access_ok` refuses a
    /// range reaching into the kernel half.
    let private wild : UserBuffer = UserBuffer.Unmapped System.UInt64.MaxValue

    // ------------------------------------------------------------------
    // The descriptor
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not open is EBADF`` (platform : SimulatedUnixPlatform) : unit =
        admit 99 8 UserBuffer.Mapped (systemOn platform)
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EBADF)

    /// The descriptor comes first on both, which is what the widely-reproduced
    /// `do_epoll_wait` listing gets wrong: a closed descriptor answers EBADF
    /// even where `maxevents` and the buffer would each have had an answer of
    /// their own.
    [<TestCaseSource(nameof platforms)>]
    let ``the descriptor is resolved before the count and the buffer`` (platform : SimulatedUnixPlatform) : unit =
        admit 99 0 wild (systemOn platform)
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EBADF)

    /// The flavours part company here: kqueue folds "not a kqueue" into "bad
    /// descriptor" where epoll has EINVAL for it. Measured on a socket as well
    /// as on the other two kinds.
    [<Test>]
    let ``a live descriptor onto the wrong object splits by flavour`` () : unit =
        let rows =
            [
                SimulatedUnixPlatform.linuxX64, UnixError.EINVAL
                SimulatedUnixPlatform.macOsArm64, UnixError.EBADF
            ]

        for platform, expected in rows do
            let system = systemOn platform

            let fileFd, registry =
                FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly system.Process.FileDescriptors

            let system =
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors = registry
                        }
                }

            let socketFd, system = withSocket system

            for fd in [ 0 ; fileFd ; socketFd ] do
                admit fd 8 UserBuffer.Mapped system
                |> shouldEqual (SocketWaitAdmission.Failed expected)

    // ------------------------------------------------------------------
    // The count
    // ------------------------------------------------------------------

    /// The one input on which the flavours disagree about whether the call
    /// blocks at all: `kevent(kq, NULL, 0, evs, 0, NULL)` returns 0 immediately
    /// where `epoll_wait` with `maxevents == 0` is EINVAL.
    [<Test>]
    let ``a zero event count splits by flavour`` () : unit =
        let linuxFd, linux = withPort (systemOn SimulatedUnixPlatform.linuxX64)

        admit linuxFd 0 UserBuffer.Mapped linux
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EINVAL)

        let darwinFd, darwin = withPort (systemOn SimulatedUnixPlatform.macOsArm64)

        admit darwinFd 0 UserBuffer.Mapped darwin
        |> shouldEqual SocketWaitAdmission.NoEvents

    /// `EP_MAX_EVENTS` is `INT_MAX / EventSize`, and it is what keeps the
    /// `maxevents * EventSize` product below inside `int32`. Darwin caps
    /// nothing.
    [<Test>]
    let ``epoll caps the event count and kqueue does not`` () : unit =
        let linuxFd, linux = withPort (systemOn SimulatedUnixPlatform.linuxX64)

        admit linuxFd LinuxEpollLimits.MaxEvents UserBuffer.Mapped linux
        |> shouldEqual (SocketWaitAdmission.DeliverOrWait (OpenFileDescriptionId 3L, LinuxEpollLimits.MaxEvents))

        admit linuxFd (LinuxEpollLimits.MaxEvents + 1) UserBuffer.Mapped linux
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EINVAL)

        let darwinFd, darwin = withPort (systemOn SimulatedUnixPlatform.macOsArm64)

        admit darwinFd (LinuxEpollLimits.MaxEvents + 1) UserBuffer.Mapped darwin
        |> shouldEqual (SocketWaitAdmission.DeliverOrWait (OpenFileDescriptionId 3L, LinuxEpollLimits.MaxEvents + 1))

    [<TestCaseSource(nameof platforms)>]
    let ``a negative event count is a caller bug`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = withPort (systemOn platform)

        let e =
            Assert.Throws<exn> (fun () -> UnixSystem.admitSocketWait fd -1 UserBuffer.Mapped system |> ignore<_>)

        e.Message |> shouldContainText "is negative, which neither kernel is ever asked"

    // ------------------------------------------------------------------
    // The buffer
    // ------------------------------------------------------------------

    /// Only epoll screens a buffer, and it screens a *range* rather than
    /// mappedness: `access_ok` rejects what reaches into the kernel half, so a
    /// merely-unmapped userspace address passes and the wait then sleeps.
    [<Test>]
    let ``only epoll screens the buffer`` () : unit =
        let linuxFd, linux = withPort (systemOn SimulatedUnixPlatform.linuxX64)

        admit linuxFd 8 wild linux
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EFAULT)

        // An ordinary low userspace address is in range, and sleeps.
        admit linuxFd 8 (UserBuffer.Unmapped 4096UL) linux
        |> shouldEqual (SocketWaitAdmission.DeliverOrWait (OpenFileDescriptionId 3L, 8))

        let darwinFd, darwin = withPort (systemOn SimulatedUnixPlatform.macOsArm64)

        admit darwinFd 8 wild darwin
        |> shouldEqual (SocketWaitAdmission.DeliverOrWait (OpenFileDescriptionId 3L, 8))

    /// The buffer screen is the *third* epoll question, behind the count and
    /// ahead of the object kind. Each of these two inputs would answer
    /// differently at any other position.
    [<Test>]
    let ``epoll screens count, then buffer, then object kind`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64
        let socketFd, system = withSocket system
        let portFd, system = withPort system

        // Count beats buffer: a zero count on a port with an unscreenable
        // buffer is EINVAL, not EFAULT.
        admit portFd 0 wild system
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EINVAL)

        // Buffer beats object kind: the same buffer on a *socket* is EFAULT,
        // not the EINVAL the wrong-kind arm would give.
        admit socketFd 8 wild system
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EFAULT)

        // ...and with a good buffer the socket does answer EINVAL.
        admit socketFd 8 UserBuffer.Mapped system
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EINVAL)

    /// The extent screened is `maxevents * EventSize`, not one element: a count
    /// that puts the *end* of the range past the limit faults even though its
    /// base address does not.
    [<Test>]
    let ``the screened extent is the whole event array`` () : unit =
        let fd, linux = withPort (systemOn SimulatedUnixPlatform.linuxX64)
        let limit = ObservedUserAddressLimit.X64FourLevelPaging

        // A base one element below the limit: room for one event, not for two.
        let base' = limit - uint64 LinuxEpollLimits.EventSize

        admit fd 1 (UserBuffer.Unmapped base') linux
        |> shouldEqual (SocketWaitAdmission.DeliverOrWait (OpenFileDescriptionId 3L, 1))

        admit fd 2 (UserBuffer.Unmapped base') linux
        |> shouldEqual (SocketWaitAdmission.Failed UnixError.EFAULT)

    /// A buffer with no address at all reaches the screen with nothing to
    /// compare, so the flavour that screens refuses and the flavour that does
    /// not proceeds. Answering "in range" on the screening flavour would be a
    /// guess, and a guest-visible one.
    [<Test>]
    let ``an addressless buffer is refused only where the flavour screens`` () : unit =
        let linuxFd, linux = withPort (systemOn SimulatedUnixPlatform.linuxX64)

        UnixSystem.admitSocketWait linuxFd 8 UserBuffer.Addressless linux
        |> shouldEqual (Error (SocketWaitRefusal.Buffer BufferRefusal.AddresslessAtScreen))

        let darwinFd, darwin = withPort (systemOn SimulatedUnixPlatform.macOsArm64)

        admit darwinFd 8 UserBuffer.Addressless darwin
        |> shouldEqual (SocketWaitAdmission.DeliverOrWait (OpenFileDescriptionId 3L, 8))

    /// An opaque buffer names real mapped memory, so it passes every address
    /// check; it has no answer only where bytes are wanted, which is at
    /// delivery rather than here.
    [<TestCaseSource(nameof platforms)>]
    let ``an opaque buffer passes the screen`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = withPort (systemOn platform)

        admit fd 8 UserBuffer.Opaque system
        |> shouldEqual (SocketWaitAdmission.DeliverOrWait (OpenFileDescriptionId 3L, 8))
