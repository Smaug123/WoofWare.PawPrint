namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The screens `epoll_wait(2)` (`UnixPoll.epollWait`) and the wait half of
/// `kevent(2)` (`UnixKqueue.kevent`, with an empty changelist and a null
/// timeout) apply before either can deliver or sleep, side by side.
///
/// Five of the eight measured rows differ between the two calls. The orderings
/// are what the fixture is really for: each adjacent pair below is separated by
/// an input that provokes exactly one of the two, which is how they were
/// measured in the first place.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketWait =

    let private context : string = "TestSocketWait"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    /// A simulated process on the flavour asked for, before anything has
    /// happened to it.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        { system with
            Machine =
                { system.Machine with
                    LocalRoutes = []
                }
        }


    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    /// Each Linux preset with the `epoll_wait` facts measured on its
    /// architecture: `sizeof(struct epoll_event)`, the largest `maxevents` that
    /// is not EINVAL, and the machine's default `TASK_SIZE_MAX`. Literals, so
    /// that a derivation that went wrong in the library cannot also go wrong here.
    let private linuxEpollRows : (SimulatedUnixPlatform * int * int * uint64) list =
        [
            SimulatedUnixPlatform.linuxX64, 12, 178_956_970, 0x0000_7FFF_FFFF_F000UL
            SimulatedUnixPlatform.linuxArm64, 16, 134_217_727, 0x0001_0000_0000_0000UL
        ]

    let private linuxEpollCases : TestCaseData list =
        linuxEpollRows
        |> List.map (fun (platform, size, cap, limit) -> TestCaseData (platform, size, cap, limit))

    let private task : int = 1

    /// A system with the flavour's event queue open -- an epoll instance or a
    /// kqueue -- and the descriptor onto it.
    let private withEventQueue (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux ->
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok (fd, system)) -> fd, system
            | other -> failwith $"expected an epoll instance, got %A{other}"
        | SimulatedUnixFlavour.Darwin ->
            match UnixKqueue.kqueue system with
            | Ok (fd, system) -> fd, system
            | Error refusal -> failwith $"expected a kqueue, got %s{KqueueRefusal.describe refusal}"

    /// A system with a socket open, and the descriptor onto it. The "wrong kind
    /// of object" the two flavours answer differently about.
    let private withSocket (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let socket =
            {
                Domain = SocketDomain.Inet
                Kind = SocketKind.Stream
                Protocol = SocketProtocol.Tcp
                Binding = None
                ReuseAddress = false
                Options = SocketOptions.initial
                Phase = SocketPhase.Idle
            }

        let fd, registry =
            FileDescriptorRegistry.createSocket (SocketId 0L) (UnixSystemState.fileDescriptors system)

        fd,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.add (SocketId 0L) socket system.Machine.Sockets
                    NextSocketId = SocketId 1L
                }
        }
        |> UnixSystemState.withFileDescriptors registry

    /// How a wait with nothing to deliver ended up.
    [<RequireQualifiedAccess>]
    type private Waited =
        /// The call failed with this errno.
        | Failed of UnixError
        /// The call returned no events at once.
        | NoEvents
        /// The call parked on this event queue for up to this many events.
        | Parked of queue : OpenFileDescriptionId * maxEvents : int

    /// The flavour's wait for socket events, for ever: `epoll_wait` with a
    /// timeout of -1, or `kevent` with no changes and a null timeout.
    let private wait (fd : int) (maxEvents : int) (buffer : UserBuffer) (system : UnixSystem<int, string>) : Waited =
        let system = Tasks.ensure task system

        let parkedOn (system : UnixSystem<int, string>) : Waited =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.EpollWait wait) -> Waited.Parked (wait.Epoll, wait.MaxEvents)
            | Some (ParkedSyscall.Kevent wait) -> Waited.Parked (wait.Kqueue, wait.MaxEvents)
            | other -> failwith $"expected a wait's park, got %A{other}"

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux ->
            match UnixPoll.epollWait task fd maxEvents buffer -1 system with
            | Ok (EpollWaitOutcome.Failed error, after) ->
                after |> shouldEqual system
                Waited.Failed error
            | Ok (EpollWaitOutcome.Answered [], _) -> Waited.NoEvents
            | Ok (EpollWaitOutcome.WouldBlock _, parked) -> parkedOn parked
            | other -> failwith $"unexpected epoll_wait answer %A{other}"
        | SimulatedUnixFlavour.Darwin ->
            match UnixKqueue.kevent task fd 0 [] maxEvents buffer KeventTimeout.Null system with
            | Ok (KeventOutcome.Failed error, after) ->
                after |> shouldEqual system
                Waited.Failed error
            | Ok (KeventOutcome.Answered [], after) ->
                after |> shouldEqual system
                Waited.NoEvents
            | Ok (KeventOutcome.WouldBlock _, parked) -> parkedOn parked
            | other -> failwith $"unexpected kevent answer %A{other}"

    /// An address the four-level-paging limit rejects: `access_ok` refuses a
    /// range reaching into the kernel half.
    let private wild : UserBuffer = UserBuffer.Unmapped System.UInt64.MaxValue

    // ------------------------------------------------------------------
    // The descriptor
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not open is EBADF`` (platform : SimulatedUnixPlatform) : unit =
        wait 99 8 UserBuffer.Mapped (systemOn platform)
        |> shouldEqual (Waited.Failed UnixError.EBADF)

    /// The descriptor comes first on both, which is what the widely-reproduced
    /// `do_epoll_wait` listing gets wrong: a closed descriptor answers EBADF
    /// even where `maxevents` and the buffer would each have had an answer of
    /// their own.
    [<TestCaseSource(nameof platforms)>]
    let ``the descriptor is resolved before the count and the buffer`` (platform : SimulatedUnixPlatform) : unit =
        wait 99 0 wild (systemOn platform)
        |> shouldEqual (Waited.Failed UnixError.EBADF)

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
                FileDescriptorRegistry.openFile
                    (InodeNumber 1L)
                    FileAccessMode.ReadOnly
                    (UnixSystemState.fileDescriptors system)

            let system = UnixSystemState.withFileDescriptors registry system

            let socketFd, system = withSocket system

            for fd in [ 0 ; fileFd ; socketFd ] do
                wait fd 8 UserBuffer.Mapped system |> shouldEqual (Waited.Failed expected)

    // ------------------------------------------------------------------
    // The count
    // ------------------------------------------------------------------

    /// The one input on which the flavours disagree about whether the call
    /// blocks at all: `kevent(kq, NULL, 0, evs, 0, NULL)` returns 0 immediately
    /// where `epoll_wait` with `maxevents == 0` is EINVAL.
    [<Test>]
    let ``a zero event count splits by flavour`` () : unit =
        let linuxFd, linux = withEventQueue (systemOn SimulatedUnixPlatform.linuxX64)

        wait linuxFd 0 UserBuffer.Mapped linux
        |> shouldEqual (Waited.Failed UnixError.EINVAL)

        let darwinFd, darwin = withEventQueue (systemOn SimulatedUnixPlatform.macOsArm64)

        wait darwinFd 0 UserBuffer.Mapped darwin |> shouldEqual Waited.NoEvents

    /// `EP_MAX_EVENTS` is `INT_MAX / sizeof(struct epoll_event)`, so it is the
    /// platform's architecture that sets it, and it is what keeps the
    /// `maxevents * sizeof` product below inside `int32`.
    [<TestCaseSource(nameof linuxEpollCases)>]
    let ``epoll caps the event count at its architecture's bound``
        (platform : SimulatedUnixPlatform, _size : int, cap : int, _limit : uint64)
        : unit
        =
        let fd, linux = withEventQueue (systemOn platform)

        wait fd cap UserBuffer.Mapped linux
        |> shouldEqual (Waited.Parked (OpenFileDescriptionId 3L, cap))

        wait fd (cap + 1) UserBuffer.Mapped linux
        |> shouldEqual (Waited.Failed UnixError.EINVAL)

    /// kqueue caps nothing: every count past either architecture's epoll bound,
    /// up to `INT_MAX`, is admitted. Measured on Darwin 27.0.0 over a stride of
    /// 65521 across [1, INT_MAX].
    [<Test>]
    let ``kqueue does not cap the event count`` () : unit =
        let darwinFd, darwin = withEventQueue (systemOn SimulatedUnixPlatform.macOsArm64)

        for count in [ 134_217_728 ; 178_956_971 ; System.Int32.MaxValue ] do
            wait darwinFd count UserBuffer.Mapped darwin
            |> shouldEqual (Waited.Parked (OpenFileDescriptionId 3L, count))

    /// A negative count is answered as 0 is: epoll's EINVAL behind the
    /// descriptor's EBADF (measured, `epoll-wait.c` section G), and kqueue's
    /// immediate return with no events (measured, `kevent-negative-count.c`).
    /// A client that calls the wait again after a signal may read its count
    /// afresh, unscreened, so either kernel can be asked one.
    [<TestCaseSource(nameof platforms)>]
    let ``a negative event count is answered as zero is`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = withEventQueue (systemOn platform)

        for descriptor in [ fd ; 99 ] do
            for buffer in [ UserBuffer.Mapped ; wild ] do
                let zero = wait descriptor 0 buffer system

                for count in [ -1 ; -2 ; System.Int32.MinValue ] do
                    wait descriptor count buffer system |> shouldEqual zero

        wait 99 -1 UserBuffer.Mapped system
        |> shouldEqual (Waited.Failed UnixError.EBADF)

        wait fd -1 UserBuffer.Mapped system
        |> shouldEqual (
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> Waited.Failed UnixError.EINVAL
            | SimulatedUnixFlavour.Darwin -> Waited.NoEvents
        )

    // ------------------------------------------------------------------
    // The buffer
    // ------------------------------------------------------------------

    /// Only epoll screens a buffer, and it screens a *range* rather than
    /// mappedness: `access_ok` rejects what reaches into the kernel half, so a
    /// merely-unmapped userspace address passes and the wait then sleeps.
    [<Test>]
    let ``only epoll screens the buffer`` () : unit =
        let linuxFd, linux = withEventQueue (systemOn SimulatedUnixPlatform.linuxX64)

        wait linuxFd 8 wild linux |> shouldEqual (Waited.Failed UnixError.EFAULT)

        // An ordinary low userspace address is in range, and sleeps.
        wait linuxFd 8 (UserBuffer.Unmapped 4096UL) linux
        |> shouldEqual (Waited.Parked (OpenFileDescriptionId 3L, 8))

        let darwinFd, darwin = withEventQueue (systemOn SimulatedUnixPlatform.macOsArm64)

        wait darwinFd 8 wild darwin
        |> shouldEqual (Waited.Parked (OpenFileDescriptionId 3L, 8))

    /// The buffer screen is the *third* epoll question, behind the count and
    /// ahead of the object kind. Each of these two inputs would answer
    /// differently at any other position.
    [<Test>]
    let ``epoll screens count, then buffer, then object kind`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64
        let socketFd, system = withSocket system
        let queueFd, system = withEventQueue system

        // Count beats buffer: a zero count on an epoll instance with an unscreenable
        // buffer is EINVAL, not EFAULT.
        wait queueFd 0 wild system |> shouldEqual (Waited.Failed UnixError.EINVAL)

        // Buffer beats object kind: the same buffer on a *socket* is EFAULT,
        // not the EINVAL the wrong-kind arm would give.
        wait socketFd 8 wild system |> shouldEqual (Waited.Failed UnixError.EFAULT)

        // ...and with a good buffer the socket does answer EINVAL.
        wait socketFd 8 UserBuffer.Mapped system
        |> shouldEqual (Waited.Failed UnixError.EINVAL)

    /// The extent screened is `maxevents * sizeof(struct epoll_event)`, not one
    /// element: a count that puts the *end* of the range past the limit faults
    /// even though its base address does not. And the element is the
    /// architecture's: a base twelve bytes below the limit holds one packed
    /// x86-64 event but not one padded arm64 event, which is the row an
    /// architecture mix-up lands on.
    [<TestCaseSource(nameof linuxEpollCases)>]
    let ``the screened extent is the whole event array``
        (platform : SimulatedUnixPlatform, size : int, _cap : int, limit : uint64)
        : unit
        =
        let fd, linux = withEventQueue (systemOn platform)

        // A base one element below the limit: room for one event, not for two.
        let base' = limit - uint64 size

        wait fd 1 (UserBuffer.Unmapped base') linux
        |> shouldEqual (Waited.Parked (OpenFileDescriptionId 3L, 1))

        wait fd 2 (UserBuffer.Unmapped base') linux
        |> shouldEqual (Waited.Failed UnixError.EFAULT)

        let packed = limit - 12UL

        wait fd 1 (UserBuffer.Unmapped packed) linux
        |> shouldEqual (
            if size = 12 then
                Waited.Parked (OpenFileDescriptionId 3L, 1)
            else
                Waited.Failed UnixError.EFAULT
        )

    /// A buffer with no address at all reaches the screen with nothing to
    /// compare, so the flavour that screens refuses and the flavour that does
    /// not proceeds. Answering "in range" on the screening flavour would be a
    /// guess, and one visible to a process.
    [<Test>]
    let ``an addressless buffer is refused only where the flavour screens`` () : unit =
        let linuxFd, linux = withEventQueue (systemOn SimulatedUnixPlatform.linuxX64)

        UnixPoll.epollWait task linuxFd 8 UserBuffer.Addressless -1 (Tasks.ensure task linux)
        |> shouldEqual (Error (EpollWaitRefusal.Buffer BufferRefusal.AddresslessAtScreen))

        let darwinFd, darwin = withEventQueue (systemOn SimulatedUnixPlatform.macOsArm64)

        wait darwinFd 8 UserBuffer.Addressless darwin
        |> shouldEqual (Waited.Parked (OpenFileDescriptionId 3L, 8))

    /// An opaque buffer names real mapped memory, so it passes every address
    /// check; it has no answer only where bytes are wanted, which is at
    /// delivery rather than here.
    [<TestCaseSource(nameof platforms)>]
    let ``an opaque buffer passes the screen`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = withEventQueue (systemOn platform)

        wait fd 8 UserBuffer.Opaque system
        |> shouldEqual (Waited.Parked (OpenFileDescriptionId 3L, 8))
