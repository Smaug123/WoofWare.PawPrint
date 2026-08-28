namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFileDescriptorRegistry =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// The access mode is *derived* from the role here rather than passed in, so
    /// that a test comparing against this cannot accidentally assert a mode of its
    /// own choosing. Which mode each role gets is asserted directly, once, in
    /// `initial gives each standard stream the access mode a redirected launch
    /// would`.
    let private standardStream (role : FileDescriptorRole) : OpenFileDescription =
        {
            Target = OpenFileTarget.StandardStream role
            AccessMode =
                match role with
                | FileDescriptorRole.StandardInput -> FileAccessMode.ReadOnly
                | FileDescriptorRole.StandardOutput
                | FileDescriptorRole.StandardError -> FileAccessMode.WriteOnly
            NonBlocking = false
            Flock = None
        }

    let private openOn (inode : InodeNumber) : OpenFileDescription =
        {
            Target = OpenFileTarget.File (inode, 0L)
            AccessMode = FileAccessMode.ReadOnly
            NonBlocking = false
            Flock = None
        }

    /// `close`, for tests whose subject is the descriptor table rather than the
    /// kernel object a close may have destroyed. That second half is
    /// `UnixSystem.close`'s business, and is asserted in
    /// `TestEmulatedKernelSockets`.
    let private closeOnly
        (fd : int)
        (registry : FileDescriptorRegistry)
        : Result<FileDescriptorRegistry, FileDescriptorCloseError>
        =
        FileDescriptorRegistry.close fd registry |> Result.map fst

    let private someInode : InodeNumber = InodeNumber 42L
    let private otherInode : InodeNumber = InodeNumber 43L

    [<Test>]
    let ``openFile takes the lowest free descriptor and a fresh description`` () : unit =
        let a, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        a |> shouldEqual 3

        let b, registry =
            FileDescriptorRegistry.openFile otherInode FileAccessMode.ReadOnly registry

        b |> shouldEqual 4

        FileDescriptorRegistry.tryFind a registry
        |> shouldEqual (Some (openOn someInode))

        FileDescriptorRegistry.tryFind b registry
        |> shouldEqual (Some (openOn otherInode))

        FileDescriptorRegistry.assertInvariants "openFile" registry
        |> ignore<FileDescriptorRegistry>

    /// The distinction `dup` exists to draw, in the other direction: two opens
    /// of the *same* inode are two descriptions, so the offsets and `flock`
    /// locks they will later carry are separate. Comparing the payloads would
    /// not show this — they are equal — so the identities are what is asserted.
    [<Test>]
    let ``two opens of one inode are two descriptions, unlike dup`` () : unit =
        let a, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        let b, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly registry

        FileDescriptorRegistry.tryFind a registry
        |> shouldEqual (FileDescriptorRegistry.tryFind b registry)

        FileDescriptorRegistry.tryFindId a registry
        |> shouldNotEqual (FileDescriptorRegistry.tryFindId b registry)

        match FileDescriptorRegistry.dup a registry with
        | Error e -> failwith $"expected dup to succeed, got %O{e}"
        | Ok (duplicate, registry) ->
            FileDescriptorRegistry.tryFindId duplicate registry
            |> shouldEqual (FileDescriptorRegistry.tryFindId a registry)

    /// Two sockets are two descriptions *and* two `flock` objects — the exact
    /// opposite of the two ports below, and the reason `OpenFileObject.Socket`
    /// carries an identity where `AnonymousInode` does not.
    ///
    /// Measured on Linux 6.18.5: two `socket(2)` calls report distinct inodes
    /// (4127 and 4130, `st_dev` 8 for both, on `sockfs`), and
    /// `flock(LOCK_EX|LOCK_NB)` succeeds on each. Two epoll ports on
    /// `anon_inodefs` share one inode and so exclude one another.
    ///
    /// No guest observer: Darwin refuses `flock` on a socket outright, and the
    /// Linux-flavour observation would need two sockets and two locks in one
    /// guest, which is what this asserts instead.
    [<Test>]
    let ``two sockets are two descriptions and two flock objects`` () : unit =
        let a, registry =
            FileDescriptorRegistry.createSocket (SocketId 0L) FileDescriptorRegistry.initial

        let b, registry = FileDescriptorRegistry.createSocket (SocketId 1L) registry

        a |> shouldEqual 3
        b |> shouldEqual 4

        FileDescriptorRegistry.tryFindId a registry
        |> shouldNotEqual (FileDescriptorRegistry.tryFindId b registry)

        // Two objects, so they do *not* contend.
        FileDescriptorRegistry.tryFindObject a registry
        |> shouldNotEqual (FileDescriptorRegistry.tryFindObject b registry)

        // And that shows up as behaviour rather than only as inequality: an
        // exclusive lock on one leaves the other free to take its own.
        let registry =
            match FileDescriptorRegistry.flock a (FlockRequest.Acquire FlockMode.Exclusive) registry with
            | registry, None -> registry
            | _, Some e -> failwith $"expected the first lock to be granted, got %O{e}"

        match FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Exclusive) registry with
        | registry, None -> FileDescriptorRegistry.assertInvariants "two sockets locked" registry |> ignore
        | _, Some e -> failwith $"expected the second lock to be granted too, got %O{e}"

    /// A `dup` of a socket descriptor names the same socket, not a copy of it.
    /// So the pair shares one `flock` slot and converts rather than contending —
    /// which is what makes the distinctness above a fact about *sockets* rather
    /// than about descriptors.
    [<Test>]
    let ``dup of a socket names the same socket`` () : unit =
        let a, registry =
            FileDescriptorRegistry.createSocket (SocketId 0L) FileDescriptorRegistry.initial

        match FileDescriptorRegistry.dup a registry with
        | Error e -> failwith $"expected dup to succeed, got %O{e}"
        | Ok (b, registry) ->

        FileDescriptorRegistry.tryFindId b registry
        |> shouldEqual (FileDescriptorRegistry.tryFindId a registry)

        FileDescriptorRegistry.tryFindObject b registry
        |> shouldEqual (FileDescriptorRegistry.tryFindObject a registry)

    /// The description a socket gets. The triple it names is the socket table's
    /// business, and is asserted in `TestEmulatedKernelSockets`.
    ///
    /// The access mode is not cosmetic: `SystemNative_Read` and
    /// `SystemNative_Write` test it *before* they look at the target, so
    /// anything narrower than `ReadWrite` would answer EBADF where a real socket
    /// answers about its connection state (measured: ENOTCONN, EINVAL, or a
    /// block — never EBADF).
    [<Test>]
    let ``a fresh socket description names its socket and is ReadWrite`` () : unit =
        let fd, registry =
            FileDescriptorRegistry.createSocket (SocketId 4L) FileDescriptorRegistry.initial

        match FileDescriptorRegistry.tryFind fd registry with
        | None -> failwith "the socket descriptor is not live"
        | Some description ->

        description.AccessMode |> shouldEqual FileAccessMode.ReadWrite
        // `socket(2)` takes no lock, exactly as `open(2)` does not.
        description.Flock |> shouldEqual None
        // The identity the caller minted, not one the registry invented.
        description.Target |> shouldEqual (OpenFileTarget.Socket (SocketId 4L))

    /// Closing the last descriptor destroys its description. What becomes of the
    /// *socket* that description named is `UnixSystem.close`'s business.
    [<Test>]
    let ``closing the last descriptor destroys the description`` () : unit =
        let fd, registry =
            FileDescriptorRegistry.createSocket (SocketId 0L) FileDescriptorRegistry.initial

        FileDescriptorRegistry.descriptions registry |> Map.count |> shouldEqual 4

        match closeOnly fd registry with
        | Error e -> failwith $"expected close to succeed, got %O{e}"
        | Ok registry ->

        FileDescriptorRegistry.descriptions registry |> Map.count |> shouldEqual 3
        FileDescriptorRegistry.assertInvariants "socket closed" registry |> ignore

    /// Two socket event ports are two *descriptions* but one `flock` object.
    /// That split is why `OpenFileObject` must stay the contention key rather
    /// than becoming a general-purpose identity: on Linux every anon-inode file
    /// shares a single inode, so an exclusive lock on one port excludes the
    /// other, while the descriptions themselves stay distinct.
    ///
    /// The object half has a guest observer — `SocketEventPortLinux.cs` locks
    /// one port and finds the other excluded — so this test exists for the
    /// description half, which has none: nothing a guest can call tells two
    /// ports apart. That half matters for the wait rather than for `flock`.
    /// `ParkedSocketWait` keys a parked task on the port's
    /// `OpenFileDescriptionId`, so two ports sharing one description identity
    /// would wake the wrong waiter.
    [<Test>]
    let ``two socket event ports are two descriptions but one flock object`` () : unit =
        let a, registry =
            FileDescriptorRegistry.createSocketEventPort FileDescriptorRegistry.initial

        let b, registry = FileDescriptorRegistry.createSocketEventPort registry

        a |> shouldEqual 3
        b |> shouldEqual 4

        // Distinct descriptions: each carries its own `Flock` slot, and each is
        // a separate wait target.
        FileDescriptorRegistry.tryFindId a registry
        |> shouldNotEqual (FileDescriptorRegistry.tryFindId b registry)

        // One object, so they contend.
        FileDescriptorRegistry.tryFindObject a registry
        |> shouldEqual (FileDescriptorRegistry.tryFindObject b registry)

        match FileDescriptorRegistry.dup a registry with
        | Error e -> failwith $"expected dup to succeed, got %O{e}"
        | Ok (duplicate, registry) ->
            // `dup` shares the description, so it is the *same* port rather than
            // an equal one — the distinction two `createSocketEventPort` calls
            // draw in the other direction above.
            FileDescriptorRegistry.tryFindId duplicate registry
            |> shouldEqual (FileDescriptorRegistry.tryFindId a registry)

            duplicate |> shouldEqual 5

            FileDescriptorRegistry.assertInvariants "two ports and a dup" registry
            |> ignore<FileDescriptorRegistry>

    /// Closing one descriptor of a `dup` pair leaves the port alive, and closing
    /// the last destroys it — the same rule as for a file, asserted here because
    /// a port is the first target kind whose *identity* is the description, so
    /// "the description outlived its descriptors" would be a different bug.
    [<Test>]
    let ``a socket event port outlives a closed descriptor but not its last`` () : unit =
        let a, registry =
            FileDescriptorRegistry.createSocketEventPort FileDescriptorRegistry.initial

        let b, registry =
            match FileDescriptorRegistry.dup a registry with
            | Ok (duplicate, registry) -> duplicate, registry
            | Error e -> failwith $"expected dup to succeed, got %O{e}"

        let id = FileDescriptorRegistry.tryFindId a registry

        let registry =
            match closeOnly a registry with
            | Ok registry -> registry
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        FileDescriptorRegistry.tryFindId b registry |> shouldEqual id
        FileDescriptorRegistry.descriptions registry |> Map.count |> shouldEqual 4

        let registry =
            match closeOnly b registry with
            | Ok registry -> registry
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        // Only the three standard streams are left.
        FileDescriptorRegistry.descriptions registry |> Map.count |> shouldEqual 3

        FileDescriptorRegistry.assertInvariants "port closed" registry
        |> ignore<FileDescriptorRegistry>

    /// Descriptor numbers are reused; description identities are not. The
    /// former is POSIX ("lowest free"), the latter is what keeps a replay trace
    /// unambiguous about which open a given description was.
    [<Test>]
    let ``a closed description's identity is never handed out again`` () : unit =
        let fd, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        let firstId = FileDescriptorRegistry.tryFindId fd registry

        let registry =
            match closeOnly fd registry with
            | Ok registry -> registry
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        let reused, registry =
            FileDescriptorRegistry.openFile otherInode FileAccessMode.ReadOnly registry

        // The *descriptor* comes back...
        reused |> shouldEqual fd
        // ...but it names a different description.
        FileDescriptorRegistry.tryFindId reused registry |> shouldNotEqual firstId

        FileDescriptorRegistry.assertInvariants "reuse" registry
        |> ignore<FileDescriptorRegistry>

    /// Both directions of staleness, because only one of them is obvious. A
    /// cursor *equal* to a live id collides on the very next `open`; a cursor
    /// *below* one collides a few opens later, and an invariant that checked
    /// only for the collision it can see today would pass the second — then
    /// silently retarget every descriptor naming that description.
    [<TestCase(7L)>]
    [<TestCase(6L)>]
    [<TestCase(0L)>]
    let ``checkInvariants rejects a NextId at or below a live description`` (next : int64) : unit =
        let registry =
            FileDescriptorRegistry.Unchecked.ofParts
                (Map.ofList [ 0, OpenFileDescriptionId 7L ])
                (Map.ofList [ OpenFileDescriptionId 7L, openOn someInode ])
                (OpenFileDescriptionId next)

        FileDescriptorRegistry.checkInvariants registry
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.NextIdNotFresh (OpenFileDescriptionId next, OpenFileDescriptionId 7L)
            ]

    [<Test>]
    let ``checkInvariants accepts a NextId above every live description`` () : unit =
        FileDescriptorRegistry.Unchecked.ofParts
            (Map.ofList [ 0, OpenFileDescriptionId 7L ])
            (Map.ofList [ OpenFileDescriptionId 7L, openOn someInode ])
            (OpenFileDescriptionId 8L)
        |> FileDescriptorRegistry.checkInvariants
        |> shouldEqual []

    [<Test>]
    let ``initial seeds stdin, stdout, stderr`` () : unit =
        FileDescriptorRegistry.tryFind 0 FileDescriptorRegistry.initial
        |> shouldEqual (Some (standardStream FileDescriptorRole.StandardInput))

        FileDescriptorRegistry.tryFind 1 FileDescriptorRegistry.initial
        |> shouldEqual (Some (standardStream FileDescriptorRole.StandardOutput))

        FileDescriptorRegistry.tryFind 2 FileDescriptorRegistry.initial
        |> shouldEqual (Some (standardStream FileDescriptorRole.StandardError))

    /// The shape a *redirected* launch produces, which is what PawPrint commits
    /// to elsewhere: `SystemNative_IsATty` always reports 0, and a `write` to fd 0
    /// is EBADF, both of which are only true of an `O_RDONLY` stdin. Under a tty
    /// all three would be `O_RDWR` — that is the same fact as their sharing one
    /// description, and is rejected for the same reason.
    ///
    /// Asserted here rather than left to the helper above, because these are the
    /// modes every readability and writability answer in the syscall handlers is
    /// derived from.
    [<Test>]
    let ``initial gives each standard stream the access mode a redirected launch would`` () : unit =
        let modeOf (fd : int) : FileAccessMode =
            match FileDescriptorRegistry.tryFind fd FileDescriptorRegistry.initial with
            | Some description -> description.AccessMode
            | None -> failwith $"fd %d{fd} should be live in the initial table"

        modeOf 0 |> shouldEqual FileAccessMode.ReadOnly
        modeOf 1 |> shouldEqual FileAccessMode.WriteOnly
        modeOf 2 |> shouldEqual FileAccessMode.WriteOnly

        // ...and hence, through the accessors the handlers actually consult:
        // stdin is readable and not writable, and the output streams the reverse.
        FileAccessMode.permitsRead (modeOf 0) |> shouldEqual true
        FileAccessMode.permitsWrite (modeOf 0) |> shouldEqual false
        FileAccessMode.permitsRead (modeOf 1) |> shouldEqual false
        FileAccessMode.permitsWrite (modeOf 1) |> shouldEqual true

    /// `dup(2)` shares the description, and the access mode lives on the
    /// description — so a `dup` of a write-only descriptor is write-only too.
    /// `fcntl(F_SETFL)` ignores the access-mode bits, and POSIX gives no other
    /// way to change one, so this is the whole of the access mode's lifecycle.
    [<Test>]
    let ``openFile records the access mode it was given, and dup shares it`` () : unit =
        for mode in
            [
                FileAccessMode.ReadOnly
                FileAccessMode.WriteOnly
                FileAccessMode.ReadWrite
            ] do
            let fd, registry =
                FileDescriptorRegistry.openFile someInode mode FileDescriptorRegistry.initial

            let duplicated, registry =
                match FileDescriptorRegistry.dup fd registry with
                | Ok result -> result
                | Error e -> failwith $"expected dup to succeed, got %O{e}"

            let modeOf (fd : int) : FileAccessMode =
                match FileDescriptorRegistry.tryFind fd registry with
                | Some description -> description.AccessMode
                | None -> failwith $"fd %d{fd} should be live"

            modeOf fd |> shouldEqual mode
            modeOf duplicated |> shouldEqual mode

            FileDescriptorRegistry.assertInvariants "openFile with an access mode" registry
            |> ignore<FileDescriptorRegistry>

    /// An implementation that pointed every file descriptor at a single shared
    /// open file description would satisfy the dup-sharing property below, and
    /// only this test would notice. PawPrint
    /// models a process launched with each standard stream separately
    /// redirected, so the three inherited descriptors name three descriptions.
    /// (Under a tty they would genuinely share one — measured with `forkpty` —
    /// but PawPrint has committed against the tty model elsewhere; see the
    /// comment on `FileDescriptorRegistry.initial`.)
    [<Test>]
    let ``initial seeds three distinct open file descriptions`` () : unit =
        let ids =
            [ 0 ; 1 ; 2 ]
            |> List.map (fun fd ->
                match FileDescriptorRegistry.tryFindId fd FileDescriptorRegistry.initial with
                | Some id -> id
                | None -> failwith $"fd %d{fd} should be live in the initial table"
            )

        ids |> List.distinct |> List.length |> shouldEqual 3

        FileDescriptorRegistry.descriptions FileDescriptorRegistry.initial
        |> Map.count
        |> shouldEqual 3

    [<Test>]
    let ``initial has no entries outside 0/1/2`` () : unit =
        for fd in [ -2 ; -1 ; 3 ; 4 ; 100 ; System.Int32.MaxValue ] do
            FileDescriptorRegistry.tryFind fd FileDescriptorRegistry.initial
            |> shouldEqual None

    [<Test>]
    let ``initial is sound`` () : unit =
        FileDescriptorRegistry.checkInvariants FileDescriptorRegistry.initial
        |> shouldEqual []

    [<Test>]
    let ``dup of unknown fd returns BadFd`` () : unit =
        FileDescriptorRegistry.dup 3 FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorDupError.BadFd)

        FileDescriptorRegistry.dup -1 FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorDupError.BadFd)

        FileDescriptorRegistry.dup System.Int32.MaxValue FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorDupError.BadFd)

    [<Test>]
    let ``dup of stdin/stdout/stderr returns fresh fd 3 sharing the description`` () : unit =
        let assertDupAllocatesFd3 (sourceFd : int) (expectedRole : FileDescriptorRole) : unit =
            match FileDescriptorRegistry.dup sourceFd FileDescriptorRegistry.initial with
            | Ok (newFd, registry) ->
                newFd |> shouldEqual 3

                FileDescriptorRegistry.tryFind newFd registry
                |> shouldEqual (Some (standardStream expectedRole))

                // The point of the indirection: the new descriptor names the
                // *same* description, not an equal copy of it.
                FileDescriptorRegistry.tryFindId newFd registry
                |> shouldEqual (FileDescriptorRegistry.tryFindId sourceFd registry)

                // dup creates no new description.
                FileDescriptorRegistry.descriptions registry |> Map.count |> shouldEqual 3

                // Source fd is unaffected — the table still resolves it to its
                // original description. dup is non-destructive on the source.
                FileDescriptorRegistry.tryFind sourceFd registry
                |> shouldEqual (FileDescriptorRegistry.tryFind sourceFd FileDescriptorRegistry.initial)
            | Error e -> failwith $"unexpected dup error: %O{e}"

        assertDupAllocatesFd3 0 FileDescriptorRole.StandardInput
        assertDupAllocatesFd3 1 FileDescriptorRole.StandardOutput
        assertDupAllocatesFd3 2 FileDescriptorRole.StandardError

    [<Test>]
    let ``repeated dup allocates strictly increasing fds starting at 3`` () : unit =
        let assertDupYields (registry : FileDescriptorRegistry) (expectedFd : int) : FileDescriptorRegistry =
            match FileDescriptorRegistry.dup 1 registry with
            | Ok (newFd, registry) ->
                newFd |> shouldEqual expectedFd
                registry
            | Error e -> failwith $"unexpected dup error: %O{e}"

        FileDescriptorRegistry.initial
        |> fun r -> assertDupYields r 3
        |> fun r -> assertDupYields r 4
        |> fun r -> assertDupYields r 5
        |> fun r -> assertDupYields r 6
        |> ignore

    [<Test>]
    let ``closing one descriptor of a dup pair leaves the other intact`` () : unit =
        match FileDescriptorRegistry.dup 1 FileDescriptorRegistry.initial with
        | Error e -> failwith $"unexpected dup error: %O{e}"
        | Ok (duped, registry) ->

        let sharedId = FileDescriptorRegistry.tryFindId duped registry

        match closeOnly duped registry with
        | Error e -> failwith $"unexpected close error: %O{e}"
        | Ok afterClose ->

        FileDescriptorRegistry.tryFind duped afterClose |> shouldEqual None

        // The description outlives the descriptor that closed, because fd 1
        // still names it.
        FileDescriptorRegistry.tryFindId 1 afterClose |> shouldEqual sharedId

        FileDescriptorRegistry.tryFind 1 afterClose
        |> shouldEqual (Some (standardStream FileDescriptorRole.StandardOutput))

        FileDescriptorRegistry.descriptions afterClose |> Map.count |> shouldEqual 3
        FileDescriptorRegistry.checkInvariants afterClose |> shouldEqual []

    [<Test>]
    let ``the last close of a description destroys it`` () : unit =
        // Close stdout with no dup outstanding: nothing names its description
        // afterwards, so the kernel would destroy it.
        match closeOnly 1 FileDescriptorRegistry.initial with
        | Error e -> failwith $"unexpected close error: %O{e}"
        | Ok afterClose ->

        FileDescriptorRegistry.descriptions afterClose |> Map.count |> shouldEqual 2

        FileDescriptorRegistry.descriptions afterClose
        |> Map.toList
        |> List.map snd
        |> shouldEqual
            [
                standardStream FileDescriptorRole.StandardInput
                standardStream FileDescriptorRole.StandardError
            ]

        FileDescriptorRegistry.checkInvariants afterClose |> shouldEqual []

    [<Test>]
    let ``a description survives until its last descriptor closes`` () : unit =
        // dup stdout twice, then close all three descriptors naming it. The
        // description must survive exactly until the final close.
        let registry = FileDescriptorRegistry.initial

        let dupOf (fd : int) (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
            match FileDescriptorRegistry.dup fd registry with
            | Ok result -> result
            | Error e -> failwith $"unexpected dup error: %O{e}"

        let a, registry = dupOf 1 registry
        let b, registry = dupOf 1 registry

        let closeOf (fd : int) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
            match closeOnly fd registry with
            | Ok result -> result
            | Error e -> failwith $"unexpected close error: %O{e}"

        let registry = closeOf 1 registry
        FileDescriptorRegistry.descriptions registry |> Map.count |> shouldEqual 3

        let registry = closeOf a registry
        FileDescriptorRegistry.descriptions registry |> Map.count |> shouldEqual 3

        let registry = closeOf b registry
        FileDescriptorRegistry.descriptions registry |> Map.count |> shouldEqual 2

        FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

    [<Test>]
    let ``close of unknown fd returns BadFd`` () : unit =
        closeOnly 3 FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorCloseError.BadFd)

        closeOnly -1 FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorCloseError.BadFd)

    /// Reference implementation: the lowest non-negative integer not in `used`.
    let private referenceLowestFree (used : Set<int>) : int =
        let rec scan candidate =
            if Set.contains candidate used then
                scan (candidate + 1)
            else
                candidate

        scan 0

    [<Test>]
    let ``dup always allocates the lowest non-negative fd not already in use`` () : unit =
        // Generate a sequence of dup and close operations against the registry,
        // tracking the expected live set as a Set<int> in parallel. After each
        // dup the newly allocated fd must equal the lowest non-negative
        // integer not currently live; closes punch holes that subsequent dups
        // must fill before extending past the existing maximum.
        let mutable observedDups = 0
        let mutable observedCloses = 0
        let mutable observedHoleFills = 0

        let property (NonNegativeInt seed : NonNegativeInt) : unit =
            let rng = System.Random (seed)
            let steps = rng.Next (1, 30)

            let mutable registry = FileDescriptorRegistry.initial
            let mutable live : Set<int> = Set.ofList [ 0 ; 1 ; 2 ]

            for _ in 1..steps do
                let liveList = Set.toList live
                // 70% dup, 30% close, biased toward dup so the table grows
                // on average and we observe gap-filling rather than only
                // catastrophic shrinkage.
                let doClose = rng.Next 10 < 3 && liveList.Length > 3

                if doClose then
                    let pickIndex = rng.Next (liveList.Length)
                    let chosen = liveList.[pickIndex]

                    match closeOnly chosen registry with
                    | Ok registry' ->
                        live <- Set.remove chosen live
                        registry <- registry'
                        observedCloses <- observedCloses + 1
                    | Error e -> failwith $"unexpected close error: %O{e}"
                else
                    let pickIndex = rng.Next (liveList.Length)
                    let chosen = liveList.[pickIndex]

                    match FileDescriptorRegistry.dup chosen registry with
                    | Ok (newFd, registry') ->
                        let expected = referenceLowestFree live
                        newFd |> shouldEqual expected

                        // Track whether the allocation filled a gap below the
                        // existing maximum (the case that distinguishes
                        // lowest-free from `max + 1`) versus extending past the top.
                        let liveMaxBefore = Set.maxElement live

                        if newFd < liveMaxBefore then
                            observedHoleFills <- observedHoleFills + 1

                        live <- Set.add newFd live
                        registry <- registry'
                        observedDups <- observedDups + 1
                    | Error e -> failwith $"unexpected dup error: %O{e}"

        Check.One (propertyConfig, property)

        // Distribution check: if observedHoleFills were 0, the property
        // could be satisfied by a buggy `max + 1` implementation, so assert
        // we observe gap-fills frequently enough that a regression would be
        // caught. With 500 iterations of ~20 steps each, biased 70/30 toward
        // dup, we expect on the order of thousands of dups and hundreds of
        // hole-fills; require at least 30 to keep the false-negative
        // probability comfortably below 1e-11.
        observedDups |> shouldBeGreaterThan 0
        observedCloses |> shouldBeGreaterThan 0
        observedHoleFills |> shouldBeGreaterThan 30

    /// The central contract of the descriptor/description split, stated as an
    /// equivalence rather than a one-way implication: two live descriptors name
    /// the same open file description *exactly* when they are related by
    /// dup-ancestry. The "only if" half kills an implementation that points
    /// every descriptor at one shared description; the "if" half kills one that
    /// mints a fresh description per `dup`.
    ///
    /// `origin` tracks that equivalence independently of the registry: each
    /// descriptor is labelled with the inherited standard-stream fd its dup
    /// chain descends from.
    [<Test>]
    let ``descriptors share a description exactly when dup-related`` () : unit =
        let mutable observedSharingPairs = 0
        let mutable observedDistinctPairs = 0

        let property (NonNegativeInt seed : NonNegativeInt) : unit =
            let rng = System.Random (seed)
            let steps = rng.Next (1, 30)

            let mutable registry = FileDescriptorRegistry.initial
            let mutable origin : Map<int, int> = Map.ofList [ 0, 0 ; 1, 1 ; 2, 2 ]

            for _ in 1..steps do
                let liveList = origin |> Map.toList |> List.map fst
                let chosen = liveList.[rng.Next (liveList.Length)]
                let doClose = rng.Next 10 < 3 && liveList.Length > 3

                if doClose then
                    match closeOnly chosen registry with
                    | Ok registry' ->
                        origin <- Map.remove chosen origin
                        registry <- registry'
                    | Error e -> failwith $"unexpected close error: %O{e}"
                else
                    match FileDescriptorRegistry.dup chosen registry with
                    | Ok (newFd, registry') ->
                        origin <- Map.add newFd (Map.find chosen origin) origin
                        registry <- registry'
                    | Error e -> failwith $"unexpected dup error: %O{e}"

                // The registry never drifts from a table a kernel could produce.
                FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

                let live = origin |> Map.toList

                for a, originA in live do
                    for b, originB in live do
                        let idA = FileDescriptorRegistry.tryFindId a registry
                        let idB = FileDescriptorRegistry.tryFindId b registry

                        idA |> shouldNotEqual None
                        idB |> shouldNotEqual None

                        if originA = originB then
                            idA |> shouldEqual idB

                            if a <> b then
                                observedSharingPairs <- observedSharingPairs + 1
                        else
                            idA |> shouldNotEqual idB
                            observedDistinctPairs <- observedDistinctPairs + 1

                // A description exists for exactly the set of origins still
                // reachable: nothing leaks, nothing is destroyed early.
                let expectedDescriptions = live |> List.map snd |> Set.ofList |> Set.count

                FileDescriptorRegistry.descriptions registry
                |> Map.count
                |> shouldEqual expectedDescriptions

        Check.One (propertyConfig, property)

        // Both halves of the equivalence must actually be exercised: without
        // sharing pairs the "if" half is vacuous, and without distinct pairs
        // the "only if" half is.
        observedSharingPairs |> shouldBeGreaterThan 100
        observedDistinctPairs |> shouldBeGreaterThan 100

    [<Test>]
    let ``checkInvariants rejects a descriptor naming an absent description`` () : unit =
        let registry =
            FileDescriptorRegistry.Unchecked.ofParts
                (Map.ofList [ 0, OpenFileDescriptionId 7L ])
                Map.empty
                (OpenFileDescriptionId 8L)

        FileDescriptorRegistry.checkInvariants registry
        |> shouldEqual [ FileDescriptorRegistryDefect.DanglingFd (0, OpenFileDescriptionId 7L) ]

    [<Test>]
    let ``checkInvariants rejects a description no descriptor names`` () : unit =
        let registry =
            FileDescriptorRegistry.Unchecked.ofParts
                Map.empty
                (Map.ofList [ OpenFileDescriptionId 7L, standardStream FileDescriptorRole.StandardOutput ])
                (OpenFileDescriptionId 8L)

        FileDescriptorRegistry.checkInvariants registry
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.UnreferencedDescription (OpenFileDescriptionId 7L)
            ]

    [<Test>]
    let ``assertInvariants passes a sound table and fails an unsound one`` () : unit =
        FileDescriptorRegistry.assertInvariants "sound" FileDescriptorRegistry.initial
        |> shouldEqual FileDescriptorRegistry.initial

        let unsound =
            FileDescriptorRegistry.Unchecked.ofParts
                (Map.ofList [ 0, OpenFileDescriptionId 7L ])
                Map.empty
                (OpenFileDescriptionId 8L)

        let exc =
            Assert.Throws<System.Exception> (fun () ->
                FileDescriptorRegistry.assertInvariants "context here" unsound |> ignore
            )

        exc.Message |> shouldContainText "context here"
        exc.Message |> shouldContainText "DanglingFd"

    /// Helpers for the `flock` tests: they all want "open a file and lock it",
    /// and threading the Result through by hand at every step obscures which
    /// step is the one under test.
    let private openOrFail (inode : InodeNumber) (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
        FileDescriptorRegistry.openFile inode FileAccessMode.ReadOnly registry

    let private lockOrFail
        (fd : int)
        (request : FlockRequest)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        match FileDescriptorRegistry.flock fd request registry with
        | registry, None -> FileDescriptorRegistry.assertInvariants "flock" registry
        | _, Some e -> failwith $"expected flock to succeed, got %O{e}"

    [<Test>]
    let ``flock of an unknown fd is BadFd`` () : unit =
        for fd in [ 3 ; -1 ; System.Int32.MaxValue ] do
            FileDescriptorRegistry.flock fd (FlockRequest.Acquire FlockMode.Shared) FileDescriptorRegistry.initial
            |> snd
            |> shouldEqual (Some FlockError.BadFd)

            FileDescriptorRegistry.flock fd FlockRequest.Release FileDescriptorRegistry.initial
            |> snd
            |> shouldEqual (Some FlockError.BadFd)

    /// The whole contention matrix, exactly as measured against `flock(2)` on
    /// both Linux and Darwin (`scratchpad/flockops.c`), for two *separate*
    /// descriptions on one inode. Shared-against-shared is the only compatible
    /// pair.
    [<TestCase(false, false, true)>]
    [<TestCase(false, true, false)>]
    [<TestCase(true, false, false)>]
    [<TestCase(true, true, false)>]
    let ``two descriptions on one file conflict unless both are shared``
        (heldExclusive : bool)
        (wantedExclusive : bool)
        (expectedGranted : bool)
        : unit
        =
        let mode (exclusive : bool) =
            if exclusive then FlockMode.Exclusive else FlockMode.Shared

        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let b, registry = openOrFail someInode registry

        let registry = lockOrFail a (FlockRequest.Acquire (mode heldExclusive)) registry

        match FileDescriptorRegistry.flock b (FlockRequest.Acquire (mode wantedExclusive)) registry with
        | registry, None ->
            expectedGranted |> shouldEqual true
            FileDescriptorRegistry.checkInvariants registry |> shouldEqual []
        | registry, Some FlockError.WouldBlock ->
            expectedGranted |> shouldEqual false
            FileDescriptorRegistry.checkInvariants registry |> shouldEqual []
        | _, Some e -> failwith $"unexpected flock error: %O{e}"

    /// A lock is per file, so a description on a different inode is not an
    /// obstacle. Without this, "one global lock" satisfies the matrix above.
    [<Test>]
    let ``an exclusive lock on one file does not block another file`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let b, registry = openOrFail otherInode registry

        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Exclusive) registry

        FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> snd
        |> shouldEqual None

    /// Conversion. `flock(2)` has no separate upgrade operation: re-locking
    /// replaces whatever this description held, and its *own* lock is never an
    /// obstacle to that. Measured on both platforms.
    [<Test>]
    let ``a description may convert its own lock but not past another holder`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial

        // Sole holder: shared upgrades to exclusive, and back down again.
        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Shared) registry
        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Exclusive) registry
        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Shared) registry

        // A second shared holder appears; now the upgrade must fail, even
        // though `a` already holds a lock on this very file.
        let b, registry = openOrFail someInode registry
        let registry = lockOrFail b (FlockRequest.Acquire FlockMode.Shared) registry

        FileDescriptorRegistry.flock a (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> snd
        |> shouldEqual (Some FlockError.WouldBlock)

        // ...and succeeds again once the other holder releases.
        let registry = lockOrFail b FlockRequest.Release registry

        FileDescriptorRegistry.flock a (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> snd
        |> shouldEqual None

    /// A *failed* conversion still drops the caller's old lock, because `flock(2)` converts by
    /// removing and then re-establishing, and those steps are not atomic.
    ///
    /// This needs a **third** description to observe at all, which is why the conversion test
    /// above misses it: with only `a` and `b`, whether `a` kept its shared lock is invisible —
    /// releasing `b` lets `a` take the exclusive lock either way. Only a bystander asking for an
    /// exclusive lock after `b` releases can tell whether `a` is still holding one.
    ///
    /// Measured (scratchpad/flockconv.c): Linux drops it, Darwin keeps it. PawPrint simulates
    /// Linux. The *error* the failed conversion reports is `EWOULDBLOCK` on both, so nothing
    /// about the return value distinguishes them.
    [<Test>]
    let ``a failed conversion drops the caller's existing lock`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let b, registry = openOrFail someInode registry
        let bystander, registry = openOrFail someInode registry

        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Shared) registry
        let registry = lockOrFail b (FlockRequest.Acquire FlockMode.Shared) registry

        // `a` tries to upgrade while `b` still holds shared: refused...
        let registry, error =
            FileDescriptorRegistry.flock a (FlockRequest.Acquire FlockMode.Exclusive) registry

        error |> shouldEqual (Some FlockError.WouldBlock)
        FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

        // ...and `a` is now holding *nothing*, so once `b` releases, the bystander can take an
        // exclusive lock. Were `a` still holding shared, this would be refused.
        let registry = lockOrFail b FlockRequest.Release registry

        FileDescriptorRegistry.flock bystander (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> snd
        |> shouldEqual None

    /// The same drop happens when the request was going to fail anyway — a caller holding
    /// *exclusive* that asks for exclusive again while a bystander holds shared. Separate from the
    /// case above because it is the reverse conversion direction, and an implementation that only
    /// cleared the lock when downgrading would pass one and fail the other.
    [<Test>]
    let ``a failed re-acquisition drops an exclusive lock too`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let b, registry = openOrFail someInode registry

        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Exclusive) registry

        // `b` cannot get in while `a` holds exclusive.
        FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Shared) registry
        |> snd
        |> shouldEqual (Some FlockError.WouldBlock)

        // Drop to shared, let `b` in, then have `a` fail an upgrade.
        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Shared) registry
        let registry = lockOrFail b (FlockRequest.Acquire FlockMode.Shared) registry

        let registry, error =
            FileDescriptorRegistry.flock a (FlockRequest.Acquire FlockMode.Exclusive) registry

        error |> shouldEqual (Some FlockError.WouldBlock)

        // `a` holds nothing now, so `b` — the surviving shared holder — can upgrade.
        FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> snd
        |> shouldEqual None

    /// A failed acquisition by a description that held nothing leaves it holding nothing: the
    /// drop is not an *extra* effect, it is the same "remove then establish" seen from a
    /// description with nothing to remove.
    [<Test>]
    let ``a failed acquisition by an unlocked description changes nothing`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let b, registry = openOrFail someInode registry
        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Exclusive) registry

        let after, error =
            FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Shared) registry

        error |> shouldEqual (Some FlockError.WouldBlock)
        // `a`'s lock is untouched by `b`'s failure, so the whole table is unchanged.
        after |> shouldEqual registry

    [<Test>]
    let ``releasing a lock that was never taken succeeds`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        lockOrFail a FlockRequest.Release registry |> ignore<FileDescriptorRegistry>

    /// The lock belongs to the *description*, so `dup` shares it: releasing
    /// through either descriptor releases the one lock, and neither contends
    /// with the other. This is the half that a lock stored per-descriptor, or
    /// per-inode, would get wrong in opposite directions.
    [<Test>]
    let ``a dup pair shares one lock`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Exclusive) registry

        let duplicate, registry =
            match FileDescriptorRegistry.dup a registry with
            | Ok result -> result
            | Error e -> failwith $"unexpected dup error: %O{e}"

        // Re-acquiring through the dup is a no-op conversion, not a conflict.
        let registry =
            lockOrFail duplicate (FlockRequest.Acquire FlockMode.Exclusive) registry

        // A third party is still excluded...
        let other, registry = openOrFail someInode registry

        FileDescriptorRegistry.flock other (FlockRequest.Acquire FlockMode.Shared) registry
        |> snd
        |> shouldEqual (Some FlockError.WouldBlock)

        // ...until the lock is released through the *dup*, which drops the
        // single shared lock rather than one of two copies.
        let registry = lockOrFail duplicate FlockRequest.Release registry

        FileDescriptorRegistry.flock other (FlockRequest.Acquire FlockMode.Shared) registry
        |> snd
        |> shouldEqual None

    /// Closing the last descriptor of a description destroys the description,
    /// and with it the lock — which is what makes a `FileStream` that was never
    /// explicitly unlocked stop blocking the next open.
    [<Test>]
    let ``closing a locked description releases its lock`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Exclusive) registry
        let b, registry = openOrFail someInode registry

        FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> snd
        |> shouldEqual (Some FlockError.WouldBlock)

        let registry =
            match closeOnly a registry with
            | Ok registry -> registry
            | Error e -> failwith $"unexpected close error: %O{e}"

        FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> snd
        |> shouldEqual None

    /// A standard stream is lockable — Linux permits `flock` on a pipe — and can
    /// never contend, because PawPrint gives each role exactly one description.
    /// Asserted rather than left implicit: it is the one place where "no
    /// conflict" is a consequence of the process model rather than of the lock
    /// rule, so a future shared-pipe model would need to revisit it.
    [<Test>]
    let ``a standard stream can be locked and conflicts with nothing`` () : unit =
        let registry =
            FileDescriptorRegistry.initial
            |> lockOrFail 1 (FlockRequest.Acquire FlockMode.Exclusive)
            |> lockOrFail 0 (FlockRequest.Acquire FlockMode.Exclusive)
            |> lockOrFail 2 (FlockRequest.Acquire FlockMode.Shared)

        FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

    [<Test>]
    let ``checkInvariants rejects two conflicting locks on one file`` () : unit =
        let locked (mode : FlockMode) : OpenFileDescription =
            {
                Target = OpenFileTarget.File (someInode, 0L)
                AccessMode = FileAccessMode.ReadOnly
                NonBlocking = false
                Flock = Some mode
            }

        let table (first : FlockMode) (second : FlockMode) =
            FileDescriptorRegistry.Unchecked.ofParts
                (Map.ofList [ 0, OpenFileDescriptionId 7L ; 1, OpenFileDescriptionId 8L ])
                (Map.ofList
                    [
                        OpenFileDescriptionId 7L, locked first
                        OpenFileDescriptionId 8L, locked second
                    ])
                (OpenFileDescriptionId 9L)

        let expected =
            [
                FileDescriptorRegistryDefect.ConflictingFlocks (OpenFileDescriptionId 7L, OpenFileDescriptionId 8L)
            ]

        FileDescriptorRegistry.checkInvariants (table FlockMode.Exclusive FlockMode.Exclusive)
        |> shouldEqual expected

        FileDescriptorRegistry.checkInvariants (table FlockMode.Exclusive FlockMode.Shared)
        |> shouldEqual expected

        FileDescriptorRegistry.checkInvariants (table FlockMode.Shared FlockMode.Exclusive)
        |> shouldEqual expected

        // ...and the compatible pair is accepted, so the check is not simply
        // "two locks on one file".
        FileDescriptorRegistry.checkInvariants (table FlockMode.Shared FlockMode.Shared)
        |> shouldEqual []

    [<Test>]
    let ``checkInvariants accepts conflicting locks on different files`` () : unit =
        let locked (inode : InodeNumber) : OpenFileDescription =
            {
                Target = OpenFileTarget.File (inode, 0L)
                AccessMode = FileAccessMode.ReadOnly
                NonBlocking = false
                Flock = Some FlockMode.Exclusive
            }

        FileDescriptorRegistry.Unchecked.ofParts
            (Map.ofList [ 0, OpenFileDescriptionId 7L ; 1, OpenFileDescriptionId 8L ])
            (Map.ofList
                [
                    OpenFileDescriptionId 7L, locked someInode
                    OpenFileDescriptionId 8L, locked otherInode
                ])
            (OpenFileDescriptionId 9L)
        |> FileDescriptorRegistry.checkInvariants
        |> shouldEqual []

    /// The mutual-exclusion guarantee over random operation sequences, checked
    /// against an independent model of who holds what.
    ///
    /// The model is deliberately *not* the registry's own view: it maps each
    /// description identity to the mode the test believes it holds, and derives
    /// "is this grant legal" from that. An implementation that lost a lock on
    /// `dup`, or failed to drop one on `close`, would agree with itself but
    /// disagree with this.
    [<Test>]
    let ``flock grants exactly the requests a kernel would`` () : unit =
        let mutable observedGrants = 0
        let mutable observedRefusals = 0
        let mutable observedConversions = 0
        let mutable observedFailedConversions = 0

        let inodes = [| InodeNumber 1L ; InodeNumber 2L |]

        let property (NonNegativeInt seed : NonNegativeInt) : unit =
            let rng = System.Random (seed)
            let steps = rng.Next (1, 40)

            let mutable registry = FileDescriptorRegistry.initial
            // fd -> description id, and description id -> (inode, held mode).
            let mutable fdToId : Map<int, OpenFileDescriptionId> = Map.empty

            let mutable held : Map<OpenFileDescriptionId, InodeNumber * FlockMode option> =
                Map.empty

            let idOf (fd : int) = Map.find fd fdToId

            for _ in 1..steps do
                let liveFds = fdToId |> Map.toList |> List.map fst
                let choice = rng.Next 10

                if choice < 3 || liveFds.IsEmpty then
                    // open
                    let inode = inodes.[rng.Next inodes.Length]

                    let fd, registry' =
                        FileDescriptorRegistry.openFile inode FileAccessMode.ReadOnly registry

                    let id =
                        match FileDescriptorRegistry.tryFindId fd registry' with
                        | Some id -> id
                        | None -> failwith "freshly opened fd should be live"

                    registry <- registry'
                    fdToId <- Map.add fd id fdToId
                    held <- Map.add id (inode, None) held
                elif choice < 5 then
                    // close
                    let fd = liveFds.[rng.Next liveFds.Length]
                    let id = idOf fd

                    match closeOnly fd registry with
                    | Ok registry' ->
                        registry <- registry'
                        fdToId <- Map.remove fd fdToId
                        // The description — and its lock — survive exactly while
                        // some other fd still names it.
                        if fdToId |> Map.exists (fun _ other -> other = id) |> not then
                            held <- Map.remove id held
                    | Error e -> failwith $"unexpected close error: %O{e}"
                elif choice < 6 then
                    // dup
                    let fd = liveFds.[rng.Next liveFds.Length]

                    match FileDescriptorRegistry.dup fd registry with
                    | Ok (newFd, registry') ->
                        registry <- registry'
                        fdToId <- Map.add newFd (idOf fd) fdToId
                    | Error e -> failwith $"unexpected dup error: %O{e}"
                elif choice < 7 then
                    // release
                    let fd = liveFds.[rng.Next liveFds.Length]
                    let id = idOf fd

                    match FileDescriptorRegistry.flock fd FlockRequest.Release registry with
                    | registry', None ->
                        registry <- registry'
                        let inode, _ = Map.find id held
                        held <- Map.add id (inode, None) held
                    | _, Some e -> failwith $"unexpected release error: %O{e}"
                else
                    // acquire
                    let fd = liveFds.[rng.Next liveFds.Length]
                    let id = idOf fd
                    let inode, existing = Map.find id held

                    let wanted =
                        if rng.Next 2 = 0 then
                            FlockMode.Shared
                        else
                            FlockMode.Exclusive

                    // The model's own answer, computed from `held` alone: some
                    // *other* description on this inode holds a lock that is
                    // incompatible with what we want.
                    let expectedBlocked =
                        held
                        |> Map.exists (fun otherId (otherInode, otherMode) ->
                            otherId <> id
                            && otherInode = inode
                            && (
                                match otherMode, wanted with
                                | None, _ -> false
                                | Some FlockMode.Shared, FlockMode.Shared -> false
                                | Some _, _ -> true
                            )
                        )

                    match FileDescriptorRegistry.flock fd (FlockRequest.Acquire wanted) registry with
                    | registry', None ->
                        expectedBlocked |> shouldEqual false
                        registry <- registry'
                        held <- Map.add id (inode, Some wanted) held
                        observedGrants <- observedGrants + 1

                        if existing.IsSome then
                            observedConversions <- observedConversions + 1
                    | registry', Some FlockError.WouldBlock ->
                        expectedBlocked |> shouldEqual true
                        registry <- registry'
                        // The refused acquisition still *dropped* whatever this
                        // description held: `flock` converts by removing and then
                        // re-establishing, and the removal has already happened.
                        // The model records that independently, so an
                        // implementation that kept the old lock (which is what
                        // Darwin does) diverges from it on the very next request.
                        held <- Map.add id (inode, None) held

                        if existing.IsSome then
                            observedFailedConversions <- observedFailedConversions + 1

                        observedRefusals <- observedRefusals + 1
                    | _, Some e -> failwith $"unexpected flock error: %O{e}"

                FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

        Check.One (propertyConfig, property)

        // Without refusals the exclusion rule is never exercised; without
        // conversions the re-lock path is never exercised.
        observedGrants |> shouldBeGreaterThan 100
        observedRefusals |> shouldBeGreaterThan 30
        observedConversions |> shouldBeGreaterThan 30
        // ...and without *failed* conversions, the drop-on-failure rule is never
        // exercised: every refusal would be of a description holding nothing, for
        // which keeping and dropping are the same thing.
        //
        // The floor is much lower than its neighbours': this is the rarest event
        // the run produces, measured at ~24 per run against ~24000 grants, so a
        // floor of 30 would flake. One occurrence suffices — a single failed
        // conversion diverges from the model on the next request — so this bound
        // proves the event happens at all, not accumulates confidence.
        observedFailedConversions |> shouldBeGreaterThan 5

    [<Test>]
    let ``tryFind crashes rather than inventing a description for a dangling fd`` () : unit =
        // `tryFind` is total over sound tables; on an unsound one it must fail
        // loudly rather than report the descriptor as closed, which would let a
        // corrupt table masquerade as EBADF.
        let registry =
            FileDescriptorRegistry.Unchecked.ofParts
                (Map.ofList [ 0, OpenFileDescriptionId 7L ])
                Map.empty
                (OpenFileDescriptionId 8L)

        let exc =
            Assert.Throws<System.Exception> (fun () -> FileDescriptorRegistry.tryFind 0 registry |> ignore)

        exc.Message |> shouldContainText "not present in the table"

    // --- the file offset ---

    /// Which offset each live descriptor is positioned at. `None` for a descriptor naming something
    /// unseekable, so the tests below can assert about standard streams without a projection that
    /// invents a number for them.
    let private offsetOf (fd : int) (registry : FileDescriptorRegistry) : int64 option =
        match FileDescriptorRegistry.tryFindTarget fd registry with
        | None -> failwith $"fd %d{fd} is not live"
        | Some (OpenFileTarget.StandardStream _)
        | Some (OpenFileTarget.SocketEventPort _)
        | Some (OpenFileTarget.Socket _) -> None
        | Some (OpenFileTarget.File (_, offset)) -> Some offset

    [<Test>]
    let ``a fresh description starts at offset zero`` () : unit =
        let fd, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        offsetOf fd registry |> shouldEqual (Some 0L)

    [<Test>]
    let ``the standard streams have no offset at all`` () : unit =
        for fd in 0..2 do
            offsetOf fd FileDescriptorRegistry.initial |> shouldEqual None

    /// The offset belongs to the *description*, so `dup` shares it: this is the same indirection
    /// that makes two descriptors from one `dup` share a `flock` lock, and it is why the offset
    /// cannot live in the per-descriptor table.
    [<Test>]
    let ``dup shares one offset`` () : unit =
        let fd, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        let copy, registry =
            match FileDescriptorRegistry.dup fd registry with
            | Ok result -> result
            | Error error -> failwith $"dup failed: %O{error}"

        let registry = FileDescriptorRegistry.setOffset fd 7L registry

        offsetOf copy registry |> shouldEqual (Some 7L)

        // ...and moving it through the copy is seen by the original, so the sharing is not
        // one-directional.
        let registry = FileDescriptorRegistry.setOffset copy 3L registry
        offsetOf fd registry |> shouldEqual (Some 3L)

    /// Two `open` calls on one inode are two descriptions, so their offsets are independent. The
    /// mirror image of the `dup` case: a model keyed on the *file* rather than the description
    /// would pass that test and fail this one.
    [<Test>]
    let ``two opens on one file hold independent offsets`` () : unit =
        let first, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        let second, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly registry

        let registry = FileDescriptorRegistry.setOffset first 9L registry

        offsetOf first registry |> shouldEqual (Some 9L)
        offsetOf second registry |> shouldEqual (Some 0L)

    /// Moving the offset does not disturb the identity the description names, which is what `flock`
    /// contention is decided on. Without this, seeking would silently stop two descriptions on one
    /// file from excluding each other.
    [<Test>]
    let ``seeking leaves the object identity alone`` () : unit =
        let first, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        let second, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly registry

        // Positioned differently, and still the same file.
        let registry = FileDescriptorRegistry.setOffset first 40L registry
        let registry = FileDescriptorRegistry.setOffset second 2L registry

        FileDescriptorRegistry.tryFindObject first registry
        |> shouldEqual (FileDescriptorRegistry.tryFindObject second registry)

        let registry, error =
            FileDescriptorRegistry.flock first (FlockRequest.Acquire FlockMode.Exclusive) registry

        error |> shouldEqual None

        let _, error =
            FileDescriptorRegistry.flock second (FlockRequest.Acquire FlockMode.Shared) registry

        error |> shouldEqual (Some FlockError.WouldBlock)

    /// `setOffset` is deliberately partial in the descriptor: both callers have already resolved the
    /// description and answered EBADF/ESPIPE, so reaching it otherwise is an interpreter bug, and a
    /// silent no-op would hide it.
    [<Test>]
    let ``setOffset refuses a descriptor that cannot hold an offset`` () : unit =
        let onStream =
            Assert.Catch (fun () ->
                FileDescriptorRegistry.setOffset 1 4L FileDescriptorRegistry.initial
                |> ignore<FileDescriptorRegistry>
            )

        onStream.Message |> shouldContainText "ESPIPE"

        let onMissing =
            Assert.Catch (fun () ->
                FileDescriptorRegistry.setOffset 4242 4L FileDescriptorRegistry.initial
                |> ignore<FileDescriptorRegistry>
            )

        onMissing.Message |> shouldContainText "EBADF"

        let fd, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        let negative =
            Assert.Catch (fun () ->
                FileDescriptorRegistry.setOffset fd -1L registry
                |> ignore<FileDescriptorRegistry>
            )

        negative.Message |> shouldContainText "negative"

    /// Closing the last descriptor destroys the offset with the description, rather than leaving it
    /// behind for the next `open` to inherit.
    [<Test>]
    let ``a reopened file starts from zero again`` () : unit =
        let fd, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        let registry = FileDescriptorRegistry.setOffset fd 11L registry

        let registry =
            match closeOnly fd registry with
            | Ok registry -> registry
            | Error error -> failwith $"close failed: %O{error}"

        let reopened, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly registry

        offsetOf reopened registry |> shouldEqual (Some 0L)

    /// A negative offset is the one unsound position still representable, so `checkInvariants` names
    let private socketOn (socketId : int64) : OpenFileDescription =
        {
            Target = OpenFileTarget.Socket (SocketId socketId)
            AccessMode = FileAccessMode.ReadWrite
            NonBlocking = false
            Flock = None
        }

    /// Two descriptions naming one socket. PawPrint models no way to produce
    /// this — `dup(2)` shares a description rather than copying it — and it
    /// would be guest-visible: `flock` contends *between* descriptions naming one
    /// object but not within one, so a duplicated identity would make a socket
    /// contend with itself.
    [<Test>]
    let ``checkInvariants rejects two descriptions naming one socket`` () : unit =
        FileDescriptorRegistry.Unchecked.ofParts
            (Map.ofList [ 0, OpenFileDescriptionId 7L ; 1, OpenFileDescriptionId 8L ])
            (Map.ofList
                [
                    OpenFileDescriptionId 7L, socketOn 5L
                    OpenFileDescriptionId 8L, socketOn 5L
                ])
            (OpenFileDescriptionId 9L)
        |> FileDescriptorRegistry.checkInvariants
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.DuplicateSocketId (
                    OpenFileDescriptionId 7L,
                    OpenFileDescriptionId 8L,
                    SocketId 5L
                )
            ]

    /// ...and two descriptions naming two *different* sockets are accepted, so
    /// the check above is not simply "two sockets".
    [<Test>]
    let ``checkInvariants accepts two descriptions naming different sockets`` () : unit =
        FileDescriptorRegistry.Unchecked.ofParts
            (Map.ofList [ 0, OpenFileDescriptionId 7L ; 1, OpenFileDescriptionId 8L ])
            (Map.ofList
                [
                    OpenFileDescriptionId 7L, socketOn 5L
                    OpenFileDescriptionId 8L, socketOn 4L
                ])
            (OpenFileDescriptionId 9L)
        |> FileDescriptorRegistry.checkInvariants
        |> shouldEqual []

    /// Every allocating operation the module offers, interleaved at random, must
    /// leave a table `checkInvariants` accepts. The duplicate-socket clause is
    /// the reason this exists in its present form: it is asserted directly above
    /// against tables `Unchecked.ofParts` built by hand, and this is what
    /// connects it to the allocation path.
    ///
    /// Identities are minted here rather than by `createSocket`, which no longer
    /// mints them — the counter lives beside the socket table in
    /// `EmulatedKernel`. That the *kernel* keeps both tables in step under the
    /// same interleaving is `TestEmulatedKernelSockets`' own random-mix
    /// property.
    [<Test>]
    let ``a random mix of allocations and closes keeps the table sound`` () : unit =
        let mutable observedSockets = 0
        let mutable observedCloses = 0
        let mutable observedDups = 0
        let mutable observedLiveSocketPairs = 0

        let property (NonNegativeInt seed : NonNegativeInt) : unit =
            let rng = System.Random (seed)
            let steps = rng.Next (1, 30)

            let mutable registry = FileDescriptorRegistry.initial
            let mutable nextSocketId = 0L

            for _ in 1..steps do
                let live = FileDescriptorRegistry.fds registry |> Map.toList |> List.map fst

                // Biased towards allocation so the table grows on average and
                // several sockets are live at once, which is what makes the
                // duplicate-identity clause reachable at all.
                //
                // `close` and `dup` need something to name, and an unlucky run
                // of closes really can empty the table — the standard streams
                // are ordinary descriptors here, with nothing pinning them — so
                // an empty table falls through to the allocating arms rather
                // than indexing an empty list.
                match (if List.isEmpty live then 9 else rng.Next 10) with
                | 0
                | 1 ->
                    match closeOnly live.[rng.Next live.Length] registry with
                    | Ok registry' ->
                        registry <- registry'
                        observedCloses <- observedCloses + 1
                    | Error e -> failwith $"unexpected close error: %O{e}"
                | 2
                | 3 ->
                    match FileDescriptorRegistry.dup live.[rng.Next live.Length] registry with
                    | Ok (_, registry') ->
                        registry <- registry'
                        observedDups <- observedDups + 1
                    | Error e -> failwith $"unexpected dup error: %O{e}"
                | 4
                | 5 ->
                    let _, registry' =
                        FileDescriptorRegistry.openFile
                            (InodeNumber (int64 (rng.Next 5)))
                            FileAccessMode.ReadOnly
                            registry

                    registry <- registry'
                | 6 ->
                    let _, registry' = FileDescriptorRegistry.createSocketEventPort registry
                    registry <- registry'
                | _ ->
                    // A different triple each time, so that a `createSocket`
                    // which keyed identity off the triple rather than off a
                    // counter would not be saved by them all being equal.
                    let domain =
                        match rng.Next 3 with
                        | 0 -> SocketDomain.InterNetwork
                        | 1 -> SocketDomain.InterNetworkV6
                        | _ -> SocketDomain.Unix

                    let kind =
                        if rng.Next 2 = 0 then
                            SocketKind.Stream
                        else
                            SocketKind.Datagram

                    // The triple no longer reaches the registry at all, but it
                    // is still drawn: `EmulatedKernel.createSocket` is what
                    // carries it to the socket table, and the kernel-level
                    // property is what asserts what becomes of it there.
                    ignore<SocketDomain * SocketKind> (domain, kind)

                    let _, registry' =
                        FileDescriptorRegistry.createSocket (SocketId nextSocketId) registry

                    nextSocketId <- nextSocketId + 1L
                    registry <- registry'
                    observedSockets <- observedSockets + 1

                let liveSockets =
                    FileDescriptorRegistry.descriptions registry
                    |> Map.toList
                    |> List.choose (fun (_, description) ->
                        match description.Target with
                        | OpenFileTarget.Socket socketId -> Some socketId
                        | _ -> None
                    )

                if liveSockets.Length > 1 then
                    observedLiveSocketPairs <- observedLiveSocketPairs + 1

                    // The property the duplicate clause protects, stated
                    // positively: distinct descriptions never share a socket.
                    liveSockets |> List.distinct |> List.length |> shouldEqual liveSockets.Length

                FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

        Check.One (propertyConfig, property)

        // Without these the run could be sound while never having exercised the
        // operations the clauses are about.
        observedSockets |> shouldBeGreaterThan 500
        observedCloses |> shouldBeGreaterThan 100
        observedDups |> shouldBeGreaterThan 100
        // And specifically: two or more sockets alive at once, which is the only
        // state in which one could collide with another.
        observedLiveSocketPairs |> shouldBeGreaterThan 500

    /// it. Reachable only through `Unchecked.ofParts`: every operation in the module maintains it.
    [<Test>]
    let ``checkInvariants rejects a negative offset`` () : unit =
        let table (offset : int64) =
            FileDescriptorRegistry.Unchecked.ofParts
                (Map.ofList [ 0, OpenFileDescriptionId 7L ])
                (Map.ofList
                    [
                        OpenFileDescriptionId 7L,
                        {
                            Target = OpenFileTarget.File (someInode, offset)
                            AccessMode = FileAccessMode.ReadOnly
                            NonBlocking = false
                            Flock = None
                        }
                    ])
                (OpenFileDescriptionId 9L)

        FileDescriptorRegistry.checkInvariants (table -1L)
        |> shouldEqual [ FileDescriptorRegistryDefect.NegativeOffset (OpenFileDescriptionId 7L, -1L) ]

        // Zero and any positive offset are sound, including one far past any file's end: seeking
        // beyond EOF is legal, so there is no upper bound to check.
        FileDescriptorRegistry.checkInvariants (table 0L) |> shouldEqual []

        FileDescriptorRegistry.checkInvariants (table System.Int64.MaxValue)
        |> shouldEqual []

    let private nonBlockingOf (fd : int) (registry : FileDescriptorRegistry) : bool =
        match FileDescriptorRegistry.tryFind fd registry with
        | Some description -> description.NonBlocking
        | None -> failwith $"fd %d{fd} should be live"

    [<Test>]
    let ``every creator starts its description blocking`` () : unit =
        // The three inherited streams...
        for fd in [ 0 ; 1 ; 2 ] do
            nonBlockingOf fd FileDescriptorRegistry.initial |> shouldEqual false

        // ...and each of the three creators: `SystemNative_Open` accepts no
        // O_NONBLOCK bit, `socket(2)` is given no SOCK_NONBLOCK, and an event
        // port is handed out as `epoll_create1`/`kqueue` make it.
        let fd, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        nonBlockingOf fd registry |> shouldEqual false

        let fd, registry = FileDescriptorRegistry.createSocket (SocketId 0L) registry
        nonBlockingOf fd registry |> shouldEqual false

        let fd, registry = FileDescriptorRegistry.createSocketEventPort registry
        nonBlockingOf fd registry |> shouldEqual false

    [<Test>]
    let ``setNonBlocking round-trips on a socket and on a file`` () : unit =
        let socketFd, registry =
            FileDescriptorRegistry.createSocket (SocketId 0L) FileDescriptorRegistry.initial

        let fileFd, registry =
            FileDescriptorRegistry.openFile someInode FileAccessMode.ReadWrite registry

        for fd in [ socketFd ; fileFd ] do
            let registry = FileDescriptorRegistry.setNonBlocking fd true registry
            nonBlockingOf fd registry |> shouldEqual true

            // Setting is idempotent, and clearing restores the blank state.
            let registry = FileDescriptorRegistry.setNonBlocking fd true registry
            nonBlockingOf fd registry |> shouldEqual true

            let registry = FileDescriptorRegistry.setNonBlocking fd false registry
            nonBlockingOf fd registry |> shouldEqual false

            FileDescriptorRegistry.assertInvariants "setNonBlocking round-trip" registry
            |> ignore<FileDescriptorRegistry>

    [<Test>]
    let ``setNonBlocking on one descriptor of a dup pair is visible through the other`` () : unit =
        let fd, registry =
            FileDescriptorRegistry.createSocket (SocketId 0L) FileDescriptorRegistry.initial

        let duplicated, registry =
            match FileDescriptorRegistry.dup fd registry with
            | Ok result -> result
            | Error e -> failwith $"expected dup to succeed, got %O{e}"

        // Set through the original, read through the duplicate...
        let registry = FileDescriptorRegistry.setNonBlocking fd true registry
        nonBlockingOf duplicated registry |> shouldEqual true

        // ...clear through the duplicate, read through the original.
        let registry = FileDescriptorRegistry.setNonBlocking duplicated false registry
        nonBlockingOf fd registry |> shouldEqual false

        // Closing one half does not disturb the survivor's flag.
        let registry = FileDescriptorRegistry.setNonBlocking fd true registry

        let registry =
            match FileDescriptorRegistry.close duplicated registry with
            | Ok (registry, destroyed) ->
                // The original still names the description, so nothing died.
                destroyed |> shouldEqual None
                registry
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        nonBlockingOf fd registry |> shouldEqual true

    [<Test>]
    let ``setNonBlocking refuses a dead fd, and refuses to flag a stream`` () : unit =
        // A dead fd is the caller's EBADF to answer, not this module's.
        (fun () ->
            FileDescriptorRegistry.setNonBlocking 99 true FileDescriptorRegistry.initial
            |> ignore
        )
        |> shouldFail<exn>

        // Setting on a standard stream would store a flag no modelled stream
        // transfer consults; the backstop refuses even when the handler screen
        // is bypassed. Clearing is a no-op statement of the truth, so it is
        // permitted on the same target.
        (fun () ->
            FileDescriptorRegistry.setNonBlocking 0 true FileDescriptorRegistry.initial
            |> ignore
        )
        |> shouldFail<exn>

        nonBlockingOf 0 (FileDescriptorRegistry.setNonBlocking 0 false FileDescriptorRegistry.initial)
        |> shouldEqual false

    /// The store is flavour-free: measured on both kernels, `F_SETFL` on an
    /// event port genuinely toggles the bit (on Darwin the call *also* reports
    /// ENOTTY, which is the handler's business — the flavour split lives in
    /// `SimulatedUnixPlatform.eventPortSetStatusFlagsError`, and the handler
    /// stores before reporting).
    [<Test>]
    let ``setNonBlocking round-trips on a socket event port`` () : unit =
        let portFd, registry =
            FileDescriptorRegistry.createSocketEventPort FileDescriptorRegistry.initial

        let registry = FileDescriptorRegistry.setNonBlocking portFd true registry
        nonBlockingOf portFd registry |> shouldEqual true

        let registry = FileDescriptorRegistry.setNonBlocking portFd false registry
        nonBlockingOf portFd registry |> shouldEqual false

    // --- what a registration reports ---

    /// Every `ReadinessLevel` there is: five booleans, so 32 of them.
    let private allLevels : ReadinessLevel list =
        [
            for bits in 0..31 ->
                {
                    In = bits &&& 0x01 <> 0
                    Out = bits &&& 0x02 <> 0
                    RdHup = bits &&& 0x04 <> 0
                    Hup = bits &&& 0x08 <> 0
                    Err = bits &&& 0x10 <> 0
                }
        ]

    /// Every `SocketEventInterest` there is: three booleans, so eight.
    let private allInterests : SocketEventInterest list =
        [
            for bits in 0..7 ->
                {
                    In = bits &&& 0x01 <> 0
                    Out = bits &&& 0x02 <> 0
                    RdHup = bits &&& 0x04 <> 0
                }
        ]

    /// The two ends of `reportedUnder`. Asking for everything reports the level
    /// itself; asking for nothing still reports `HUP` and `ERR`, which is what
    /// makes them not interest and why the record has no field for them
    /// (measured, a pending refusal registered with interest 0 reports 0x18).
    [<Test>]
    let ``a full interest reports the level, and an empty one reports HUP and ERR`` () : unit =
        let everything : SocketEventInterest =
            {
                In = true
                Out = true
                RdHup = true
            }

        let nothing : SocketEventInterest =
            {
                In = false
                Out = false
                RdHup = false
            }

        for level in allLevels do
            ReadinessLevel.reportedUnder everything level |> shouldEqual level

            ReadinessLevel.reportedUnder nothing level
            |> shouldEqual
                { ReadinessLevel.none with
                    Hup = level.Hup
                    Err = level.Err
                }

    /// A report never invents readiness, and widening what was asked for never
    /// withdraws any.
    [<Test>]
    let ``a report is a sub-level, and grows with the interest`` () : unit =
        let subLevel (small : ReadinessLevel) (big : ReadinessLevel) : bool =
            (not small.In || big.In)
            && (not small.Out || big.Out)
            && (not small.RdHup || big.RdHup)
            && (not small.Hup || big.Hup)
            && (not small.Err || big.Err)

        for level in allLevels do
            for interest in allInterests do
                let reported = ReadinessLevel.reportedUnder interest level

                if not (subLevel reported level) then
                    failwith $"reportedUnder %O{interest} %O{level} = %O{reported}, which is not a sub-level of it"

                let widened =
                    { interest with
                        In = true
                    }

                if not (subLevel reported (ReadinessLevel.reportedUnder widened level)) then
                    failwith $"widening %O{interest} to %O{widened} withdrew part of %O{reported}"

    /// Which field each interest bit gates, pinned one bit at a time: clearing
    /// exactly one of them may change exactly its own condition, and nothing
    /// else. This is what a swapped pair of fields fails.
    [<Test>]
    let ``each interest bit gates its own condition alone`` () : unit =
        let clearings
            : (string * (SocketEventInterest -> SocketEventInterest) * (ReadinessLevel -> ReadinessLevel)) list =
            [
                "In",
                (fun i ->
                    { i with
                        In = false
                    }
                ),
                (fun r ->
                    { r with
                        In = false
                    }
                )
                "Out",
                (fun i ->
                    { i with
                        Out = false
                    }
                ),
                (fun r ->
                    { r with
                        Out = false
                    }
                )
                "RdHup",
                (fun i ->
                    { i with
                        RdHup = false
                    }
                ),
                (fun r ->
                    { r with
                        RdHup = false
                    }
                )
            ]

        for level in allLevels do
            for interest in allInterests do
                for name, clearInterest, clearReport in clearings do
                    let before = ReadinessLevel.reportedUnder interest level
                    let after = ReadinessLevel.reportedUnder (clearInterest interest) level

                    if after <> clearReport before then
                        failwith
                            $"clearing %s{name} from %O{interest} took %O{level} from %O{before} to %O{after}, which is not %O{before} with %s{name} cleared"

    // --- socket event registrations ---

    /// The interest table of the port `portFd` names. Fails on anything else, so
    /// a test cannot silently assert about the wrong descriptor.
    let private registrationsOf
        (portFd : int)
        (registry : FileDescriptorRegistry)
        : Map<int * OpenFileDescriptionId, SocketEventRegistration>
        =
        match FileDescriptorRegistry.tryFindTarget portFd registry with
        | Some (OpenFileTarget.SocketEventPort portState) -> portState.Registrations
        | other -> failwith $"fd %d{portFd} is not a socket event port: %O{other}"

    let private readWrite : SocketEventInterest =
        {
            In = true
            Out = true
            RdHup = false
        }

    let private change
        (portFd : int)
        (targetFd : int)
        (change : SocketEventRegistrationChange)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        match FileDescriptorRegistry.changeSocketEventRegistration portFd targetFd 0L change registry with
        | Ok registry -> registry
        | Error error -> failwith $"changeSocketEventRegistration failed: %O{error}"

    /// No guest can observe the stored *values* yet — delivering them is the
    /// readiness wake, which has no producer until `SystemNative_Connect`
    /// lands — so the write-back is pinned here: a handler recording zeroes
    /// would survive every guest row (their observers are only EEXIST/ENOENT,
    /// i.e. presence) and fail this.
    [<Test>]
    let ``a registration records the interest and data it was given, and Modify replaces both`` () : unit =
        let portFd, registry =
            FileDescriptorRegistry.createSocketEventPort FileDescriptorRegistry.initial

        let sockFd, registry = FileDescriptorRegistry.createSocket (SocketId 0L) registry

        let sockId =
            match FileDescriptorRegistry.tryFindId sockFd registry with
            | Some id -> id
            | None -> failwith "socket fd not live"

        let registry =
            change portFd sockFd (SocketEventRegistrationChange.Add (readWrite, 0xABCDUL)) registry

        registrationsOf portFd registry
        |> shouldEqual (
            Map.ofList
                [
                    (sockFd, sockId),
                    {
                        Interest = readWrite
                        Data = 0xABCDUL
                        RegisteredAt = 0L
                    }
                ]
        )

        let readOnly =
            { readWrite with
                Out = false
            }

        let registry =
            change portFd sockFd (SocketEventRegistrationChange.Modify (readOnly, 77UL)) registry

        registrationsOf portFd registry
        |> shouldEqual (
            Map.ofList
                [
                    (sockFd, sockId),
                    {
                        Interest = readOnly
                        Data = 77UL
                        RegisteredAt = 0L
                    }
                ]
        )

        let registry = change portFd sockFd SocketEventRegistrationChange.Remove registry
        registrationsOf portFd registry |> shouldEqual Map.empty
        FileDescriptorRegistry.assertInvariants "after remove" registry |> ignore

    /// The registration key is the (fd, description) *pair*, exactly as epoll
    /// keys it: a `dup` of the target admits a second registration, and a
    /// `dup` of the port operates on the one shared table.
    [<Test>]
    let ``dup of the target is a second key; dup of the port is the same table`` () : unit =
        let portFd, registry =
            FileDescriptorRegistry.createSocketEventPort FileDescriptorRegistry.initial

        let sockFd, registry = FileDescriptorRegistry.createSocket (SocketId 0L) registry

        let dupFd, registry =
            match FileDescriptorRegistry.dup sockFd registry with
            | Ok result -> result
            | Error error -> failwith $"dup failed: %O{error}"

        let registry =
            change portFd sockFd (SocketEventRegistrationChange.Add (readWrite, 1UL)) registry

        let registry =
            change portFd dupFd (SocketEventRegistrationChange.Add (readWrite, 2UL)) registry

        (registrationsOf portFd registry).Count |> shouldEqual 2

        // The port's dup reaches the same table: a re-Add through it answers
        // AlreadyRegistered, and a Remove through it is visible via the
        // original port fd.
        let dupPortFd, registry =
            match FileDescriptorRegistry.dup portFd registry with
            | Ok result -> result
            | Error error -> failwith $"dup failed: %O{error}"

        FileDescriptorRegistry.changeSocketEventRegistration
            dupPortFd
            sockFd
            0L
            (SocketEventRegistrationChange.Add (readWrite, 3UL))
            registry
        |> shouldEqual (Error SocketEventRegistrationError.AlreadyRegistered)

        let registry = change dupPortFd sockFd SocketEventRegistrationChange.Remove registry
        (registrationsOf portFd registry).Count |> shouldEqual 1

    /// Linux removes a destroyed description's registrations at file-release
    /// time (`eventpoll_release`); PawPrint's `close` does the same sweep. No
    /// syscall can see the difference — the dead key can never be probed again
    /// — so this is the only observer, and it is what keeps the future
    /// readiness wake from delivering out of a corpse.
    [<Test>]
    let ``closing the target's last descriptor sweeps its registrations; a surviving dup keeps them`` () : unit =
        let portFd, registry =
            FileDescriptorRegistry.createSocketEventPort FileDescriptorRegistry.initial

        let sockFd, registry = FileDescriptorRegistry.createSocket (SocketId 0L) registry

        let dupFd, registry =
            match FileDescriptorRegistry.dup sockFd registry with
            | Ok result -> result
            | Error error -> failwith $"dup failed: %O{error}"

        let registry =
            change portFd sockFd (SocketEventRegistrationChange.Add (readWrite, 1UL)) registry

        // Closing `sockFd` leaves the description alive through the dup, so
        // the registration — keyed on the now-dead fd number — survives, which
        // is Linux's own (notorious) behaviour.
        let registry =
            match closeOnly sockFd registry with
            | Ok registry -> registry
            | Error error -> failwith $"close failed: %O{error}"

        (registrationsOf portFd registry).Count |> shouldEqual 1
        FileDescriptorRegistry.assertInvariants "dup still live" registry |> ignore

        // Closing the last descriptor destroys the description and sweeps.
        let registry =
            match closeOnly dupFd registry with
            | Ok registry -> registry
            | Error error -> failwith $"close failed: %O{error}"

        registrationsOf portFd registry |> shouldEqual Map.empty
        FileDescriptorRegistry.assertInvariants "after sweep" registry |> ignore

    [<Test>]
    let ``checkInvariants rejects a registration naming a dead description`` () : unit =
        let portId = OpenFileDescriptionId 0L
        let deadId = OpenFileDescriptionId 99L

        let registry =
            FileDescriptorRegistry.Unchecked.ofParts
                (Map.ofList [ 3, portId ])
                (Map.ofList
                    [
                        portId,
                        {
                            Target =
                                OpenFileTarget.SocketEventPort
                                    {
                                        Registrations =
                                            Map.ofList
                                                [
                                                    (4, deadId),
                                                    {
                                                        Interest = readWrite
                                                        Data = 0UL
                                                        RegisteredAt = 0L
                                                    }
                                                ]
                                        Ready = []
                                    }
                            AccessMode = FileAccessMode.ReadWrite
                            NonBlocking = false
                            Flock = None
                        }
                    ])
                (OpenFileDescriptionId 100L)

        FileDescriptorRegistry.checkInvariants registry
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.SocketEventRegistrationTargetDead (portId, deadId)
            ]

    /// A port whose ready list disagrees with its interest table: one entry
    /// nothing registers, and one registered entry pending twice.
    [<Test>]
    let ``checkInvariants rejects unregistered and duplicated ready entries`` () : unit =
        let portFd, registry =
            FileDescriptorRegistry.createSocketEventPort FileDescriptorRegistry.initial

        let sockFd, registry = FileDescriptorRegistry.createSocket (SocketId 0L) registry

        let sockId =
            match FileDescriptorRegistry.tryFindId sockFd registry with
            | Some id -> id
            | None -> failwith "socket fd not live"

        let portId =
            match FileDescriptorRegistry.tryFindId portFd registry with
            | Some id -> id
            | None -> failwith "port fd not live"

        let registry =
            change portFd sockFd (SocketEventRegistrationChange.Add (readWrite, 1UL)) registry

        let withReady (ready : (int * OpenFileDescriptionId) list) : FileDescriptorRegistry =
            FileDescriptorRegistry.Unchecked.mapDescription
                portId
                (fun description ->
                    match description.Target with
                    | OpenFileTarget.SocketEventPort portState ->
                        { description with
                            Target =
                                OpenFileTarget.SocketEventPort
                                    { portState with
                                        Ready = ready
                                    }
                        }
                    | other -> failwith $"not a port: %O{other}"
                )
                registry

        FileDescriptorRegistry.checkInvariants (withReady [ 7, OpenFileDescriptionId 55L ])
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.SocketEventReadyEntryUnregistered (portId, 7, OpenFileDescriptionId 55L)
            ]

        FileDescriptorRegistry.checkInvariants (withReady [ sockFd, sockId ; sockFd, sockId ])
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.SocketEventReadyEntryDuplicated (portId, sockFd, sockId)
            ]

        FileDescriptorRegistry.checkInvariants (withReady [ sockFd, sockId ])
        |> shouldEqual []
