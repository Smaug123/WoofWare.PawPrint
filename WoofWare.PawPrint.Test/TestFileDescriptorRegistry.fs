namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFileDescriptorRegistry =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private standardStream (role : FileDescriptorRole) : OpenFileDescription =
        {
            Object = OpenFileObject.StandardStream role
            Flock = None
        }

    let private openOn (inode : InodeNumber) : OpenFileDescription =
        {
            Object = OpenFileObject.File inode
            Flock = None
        }

    let private someInode : InodeNumber = InodeNumber 42L
    let private otherInode : InodeNumber = InodeNumber 43L

    [<Test>]
    let ``openFile takes the lowest free descriptor and a fresh description`` () : unit =
        let a, registry =
            FileDescriptorRegistry.openFile someInode FileDescriptorRegistry.initial

        a |> shouldEqual 3

        let b, registry = FileDescriptorRegistry.openFile otherInode registry
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
            FileDescriptorRegistry.openFile someInode FileDescriptorRegistry.initial

        let b, registry = FileDescriptorRegistry.openFile someInode registry

        FileDescriptorRegistry.tryFind a registry
        |> shouldEqual (FileDescriptorRegistry.tryFind b registry)

        FileDescriptorRegistry.tryFindId a registry
        |> shouldNotEqual (FileDescriptorRegistry.tryFindId b registry)

        match FileDescriptorRegistry.dup a registry with
        | Error e -> failwith $"expected dup to succeed, got %O{e}"
        | Ok (duplicate, registry) ->
            FileDescriptorRegistry.tryFindId duplicate registry
            |> shouldEqual (FileDescriptorRegistry.tryFindId a registry)

    /// Descriptor numbers are reused; description identities are not. The
    /// former is POSIX ("lowest free"), the latter is what keeps a replay trace
    /// unambiguous about which open a given description was.
    [<Test>]
    let ``a closed description's identity is never handed out again`` () : unit =
        let fd, registry =
            FileDescriptorRegistry.openFile someInode FileDescriptorRegistry.initial

        let firstId = FileDescriptorRegistry.tryFindId fd registry

        let registry =
            match FileDescriptorRegistry.close fd registry with
            | Ok registry -> registry
            | Error e -> failwith $"expected close to succeed, got %O{e}"

        let reused, registry = FileDescriptorRegistry.openFile otherInode registry

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

    /// Load-bearing, not decorative: an implementation that pointed every file
    /// descriptor at a single shared open file description would satisfy the
    /// dup-sharing property below, and only this test would notice. PawPrint
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

        match FileDescriptorRegistry.close duped registry with
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
        match FileDescriptorRegistry.close 1 FileDescriptorRegistry.initial with
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
            match FileDescriptorRegistry.close fd registry with
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
        FileDescriptorRegistry.close 3 FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorCloseError.BadFd)

        FileDescriptorRegistry.close -1 FileDescriptorRegistry.initial
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

                    match FileDescriptorRegistry.close chosen registry with
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
                        // existing maximum (the load-bearing case for the
                        // lowest-free contract) versus extending past the top.
                        let liveMaxBefore = Set.maxElement live

                        if newFd < liveMaxBefore then
                            observedHoleFills <- observedHoleFills + 1

                        live <- Set.add newFd live
                        registry <- registry'
                        observedDups <- observedDups + 1
                    | Error e -> failwith $"unexpected dup error: %O{e}"

        Check.One (propertyConfig, property)

        // Distribution check: the test is only load-bearing if it exercises
        // the gap-filling case. If observedHoleFills were 0, the property
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
    /// mints a fresh description per `dup` (i.e. today's copying behaviour).
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
                    match FileDescriptorRegistry.close chosen registry with
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
        FileDescriptorRegistry.openFile inode registry

    let private lockOrFail
        (fd : int)
        (request : FlockRequest)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry
        =
        match FileDescriptorRegistry.flock fd request registry with
        | Ok registry -> FileDescriptorRegistry.assertInvariants "flock" registry
        | Error e -> failwith $"expected flock to succeed, got %O{e}"

    [<Test>]
    let ``flock of an unknown fd is BadFd`` () : unit =
        for fd in [ 3 ; -1 ; System.Int32.MaxValue ] do
            FileDescriptorRegistry.flock fd (FlockRequest.Acquire FlockMode.Shared) FileDescriptorRegistry.initial
            |> shouldEqual (Error FlockError.BadFd)

            FileDescriptorRegistry.flock fd FlockRequest.Release FileDescriptorRegistry.initial
            |> shouldEqual (Error FlockError.BadFd)

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
        | Ok registry ->
            expectedGranted |> shouldEqual true
            FileDescriptorRegistry.checkInvariants registry |> shouldEqual []
        | Error FlockError.WouldBlock -> expectedGranted |> shouldEqual false
        | Error e -> failwith $"unexpected flock error: %O{e}"

    /// A lock is per file, so a description on a different inode is not an
    /// obstacle. Without this, "one global lock" satisfies the matrix above.
    [<Test>]
    let ``an exclusive lock on one file does not block another file`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let b, registry = openOrFail otherInode registry

        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Exclusive) registry

        FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> Result.isOk
        |> shouldEqual true

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
        |> shouldEqual (Error FlockError.WouldBlock)

        // ...and succeeds again once the other holder releases.
        let registry = lockOrFail b FlockRequest.Release registry

        FileDescriptorRegistry.flock a (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> Result.isOk
        |> shouldEqual true

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
        |> shouldEqual (Error FlockError.WouldBlock)

        // ...until the lock is released through the *dup*, which drops the
        // single shared lock rather than one of two copies.
        let registry = lockOrFail duplicate FlockRequest.Release registry

        FileDescriptorRegistry.flock other (FlockRequest.Acquire FlockMode.Shared) registry
        |> Result.isOk
        |> shouldEqual true

    /// Closing the last descriptor of a description destroys the description,
    /// and with it the lock — which is what makes a `FileStream` that was never
    /// explicitly unlocked stop blocking the next open.
    [<Test>]
    let ``closing a locked description releases its lock`` () : unit =
        let a, registry = openOrFail someInode FileDescriptorRegistry.initial
        let registry = lockOrFail a (FlockRequest.Acquire FlockMode.Exclusive) registry
        let b, registry = openOrFail someInode registry

        FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> shouldEqual (Error FlockError.WouldBlock)

        let registry =
            match FileDescriptorRegistry.close a registry with
            | Ok registry -> registry
            | Error e -> failwith $"unexpected close error: %O{e}"

        FileDescriptorRegistry.flock b (FlockRequest.Acquire FlockMode.Exclusive) registry
        |> Result.isOk
        |> shouldEqual true

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
                Object = OpenFileObject.File someInode
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
                Object = OpenFileObject.File inode
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
                    let fd, registry' = FileDescriptorRegistry.openFile inode registry

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

                    match FileDescriptorRegistry.close fd registry with
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
                    | Ok registry' ->
                        registry <- registry'
                        let inode, _ = Map.find id held
                        held <- Map.add id (inode, None) held
                    | Error e -> failwith $"unexpected release error: %O{e}"
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
                    | Ok registry' ->
                        expectedBlocked |> shouldEqual false
                        registry <- registry'
                        held <- Map.add id (inode, Some wanted) held
                        observedGrants <- observedGrants + 1

                        if existing.IsSome then
                            observedConversions <- observedConversions + 1
                    | Error FlockError.WouldBlock ->
                        expectedBlocked |> shouldEqual true
                        observedRefusals <- observedRefusals + 1
                    | Error e -> failwith $"unexpected flock error: %O{e}"

                FileDescriptorRegistry.checkInvariants registry |> shouldEqual []

        Check.One (propertyConfig, property)

        // Without refusals the exclusion rule is never exercised; without
        // conversions the re-lock path is never exercised.
        observedGrants |> shouldBeGreaterThan 100
        observedRefusals |> shouldBeGreaterThan 30
        observedConversions |> shouldBeGreaterThan 30

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
