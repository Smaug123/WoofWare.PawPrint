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

    [<Test>]
    let ``initial seeds stdin, stdout, stderr with OwnsResource = false`` () : unit =
        FileDescriptorRegistry.tryFind 0 FileDescriptorRegistry.initial
        |> shouldEqual (
            Some
                {
                    Role = FileDescriptorRole.StandardInput
                    OwnsResource = false
                }
        )

        FileDescriptorRegistry.tryFind 1 FileDescriptorRegistry.initial
        |> shouldEqual (
            Some
                {
                    Role = FileDescriptorRole.StandardOutput
                    OwnsResource = false
                }
        )

        FileDescriptorRegistry.tryFind 2 FileDescriptorRegistry.initial
        |> shouldEqual (
            Some
                {
                    Role = FileDescriptorRole.StandardError
                    OwnsResource = false
                }
        )

    [<Test>]
    let ``initial has no entries outside 0/1/2`` () : unit =
        for fd in [ -2 ; -1 ; 3 ; 4 ; 100 ; System.Int32.MaxValue ] do
            FileDescriptorRegistry.tryFind fd FileDescriptorRegistry.initial
            |> shouldEqual None

    [<Test>]
    let ``dup of unknown fd returns BadFd`` () : unit =
        FileDescriptorRegistry.dup 3 FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorDupError.BadFd)

        FileDescriptorRegistry.dup -1 FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorDupError.BadFd)

        FileDescriptorRegistry.dup System.Int32.MaxValue FileDescriptorRegistry.initial
        |> shouldEqual (Error FileDescriptorDupError.BadFd)

    [<Test>]
    let ``dup of stdin/stdout/stderr returns fresh fd 3 with matching role`` () : unit =
        let assertDupAllocatesFd3WithRole (sourceFd : int) (expectedRole : FileDescriptorRole) : unit =
            match FileDescriptorRegistry.dup sourceFd FileDescriptorRegistry.initial with
            | Ok (newFd, registry) ->
                newFd |> shouldEqual 3

                FileDescriptorRegistry.tryFind newFd registry
                |> shouldEqual (
                    Some
                        {
                            Role = expectedRole
                            OwnsResource = true
                        }
                )

                // Source fd is unaffected — the table still resolves it to its
                // original entry. dup is non-destructive on the source.
                FileDescriptorRegistry.tryFind sourceFd registry
                |> shouldEqual (FileDescriptorRegistry.tryFind sourceFd FileDescriptorRegistry.initial)
            | Error e -> failwith $"unexpected dup error: %O{e}"

        assertDupAllocatesFd3WithRole 0 FileDescriptorRole.StandardInput
        assertDupAllocatesFd3WithRole 1 FileDescriptorRole.StandardOutput
        assertDupAllocatesFd3WithRole 2 FileDescriptorRole.StandardError

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

    [<Test>]
    let ``dup preserves the role of the source fd across the new fd`` () : unit =
        let property (NonNegativeInt seed : NonNegativeInt) : unit =
            let rng = System.Random (seed)
            let steps = rng.Next (1, 15)

            let mutable registry = FileDescriptorRegistry.initial

            for _ in 1..steps do
                let liveList =
                    [ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ]
                    |> List.choose (fun fd ->
                        FileDescriptorRegistry.tryFind fd registry
                        |> Option.map (fun entry -> fd, entry)
                    )

                let pickIndex = rng.Next (liveList.Length)
                let sourceFd, sourceEntry = liveList.[pickIndex]

                match FileDescriptorRegistry.dup sourceFd registry with
                | Ok (newFd, registry') ->
                    match FileDescriptorRegistry.tryFind newFd registry' with
                    | Some newEntry ->
                        newEntry.Role |> shouldEqual sourceEntry.Role
                        // Any fd minted by SystemNative_Dup is owned by the
                        // simulated process and must be closed (when Close
                        // lands) to reclaim the slot. Inherited fds 0/1/2 are
                        // the only un-owned entries.
                        newEntry.OwnsResource |> shouldEqual true
                    | None -> failwith $"newly-allocated fd %d{newFd} not findable"

                    registry <- registry'
                | Error e -> failwith $"unexpected dup error: %O{e}"

        Check.One (propertyConfig, property)
