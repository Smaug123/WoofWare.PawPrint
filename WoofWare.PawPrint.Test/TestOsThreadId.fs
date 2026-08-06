namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedKernel.osThreadIdForGuest` / `osThreadIdForInternal` are the policy
/// behind `SystemNative_TryGetUInt32OSThreadId` (Linux CoreLib) and
/// `SystemNative_GetUInt64OSThreadId` (macOS CoreLib) — the OS thread id
/// `System.Threading.Lock` uses as its owner identity.
///
/// The reason this is worth a property module rather than a couple of examples:
/// the load-bearing invariant is *uniqueness across every live thread*, and
/// there are two independent producers minting into one namespace. A collision
/// would not crash — it would make `Lock` treat two threads as one, silently,
/// because `Lock` reads a matching id as "the same thread re-entering". So
/// disjointness between the producers, and injectivity within each, are
/// established here once rather than hoped for.
///
/// `TestCpuPlacement` covers the sibling policy (`cpuForRotation`) and the
/// shared `NextGuestThreadOrdinal` cursor's bookkeeping.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOsThreadId =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// Exactly the values the `IlMachineState` cursors can hold: non-negative,
    /// and below the `Int32.MaxValue` tripwire both producers reject.
    let private ordinalFrom (seed : int) : int = abs (seed % 1_000_000)

    let private raw (OsThreadId.OsThreadId i : OsThreadId) : uint32 = i

    let private ints = ArbMap.defaults |> ArbMap.arbitrary<int>
    let private intPairs = ArbMap.defaults |> ArbMap.arbitrary<int * int>

    /// `TryGetUInt32OSThreadId` returns this to mean "this platform does not
    /// know how to get an OS thread id", so a real id must never equal it.
    let private unknownSentinel : uint32 = 0xFFFF_FFFFu

    let private succeeds (f : unit -> 'a) : bool =
        try
            f () |> ignore<'a>
            true
        with _ ->
            false

    // --- The pure minting policy ---

    [<Test>]
    let ``no minted id is either sentinel`` () =
        // `0` and `(uint32)-1` are both fatal, and for different reasons.
        // `(uint32)-1` is the PAL's "cannot determine" signal, so a thread that
        // genuinely had that id would be indistinguishable from an unsupported
        // platform. `0` is worse: CoreLib's `Lock.ThreadId.InitializeForCurrentThread`
        // maps a zero id to `0xFFFF_FFFF` by decrementing it, so *every* thread
        // that minted `0` would end up sharing one id.
        let property (seed : int) : bool =
            let ordinal = ordinalFrom seed
            let guest = raw (EmulatedKernel.osThreadIdForGuest ordinal)
            let internalId = raw (EmulatedKernel.osThreadIdForInternal ordinal)

            guest <> 0u
            && guest <> unknownSentinel
            && internalId <> 0u
            && internalId <> unknownSentinel

        Check.One (propertyConfig, Prop.forAll ints property)

    [<Test>]
    let ``the extremes of the accepted range are also safe`` () =
        // The properties above sample; these are the two values that actually
        // sit against the tripwire. `Int32.MaxValue - 1` is the largest ordinal
        // either producer accepts, and for the guest producer
        // (`2*ordinal + 1`) the very next one would land exactly on
        // `0xFFFF_FFFF`. That near-miss is the whole reason the bound is
        // `Int32.MaxValue - 1` rather than `Int32.MaxValue`, so pin it.
        let maxOrdinal = System.Int32.MaxValue - 1

        raw (EmulatedKernel.osThreadIdForGuest 0) |> shouldEqual 1u
        raw (EmulatedKernel.osThreadIdForInternal 0) |> shouldEqual 2u

        raw (EmulatedKernel.osThreadIdForGuest maxOrdinal) |> shouldEqual 0xFFFF_FFFDu

        raw (EmulatedKernel.osThreadIdForInternal maxOrdinal)
        |> shouldEqual 0xFFFF_FFFEu

        succeeds (fun () -> EmulatedKernel.osThreadIdForGuest System.Int32.MaxValue)
        |> shouldEqual false

        succeeds (fun () -> EmulatedKernel.osThreadIdForInternal System.Int32.MaxValue)
        |> shouldEqual false

    [<Test>]
    let ``each producer is injective`` () =
        // Two distinct guest threads must never share an id, and likewise two
        // distinct internal threads.
        let property (a : int, b : int) : bool =
            let x = ordinalFrom a
            let y = ordinalFrom b

            if x = y then
                true
            else
                EmulatedKernel.osThreadIdForGuest x <> EmulatedKernel.osThreadIdForGuest y
                && EmulatedKernel.osThreadIdForInternal x <> EmulatedKernel.osThreadIdForInternal y

        Check.One (propertyConfig, Prop.forAll intPairs property)

    [<Test>]
    let ``the two producers never collide`` () =
        // The half that injectivity alone would miss, and the half that matters
        // most: the signal dispatcher runs *guest* handler code, so if its id
        // aliased a guest thread's, a handler taking a `Lock` that guest thread
        // already holds would be waved through as a re-entrant acquire.
        //
        // Stated over independent ordinals, because the two producers draw from
        // different cursors (`NextGuestThreadOrdinal` and `NextThreadId`) and so
        // can be at arbitrary relative positions.
        let property (a : int, b : int) : bool =
            EmulatedKernel.osThreadIdForGuest (ordinalFrom a)
            <> EmulatedKernel.osThreadIdForInternal (ordinalFrom b)

        Check.One (propertyConfig, Prop.forAll intPairs property)

    [<Test>]
    let ``guest ids are odd and internal ids are even`` () =
        // The mechanism behind the disjointness property above, pinned
        // separately so that a future change to either producer fails here —
        // naming the cause — rather than only in the collision property, which
        // would merely report that some pair now matches.
        let property (seed : int) : bool =
            let ordinal = ordinalFrom seed

            raw (EmulatedKernel.osThreadIdForGuest ordinal) % 2u = 1u
            && raw (EmulatedKernel.osThreadIdForInternal ordinal) % 2u = 0u

        Check.One (propertyConfig, Prop.forAll ints property)

    [<Test>]
    let ``a negative ordinal fails loudly`` () =
        // There is a `FrameId -1` sentinel in this codebase and
        // `allocateParkedThread` uses it, so a negative thread ordinal is a
        // mistake someone could plausibly make. Minting `2*(-1) + 1` would
        // silently produce `0xFFFF_FFFF` — the "cannot determine" sentinel.
        let property (seed : int) : bool =
            let negative = -1 - abs (seed % 1_000_000)

            not (succeeds (fun () -> EmulatedKernel.osThreadIdForGuest negative))
            && not (succeeds (fun () -> EmulatedKernel.osThreadIdForInternal negative))

        Check.One (propertyConfig, Prop.forAll ints property)

    // --- Wiring: which creation sites consume which cursor ---
    //
    // The properties above cover the pure policy. These cover the part a later
    // change is most likely to break: that guest ids come from the guest
    // ordinal and internal ids do not, which is what keeps an interpreter-
    // internal allocation from shifting guest-observable ids.

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private machine () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    let private osThreadIdOf (thread : ThreadId) (state : IlMachineState) : OsThreadId =
        state.ThreadState.[thread].OsThreadId

    [<Test>]
    let ``guest threads are numbered from the guest ordinal`` () =
        let state = machine ()

        let state, first =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state

        let state, second =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        osThreadIdOf first state |> shouldEqual (OsThreadId 1u)
        osThreadIdOf second state |> shouldEqual (OsThreadId 3u)

    [<Test>]
    let ``an interleaved parked thread does not shift guest ids`` () =
        // The exact analogue of TestCpuPlacement's
        // `an interleaved parked thread does not shift guest placements`, and
        // for the same reason: `allocateParkedThread` consumes `NextThreadId`,
        // and it is minted lazily on the guest's first
        // `SystemNative_InitializeTerminalAndSignalHandling`. If ids were keyed
        // off `NextThreadId`, a guest that touched Console before spawning a
        // worker would give that worker a different id from an otherwise
        // identical guest that did not — an interpreter detail leaking into a
        // value the guest can read.
        let idsFrom (allocateParkedFirst : bool) : OsThreadId list =
            let mutable state = machine ()

            if allocateParkedFirst then
                let state', _ = IlMachineState.allocateParkedThread state
                state <- state'

            [ 1..5 ]
            |> List.map (fun i ->
                let state', thread =
                    IlMachineState.allocateUnstartedThread (ManagedHeapAddress i) state

                state <- state'
                osThreadIdOf thread state
            )

        idsFrom true |> shouldEqual (idsFrom false)

        // ...and the equivalence is not two identical constants.
        idsFrom false
        |> shouldEqual
            [
                OsThreadId 1u
                OsThreadId 3u
                OsThreadId 5u
                OsThreadId 7u
                OsThreadId 9u
            ]

    [<Test>]
    let ``the parked dispatcher gets a real id disjoint from every guest id`` () =
        // Not a placeholder, unlike its `CpuId 0`: a processor index is a
        // shared-resource key, so aliasing is meaningful there and harmless. A
        // thread id is an ownership identity, and the dispatcher runs guest
        // handler code.
        let mutable state = machine ()

        let guestIds =
            [ 1..3 ]
            |> List.map (fun i ->
                let state', thread =
                    IlMachineState.allocateUnstartedThread (ManagedHeapAddress i) state

                state <- state'
                osThreadIdOf thread state
            )

        let state', parked = IlMachineState.allocateParkedThread state
        state <- state'

        let parkedId = osThreadIdOf parked state

        guestIds |> List.contains parkedId |> shouldEqual false
        raw parkedId % 2u |> shouldEqual 0u

    [<Test>]
    let ``every live thread has a distinct id`` () =
        // The whole point, stated end-to-end over the real allocation entry
        // points rather than over the producers in isolation: whatever mixture
        // of guest and interpreter-internal threads a run creates, no two of
        // them share an id.
        let property (seed : int) : bool =
            let mutable state = machine ()
            // A deterministic but varied interleaving of the two allocators.
            let pattern = [ 0..15 ] |> List.map (fun i -> ((seed >>> (i % 24)) &&& 1) = 1)

            let ids =
                pattern
                |> List.mapi (fun i isParked ->
                    if isParked then
                        let state', thread = IlMachineState.allocateParkedThread state
                        state <- state'
                        osThreadIdOf thread state
                    else
                        let state', thread =
                            IlMachineState.allocateUnstartedThread (ManagedHeapAddress (i + 1)) state

                        state <- state'
                        osThreadIdOf thread state
                )

            (ids |> List.distinct |> List.length) = List.length ids

        Check.One (propertyConfig, Prop.forAll ints property)
