namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedKernel.osThreadId` is the policy behind
/// `SystemNative_TryGetUInt32OSThreadId` (Linux CoreLib) and
/// `SystemNative_GetUInt64OSThreadId` (macOS CoreLib) — the OS thread id
/// `System.Threading.Lock` uses as its owner identity.
///
/// The invariant is *uniqueness across every live thread*, and a collision
/// would not crash: it would make `Lock` treat two threads as one, silently,
/// because `Lock` reads a matching id as "the same thread re-entering".
/// Uniqueness is inherited from `ThreadId` — the policy is a function of it —
/// so these tests establish that the function is injective, that it dodges the
/// two fatal sentinel values, and that every allocation site feeds it a
/// distinct `ThreadId`.
///
/// `TestCpuPlacement` covers the sibling policy (`cpuForRotation`), which
/// deliberately keys off a *different* cursor; the contrast is the subject of
/// `a parked thread does consume an id, unlike a rotation slot` below.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOsThreadId =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// Exactly the values `IlMachineState.NextThreadId` can hold: non-negative.
    let private threadIdFrom (seed : int) : ThreadId = ThreadId (abs (seed % 1_000_000))

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
            let id = raw (EmulatedKernel.osThreadId (threadIdFrom seed))
            id <> 0u && id <> unknownSentinel

        Check.One (propertyConfig, Prop.forAll ints property)

    [<Test>]
    let ``the extremes of the thread-id range are safe`` () =
        // The property above samples; these are the values that actually sit
        // against the two sentinels, and they are what makes the asymmetric
        // guard in `osThreadId` correct. The low end needs the `+ 1`: thread
        // id `0` is real and immediate (it is the entry thread), and would
        // otherwise mint the fatal `0`. The high end needs no guard at all,
        // and this pins why — the largest id an `int` thread id can produce is
        // `0x8000_0000`, comfortably short of the `0xFFFF_FFFF` sentinel, so
        // the upper bound is unreachable by construction rather than by a
        // check someone could later relax.
        raw (EmulatedKernel.osThreadId (ThreadId 0)) |> shouldEqual 1u

        raw (EmulatedKernel.osThreadId (ThreadId System.Int32.MaxValue))
        |> shouldEqual 0x8000_0000u

        succeeds (fun () -> EmulatedKernel.osThreadId (ThreadId System.Int32.MaxValue))
        |> shouldEqual true

    [<Test>]
    let ``the policy is injective`` () =
        // Two distinct threads must never share an id. This is the whole of the
        // uniqueness argument for the pure policy: because every thread's id is
        // a function of its `ThreadId`, and `ThreadId`s are unique and never
        // reused, injectivity here is uniqueness everywhere.
        let property (a : int, b : int) : bool =
            let x = threadIdFrom a
            let y = threadIdFrom b

            if x = y then
                true
            else
                EmulatedKernel.osThreadId x <> EmulatedKernel.osThreadId y

        Check.One (propertyConfig, Prop.forAll intPairs property)

    [<Test>]
    let ``a negative thread id fails loudly`` () =
        // There is a `FrameId -1` sentinel in this codebase and
        // `allocateParkedThread` uses it, so a negative id is a mistake someone
        // could plausibly make. `ThreadId -1` would mint `uint32 -1 + 1` — which
        // wraps to exactly the fatal `0`, the one value the `+ 1` exists to
        // avoid. Wrapping silently is precisely the failure mode this module is
        // about, so it must throw.
        let property (seed : int) : bool =
            let negative = ThreadId (-1 - abs (seed % 1_000_000))
            not (succeeds (fun () -> EmulatedKernel.osThreadId negative))

        Check.One (propertyConfig, Prop.forAll ints property)

    // --- Wiring: that the allocation sites feed the policy distinct ids ---

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
    let ``guest threads are numbered from their thread id`` () =
        let state = machine ()

        let state, first =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state

        let state, second =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        osThreadIdOf first state |> shouldEqual (OsThreadId 1u)
        osThreadIdOf second state |> shouldEqual (OsThreadId 2u)

    [<Test>]
    let ``a parked thread does consume an id, unlike a rotation slot`` () =
        // The contrast with TestCpuPlacement's
        // `an interleaved parked thread does not shift guest placements`: the
        // asymmetry is a decision, and "fixing" it must fail a test that says
        // why.
        //
        // The signal dispatcher is minted lazily, on the guest's first
        // `SystemNative_InitializeTerminalAndSignalHandling`, so a guest that
        // touches Console before spawning a worker gives that worker a
        // different id from an otherwise identical guest that did not. That is
        // fine for an id and not for a core: an id is opaque (nothing may do
        // anything with it but compare it for equality, and
        // `SystemNativeOSThreadId.cs` says so to the guest), whereas a `CpuId`
        // is drawn from a small cyclic range and compared *between* threads, so
        // a shift there changes which threads appear to share a core. Real
        // Linux shifts tids the same way: its signal-handling thread is an
        // ordinary `pthread_create`.
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

        idsFrom false
        |> shouldEqual
            [
                OsThreadId 1u
                OsThreadId 2u
                OsThreadId 3u
                OsThreadId 4u
                OsThreadId 5u
            ]

        // Shifted by exactly the one id the dispatcher took, and still all
        // distinct.
        idsFrom true
        |> shouldEqual
            [
                OsThreadId 2u
                OsThreadId 3u
                OsThreadId 4u
                OsThreadId 5u
                OsThreadId 6u
            ]

    [<Test>]
    let ``the parked dispatcher gets a real id distinct from every guest id`` () =
        // Not a placeholder, unlike its `CpuId 0`: a processor index is a
        // shared-resource key, so aliasing is meaningful there and harmless. A
        // thread id is an ownership identity, and the dispatcher runs guest
        // handler code — a handler taking a `Lock` that a guest thread already
        // holds would be waved through as a re-entrant acquire if the two
        // shared an id.
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
        raw parkedId |> shouldNotEqual 0u

    [<Test>]
    let ``every live thread has a distinct id`` () =
        // The whole point, stated end-to-end over the real allocation entry
        // points rather than over the policy in isolation: whatever mixture of
        // guest and interpreter-internal threads a run creates, no two of them
        // share an id. This is the property that survives if the minting
        // formula is ever changed, and the one `Lock` actually depends on.
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
