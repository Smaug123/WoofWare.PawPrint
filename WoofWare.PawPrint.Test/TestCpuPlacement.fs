namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedKernel.cpuForRotation` is the placement policy behind
/// `SystemNative_SchedGetCpu`: it decides which simulated logical processor the
/// n-th guest-visible thread is pinned to. It is the only producer of `CpuId`
/// for threads a guest can observe (`allocateParkedThread` mints a fixed core 0
/// for PawPrint-internal threads no guest can name), so the invariant "every
/// `CpuId` a guest can read names a processor it also counts through
/// `Environment.ProcessorCount`" is established here and nowhere else — which
/// makes it worth establishing by property rather than by example.
///
/// `TestEffectiveProcessorCount` covers how the count itself is resolved; this
/// module takes that as given and covers the rotation over it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCpuPlacement =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// The largest count `withProcessorCount` will accept is unbounded above,
    /// but generating enormous ones only slows the properties down without
    /// exercising anything new, so fold into a workable band.
    let private countFrom (seed : int) : int = 1 + (abs (seed % 64))

    /// Non-negative rotation cursor, i.e. exactly the values
    /// `IlMachineState.NextGuestThreadOrdinal` can hold.
    let private rotationFrom (seed : int) : int = abs (seed % 100_000)

    let private kernelWith (count : int) : EmulatedKernel =
        EmulatedKernel.initial |> EmulatedKernel.withProcessorCount count

    let private cpuIndex (CpuId.CpuId i : CpuId) : int = i

    let private ints = ArbMap.defaults |> ArbMap.arbitrary<int>
    let private intPairs = ArbMap.defaults |> ArbMap.arbitrary<int * int>

    [<Test>]
    let ``the default kernel places every thread on cpu 0`` () =
        // `defaultProcessorCount` is 1, so this is the replay contract every
        // existing run observes: change it and every recorded trace that reads
        // `Thread.GetCurrentProcessorId()` changes with it.
        let property (rotationSeed : int) : bool =
            EmulatedKernel.cpuForRotation (rotationFrom rotationSeed) EmulatedKernel.initial = CpuId 0

        Check.One (propertyConfig, Prop.forAll ints property)

    [<Test>]
    let ``every placement names a processor the guest counts`` () =
        // The load-bearing invariant. BCL callers index per-CPU shards sized off
        // `Environment.ProcessorCount` with this value
        // (`SharedArrayPool`, `TimerQueue.Instances`), so a placement outside
        // the range would be an out-of-bounds shard index in guest code.
        let property (countSeed : int, rotationSeed : int) : bool =
            let count = countFrom countSeed
            let kernel = kernelWith count

            let cpu =
                cpuIndex (EmulatedKernel.cpuForRotation (rotationFrom rotationSeed) kernel)

            cpu >= 0 && cpu < EmulatedKernel.effectiveProcessorCount kernel

        Check.One (propertyConfig, Prop.forAll intPairs property)

    [<Test>]
    let ``the first N threads cover every processor exactly once`` () =
        // Round-robin rather than, say, hashing: with N cores and N threads no
        // core is doubled up and none is left idle. This is what makes a
        // host-configured `ProcessorCount` actually exercise the guest's
        // multi-shard code paths.
        let property (countSeed : int) : bool =
            let count = countFrom countSeed
            let kernel = kernelWith count

            let placements =
                List.init count (fun rotation -> cpuIndex (EmulatedKernel.cpuForRotation rotation kernel))

            List.sort placements = List.init count id

        Check.One (propertyConfig, Prop.forAll ints property)

    [<Test>]
    let ``placement is periodic in the processor count`` () =
        // Threads beyond the N-th wrap round rather than running off the end,
        // so an unbounded number of guest threads still produces bounded,
        // in-range placements.
        let property (countSeed : int, rotationSeed : int) : bool =
            let count = countFrom countSeed
            let kernel = kernelWith count
            let rotation = rotationFrom rotationSeed

            EmulatedKernel.cpuForRotation rotation kernel = EmulatedKernel.cpuForRotation (rotation + count) kernel

        Check.One (propertyConfig, Prop.forAll intPairs property)

    [<Test>]
    let ``the environment knob moves placement, not just the reported count`` () =
        // `DOTNET_PROCESSOR_COUNT` overrides `KernelConfig.ProcessorCount` for
        // `Environment.ProcessorCount`, and placement has to follow it: a guest
        // that shards by the count it observes must not be handed CPU indices
        // drawn from a different count.
        let property (countSeed : int, rotationSeed : int) : bool =
            let configured = countFrom countSeed

            let kernel =
                EmulatedKernel.initial
                |> EmulatedKernel.withProcessorCount 1
                |> EmulatedKernel.withEnvironment (Map.ofList [ "DOTNET_PROCESSOR_COUNT", string<int> configured ])

            let cpu =
                cpuIndex (EmulatedKernel.cpuForRotation (rotationFrom rotationSeed) kernel)

            cpu >= 0 && cpu < configured

        Check.One (propertyConfig, Prop.forAll intPairs property)

    /// Did the thunk complete, rather than failing the way PawPrint reports a
    /// violated kernel invariant?
    let private succeeds (f : unit -> 'a) : bool =
        try
            f () |> ignore<'a>
            true
        with _ ->
            false

    [<Test>]
    let ``a negative rotation is rejected`` () =
        // The cursor counts threads created so far, so a negative value means a
        // caller has corrupted it. Fail loudly rather than returning F#'s
        // negative remainder, which would be an out-of-range shard index in the
        // guest.
        let property (seed : int) : bool =
            let rotation = rotationFrom seed

            succeeds (fun () -> EmulatedKernel.cpuForRotation rotation EmulatedKernel.initial)
            && not (succeeds (fun () -> EmulatedKernel.cpuForRotation (-1 - rotation) EmulatedKernel.initial))

        Check.One (propertyConfig, Prop.forAll ints property)

    [<Test>]
    let ``a record-copied non-positive processor count is rejected`` () =
        // `withProcessorCount` guards construction, but record-copy bypasses it
        // and `rotation % 0` would divide by zero while `rotation % -n` would
        // yield a negative shard index. Assert at the point of use, sweeping
        // the whole illegal range rather than pinning the single zero case.
        let property (countSeed : int, rotationSeed : int) : bool =
            let count = -(abs (countSeed % 64))

            let kernel =
                { EmulatedKernel.initial with
                    ProcessorCount = count
                }

            not (succeeds (fun () -> EmulatedKernel.cpuForRotation (rotationFrom rotationSeed) kernel))

        Check.One (propertyConfig, Prop.forAll intPairs property)

    // --- Cursor bookkeeping ---
    //
    // The properties above cover the pure placement function. These cover the
    // wiring: which thread-creation entry points advance
    // `IlMachineState.NextGuestThreadOrdinal`, and by how much. That is the part of
    // this feature most likely to be got wrong by a later change, and pinning
    // it here gives a far clearer failure than a numbered return code from the
    // end-to-end `SchedGetCpuPlacement.cs`.

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    /// A machine whose kernel reports `count` processors, with no threads yet.
    let private machineWith (count : int) : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib
        |> fun state -> state.MapKernel (EmulatedKernel.withProcessorCount count)

    let private cpuOf (thread : ThreadId) (state : IlMachineState) : CpuId = state.ThreadState.[thread].Cpu

    [<Test>]
    let ``a fresh machine starts its rotation at zero`` () =
        (machineWith 4).NextGuestThreadOrdinal |> shouldEqual 0

    [<Test>]
    let ``allocateUnstartedThread advances the rotation by exactly one`` () =
        // And places the thread using the *pre*-increment cursor, so the first
        // guest thread created lands on core 0 rather than core 1.
        let state = machineWith 4

        let state, first =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state

        state.NextGuestThreadOrdinal |> shouldEqual 1
        cpuOf first state |> shouldEqual (CpuId 0)

        let state, second =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        state.NextGuestThreadOrdinal |> shouldEqual 2
        cpuOf second state |> shouldEqual (CpuId 1)

    [<Test>]
    let ``allocateParkedThread leaves the rotation untouched`` () =
        // The load-bearing half: a PawPrint-internal auxiliary thread must not
        // consume a rotation slot, or the interpreter's own bookkeeping would
        // shift which core guest threads observe. It still gets a placement,
        // because the field is total — a fixed core 0.
        let state = machineWith 4

        let state, parked = IlMachineState.allocateParkedThread state

        state.NextGuestThreadOrdinal |> shouldEqual 0
        cpuOf parked state |> shouldEqual (CpuId 0)

    [<Test>]
    let ``an interleaved parked thread does not shift guest placements`` () =
        // Stated as an equivalence rather than as absolute expected values:
        // whatever the guest threads would have observed without the
        // interpreter-internal thread, they observe with it too. This is the
        // property that would break if placement were ever keyed off
        // `NextThreadId`, which `allocateParkedThread` *does* consume.
        let placementsWithParked =
            let mutable state = machineWith 3

            let state', _ = IlMachineState.allocateParkedThread state
            state <- state'

            [ 1..5 ]
            |> List.map (fun i ->
                let state', thread =
                    IlMachineState.allocateUnstartedThread (ManagedHeapAddress i) state

                state <- state'
                cpuOf thread state
            )

        let placementsWithout =
            let mutable state = machineWith 3

            [ 1..5 ]
            |> List.map (fun i ->
                let state', thread =
                    IlMachineState.allocateUnstartedThread (ManagedHeapAddress i) state

                state <- state'
                cpuOf thread state
            )

        placementsWithParked |> shouldEqual placementsWithout

        // ...and they really are the round-robin we expect, so the equivalence
        // above is not two identical constants.
        placementsWithout
        |> shouldEqual [ CpuId 0 ; CpuId 1 ; CpuId 2 ; CpuId 0 ; CpuId 1 ]

    [<Test>]
    let ``placement is a pure function of the cursor and the count`` () =
        // Nothing else about the kernel — the clock, the PRNG streams, the
        // output log — may influence it, or replay would depend on when a
        // thread happened to be created rather than on how many preceded it.
        let property (countSeed : int, rotationSeed : int) : bool =
            let count = countFrom countSeed
            let rotation = rotationFrom rotationSeed

            let plain = kernelWith count

            let busy =
                { plain with
                    VirtualClockMs = 1234L
                    StepCounter = 5678L
                    NonCryptoRandomState = 0xDEADBEEFUL
                }

            EmulatedKernel.cpuForRotation rotation plain = EmulatedKernel.cpuForRotation rotation busy

        Check.One (propertyConfig, Prop.forAll intPairs property)
