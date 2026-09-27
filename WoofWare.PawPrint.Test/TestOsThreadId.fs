namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The OS thread id a guest reads through `SystemNative_TryGetUInt32OSThreadId`
/// (Linux CoreLib) and `SystemNative_GetUInt64OSThreadId` (macOS CoreLib), which
/// `System.Threading.Lock` uses as its owner identity.
///
/// The kernel mints the id (`UnixTaskLifecycle.spawn`) and `OsThreadIdPal` projects
/// it to the shim's two widths. Two things matter to a guest. No two live threads
/// may share an id, because `Lock` reads a matching id as the same thread
/// re-entering. And the numbers are exactly what PawPrint reported when it
/// numbered threads itself, `pid + ThreadId`, since a guest can print them: the
/// property `the kernel numbers threads as pid plus ThreadId` below is the oracle
/// for that.
///
/// `TestCpuPlacement` covers the sibling policy (`cpuForRotation`), which
/// deliberately keys off a *different* cursor; the contrast is the subject of
/// `a parked thread does consume an id, unlike a rotation slot` below.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOsThreadId =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        BaseClassTypes.ofCorelib corelib

    /// A frame on any concrete method, for `addThread` to start the entry thread on.
    let private aFrame (state : IlMachineState) : IlMachineState * MethodState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let objectToString =
            baseClassTypes.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && (MethodInfo.arity method = 0))

        let state, signature =
            IlMachineState.concretizeMethodSignature
                loggerFactory
                baseClassTypes
                state
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                objectToString.Signature

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il (MethodInstructions.onlyRet ())) signature

        match
            MethodState.Empty
                state.ConcreteTypes
                baseClassTypes
                state._LoadedAssemblies
                corelib
                method
                ImmutableArray.Empty
                (ImmutableArray.Create (CliType.ObjectRef None))
                None
        with
        | Ok methodState -> state, methodState
        | Error missing -> failwith $"unexpected missing assembly references creating frame: %O{missing}"

    /// A machine on `config`'s kernel, as `Program` builds one, with its entry thread.
    let private machineOn (config : KernelConfig) : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state =
            (IlMachineState.initial loggerFactory ImmutableArray.Empty corelib)
                .MapKernel (fun _ -> KernelConfig.toKernel config)

        let state, frame = aFrame state
        let state, entry = IlMachineState.addThread frame state
        entry |> shouldEqual (ThreadId 0)
        state

    let private machine () : IlMachineState = machineOn KernelConfig.Default

    let private idOf (thread : ThreadId) (state : IlMachineState) : uint64 =
        OsThreadId.toUInt64 (UnixTaskTable.osThreadIdOf thread state.Kernel.Tasks)

    // --- What the shim reports of an id ---

    /// An id the kernel minted: a Darwin leader's, which may be any of them.
    let private darwinLeaderId (id : uint64) : OsThreadId =
        let kernel =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    LeaderThreadId = Some id
                }

        UnixTaskTable.osThreadIdOf kernel.Leader kernel.Tasks

    [<Test>]
    let ``the 64-bit shim reports the id whole, and the 32-bit one its low half with 0 as the sentinel`` () =
        // `SystemNative_TryGetUInt32OSThreadId` returns `(uint32_t)-1` for an id whose
        // low half is 0, because that is how it says "cannot determine"; CoreLib's
        // `Lock` would then use the managed thread id instead.
        let expected32 (id : uint64) : uint32 =
            if id &&& 0xFFFF_FFFFUL = 0UL then
                0xFFFF_FFFFu
            else
                uint32 (id &&& 0xFFFF_FFFFUL)

        let property (id : uint64) : bool =
            let minted = darwinLeaderId id

            OsThreadIdPal.getUInt64 minted = id
            && OsThreadIdPal.tryGetUInt32 minted = expected32 id

        for id in
            [
                1UL
                4242UL
                0xFFFF_FFFEUL
                0xFFFF_FFFFUL
                0x1_0000_0000UL
                0x1_0000_0007UL
            ] do
            property id |> shouldEqual true

        OsThreadIdPal.tryGetUInt32 (darwinLeaderId 0x1_0000_0000UL)
        |> shouldEqual 0xFFFF_FFFFu

        let ids =
            Gen.oneof
                [
                    Gen.choose (1, System.Int32.MaxValue) |> Gen.map uint64
                    ArbMap.defaults
                    |> ArbMap.generate<uint64>
                    |> Gen.map (fun i -> max 1UL (min i (System.UInt64.MaxValue - 1UL)))
                    ArbMap.defaults
                    |> ArbMap.generate<uint32>
                    |> Gen.map (fun high -> (uint64 (max high 1u) <<< 32))
                ]

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen ids) property)

    // --- Old numbering against new ---

    /// What a run does to its threads, in order.
    [<RequireQualifiedAccess>]
    type private Op =
        /// A guest constructs a `Thread`, on the `parent`th live thread.
        | Construct of parent : int
        /// The signal dispatcher is created, by the `parent`th live thread.
        | Dispatcher of parent : int
        /// The `thread`th live thread other than the entry thread terminates.
        | Terminate of thread : int

    [<Test>]
    let ``the kernel numbers threads as pid plus ThreadId, and the shim reports what it did`` () =
        // The oracle: before the kernel minted ids, PawPrint gave `ThreadId i` of
        // process `pid` the id `pid + i`, reported whole by both shim entry points.
        // Registration order is creation order and no id comes back before
        // `pid_max`, so the kernel's counter must agree on Linux; and on Darwin the
        // counter starts at the pid by default, so it must agree there too. Runs are
        // short enough, and the pid far enough below Linux's default `pid_max`,
        // that no id wraps.
        let run (platform : SimulatedUnixPlatform, pid : int32, ops : Op list) : unit =
            let state =
                machineOn
                    { KernelConfig.Default with
                        UnixPlatform = platform
                        ProcessId = ProcessId.parseOrFail "test" pid
                    }

            let live (state : IlMachineState) : ThreadId list =
                state.Kernel.Tasks |> Map.keys |> List.ofSeq

            let step (state : IlMachineState, address : int) (op : Op) =
                match op with
                | Op.Construct parent ->
                    let parents = live state
                    let parent = parents.[parent % parents.Length]

                    IlMachineState.allocateUnstartedThread parent (ManagedHeapAddress address) state
                    |> fst,
                    address + 1
                | Op.Dispatcher parent ->
                    let parents = live state
                    let parent = parents.[parent % parents.Length]
                    IlMachineState.allocateParkedThread parent state |> fst, address
                | Op.Terminate thread ->
                    match live state |> List.filter (fun t -> t <> state.Kernel.Leader) with
                    | [] -> state, address
                    | others -> Scheduler.onThreadTerminated others.[thread % others.Length] state, address

            let check (state : IlMachineState) : unit =
                for thread in live state do
                    let (ThreadId i) = thread
                    let old = uint32 pid + uint32 i
                    let id = UnixTaskTable.osThreadIdOf thread state.Kernel.Tasks

                    (thread, OsThreadIdPal.tryGetUInt32 id) |> shouldEqual (thread, old)
                    (thread, OsThreadIdPal.getUInt64 id) |> shouldEqual (thread, uint64 old)

                EmulatedKernel.checkInvariants state.Kernel |> shouldEqual []

            check state

            ((state, 1), ops)
            ||> List.fold (fun acc op ->
                let state, address = step acc op
                check state
                state, address
            )
            |> ignore

        let gen =
            gen {
                let! platform =
                    Gen.elements
                        [
                            SimulatedUnixPlatform.linuxX64
                            SimulatedUnixPlatform.linuxArm64
                            SimulatedUnixPlatform.macOsArm64
                        ]

                let! length = Gen.choose (0, 40)

                let! pid =
                    Gen.oneof
                        [
                            Gen.choose (1, 100)
                            Gen.choose (1, UnixSystem.defaultPidMax - 1 - length)
                            Gen.constant (UnixSystem.defaultPidMax - 1 - length)
                        ]

                let! ops =
                    Gen.frequency
                        [
                            5, Gen.choose (0, 20) |> Gen.map Op.Construct
                            1, Gen.choose (0, 20) |> Gen.map Op.Dispatcher
                            3, Gen.choose (0, 20) |> Gen.map Op.Terminate
                        ]
                    |> Gen.listOfLength length

                return platform, pid, ops
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 100, Prop.forAll (Arb.fromGen gen) run)

    // --- Wiring ---

    [<Test>]
    let ``guest threads take the ids after the entry thread's`` () =
        let state = machine ()
        idOf (ThreadId 0) state |> shouldEqual 4242UL

        let state, first =
            IlMachineState.allocateUnstartedThread (ThreadId 0) (ManagedHeapAddress 1) state

        let state, second =
            IlMachineState.allocateUnstartedThread first (ManagedHeapAddress 2) state

        idOf first state |> shouldEqual 4243UL
        idOf second state |> shouldEqual 4244UL

    [<Test>]
    let ``a Darwin configuration can start the counter away from the pid`` () =
        let state =
            machineOn
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    LeaderThreadId = Some 2897490UL
                }

        idOf (ThreadId 0) state |> shouldEqual 2897490UL

        UnixSystem.processId (EmulatedKernel.unix state.Kernel)
        |> shouldEqual UnixSystem.defaultProcessId

        let state, first =
            IlMachineState.allocateUnstartedThread (ThreadId 0) (ManagedHeapAddress 1) state

        idOf first state |> shouldEqual 2897491UL

    [<Test>]
    let ``the thread-id knobs refuse the other flavour`` () =
        let linuxLeader () =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    LeaderThreadId = Some 7UL
                }
            |> ignore<EmulatedKernel>

        (Assert.Throws<exn> (TestDelegate linuxLeader)).Message
        |> shouldContainText "KernelConfig.LeaderThreadId"

        let darwinPidMax () =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    PidMax = Some 1000
                }
            |> ignore<EmulatedKernel>

        (Assert.Throws<exn> (TestDelegate darwinPidMax)).Message
        |> shouldContainText "KernelConfig.PidMax"

    [<Test>]
    let ``a configured pid_max wraps the ids`` () =
        let state =
            machineOn
                { KernelConfig.Default with
                    ProcessId = ProcessId.parseOrFail "test" 998
                    PidMax = Some 1000
                }

        let state, first =
            IlMachineState.allocateUnstartedThread (ThreadId 0) (ManagedHeapAddress 1) state

        let state, second =
            IlMachineState.allocateUnstartedThread (ThreadId 0) (ManagedHeapAddress 2) state

        (idOf first state, idOf second state) |> shouldEqual (999UL, 300UL)

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
        let idsFrom (allocateParkedFirst : bool) : uint64 list =
            let mutable state = machine ()

            if allocateParkedFirst then
                let state', _ = IlMachineState.allocateParkedThread (ThreadId 0) state
                state <- state'

            [ 1..5 ]
            |> List.map (fun i ->
                let state', thread =
                    IlMachineState.allocateUnstartedThread (ThreadId 0) (ManagedHeapAddress i) state

                state <- state'
                idOf thread state
            )

        idsFrom false |> shouldEqual [ 4243UL .. 4247UL ]

        // Shifted by exactly the one id the dispatcher took, and still all
        // distinct.
        idsFrom true |> shouldEqual [ 4244UL .. 4248UL ]

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
                    IlMachineState.allocateUnstartedThread (ThreadId 0) (ManagedHeapAddress i) state

                state <- state'
                idOf thread state
            )

        let state', parked = IlMachineState.allocateParkedThread (ThreadId 0) state
        state <- state'

        let parkedId = idOf parked state

        (idOf (ThreadId 0) state :: guestIds)
        |> List.contains parkedId
        |> shouldEqual false
