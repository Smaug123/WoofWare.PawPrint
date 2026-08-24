namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// What the kernel knows about a thread.
///
/// `Cpu` and `OsThreadId` used to be total fields on `ThreadState`, because a
/// `Map<ThreadId, _>` has no truthful default for an absent key: "core 0" is a
/// guess and a shared OS thread id silently breaks `System.Threading.Lock`.
/// They are now fields of a `UnixTaskState` in the kernel, and the guarantee
/// that replaces compile-time totality is that a key is never absent — one task
/// per live thread, minted at creation.
///
/// These are the rows that hold the replacement guarantee up. Without them the
/// move traded a property the compiler enforced for one nothing checks.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTaskState =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private machine () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    let private threads (state : IlMachineState) : Set<ThreadId> =
        state.ThreadState |> Map.toSeq |> Seq.map fst |> Set.ofSeq

    /// The invariant this whole change rests on.
    let private agrees (state : IlMachineState) : unit =
        EmulatedKernel.checkTaskInvariants (threads state) state.Kernel |> shouldBeEmpty

    [<Test>]
    let ``a fresh machine's tasks agree with its threads`` () : unit = agrees (machine ())

    [<Test>]
    let ``an unstarted guest thread gets a task`` () : unit =
        let state, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        agrees state

        EmulatedKernel.osThreadIdOf thread state.Kernel
        |> shouldEqual (EmulatedKernel.osThreadId thread)

        EmulatedKernel.parkedSocketWaitFor thread state.Kernel |> shouldEqual None

    [<Test>]
    let ``a parked interpreter thread gets a task too`` () : unit =
        // It runs guest code when a signal is dispatched, so it needs a real OS
        // thread id; only its core is a placeholder.
        let state, thread = machine () |> IlMachineState.allocateParkedThread

        agrees state
        EmulatedKernel.cpuOf thread state.Kernel |> shouldEqual (CpuId 0)

        EmulatedKernel.osThreadIdOf thread state.Kernel
        |> shouldEqual (EmulatedKernel.osThreadId thread)

    [<Test>]
    let ``several threads of both kinds keep the sets in step`` () : unit =
        let state = machine ()
        let state, _ = IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state
        let state, parked = IlMachineState.allocateParkedThread state
        let state, _ = IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        agrees state
        state.Kernel.Tasks.Count |> shouldEqual (Map.count state.ThreadState)

        // Distinct OS thread ids: an alias would let one thread be mistaken for
        // another as a `Lock` owner.
        let ids =
            threads state
            |> Set.toList
            |> List.map (fun t -> EmulatedKernel.osThreadIdOf t state.Kernel)

        ids |> List.distinct |> List.length |> shouldEqual ids.Length
        ids |> List.contains (EmulatedKernel.osThreadId parked) |> shouldEqual true

    [<Test>]
    let ``guest threads take successive cores in the rotation`` () : unit =
        let state = (machine ()).MapKernel (EmulatedKernel.withProcessorCount 4)

        let state, first =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state

        let state, second =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        EmulatedKernel.cpuOf first state.Kernel |> shouldEqual (CpuId 0)
        EmulatedKernel.cpuOf second state.Kernel |> shouldEqual (CpuId 1)

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    /// A frame on any concrete method: nothing reads its instructions, only that
    /// `addThread` has something to start the thread on.
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

    [<Test>]
    let ``addThread places on the rotation, not on core zero`` () : unit =
        // `addThread` has one production caller — the entry thread, always at
        // rotation 0, where `cpuForRotation 0` and `CpuId 0` agree. So its
        // placement is currently indistinguishable from the constant, and a
        // second caller would silently land on core 0. This pins the contract
        // instead: the second thread through here takes the next slot.
        let state = (machine ()).MapKernel (EmulatedKernel.withProcessorCount 4)
        let state, frame = aFrame state
        let state, first = IlMachineState.addThread frame state
        let state, second = IlMachineState.addThread frame state

        EmulatedKernel.cpuOf first state.Kernel |> shouldEqual (CpuId 0)
        EmulatedKernel.cpuOf second state.Kernel |> shouldEqual (CpuId 1)
        agrees state

    [<Test>]
    let ``a thread with no task is refused`` () : unit =
        let state, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        let stripped =
            state.MapKernel (fun kernel ->
                { kernel with
                    Tasks = Map.remove thread kernel.Tasks
                }
            )

        EmulatedKernel.checkTaskInvariants (threads stripped) stripped.Kernel
        |> shouldEqual [ EmulatedKernelDefect.ThreadWithoutTask thread ]

        let exn =
            Assert.Throws<exn> (fun () -> EmulatedKernel.cpuOf thread stripped.Kernel |> ignore<CpuId>)

        exn.Message |> shouldContainText "names no task"

    [<Test>]
    let ``a task with no thread is refused`` () : unit =
        let state = machine ()
        let ghost = ThreadId 99

        let haunted =
            state.MapKernel (EmulatedKernel.registerTask ghost (CpuId 0) (EmulatedKernel.osThreadId ghost))

        EmulatedKernel.checkTaskInvariants (threads haunted) haunted.Kernel
        |> shouldEqual [ EmulatedKernelDefect.TaskWithoutThread ghost ]

    [<Test>]
    let ``registering a thread twice is refused`` () : unit =
        // Re-registration would silently discard the first registration's core
        // and OS thread id, which is how a thread would end up aliasing another.
        let state, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        let exn =
            Assert.Throws<exn> (fun () ->
                EmulatedKernel.registerTask thread (CpuId 3) (OsThreadId 7u) state.Kernel
                |> ignore<EmulatedKernel>
            )

        exn.Message |> shouldContainText "already names a task"

    [<Test>]
    let ``a park and its release round-trip`` () : unit =
        // On a machine with several cores, and on the *second* thread, so that
        // the task under test is not on core 0: a fixture whose thread already
        // sits there cannot tell "parking left the core alone" from "parking
        // reset it to zero".
        let state = (machine ()).MapKernel (EmulatedKernel.withProcessorCount 4)
        let state, _ = IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state

        let state, thread =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        EmulatedKernel.cpuOf thread state.Kernel |> shouldEqual (CpuId 1)

        let wait : ParkedSocketWait =
            {
                Port = OpenFileDescriptionId 5L
                MaxEvents = 8
            }

        let parked =
            state.MapKernel (EmulatedKernel.withParkedSocketWait thread (Some wait))

        EmulatedKernel.parkedSocketWaitFor thread parked.Kernel
        |> shouldEqual (Some wait)

        agrees parked

        let released = parked.MapKernel (EmulatedKernel.withParkedSocketWait thread None)
        EmulatedKernel.parkedSocketWaitFor thread released.Kernel |> shouldEqual None

        // Parking must not disturb the rest of the task.
        EmulatedKernel.cpuOf thread released.Kernel
        |> shouldEqual (EmulatedKernel.cpuOf thread state.Kernel)

        EmulatedKernel.osThreadIdOf thread released.Kernel
        |> shouldEqual (EmulatedKernel.osThreadIdOf thread state.Kernel)
