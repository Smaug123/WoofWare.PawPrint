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

    let private threads (state : IlMachineState) : Map<ThreadId, ThreadStatus> =
        state.ThreadState |> Map.map (fun _ ts -> ts.Status)

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

        UnixTaskTable.osThreadIdOf thread state.Kernel.Tasks
        |> shouldEqual (EmulatedKernel.osThreadId thread)

        UnixTaskTable.parkedFor thread state.Kernel.Tasks |> shouldEqual None

    [<Test>]
    let ``a parked interpreter thread gets a task too`` () : unit =
        // It runs guest code when a signal is dispatched, so it needs a real OS
        // thread id; only its core is a placeholder.
        let state, thread = machine () |> IlMachineState.allocateParkedThread

        agrees state
        UnixTaskTable.cpuOf thread state.Kernel.Tasks |> shouldEqual (CpuId 0)

        UnixTaskTable.osThreadIdOf thread state.Kernel.Tasks
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
            |> Map.toList
            |> List.map (fun (t, _) -> UnixTaskTable.osThreadIdOf t state.Kernel.Tasks)

        ids |> List.distinct |> List.length |> shouldEqual ids.Length
        ids |> List.contains (EmulatedKernel.osThreadId parked) |> shouldEqual true

    [<Test>]
    let ``guest threads take successive cores in the rotation`` () : unit =
        let state =
            (machine ()).MapKernel (EmulatedKernel.mapMachine (UnixMachineState.withProcessorCount 4))

        let state, first =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state

        let state, second =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        UnixTaskTable.cpuOf first state.Kernel.Tasks |> shouldEqual (CpuId 0)
        UnixTaskTable.cpuOf second state.Kernel.Tasks |> shouldEqual (CpuId 1)

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
        let state =
            (machine ()).MapKernel (EmulatedKernel.mapMachine (UnixMachineState.withProcessorCount 4))

        let state, frame = aFrame state
        let state, first = IlMachineState.addThread frame state
        let state, second = IlMachineState.addThread frame state

        UnixTaskTable.cpuOf first state.Kernel.Tasks |> shouldEqual (CpuId 0)
        UnixTaskTable.cpuOf second state.Kernel.Tasks |> shouldEqual (CpuId 1)
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
            Assert.Throws<exn> (fun () -> UnixTaskTable.cpuOf thread stripped.Kernel.Tasks |> ignore<CpuId>)

        exn.Message |> shouldContainText "names no task"

    [<Test>]
    let ``a task with no thread is refused`` () : unit =
        let state = machine ()
        let ghost = ThreadId 99

        let haunted =
            state.MapKernel (
                EmulatedKernel.mapTasks (UnixTaskTable.register ghost (CpuId 0) (EmulatedKernel.osThreadId ghost))
            )

        EmulatedKernel.checkTaskInvariants (threads haunted) haunted.Kernel
        |> shouldEqual [ EmulatedKernelDefect.TaskWithoutThread ghost ]

    let private aLock : ParkedSyscall =
        ParkedSyscall.Flock
            {
                ParkedFlock.Requester = OpenFileDescriptionId 3L
                Mode = FlockMode.Exclusive
            }

    let private aWait : ParkedSyscall =
        ParkedSyscall.SocketWait
            {
                ParkedSocketWait.Port = OpenFileDescriptionId 3L
                MaxEvents = 8
            }

    /// Every kind of park, so that the rows below say the invariant is about *whether* a thread
    /// is parked rather than about which syscall it is parked in. One record field and one park
    /// status are exactly what let one statement of the rule cover every parking syscall, and a
    /// row per kind is what would otherwise have to be written again for a fifth.
    let private parks : ParkedSyscall list = [ aLock ; aWait ]

    /// A thread with a task, and `parked` written on it.
    let private threadParkedIn (parked : ParkedSyscall) : IlMachineState * ThreadId =
        let state, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        state.MapKernel (EmulatedKernel.mapTasks (UnixTaskTable.withParked thread (Some parked))), thread

    [<Test>]
    let ``a syscall waiter with no record is refused`` () : unit =
        // The status says the thread is asleep in a syscall; the record says which, and in what.
        // A thread with the one and not the other is a state nothing can act on: no sweep can
        // decide whether to wake it, and no re-entered handler could decide what to finish.
        let state, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        let statuses = threads state |> Map.add thread ThreadStatus.BlockedInSyscall

        EmulatedKernel.checkTaskInvariants statuses state.Kernel
        |> shouldEqual [ EmulatedKernelDefect.SyscallWaiterWithoutRecord thread ]

    [<TestCaseSource(nameof parks)>]
    let ``a park record on a thread that cannot be waiting is refused`` (parked : ParkedSyscall) : unit =
        let recorded, thread = threadParkedIn parked
        let statuses = threads recorded |> Map.add thread ThreadStatus.Terminated

        EmulatedKernel.checkTaskInvariants statuses recorded.Kernel
        |> shouldEqual
            [
                EmulatedKernelDefect.SyscallRecordWithoutWaiter (thread, ThreadStatus.Terminated)
            ]

    [<TestCaseSource(nameof parks)>]
    let ``a woken waiter keeps its record`` (parked : ParkedSyscall) : unit =
        // Not slack in the invariant, but the window it exists to permit: between the sweep
        // flipping a waiter to Runnable and the woken thread re-entering its handler, the record
        // must still be there -- it is what tells the re-entry that it is a re-entry, and what
        // says what to finish against.
        let recorded, thread = threadParkedIn parked
        let statuses = threads recorded |> Map.add thread ThreadStatus.Runnable

        EmulatedKernel.checkTaskInvariants statuses recorded.Kernel |> shouldBeEmpty

    [<TestCaseSource(nameof parks)>]
    let ``a parked waiter agrees with its record`` (parked : ParkedSyscall) : unit =
        let recorded, thread = threadParkedIn parked
        let statuses = threads recorded |> Map.add thread ThreadStatus.BlockedInSyscall

        EmulatedKernel.checkTaskInvariants statuses recorded.Kernel |> shouldBeEmpty

    [<Test>]
    let ``parking over another syscall's park is refused`` () : unit =
        // A task blocks in one syscall at a time, and no completion may leave its record behind.
        // Two independent optional fields let a forgotten clear be *found* -- both set at once is
        // a state the invariant reports -- but one field would instead let the next park silently
        // overwrite it, which is why the write refuses rather than the check catching it later.
        // `checkTaskInvariants` is a test-time oracle; nothing in the driver loop runs it, so this
        // is the only place a live run is told.
        let parked, thread = threadParkedIn aWait

        let exn =
            Assert.Throws<exn> (fun () ->
                parked.MapKernel (EmulatedKernel.mapTasks (UnixTaskTable.withParked thread (Some aLock)))
                |> ignore<IlMachineState>
            )

        exn.Message |> shouldContainText "blocks in one syscall at a time"

    [<TestCaseSource(nameof parks)>]
    let ``re-parking in the same syscall is allowed`` (parked : ParkedSyscall) : unit =
        // The lawful overwrite, and the reason the refusal above is by kind rather than by
        // equality: a beaten `flock` waiter re-parks on the same condition, and a socket waiter
        // whose port was drained before it ran parks again on the same port.
        let state, thread = threadParkedIn parked

        state.MapKernel (EmulatedKernel.mapTasks (UnixTaskTable.withParked thread (Some parked)))
        |> fun state -> UnixTaskTable.parkedFor thread state.Kernel.Tasks
        |> shouldEqual (Some parked)

    [<TestCaseSource(nameof parks)>]
    let ``clearing a park lets the other syscall park`` (parked : ParkedSyscall) : unit =
        // The refusal is about an *unclosed* park, not about a task's history: a completion that
        // clears its record leaves the task free to block in anything.
        let state, thread = threadParkedIn parked

        let other = parks |> List.find (fun p -> p <> parked)

        state.MapKernel (EmulatedKernel.mapTasks (UnixTaskTable.withParked thread None))
        |> fun state -> state.MapKernel (EmulatedKernel.mapTasks (UnixTaskTable.withParked thread (Some other)))
        |> fun state -> UnixTaskTable.parkedFor thread state.Kernel.Tasks
        |> shouldEqual (Some other)

    [<Test>]
    let ``registering a thread twice is refused`` () : unit =
        // Re-registration would silently discard the first registration's core
        // and OS thread id, which is how a thread would end up aliasing another.
        let state, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        let exn =
            Assert.Throws<exn> (fun () ->
                EmulatedKernel.mapTasks (UnixTaskTable.register thread (CpuId 3) (OsThreadId 7u)) state.Kernel
                |> ignore<EmulatedKernel>
            )

        exn.Message |> shouldContainText "already names a task"

    [<Test>]
    let ``waking a thread that is not parked in a syscall is refused`` () : unit =
        // The sweep observes a thread parked and then wakes it, so a thread that is no longer
        // parked by the time it is woken means the sweep raced its own observation — and waking
        // it anyway would set a thread Runnable that some *other* mechanism had meanwhile put to
        // sleep, losing that wait with nothing to say so.
        let state, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        let exn =
            Assert.Throws<exn> (fun () -> Scheduler.wakeFromSyscall thread state |> ignore<IlMachineState>)

        exn.Message |> shouldContainText "is not parked in a syscall"

    [<Test>]
    let ``a park and its release round-trip`` () : unit =
        // On a machine with several cores, and on the *second* thread, so that
        // the task under test is not on core 0: a fixture whose thread already
        // sits there cannot tell "parking left the core alone" from "parking
        // reset it to zero".
        let state =
            (machine ()).MapKernel (EmulatedKernel.mapMachine (UnixMachineState.withProcessorCount 4))

        let state, _ = IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state

        let state, thread =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        UnixTaskTable.cpuOf thread state.Kernel.Tasks |> shouldEqual (CpuId 1)

        let wait : ParkedSocketWait =
            {
                Port = OpenFileDescriptionId 5L
                MaxEvents = 8
            }

        // The status goes with the record, because a park writes both and `checkTaskInvariants`
        // refuses either alone: a record on a thread that has not started is a state no wait
        // can have produced.
        let parked =
            state.MapKernel (
                EmulatedKernel.mapTasks (UnixTaskTable.withParked thread (Some (ParkedSyscall.SocketWait wait)))
            )
            |> Scheduler.parkInSyscall thread

        UnixTaskTable.parkedFor thread parked.Kernel.Tasks
        |> shouldEqual (Some (ParkedSyscall.SocketWait wait))

        agrees parked

        // Woken first and released second, which is the order a real wake takes: the sweep flips
        // the status and the record stands until the re-entered handler has finished with it.
        // Both halves of that sequence are states the invariant permits.
        let woken = Scheduler.wakeFromSyscall thread parked

        agrees woken

        let released =
            woken.MapKernel (EmulatedKernel.mapTasks (UnixTaskTable.withParked thread None))

        UnixTaskTable.parkedFor thread released.Kernel.Tasks |> shouldEqual None

        agrees released

        // Parking must not disturb the rest of the task.
        UnixTaskTable.cpuOf thread released.Kernel.Tasks
        |> shouldEqual (UnixTaskTable.cpuOf thread state.Kernel.Tasks)

        UnixTaskTable.osThreadIdOf thread released.Kernel.Tasks
        |> shouldEqual (UnixTaskTable.osThreadIdOf thread state.Kernel.Tasks)
