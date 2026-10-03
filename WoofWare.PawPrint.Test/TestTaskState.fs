namespace WoofWare.PawPrint.Test

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
/// per thread with an OS thread, minted when the thread is started.
///
/// These are the rows that hold the replacement guarantee up. Without them the
/// move traded a property the compiler enforced for one nothing checks.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTaskState =

    /// A machine before `addThread` has given the leader its thread.
    let private bare () : IlMachineState = ThreadFixtures.bare ()

    let private aFrame (state : IlMachineState) : IlMachineState * MethodState = ThreadFixtures.aFrame state

    /// A machine whose leader has its thread, as `Program` leaves it before `Main` runs.
    let private machine () : IlMachineState =
        let state, frame = aFrame (bare ())
        IlMachineState.addThread frame state |> fst

    /// `bare`, on a machine of four processors.
    let private bareOnFour () : IlMachineState =
        (bare ())
            .MapKernel (fun _ ->
                EmulatedKernel.initialImage
                |> UnixBootImage.withProcessorCount 4
                |> EmulatedKernel.boot
            )

    /// `machine`, on a machine of four processors.
    let private machineOnFour () : IlMachineState =
        let state, frame = aFrame (bareOnFour ())
        IlMachineState.addThread frame state |> fst

    let private threads (state : IlMachineState) : Map<ThreadId, ThreadStatus> =
        state.ThreadState |> Map.map (fun _ ts -> ts.Status)

    /// The invariant this whole change rests on.
    let private agrees (state : IlMachineState) : unit =
        EmulatedKernel.checkTaskInvariants (threads state) state.Kernel |> shouldBeEmpty

    [<Test>]
    let ``a fresh machine's one task is the leader, whose thread addThread makes`` () : unit =
        let state = bare ()
        state.Kernel.Leader |> shouldEqual (ThreadId 0)
        state.Kernel.Tasks |> Map.keys |> List.ofSeq |> shouldEqual [ ThreadId 0 ]

        UnixTaskTable.osThreadIdOf (ThreadId 0) state.Kernel.Tasks
        |> OsThreadId.toUInt64
        |> shouldEqual (uint64 (ProcessId.toInt32 state.Kernel.Process.ProcessId))

        EmulatedKernel.checkTaskInvariants (threads state) state.Kernel
        |> shouldEqual [ EmulatedKernelDefect.TaskWithoutThread (ThreadId 0) ]

        let state, frame = aFrame state
        let state, thread = IlMachineState.addThread frame state
        thread |> shouldEqual (ThreadId 0)
        agrees state

    [<Test>]
    let ``a guest thread gets its task when it is started, not when it is constructed`` () : unit =
        let constructed, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        agrees constructed
        thread |> shouldEqual (ThreadId 1)
        Map.containsKey thread constructed.Kernel.Tasks |> shouldEqual false

        let state = ThreadFixtures.start (ThreadId 0) thread constructed
        agrees state

        // The id after the leader's, which is the process ID.
        UnixTaskTable.osThreadIdOf thread state.Kernel.Tasks
        |> OsThreadId.toUInt64
        |> shouldEqual 4243UL

        UnixTaskTable.parkedFor thread state.Kernel.Tasks |> shouldEqual None

    [<Test>]
    let ``a task for a thread that has not been started is refused`` () : unit =
        // A real process has no task for a `Thread` it has constructed and not started:
        // CoreCLR creates the OS thread in `Start`. A task here would also spend an id that
        // the next thread to start should have had.
        let state, thread =
            machine () |> IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1)

        let haunted = state.MapKernel (KernelTasks.ensure thread)

        EmulatedKernel.checkTaskInvariants (threads haunted) haunted.Kernel
        |> shouldEqual [ EmulatedKernelDefect.TaskWithoutThread thread ]

    [<Test>]
    let ``the thread that starts a thread creates its task`` () : unit =
        // The kernel's `clone` is made by the thread calling `Start`, so the new task inherits
        // that thread's signal mask. A starter inside a signal handler is refused, because a
        // mask is held only as handler frames here; so the refusal names the starter, and
        // another thread can start a thread meanwhile.
        let state, starter =
            machine ()
            |> ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1)

        let state, first =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        let state, second =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 3) state

        let inHandler =
            state.MapKernel (SignalFrames.enter starter (Set.singleton Signal.SIGUSR1))

        let exn =
            Assert.Throws<exn> (fun () -> ThreadFixtures.start starter first inHandler |> ignore<IlMachineState>)

        exn.Message
        |> shouldContainText $"task %O{starter} creates a thread from inside a signal handler"

        ThreadFixtures.start (ThreadId 0) second inHandler
        |> fun state -> Map.containsKey second state.Kernel.Tasks
        |> shouldEqual true

    [<Test>]
    let ``a parked interpreter thread gets a task too`` () : unit =
        // It runs guest code when a signal is dispatched, so it needs a real OS
        // thread id; only its core is a placeholder.
        let state, thread = machine () |> IlMachineState.allocateParkedThread (ThreadId 0)

        agrees state
        UnixTaskTable.cpuOf thread state.Kernel.Tasks |> shouldEqual (CpuId 0)

        UnixTaskTable.osThreadIdOf thread state.Kernel.Tasks
        |> OsThreadId.toUInt64
        |> shouldEqual 4243UL

    [<Test>]
    let ``several threads of both kinds keep the sets in step`` () : unit =
        let state = machine ()

        let state, _ =
            ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1) state

        let state, parked = IlMachineState.allocateParkedThread (ThreadId 0) state

        let state, _ =
            ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 2) state

        agrees state
        state.Kernel.Tasks.Count |> shouldEqual (Map.count state.ThreadState)

        // Distinct OS thread ids: an alias would let one thread be mistaken for
        // another as a `Lock` owner.
        let ids =
            threads state
            |> Map.toList
            |> List.map (fun (t, _) -> UnixTaskTable.osThreadIdOf t state.Kernel.Tasks)

        ids |> List.distinct |> List.length |> shouldEqual ids.Length

        ids
        |> List.contains (UnixTaskTable.osThreadIdOf parked state.Kernel.Tasks)
        |> shouldEqual true

    [<Test>]
    let ``guest threads take successive cores in the rotation, in construction order`` () : unit =
        let state = machineOnFour ()

        let state, first =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 1) state

        let state, second =
            IlMachineState.allocateUnstartedThread (ManagedHeapAddress 2) state

        // Started in the reverse order, so that a placement chosen at the start would
        // swap the two.
        let state =
            state
            |> ThreadFixtures.start (ThreadId 0) second
            |> ThreadFixtures.start (ThreadId 0) first

        // The entry thread took the rotation's first slot.
        UnixTaskTable.cpuOf first state.Kernel.Tasks |> shouldEqual (CpuId 1)
        UnixTaskTable.cpuOf second state.Kernel.Tasks |> shouldEqual (CpuId 2)

    [<Test>]
    let ``addThread gives the leader its thread once, on the rotation's first slot`` () : unit =
        let state = bareOnFour ()

        let state, frame = aFrame state
        let state, first = IlMachineState.addThread frame state

        first |> shouldEqual state.Kernel.Leader
        UnixTaskTable.cpuOf first state.Kernel.Tasks |> shouldEqual (CpuId 0)
        state.NextCpuRotation |> shouldEqual 1
        agrees state

        // A process has one first thread; every other is created by a running one.
        let exn =
            Assert.Throws<exn> (fun () -> IlMachineState.addThread frame state |> ignore<IlMachineState * ThreadId>)

        exn.Message |> shouldContainText "already has a thread"

    [<Test>]
    let ``a thread with no task is refused`` () : unit =
        let state, thread =
            machine ()
            |> ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1)

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

        let haunted = state.MapKernel (KernelTasks.ensure ghost)

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
                Buffer = UserBuffer.Mapped
                Deadline = None
            }

    /// Every kind of park, so that the rows below say the invariant is about *whether* a thread
    /// is parked rather than about which syscall it is parked in. One record field and one park
    /// status are exactly what let one statement of the rule cover every parking syscall, and a
    /// row per kind is what would otherwise have to be written again for a fifth.
    let private parks : ParkedSyscall list = [ aLock ; aWait ]

    /// A thread with a task, and `parked` written on it.
    let private threadParkedIn (parked : ParkedSyscall) : IlMachineState * ThreadId =
        let state, thread =
            machine ()
            |> ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1)

        state.MapKernel (EmulatedKernel.mapUnix (UnixWait.park thread parked)), thread

    [<Test>]
    let ``a syscall waiter with no record is refused`` () : unit =
        // The status says the thread is asleep in a syscall; the record says which, and in what.
        // A thread with the one and not the other is a state nothing can act on: no sweep can
        // decide whether to wake it, and no re-entered handler could decide what to finish.
        let state, thread =
            machine ()
            |> ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1)

        let statuses = threads state |> Map.add thread ThreadStatus.BlockedInSyscall

        EmulatedKernel.checkTaskInvariants statuses state.Kernel
        |> shouldEqual [ EmulatedKernelDefect.SyscallWaiterWithoutRecord thread ]

    [<TestCaseSource(nameof parks)>]
    let ``a park record on a thread that cannot be waiting is refused`` (parked : ParkedSyscall) : unit =
        // A thread asleep in `Thread.Sleep`, which has a task but cannot also be in a syscall.
        let recorded, thread = threadParkedIn parked
        let asleep = ThreadStatus.BlockedOnSleep None
        let statuses = threads recorded |> Map.add thread asleep

        EmulatedKernel.checkTaskInvariants statuses recorded.Kernel
        |> shouldEqual [ EmulatedKernelDefect.SyscallRecordWithoutWaiter (thread, asleep) ]

    [<Test>]
    let ``a terminated thread that still has a task is refused`` () : unit =
        // A thread's exit removes its task, so a terminated thread holding one is a thread whose
        // exit the kernel was never told of.
        let state, thread =
            machine ()
            |> ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1)

        let statuses = threads state |> Map.add thread ThreadStatus.Terminated

        EmulatedKernel.checkTaskInvariants statuses state.Kernel
        |> shouldEqual [ EmulatedKernelDefect.TaskWithoutThread thread ]

    [<Test>]
    let ``a terminated worker leaves no task behind`` () : unit =
        // Two workers, so that the one terminating is not the kernel's last task, and so that
        // "removes the terminated thread's task" is told apart from "removes every task".
        let state = machine ()

        let state, first =
            ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1) state

        let state, second =
            ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 2) state

        let before = state.Kernel.Tasks
        let state = Scheduler.onThreadTerminated first state

        state.ThreadState.[first].Status |> shouldEqual ThreadStatus.Terminated
        state.Kernel.Tasks |> shouldEqual (Map.remove first before)

        UnixTaskTable.cpuOf second state.Kernel.Tasks
        |> shouldEqual (UnixTaskTable.cpuOf second before)

        agrees state
        EmulatedKernel.checkInvariants state.Kernel |> shouldBeEmpty

    [<Test>]
    let ``a terminated worker's handler frames go with it`` () : unit =
        // A second thread, so that the worker is not the kernel's last task.
        let state, _ =
            machine ()
            |> ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1)

        let state, worker =
            ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 2) state

        let state =
            state.MapKernel (SignalFrames.enter worker (Set.singleton Signal.SIGUSR1))

        SignalState.tasksWithFrames state.Kernel.Process.Signals
        |> shouldEqual (Set.singleton worker)

        let state = Scheduler.onThreadTerminated worker state

        SignalState.tasksWithFrames state.Kernel.Process.Signals |> shouldBeEmpty
        EmulatedKernel.checkInvariants state.Kernel |> shouldBeEmpty

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
                parked.MapKernel (EmulatedKernel.mapUnix (UnixWait.park thread aLock))
                |> ignore<IlMachineState>
            )

        exn.Message |> shouldContainText "blocks in one syscall at a time"

    [<TestCaseSource(nameof parks)>]
    let ``re-parking in the same syscall is allowed`` (parked : ParkedSyscall) : unit =
        // The lawful overwrite, and the reason the refusal above is by kind rather than by
        // equality: a beaten `flock` waiter re-parks on the same condition, and a socket waiter
        // whose port was drained before it ran parks again on the same port.
        let state, thread = threadParkedIn parked

        state.MapKernel (EmulatedKernel.mapUnix (UnixWait.park thread parked))
        |> fun state -> UnixTaskTable.parkedFor thread state.Kernel.Tasks
        |> shouldEqual (Some parked)

    [<TestCaseSource(nameof parks)>]
    let ``clearing a park lets the other syscall park`` (parked : ParkedSyscall) : unit =
        // The refusal is about an *unclosed* park, not about a task's history: a completion that
        // clears its record leaves the task free to block in anything.
        let state, thread = threadParkedIn parked

        let other = parks |> List.find (fun p -> p <> parked)

        state.MapKernel (EmulatedKernel.mapTasks (UnixTaskTable.unpark thread))
        |> fun state -> state.MapKernel (EmulatedKernel.mapUnix (UnixWait.park thread other))
        |> fun state -> UnixTaskTable.parkedFor thread state.Kernel.Tasks
        |> shouldEqual (Some other)

    [<Test>]
    let ``spawning a thread's task twice is refused`` () : unit =
        // A second spawn would silently discard the first one's core and OS thread
        // id, which is how a thread would end up aliasing another.
        let state, thread =
            machine ()
            |> ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1)

        let exn =
            Assert.Throws<exn> (fun () ->
                UnixTaskLifecycle.spawn (ThreadId 0) thread (CpuId 3) (EmulatedKernel.unix state.Kernel)
                |> ignore<Result<OsThreadId * UnixSystem<ThreadId, NativeSignalHandler>, UnixError>>
            )

        exn.Message |> shouldContainText "already names a task"

    [<Test>]
    let ``waking a thread that is not parked in a syscall is refused`` () : unit =
        // The sweep observes a thread parked and then wakes it, so a thread that is no longer
        // parked by the time it is woken means the sweep raced its own observation — and waking
        // it anyway would set a thread Runnable that some *other* mechanism had meanwhile put to
        // sleep, losing that wait with nothing to say so.
        let state, thread =
            machine ()
            |> ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1)

        let exn =
            Assert.Throws<exn> (fun () -> Scheduler.wakeFromSyscall thread state |> ignore<IlMachineState>)

        exn.Message |> shouldContainText "is not parked in a syscall"

    [<Test>]
    let ``a park and its release round-trip`` () : unit =
        // On a machine with several cores, and on a thread after the entry
        // thread, so that the task under test is not on core 0: a fixture whose
        // thread already sits there cannot tell "parking left the core alone"
        // from "parking reset it to zero".
        let state = machineOnFour ()

        let state, thread =
            ThreadFixtures.constructAndStart (ThreadId 0) (ManagedHeapAddress 1) state

        UnixTaskTable.cpuOf thread state.Kernel.Tasks |> shouldEqual (CpuId 1)

        let wait : ParkedSocketWait =
            {
                Port = OpenFileDescriptionId 5L
                MaxEvents = 8
                Buffer = UserBuffer.Mapped
                Deadline = None
            }

        // The status goes with the record, because a park writes both and `checkTaskInvariants`
        // refuses either alone: a record on a thread that is not parked in a syscall is a state
        // no wait can have produced.
        let parked =
            state.MapKernel (EmulatedKernel.mapUnix (UnixWait.park thread (ParkedSyscall.SocketWait wait)))
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
            woken.MapKernel (EmulatedKernel.mapTasks (UnixTaskTable.unpark thread))

        UnixTaskTable.parkedFor thread released.Kernel.Tasks |> shouldEqual None

        agrees released

        // Parking must not disturb the rest of the task.
        UnixTaskTable.cpuOf thread released.Kernel.Tasks
        |> shouldEqual (UnixTaskTable.cpuOf thread state.Kernel.Tasks)

        UnixTaskTable.osThreadIdOf thread released.Kernel.Tasks
        |> shouldEqual (UnixTaskTable.osThreadIdOf thread state.Kernel.Tasks)
