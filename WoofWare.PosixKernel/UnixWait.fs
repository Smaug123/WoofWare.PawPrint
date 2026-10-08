namespace WoofWare.PosixKernel

/// A wait queue whose waiters a real kernel wakes one at a time.
[<RequireQualifiedAccess>]
type internal ExclusiveWaitQueue =
    /// The waiters in `epoll_wait` on the epoll instance this description names.
    | Epoll of OpenFileDescriptionId
    /// The waiters in `accept` on the listening socket this description names.
    | Listener of OpenFileDescriptionId
    /// The waiters in `read` for bytes from this pipe.
    | PipeReaders of PipeId
    /// The waiters in `write` for room in this pipe.
    | PipeWriters of PipeId

/// Parking a task in a syscall, and deciding which parked tasks a system wakes.
///
/// Wakes are pulled rather than pushed: nothing that makes a condition true
/// wakes anyone. A client asks `wakes` of each state it reaches instead, so no
/// producer can forget to wake a waiter.
[<RequireQualifiedAccess>]
module UnixWait =

    /// Record that `task` has parked in `parked`, placing it after every park
    /// made before it, and taking the holds the call keeps on the open file
    /// descriptions it names (`ParkedSyscall.descriptions`).
    ///
    /// Refuses to replace a park of one syscall with a park of another, and
    /// otherwise accepts a re-park of the same syscall, which moves the task to
    /// the back of park order and lets go of the holds the earlier park took.
    let internal park<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (parked : ParkedSyscall)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let (ParkOrdinal.ParkOrdinal ordinal) = system.Machine.NextParkOrdinal

        let taskPark =
            {
                Syscall = parked
                Ordinal = system.Machine.NextParkOrdinal
            }

        { system with
            Machine =
                { system.Machine with
                    NextParkOrdinal = ParkOrdinal.ParkOrdinal (ordinal + 1L)
                }
        }
        |> UnixParkState.setPark task taskPark

    /// The tasks of `asleep` that `system` wakes now, in the order they parked,
    /// each with the primitives of its wake condition which hold.
    ///
    /// `asleep` is the client's: the tasks it is holding asleep in a syscall.
    /// Every one must be parked. A task the client has woken keeps its park
    /// until its call finishes, so the parks alone cannot say who is asleep.
    ///
    /// A waiter whose `flock` has become grantable, whose polled descriptor is
    /// ready, whose deadline has passed, or to which a signal with a handler is
    /// deliverable wakes, whoever else is waiting for the same thing. Waiters on an epoll instance, and waiters in `accept`
    /// on a listener, queue exclusively: something to take wakes one of them,
    /// the one that parked *last* on an epoll instance and the one that
    /// parked *first* on a listener. Under Linux, so do the waiters in `read`
    /// for a pipe's bytes and those in `write` for its room, the one that
    /// parked first waking; under Darwin every one of them wakes. Exclusive
    /// queues wake none while any task parked on the same queue has been woken
    /// and has not yet finished its call (that is, is parked but not in
    /// `asleep`), since that task will take it. The end of a pipe closing
    /// wakes every waiter on the other end, under either flavour; and an event
    /// to report, or a close that drains a kqueue, wakes every waiter in
    /// `kevent` on it.
    ///
    /// A woken task is owed no success: several waiters for one lock all wake,
    /// and all but one find it taken again and re-park.
    let rec wakes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (asleep : Set<'Task>)
        (system : UnixSystem<'Task, 'Handler>)
        : ('Task * Set<WakePrimitive>) list
        =
        wakesAmong [ (), asleep, system ] system.Machine
        |> List.map (fun ((), task, fired) -> task, fired)

    /// `wakes`, of every process on `machine` at once: each of `views` is one
    /// process's view of `machine`, with the tasks of it the client holds
    /// asleep, and the process is named by its `'Process`. Every process with
    /// a task parked must be among them, since a parked task not asleep stands
    /// for a woken call on whatever queue it waits on.
    ///
    /// Each task's condition is asked of its own process's view, and a queue
    /// waiters wait on exclusively wakes one of them by park order across
    /// every view, the ordinals being the machine's. The answer is in park
    /// order across every view.
    and internal wakesAmong<'Process, 'Task, 'Handler
        when 'Process : equality and 'Task : comparison and 'Handler : equality>
        (views : ('Process * Set<'Task> * UnixSystem<'Task, 'Handler>) list)
        (machine : UnixMachineState)
        : ('Process * 'Task * Set<WakePrimitive>) list
        =
        let satisfied =
            views
            |> List.collect (fun (owner, asleep, system) ->
                asleep
                |> Set.toList
                |> List.choose (fun task ->
                    match UnixTaskTable.parkOf task system.Tasks with
                    | None ->
                        failwith
                            $"UnixWait.wakes: task %O{task} is asleep in a syscall but records no park, so nothing says what it waits for. A park is recorded by `UnixWait.park` when the task goes to sleep (this is a bug in the client)."
                    | Some park ->
                        let fired = WakeCondition.satisfied task (WakeCondition.ofPark park.Syscall) system

                        if Set.isEmpty fired then
                            None
                        else
                            Some (park.Ordinal, (owner, task), fired)
                )
            )
            |> List.sortBy (fun (ordinal, _, _) -> ordinal)

        // Measured on Linux 6.18.5 (`epoll-wait.c`, section F): `epoll_wait`
        // queues its waiters exclusively at the *front* of the epoll instance's wait
        // queue, so each signal wakes exactly one of them, the one that parked
        // last; three threads parked in any order returned in the reverse of
        // it, one per datagram, in 60 trials of 60, and a thread that waited
        // again went back to the front every time.
        //
        // Measured on Linux 6.18.5 and Darwin 27.0.0 (`blocking-accept.c`,
        // section B): a blocking `accept` is the other way round. Each
        // connection woke exactly one of three accepters, the one that parked
        // *first*, in 60 trials of 60 on each; and a thread that accepted again
        // went to the back of the queue every time, in 20 of 20.
        //
        // A real kernel wakes one waiter per *signal*, and this sweep sees only
        // states, so it cannot tell a second signal from the first one seen
        // again. So a woken waiter that has yet to finish stands for every
        // signal since it was woken, and nobody else on its queue wakes until
        // it has taken what woke it or parked again. Where a real second signal
        // would have woken another waiter (two connections back to back woke
        // two accepters, in 50 of 50 on each flavour), whichever of the two ran
        // first would have taken the first; this answers the schedule in which
        // the first one ran first. Every answer is one a real kernel gives, and
        // the schedules in which the second waiter runs first are not reached.
        //
        // Measured on Linux 6.18.5 and Darwin 27.0.0 (`pipe-blocking.c`,
        // sections A and B): on Linux each 1-byte write woke exactly one of
        // three readers asleep on an empty pipe, the one that parked *first*,
        // in 60 trials of 60, and a reader that read again went to the back;
        // writers asleep on a full pipe likewise, a freed slot at a time. On
        // Darwin exactly one returned per write too, but not by park order (the
        // first-parked won 6 to 10 of each 10): every sleeper wakes and they
        // race, which is the client scheduler's to decide. The last writer or
        // reader closing wakes every sleeper on the other end on both, as
        // Linux's `pipe_release` does.
        //
        // Measured on Darwin 27.0.0 (`kqueue-kevent.c`, section E5): a close
        // that drains a kqueue ends every `kevent` wait on it at once, whichever
        // descriptor each was entered through. And the waiters on one kqueue
        // showed no fixed order when an event arrived
        // (`signal-interrupt-requeue.c`, section C), which is every sleeper
        // waking to race.
        //
        // Waiters on an `flock` are the opposite, deliberately: a release
        // wakes every blocker and they race, as `flock(2)` does, and which of
        // them wins is not observable from userspace on any platform. Waking
        // them all does not invent a winner; it leaves the choice to the
        // client's scheduler. A `poll` queues itself on each description it
        // watches non-exclusively, so every poller of a description that
        // becomes ready wakes; and a deadline, like a signal, is each waiter's
        // own.
        let linux =
            match SimulatedUnixPlatform.flavour machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        // The pipe a parked transfer's description names. `WakeCondition.satisfied`
        // has already failed loudly for a description that is gone or names
        // something else, for every task asked about; a task woken and not yet
        // finished is not asked, so is looked up leniently here.
        let pipeOf (description : OpenFileDescriptionId) : PipeId option =
            match OpenFileTable.tryFind description machine.OpenFiles with
            | Some {
                       Target = OpenFileTarget.Pipe (pipeId, _)
                   } -> Some pipeId
            | Some _
            | None -> None

        let exclusiveQueueOf (primitive : WakePrimitive) : ExclusiveWaitQueue option =
            match primitive with
            | WakePrimitive.EpollEventDeliverable epoll -> Some (ExclusiveWaitQueue.Epoll epoll)
            | WakePrimitive.AcceptQueueNonEmpty listener -> Some (ExclusiveWaitQueue.Listener listener)
            | WakePrimitive.PipeHasBytes reader when linux -> pipeOf reader |> Option.map ExclusiveWaitQueue.PipeReaders
            | WakePrimitive.PipeHasRoom (writer, _, _) when linux ->
                pipeOf writer |> Option.map ExclusiveWaitQueue.PipeWriters
            | WakePrimitive.PipeHasBytes _
            | WakePrimitive.PipeHasRoom _
            | WakePrimitive.PipeWriteEndClosed _
            | WakePrimitive.PipeReadEndClosed _
            | WakePrimitive.PipeReadWhileNonBlocking _
            | WakePrimitive.FlockGrantable _
            | WakePrimitive.KqueueDrained _
            | WakePrimitive.KqueueEventDeliverable _
            | WakePrimitive.KqueuePollReportable
            | WakePrimitive.DescriptorReady _
            | WakePrimitive.DeadlinePassed _
            | WakePrimitive.SignalDeliverable
            | WakePrimitive.EndedByClose -> None

        let finishing : Set<ExclusiveWaitQueue> =
            views
            |> Seq.collect (fun (_, asleep, system) ->
                system.Tasks
                |> Map.toSeq
                |> Seq.filter (fun (task, _) -> not (Set.contains task asleep))
            )
            |> Seq.choose (fun (_, state) ->
                match state.Parked with
                | Some {
                           Syscall = ParkedSyscall.EpollWait wait
                       } -> Some (ExclusiveWaitQueue.Epoll wait.Epoll)
                // A call a close has ended waits on no queue.
                | Some {
                           Syscall = ParkedSyscall.Accept accept
                       } ->
                    SleepTarget.description accept.Listener
                    |> Option.map ExclusiveWaitQueue.Listener
                | Some {
                           Syscall = ParkedSyscall.PipeRead read
                       } when linux ->
                    SleepTarget.description read.Reader
                    |> Option.bind pipeOf
                    |> Option.map ExclusiveWaitQueue.PipeReaders
                | Some {
                           Syscall = ParkedSyscall.PipeWrite write
                       } when linux ->
                    SleepTarget.description write.Writer
                    |> Option.bind pipeOf
                    |> Option.map ExclusiveWaitQueue.PipeWriters
                | Some {
                           Syscall = ParkedSyscall.Flock _
                       }
                | Some {
                           Syscall = ParkedSyscall.Kevent _
                       }
                | Some {
                           Syscall = ParkedSyscall.Poll _
                       }
                | Some {
                           Syscall = ParkedSyscall.KqueuePoll _
                       }
                | Some {
                           Syscall = ParkedSyscall.PipeRead _
                       }
                | Some {
                           Syscall = ParkedSyscall.PipeWrite _
                       }
                | None -> None
            )
            |> Set.ofSeq

        let chosen : Map<ExclusiveWaitQueue, 'Process * 'Task> =
            satisfied
            |> List.collect (fun (ordinal, task, fired) ->
                fired
                |> Set.toList
                |> List.choose (fun primitive ->
                    exclusiveQueueOf primitive |> Option.map (fun queue -> queue, (ordinal, task))
                )
            )
            |> List.groupBy fst
            |> List.choose (fun (queue, waiters) ->
                if Set.contains queue finishing then
                    None
                else
                    let waiters = waiters |> List.map snd

                    let _, woken =
                        match queue with
                        | ExclusiveWaitQueue.Epoll _ -> List.maxBy fst waiters
                        | ExclusiveWaitQueue.Listener _
                        | ExclusiveWaitQueue.PipeReaders _
                        | ExclusiveWaitQueue.PipeWriters _ -> List.minBy fst waiters

                    Some (queue, woken)
            )
            |> Map.ofList

        satisfied
        |> List.filter (fun (_, task, fired) ->
            fired
            |> Set.exists (fun primitive ->
                match exclusiveQueueOf primitive with
                | Some queue -> Map.tryFind queue chosen = Some task
                | None -> true
            )
        )
        |> List.map (fun (_, (owner, task), fired) -> owner, task, fired)

    /// Every deadline the parks of `asleep` are waiting for, in nanoseconds since
    /// boot, with a repeat for each park that waits for it.
    ///
    /// What a client with nothing runnable reads to learn how far it may advance
    /// the clock before one of these calls times out. `asleep` means what it
    /// means to `wakes`.
    let deadlines<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (asleep : Set<'Task>)
        (system : UnixSystem<'Task, 'Handler>)
        : int64 list
        =
        asleep
        |> Set.toList
        |> List.collect (fun task ->
            match UnixTaskTable.parkOf task system.Tasks with
            | None ->
                failwith
                    $"UnixWait.deadlines: task %O{task} is asleep in a syscall but records no park, so nothing says what it waits for. A park is recorded by `UnixWait.park` when the task goes to sleep (this is a bug in the client)."
            | Some park -> WakeCondition.deadlines (WakeCondition.ofPark park.Syscall)
        )
