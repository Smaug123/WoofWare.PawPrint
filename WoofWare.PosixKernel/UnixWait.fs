namespace WoofWare.PosixKernel

/// A wait queue whose waiters a real kernel wakes one at a time.
[<RequireQualifiedAccess>]
type internal ExclusiveWaitQueue =
    /// The waiters in `epoll_wait` on the socket event port this description names.
    | SocketEventPort of OpenFileDescriptionId
    /// The waiters in `accept` on the listening socket this description names.
    | Listener of OpenFileDescriptionId

/// Parking a task in a syscall, and deciding which parked tasks a system wakes.
///
/// Wakes are pulled rather than pushed: nothing that makes a condition true
/// wakes anyone. A client asks `wakes` of each state it reaches instead, so no
/// producer can forget to wake a waiter.
[<RequireQualifiedAccess>]
module UnixWait =

    /// Record that `task` has parked in `parked`, placing it after every park
    /// made before it.
    ///
    /// Refuses to replace a park of one syscall with a park of another, and
    /// otherwise accepts a re-park of the same syscall, which moves the task to
    /// the back of park order.
    let park<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
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
            Tasks = UnixTaskTable.withPark task taskPark system.Tasks
        }

    /// The tasks of `asleep` that `system` wakes now, in the order they parked,
    /// each with the primitives of its wake condition which hold.
    ///
    /// `asleep` is the client's: the tasks it is holding asleep in a syscall.
    /// Every one must be parked. A task the client has woken keeps its park
    /// until its call finishes, so the parks alone cannot say who is asleep.
    ///
    /// A waiter whose `flock` has become grantable, whose polled descriptor is
    /// ready, whose deadline has passed, or to which a signal with a handler is
    /// deliverable wakes, whoever else is waiting for the same thing. Waiters on a socket event port, and waiters in `accept`
    /// on a listener, queue exclusively: something to take wakes one of them,
    /// the one that parked *last* on a socket event port and the one that
    /// parked *first* on a listener. It wakes none of them while any task
    /// parked on the same port or listener has been woken and has not yet
    /// finished its call (that is, is parked but not in `asleep`), since that
    /// task will take it.
    ///
    /// A woken task is owed no success: several waiters for one lock all wake,
    /// and all but one find it taken again and re-park.
    let wakes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (asleep : Set<'Task>)
        (system : UnixSystem<'Task, 'Handler>)
        : ('Task * Set<WakePrimitive>) list
        =
        let satisfied =
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
                        Some (park.Ordinal, task, fired)
            )
            |> List.sortBy (fun (ordinal, _, _) -> ordinal)

        // Measured on Linux 6.18.5 (`epoll-wait.c`, section F): `epoll_wait`
        // queues its waiters exclusively at the *front* of the port's wait
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
        // Waiters on an `flock` are the opposite, deliberately: a release
        // wakes every blocker and they race, as `flock(2)` does, and which of
        // them wins is not observable from userspace on any platform. Waking
        // them all does not invent a winner; it leaves the choice to the
        // client's scheduler. A `poll` queues itself on each description it
        // watches non-exclusively, so every poller of a description that
        // becomes ready wakes; and a deadline, like a signal, is each waiter's
        // own.
        let exclusiveQueueOf (primitive : WakePrimitive) : ExclusiveWaitQueue option =
            match primitive with
            | WakePrimitive.SocketEventDeliverable port -> Some (ExclusiveWaitQueue.SocketEventPort port)
            | WakePrimitive.AcceptQueueNonEmpty listener -> Some (ExclusiveWaitQueue.Listener listener)
            | WakePrimitive.FlockGrantable _
            | WakePrimitive.DescriptorReady _
            | WakePrimitive.DeadlinePassed _
            | WakePrimitive.SignalDeliverable -> None

        let finishing : Set<ExclusiveWaitQueue> =
            system.Tasks
            |> Map.toSeq
            |> Seq.choose (fun (task, state) ->
                if Set.contains task asleep then
                    None
                else
                    match state.Parked with
                    | Some {
                               Syscall = ParkedSyscall.SocketWait wait
                           } -> Some (ExclusiveWaitQueue.SocketEventPort wait.Port)
                    | Some {
                               Syscall = ParkedSyscall.Accept accept
                           } -> Some (ExclusiveWaitQueue.Listener accept.Listener)
                    | Some {
                               Syscall = ParkedSyscall.Flock _ | ParkedSyscall.Poll _
                           }
                    | None -> None
            )
            |> Set.ofSeq

        let chosen : Map<ExclusiveWaitQueue, 'Task> =
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
                        | ExclusiveWaitQueue.SocketEventPort _ -> List.maxBy fst waiters
                        | ExclusiveWaitQueue.Listener _ -> List.minBy fst waiters

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
        |> List.map (fun (_, task, fired) -> task, fired)

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
