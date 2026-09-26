namespace WoofWare.PosixKernel

/// A sweep this library will not answer, because which of the waiters it would
/// wake is unmodelled.
[<RequireQualifiedAccess>]
type WakeRefusal<'Task> =
    /// Several tasks are parked in a wait on the socket event port `port`,
    /// which now has a deliverable event.
    ///
    /// A real kernel queues such waiters exclusively and wakes exactly one of
    /// them, chosen by park order. This library records park order but has not
    /// measured which end of the queue wakes.
    | ExclusiveWaiters of port : OpenFileDescriptionId * waiters : 'Task list

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
    /// Every waiter whose condition holds wakes, except where a real kernel
    /// would wake only one of several, which is refused rather than answered.
    /// A woken task is owed no success: several waiters for one lock all wake,
    /// and all but one find it taken again and re-park.
    let wakes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (asleep : Set<'Task>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<('Task * Set<WakePrimitive>) list, WakeRefusal<'Task>>
        =
        let woken =
            asleep
            |> Set.toList
            |> List.choose (fun task ->
                match UnixTaskTable.parkOf task system.Tasks with
                | None ->
                    failwith
                        $"UnixWait.wakes: task %O{task} is asleep in a syscall but records no park, so nothing says what it waits for. A park is recorded by `UnixWait.park` when the task goes to sleep (this is a bug in the client)."
                | Some park ->
                    let fired = WakeCondition.satisfied (WakeCondition.ofPark park.Syscall) system

                    if Set.isEmpty fired then
                        None
                    else
                        Some (park.Ordinal, task, fired)
            )
            |> List.sortBy (fun (ordinal, _, _) -> ordinal)

        // `epoll_wait` queues its waiters exclusively, so one event wakes one of
        // them. Waiters on an `flock` are the opposite, deliberately: a release
        // wakes every blocker and they race, as `flock(2)` does, and which of them
        // wins is not observable from userspace on any platform. Waking them all
        // does not invent a winner; it leaves the choice to the client's
        // scheduler. Several tasks really can share one lock condition, because a
        // lock belongs to the open file description: two tasks blocking through
        // one shared descriptor for the same mode wait for the same primitive.
        let exclusive =
            woken
            |> List.collect (fun (_, task, fired) ->
                fired
                |> Set.toList
                |> List.choose (fun primitive ->
                    match primitive with
                    | WakePrimitive.SocketEventDeliverable port -> Some (port, task)
                    | WakePrimitive.FlockGrantable _
                    | WakePrimitive.DeadlinePassed _ -> None
                )
            )
            |> List.groupBy fst
            |> List.tryFind (fun (_, waiters) -> List.length waiters > 1)

        match exclusive with
        | Some (port, waiters) -> Error (WakeRefusal.ExclusiveWaiters (port, List.map snd waiters))
        | None -> woken |> List.map (fun (_, task, fired) -> task, fired) |> Ok

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
