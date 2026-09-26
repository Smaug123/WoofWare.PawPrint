namespace WoofWare.PosixKernel

/// One thing a task parked in a syscall can be waiting for, which holds or does
/// not of a given system.
///
/// A primitive names live kernel objects rather than a snapshot of them, and
/// stays true to what a real kernel waits on rather than to what is convenient
/// to evaluate. Keeping those objects alive while something waits on them is the
/// client's obligation, and it is what `close` refuses to break.
[<RequireQualifiedAccess>]
type WakePrimitive =
    /// An `flock` acquisition of `mode` by the open file description
    /// `requester`, parked because another description naming the same object
    /// holds a conflicting lock.
    ///
    /// Note what this does *not* say: nothing about which description obstructs
    /// it. A waiter waits for its lock to become available, not for a particular
    /// holder to go away — a new acquirer between the release and the wake puts
    /// it back to sleep, as it does on a real kernel.
    ///
    /// `requester` is the open file description rather than the descriptor the
    /// call was made through, because the lock belongs to the description: a
    /// `dup` of that descriptor waits on the same lock, and the number itself is
    /// reusable while the description lives on.
    | FlockGrantable of requester : OpenFileDescriptionId * mode : FlockMode
    /// A wait for events on the socket event port the open file description
    /// `port` names, parked because the port had nothing to deliver.
    ///
    /// Carries no event count, unlike the record a client parks with: how many
    /// events the caller asked for decides what the *finishing* call copies out,
    /// and says nothing about whether it can finish at all. A single deliverable
    /// event satisfies a wait for any number of them.
    ///
    /// `port` is the open file description rather than the descriptor, for the
    /// reason `FlockGrantable`'s requester is: the number can be closed and
    /// reused while the wait sleeps, and a `dup` of it waits on the same port.
    | SocketEventDeliverable of port : OpenFileDescriptionId
    /// The machine's monotonic clock (`UnixMachineState.NanosecondsSinceBoot`)
    /// has reached `nanosecondsSinceBoot`.
    ///
    /// Absolute rather than relative, so that it means the same instant however
    /// often it is asked: a syscall's relative timeout becomes one of these when
    /// the call parks.
    | DeadlinePassed of nanosecondsSinceBoot : int64

/// What a task parked in a syscall is waiting for: one primitive, or the first
/// of several.
///
/// Data, and deliberately transparent: a client that cannot make progress needs
/// to *read* a condition as well as evaluate it — for example, to advance the
/// virtual clock to the nearest deadline when nothing is runnable, which
/// `WakeCondition.deadlines` answers and a predicate could not. So no case may
/// carry a function.
[<RequireQualifiedAccess>]
type WakeCondition =
    /// Wait for this one primitive.
    | Primitive of WakePrimitive
    /// Wait until any of these holds. Never empty: a wait for any of nothing
    /// could never end.
    ///
    /// Only the set of primitives it contains matters, so nesting and repetition
    /// are immaterial: `satisfied` of an `AnyOf` is the union of `satisfied` of
    /// its members.
    | AnyOf of first : WakeCondition * rest : WakeCondition list

[<RequireQualifiedAccess>]
module WakeCondition =

    // A primitive that names a description no longer in the table has had its
    // wait broken underneath it: see `satisfied`.
    let private holds<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (primitive : WakePrimitive)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match primitive with
        | WakePrimitive.FlockGrantable (requester, mode) ->
            let registry = system.Process.FileDescriptors

            match FileDescriptorRegistry.descriptions registry |> Map.tryFind requester with
            | None ->
                failwith
                    $"WakeCondition.satisfied: open file description %O{requester} is not in the table, so a task parked on an flock of it has had that description closed underneath it. This library's table models no reference from a waiter to what it waits on, so a client that parks must refuse such a close (as `close` does for a task parked in a socket-event wait)."
            | Some description ->
                FileDescriptorRegistry.flockConflicts (OpenFileDescription.object description) requester mode registry
                |> not
        | WakePrimitive.SocketEventDeliverable port -> SocketEventPort.hasDeliverableEvent port system
        | WakePrimitive.DeadlinePassed deadline -> system.Machine.NanosecondsSinceBoot >= deadline

    /// The primitives of `condition` which hold of `system`: empty exactly when
    /// the syscall that parked on it would get no further now.
    ///
    /// A set rather than a yes or no so that the call which finishes the wait
    /// can tell *why* it woke — an event, or its deadline — which a real kernel
    /// reports differently.
    ///
    /// Pure, and cheap enough to poll: a client that has parked a task asks this
    /// of each candidate state until it is non-empty, then finishes the call
    /// against the object the condition names — see `SyscallOutcome.WouldBlock`
    /// for why that is not the same as re-issuing it against the descriptor it
    /// was made through. It is never a promise that finishing succeeds: another
    /// task can take the lock in between, and the caller then parks again.
    ///
    /// **A condition is only ever asked of a system whose kernel objects it
    /// still names.** A `flock` waiter on a real kernel holds a reference to the
    /// open file it waits on, so that file cannot be destroyed underneath it;
    /// this library's descriptor table models no such reference, so a client
    /// that parks a task must also stop the description being closed while it
    /// waits — as `close` already refuses to strand a task parked in a wait on
    /// a socket event port. Asking about a description that has
    /// gone is that obligation being broken, and it fails loudly rather than
    /// answering: the honest answers are "grantable", which wakes the task into
    /// an `EBADF` no kernel produces, and "not yet", which sleeps forever.
    let rec satisfied<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (condition : WakeCondition)
        (system : UnixSystem<'Task, 'Handler>)
        : Set<WakePrimitive>
        =
        match condition with
        | WakeCondition.Primitive primitive ->
            if holds primitive system then
                Set.singleton primitive
            else
                Set.empty
        | WakeCondition.AnyOf (first, rest) ->
            (satisfied first system, rest)
            ||> List.fold (fun acc condition -> Set.union acc (satisfied condition system))

    /// Every deadline in `condition`, in nanoseconds since boot, one per
    /// `DeadlinePassed` it contains.
    ///
    /// What a client whose tasks are all asleep reads to learn how far it may
    /// advance the clock before a parked call's timeout would fire.
    let rec deadlines (condition : WakeCondition) : int64 list =
        match condition with
        | WakeCondition.Primitive (WakePrimitive.DeadlinePassed deadline) -> [ deadline ]
        | WakeCondition.Primitive (WakePrimitive.FlockGrantable _)
        | WakeCondition.Primitive (WakePrimitive.SocketEventDeliverable _) -> []
        | WakeCondition.AnyOf (first, rest) -> deadlines first @ List.collect deadlines rest

    /// What the task holding `parked` is waiting for.
    ///
    /// The direction that generalises, and the one every reader of a park should
    /// use. A record is *richer* than its condition — a socket wait also carries
    /// the event count its finishing call will copy out with, which no condition
    /// mentions — so record to condition is total where condition to record is
    /// not.
    ///
    /// Deriving rather than storing the condition beside the record is what stops
    /// the two disagreeing: a client cannot park a task on one object while
    /// polling for another, because the thing polled *is* the thing parked on.
    let ofPark (parked : ParkedSyscall) : WakeCondition =
        match parked with
        | ParkedSyscall.Flock parked ->
            WakeCondition.Primitive (WakePrimitive.FlockGrantable (parked.Requester, parked.Mode))
        | ParkedSyscall.SocketWait wait -> WakeCondition.Primitive (WakePrimitive.SocketEventDeliverable wait.Port)

/// What became of a request this kernel could answer, where "answer" may be
/// "the calling task sleeps".
[<RequireQualifiedAccess>]
type SyscallOutcome =
    /// The entry point returned.
    | Answered of SyscallAnswer
    /// The entry point did not return. The calling task sleeps until
    /// `WakeCondition.satisfied` of this condition is non-empty, and then
    /// finishes the call; what sleeping means, and when to re-ask, are the client's
    /// scheduler's business, which is why this library has no opinion on either.
    ///
    /// **Finishing is not re-issuing the original call.** The syscall's
    /// arguments named a descriptor; the condition names the kernel object that
    /// descriptor stood for, and a sleeping task keeps the object rather than
    /// the number. Descriptor numbers are reused as soon as they are free, so a
    /// `close` of the number this call was made through — which a `dup` elsewhere
    /// makes survivable — can leave that number naming something else entirely
    /// by the time the waiter wakes. `ParkedSocketWait` holds its port by
    /// description identity for exactly this reason.
    ///
    /// The system this rides with is the one a real kernel sleeps *in*, not the
    /// one the call arrived with: `flock` removes the caller's old lock before
    /// it establishes the new one, so a parked conversion is already holding
    /// nothing. That advance is the whole reason blocking is an outcome here
    /// rather than a refusal, which by design carries no system at all.
    | WouldBlock of WakeCondition
