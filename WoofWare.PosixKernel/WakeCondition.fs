namespace WoofWare.PosixKernel

/// What a task parked in a syscall is waiting for.
///
/// Data, and deliberately transparent: a client that cannot make progress needs
/// to *read* a condition as well as evaluate it — PawPrint's driver advances the
/// virtual clock to the nearest deadline when nothing is runnable, which a
/// predicate cannot answer. So no case may carry a function.
///
/// A condition names live kernel objects rather than a snapshot of them, and
/// stays true to what a real kernel waits on rather than to what is convenient
/// to evaluate. Keeping those objects alive while something waits on them is the
/// client's obligation, and it is what `close` refuses to break.
[<RequireQualifiedAccess>]
type WakeCondition =
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

[<RequireQualifiedAccess>]
module WakeCondition =

    /// Would the syscall that parked on this condition get further now?
    ///
    /// Pure, and cheap enough to poll: a client that has parked a task asks this
    /// of each candidate state until it answers `true`, then finishes the call
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
    /// waits — as `close` already refuses to strand a task parked in
    /// `SystemNative_WaitForSocketEvents`. Asking about a description that has
    /// gone is that obligation being broken, and it fails loudly rather than
    /// answering: the honest answers are "grantable", which wakes the task into
    /// an `EBADF` no kernel produces, and "not yet", which sleeps forever.
    let isSatisfied<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (condition : WakeCondition)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match condition with
        | WakeCondition.FlockGrantable (requester, mode) ->
            let registry = system.Process.FileDescriptors

            match FileDescriptorRegistry.descriptions registry |> Map.tryFind requester with
            | None ->
                failwith
                    $"WakeCondition.isSatisfied: open file description %O{requester} is not in the table, so a task parked on an flock of it has had that description closed underneath it. This library's table models no reference from a waiter to what it waits on, so a client that parks must refuse such a close (as `close` does for a task parked in a socket-event wait)."
            | Some description ->
                FileDescriptorRegistry.flockConflicts (OpenFileDescription.object description) requester mode registry
                |> not
        | WakeCondition.SocketEventDeliverable port -> SocketEventPort.hasDeliverableEvent port system

    /// What the task holding `parked` is waiting for.
    ///
    /// The direction that generalises, and the one every reader of a park should
    /// use. A record is *richer* than its condition — a socket wait also carries
    /// the event count its finishing call will copy out with, which no condition
    /// mentions — so record to condition is total where condition to record is
    /// not, and only `flock`, whose record is exactly its condition, has a
    /// `parkFlock` going the other way.
    ///
    /// Deriving rather than storing the condition beside the record is what stops
    /// the two disagreeing: a client cannot park a task on one object while
    /// polling for another, because the thing polled *is* the thing parked on.
    let ofPark (parked : ParkedSyscall) : WakeCondition =
        match parked with
        | ParkedSyscall.Flock parked -> WakeCondition.FlockGrantable (parked.Requester, parked.Mode)
        | ParkedSyscall.SocketWait wait -> WakeCondition.SocketEventDeliverable wait.Port

/// What became of a request this kernel could answer, where "answer" may be
/// "the calling task sleeps".
[<RequireQualifiedAccess>]
type SyscallOutcome =
    /// The entry point returned.
    | Answered of SyscallAnswer
    /// The entry point did not return. The calling task sleeps until
    /// `WakeCondition.isSatisfied` holds of this condition, and then finishes
    /// the call; what sleeping means, and when to re-ask, are the client's
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
