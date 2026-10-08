namespace WoofWare.PosixKernel

/// One thing a task parked in a syscall can be waiting for, which holds or does
/// not of a given system.
///
/// A primitive names live kernel objects rather than a snapshot of them, and
/// stays true to what a real kernel waits on rather than to what is convenient
/// to evaluate. The park a primitive comes from holds the open file
/// descriptions it names (`ParkedSyscall.descriptions`), so they stay alive
/// while something waits on them.
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
    /// A wait for events on the epoll instance the open file description
    /// `epoll` names, parked because the instance had nothing to deliver.
    ///
    /// Carries no event count, unlike the record a client parks with: how many
    /// events the caller asked for decides what the *finishing* call copies out,
    /// and says nothing about whether it can finish at all. A single deliverable
    /// event satisfies a wait for any number of them.
    ///
    /// `epoll` is the open file description rather than the descriptor, for the
    /// reason `FlockGrantable`'s requester is: the number can be closed and
    /// reused while the wait sleeps, and a `dup` of it waits on the same epoll instance.
    | EpollEventDeliverable of epoll : OpenFileDescriptionId
    /// The kqueue the open file description `kqueue` names has been drained:
    /// a `close(2)` of a descriptor a `kevent` wait on it was entered through
    /// has ended every wait on it (see `KqueueState.Drained`).
    | KqueueDrained of kqueue : OpenFileDescriptionId
    /// A wait for events on the kqueue the open file description `kqueue`
    /// names would report at least one now (`KqueueQueue.hasDeliverableEvent`).
    ///
    /// Every waiter on one kqueue wakes for it, and the first to finish takes
    /// what it reports; the rest wait again.
    | KqueueEventDeliverable of kqueue : OpenFileDescriptionId
    /// The open file description `description` presents at least one of
    /// `conditions`, in the numbering `<poll.h>` and `<sys/epoll.h>` share, as
    /// `LinuxReadiness.ofDescription` reads its level.
    ///
    /// What a `poll(2)` entry waits for, with `conditions` its request plus the
    /// `POLLERR` and `POLLHUP` a poll reports unasked. It never waits on a
    /// epoll instance, whose level is not modelled.
    | DescriptorReady of description : OpenFileDescriptionId * conditions : uint32
    /// The Darwin `poll` the waiting task is asleep in would report something
    /// were it to scan the kqueue it made for itself now
    /// (`KqueuePoll.reportable`).
    ///
    /// Names no kernel object, as `SignalDeliverable` names none: the kqueue
    /// is the waiter's own, named by its park (`ParkedKqueuePoll.Queue`). Holds only
    /// once a scan would add something to some entry's `revents`: a filter
    /// activated whose report would add nothing does not wake the waiter here,
    /// where a real kernel wakes it to find nothing and sleep again, having
    /// consumed the registration. Answering when the waiter next scans is the
    /// schedule in which it had not yet run.
    | KqueuePollReportable
    /// The listening socket the open file description `listener` names holds a
    /// completed connection in its accept queue.
    ///
    /// What a blocking `accept(2)` waits for. `listener` is the description
    /// rather than the socket, as for the other primitives: it is what the call
    /// holds.
    | AcceptQueueNonEmpty of listener : OpenFileDescriptionId
    /// The pipe whose read end the open file description `reader` names holds
    /// bytes.
    ///
    /// What a blocking `read(2)` of an empty pipe waits for, beside
    /// `PipeWriteEndClosed`. Two primitives rather than one because a kernel
    /// wakes its sleeping readers differently for the two: bytes arriving wake
    /// one of them on Linux, and the last writer closing wakes them all.
    | PipeHasBytes of reader : OpenFileDescriptionId
    /// The pipe whose read end the open file description `reader` names has no
    /// write end open, so a read of it answers end of file.
    | PipeWriteEndClosed of reader : OpenFileDescriptionId
    /// The pipe whose write end the open file description `writer` names has
    /// room for a sleeping write of `count` bytes, the first `written` of them
    /// already in, to put more in (`PipeBuffer.resumeTakes`).
    ///
    /// What a blocking `write(2)` into a pipe with no room for the rest of it
    /// waits for, beside `PipeReadEndClosed`. Carries the write's progress
    /// because whether a write of at most `PIPE_BUF` bytes can resume depends on
    /// how many it has to put in, which it takes whole or not at all.
    | PipeHasRoom of writer : OpenFileDescriptionId * count : int * written : int
    /// The pipe whose write end the open file description `writer` names has
    /// no read end open, so a write into it answers `EPIPE`.
    | PipeReadEndClosed of writer : OpenFileDescriptionId
    /// Under Darwin, the open file description `writer` carries `O_NONBLOCK`,
    /// and a read has taken bytes from the pipe whose write end it names since
    /// it had seen `reads` of them (`PipeState.Reads`). Never under Linux.
    ///
    /// Darwin wakes every writer asleep on a pipe at each read, room or not,
    /// and one that still cannot write, through a description that has become
    /// non-blocking, gives up; Linux wakes a writer only once it can write.
    /// What a write of at most `PIPE_BUF` bytes waits for besides room for all
    /// of it, which `PipeHasRoom` is.
    | PipeReadWhileNonBlocking of writer : OpenFileDescriptionId * reads : int64
    /// Under Darwin, a close has ended the blocking `accept`, pipe `read` or
    /// pipe `write` the waiting task is asleep in (`SleepTarget.EndedByClose`).
    ///
    /// Names no kernel object, as `SignalDeliverable` names none: the call
    /// holds none any more, and what it answers is in the waiter's own park.
    /// Every wait of those three calls waits for it, so that a condition handed
    /// out as the call went to sleep still says when the close ends it. Never
    /// holds under Linux, whose close leaves such a call asleep.
    | EndedByClose
    /// The machine's monotonic clock (`UnixMachineState.NanosecondsSinceBoot`)
    /// has reached `nanosecondsSinceBoot`.
    ///
    /// Absolute rather than relative, so that it means the same instant however
    /// often it is asked: a syscall's relative timeout becomes one of these when
    /// the call parks.
    | DeadlinePassed of nanosecondsSinceBoot : int64
    /// A signal with a handler is deliverable to the task that waits: were it to
    /// return to user mode now, it would run that handler. For a task in
    /// `sigsuspend`, any signal its temporary mask lets through that its
    /// finishing call would act on (`SyscallInterruption.wakes`).
    ///
    /// Names no kernel object, because what it asks about is the waiter itself:
    /// a condition is always some task's, and `satisfied` is told whose. Every
    /// park waits for it, since every sleep this library models is one a signal
    /// interrupts; `SyscallInterruption.ofPark` says how the interrupted call
    /// then ends.
    | SignalDeliverable

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

    /// The pipe the open file description `description` names `pipeEnd` of, and
    /// that pipe; fails loudly, naming the waiter's `primitive`, for a
    /// description that is gone or names something else.
    let private pipeOfWaiter<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (primitive : WakePrimitive)
        (description : OpenFileDescriptionId)
        (pipeEnd : PipeEnd)
        (system : UnixSystem<'Task, 'Handler>)
        : PipeId * PipeState
        =
        match OpenFileTable.tryFind description system.Machine.OpenFiles with
        | None ->
            failwith
                $"WakeCondition.satisfied: open file description %O{description} is not in the table, but a task waits on it (%A{primitive}), and a park holds what it waits on until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."
        | Some found ->
            match found.Target with
            | OpenFileTarget.Pipe (pipeId, named) when named = pipeEnd ->
                pipeId, UnixMachineState.pipe pipeId system.Machine
            | target ->
                failwith
                    $"WakeCondition.satisfied: a task waits on open file description %O{description} for %A{primitive}, but the description names %A{target} rather than the %A{pipeEnd} end of a pipe (this is a bug in the caller that recorded the park)."

    /// Whether a close has ended the call `task` is asleep in.
    let private endedByClose<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match
            Map.tryFind task system.Tasks
            |> Option.bind UnixTaskState.park
            |> Option.map (fun park -> park.Syscall)
        with
        | Some (ParkedSyscall.Accept {
                                         Listener = SleepTarget.EndedByClose _
                                     })
        | Some (ParkedSyscall.PipeRead {
                                           Reader = SleepTarget.EndedByClose _
                                       })
        | Some (ParkedSyscall.PipeWrite {
                                            Writer = SleepTarget.EndedByClose _
                                        }) -> true
        | Some (ParkedSyscall.Accept _)
        | Some (ParkedSyscall.PipeRead _)
        | Some (ParkedSyscall.PipeWrite _)
        | Some (ParkedSyscall.EpollWait _)
        | Some (ParkedSyscall.Kevent _)
        | Some (ParkedSyscall.Flock _)
        | Some (ParkedSyscall.Poll _)
        | Some (ParkedSyscall.KqueuePoll _)
        | Some ParkedSyscall.SigSuspend
        | None -> false

    // A primitive that names a description no longer in the table has had its
    // wait broken underneath it: see `satisfied`.
    let private holds<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (primitive : WakePrimitive)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match primitive with
        | WakePrimitive.FlockGrantable (requester, mode) ->
            let openFiles = system.Machine.OpenFiles

            match OpenFileTable.tryFind requester openFiles with
            | None ->
                failwith
                    $"WakeCondition.satisfied: open file description %O{requester} is not in the table, but a task is parked on an flock of it, and a park holds what it waits on until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."
            | Some description ->
                OpenFileTable.flockConflicts (OpenFileDescription.object requester description) requester mode openFiles
                |> not
        | WakePrimitive.EpollEventDeliverable epoll -> EpollReadyList.hasDeliverableEvent epoll system
        | WakePrimitive.KqueueEventDeliverable kqueue -> KqueueQueue.hasDeliverableEvent kqueue system
        | WakePrimitive.KqueuePollReportable -> KqueuePoll.reportable task system
        | WakePrimitive.KqueueDrained kqueue ->
            match OpenFileTable.tryFind kqueue system.Machine.OpenFiles with
            | None ->
                failwith
                    $"WakeCondition.satisfied: open file description %O{kqueue} is not in the table, but a task waits in kevent on it, and a park holds what it waits on until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."
            | Some description ->
                match description.Target with
                | OpenFileTarget.Kqueue state -> state.Drained
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Socket _
                | OpenFileTarget.Pipe _
                | OpenFileTarget.Epoll _ ->
                    failwith
                        $"WakeCondition.satisfied: a task waits in kevent on open file description %O{kqueue}, which names %A{description.Target} rather than a kqueue (this is a bug in the caller that recorded the park)."
        | WakePrimitive.DescriptorReady (description, conditions) ->
            if (OpenFileTable.tryFind description system.Machine.OpenFiles).IsNone then
                failwith
                    $"WakeCondition.satisfied: open file description %O{description} is not in the table, but a task waits for it to become ready, and a park holds what it waits on until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."

            LinuxReadiness.ofDescription description system &&& conditions <> 0u
        | WakePrimitive.AcceptQueueNonEmpty listener ->
            match OpenFileTable.tryFind listener system.Machine.OpenFiles with
            | None ->
                failwith
                    $"WakeCondition.satisfied: open file description %O{listener} is not in the table, but a task is parked in an accept on it, and a park holds what it waits on until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."
            | Some description ->

            match description.Target with
            | OpenFileTarget.Socket socketId ->
                match (UnixMachineState.socket socketId system.Machine).Phase with
                | SocketPhase.Listening listenState -> not (List.isEmpty listenState.Queue)
                | phase ->
                    failwith
                        $"WakeCondition.satisfied: a task is parked in an accept on socket %O{socketId}, which is in %A{phase} rather than listening. Nothing takes a live listener out of listening, so the park was recorded on a socket that was never one (this is a bug in the caller that recorded it)."
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Pipe _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _ ->
                failwith
                    $"WakeCondition.satisfied: a task is parked in an accept on open file description %O{listener}, which names %A{description.Target} rather than a socket (this is a bug in the caller that recorded it)."
        | WakePrimitive.PipeHasBytes reader ->
            let _, pipe = pipeOfWaiter primitive reader PipeEnd.Read system
            PipeBuffer.held pipe.Buffer > 0
        | WakePrimitive.PipeWriteEndClosed reader ->
            let pipeId, pipe = pipeOfWaiter primitive reader PipeEnd.Read system
            not (UnixMachineState.pipeEndOpen pipeId pipe PipeEnd.Write system.Machine)
        | WakePrimitive.PipeHasRoom (writer, count, written) ->
            let _, pipe = pipeOfWaiter primitive writer PipeEnd.Write system
            PipeBuffer.resumeTakes count written pipe.Buffer > 0
        | WakePrimitive.PipeReadEndClosed writer ->
            let pipeId, pipe = pipeOfWaiter primitive writer PipeEnd.Write system
            not (UnixMachineState.pipeEndOpen pipeId pipe PipeEnd.Read system.Machine)
        | WakePrimitive.PipeReadWhileNonBlocking (writer, reads) ->
            let _, pipe = pipeOfWaiter primitive writer PipeEnd.Write system

            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> false
            | SimulatedUnixFlavour.Darwin ->
                pipe.Reads > reads
                && (OpenFileTable.get "WakeCondition" writer system.Machine.OpenFiles).NonBlocking
        | WakePrimitive.DeadlinePassed deadline -> system.Machine.NanosecondsSinceBoot >= deadline
        | WakePrimitive.SignalDeliverable -> SyscallInterruption.wakes task system
        // `satisfied` answers a call a close has ended before it asks any
        // primitive, so this is asked only of one no close has ended.
        | WakePrimitive.EndedByClose -> false

    /// The primitives of `condition`, the wake condition of `task`, which hold of
    /// `system`: empty exactly when the syscall that parked on it would get no
    /// further now.
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
    /// still names.** A waiter on a real kernel holds a reference to the open
    /// file it waits on, so that file cannot be destroyed underneath it, and a
    /// park here does the same: the descriptions it names outlive their last
    /// descriptor until the call returns. Asking about a description that has
    /// gone means a park was ended or a description destroyed some other way,
    /// and it fails loudly rather than answering: the honest answers are
    /// "grantable", which wakes the task into an `EBADF` no kernel produces,
    /// and "not yet", which sleeps forever.
    ///
    /// A task whose call a close has ended (`WakePrimitive.EndedByClose`)
    /// answers that primitive alone: the call holds nothing any more, and the
    /// descriptions its other primitives name may have gone with the close.
    let satisfied<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (condition : WakeCondition)
        (system : UnixSystem<'Task, 'Handler>)
        : Set<WakePrimitive>
        =
        let rec primitives (condition : WakeCondition) : WakePrimitive list =
            match condition with
            | WakeCondition.Primitive primitive -> [ primitive ]
            | WakeCondition.AnyOf (first, rest) -> primitives first @ List.collect primitives rest

        let rec live (condition : WakeCondition) : Set<WakePrimitive> =
            match condition with
            | WakeCondition.Primitive primitive ->
                if holds task primitive system then
                    Set.singleton primitive
                else
                    Set.empty
            | WakeCondition.AnyOf (first, rest) ->
                (live first, rest)
                ||> List.fold (fun acc condition -> Set.union acc (live condition))

        if endedByClose task system then
            if List.contains WakePrimitive.EndedByClose (primitives condition) then
                Set.singleton WakePrimitive.EndedByClose
            else
                failwith
                    $"WakeCondition.satisfied: a close has ended the call task %O{task} is asleep in, and it was asked about %A{condition}, which does not wait for that, so is not the wake condition of the call (this is a bug in the caller)."
        else
            live condition

    /// Every deadline in `condition`, in nanoseconds since boot, one per
    /// `DeadlinePassed` it contains.
    ///
    /// What a client whose tasks are all asleep reads to learn how far it may
    /// advance the clock before a parked call's timeout would fire.
    let rec deadlines (condition : WakeCondition) : int64 list =
        match condition with
        | WakeCondition.Primitive (WakePrimitive.DeadlinePassed deadline) -> [ deadline ]
        | WakeCondition.Primitive (WakePrimitive.FlockGrantable _)
        | WakeCondition.Primitive (WakePrimitive.EpollEventDeliverable _)
        | WakeCondition.Primitive (WakePrimitive.KqueueDrained _)
        | WakeCondition.Primitive (WakePrimitive.KqueueEventDeliverable _)
        | WakeCondition.Primitive WakePrimitive.KqueuePollReportable
        | WakeCondition.Primitive (WakePrimitive.DescriptorReady _)
        | WakeCondition.Primitive (WakePrimitive.AcceptQueueNonEmpty _)
        | WakeCondition.Primitive (WakePrimitive.PipeHasBytes _)
        | WakeCondition.Primitive (WakePrimitive.PipeWriteEndClosed _)
        | WakeCondition.Primitive (WakePrimitive.PipeHasRoom _)
        | WakeCondition.Primitive (WakePrimitive.PipeReadEndClosed _)
        | WakeCondition.Primitive (WakePrimitive.PipeReadWhileNonBlocking _)
        | WakeCondition.Primitive WakePrimitive.SignalDeliverable
        | WakeCondition.Primitive WakePrimitive.EndedByClose -> []
        | WakeCondition.AnyOf (first, rest) -> deadlines first @ List.collect deadlines rest

    /// What the task holding `parked` is waiting for: what its syscall waits
    /// for, or a signal (`WakePrimitive.SignalDeliverable`), whichever comes
    /// first.
    ///
    /// The direction that generalises, and the one every reader of a park should
    /// use. A record is *richer* than its condition — a parked `epoll_wait` also carries
    /// the event count its finishing call will copy out with, which no condition
    /// mentions — so record to condition is total where condition to record is
    /// not. A parked `poll` that watches no descriptor and has no deadline waits
    /// for a signal alone, and so does a parked `sigsuspend`.
    ///
    /// Deriving rather than storing the condition beside the record is what stops
    /// the two disagreeing: a client cannot park a task on one object while
    /// polling for another, because the thing polled *is* the thing parked on.
    let ofPark (parked : ParkedSyscall) : WakeCondition =
        let ended = WakeCondition.Primitive WakePrimitive.EndedByClose

        let own : WakeCondition list =
            match parked with
            | ParkedSyscall.Flock parked ->
                [
                    WakeCondition.Primitive (WakePrimitive.FlockGrantable (parked.Requester, parked.Mode))
                ]
            | ParkedSyscall.EpollWait wait ->
                let deliverable =
                    WakeCondition.Primitive (WakePrimitive.EpollEventDeliverable wait.Epoll)

                match wait.Deadline with
                | None -> [ deliverable ]
                | Some deadline ->
                    [
                        deliverable
                        WakeCondition.Primitive (WakePrimitive.DeadlinePassed deadline)
                    ]
            | ParkedSyscall.Kevent wait ->
                let deliverable =
                    WakeCondition.Primitive (WakePrimitive.KqueueEventDeliverable wait.Kqueue)

                let drained = WakeCondition.Primitive (WakePrimitive.KqueueDrained wait.Kqueue)

                match wait.Deadline with
                | None -> [ deliverable ; drained ]
                | Some deadline ->
                    [
                        deliverable
                        drained
                        WakeCondition.Primitive (WakePrimitive.DeadlinePassed deadline)
                    ]
            | ParkedSyscall.Poll poll ->
                let watched =
                    poll.Entries
                    |> List.choose (fun entry ->
                        match entry with
                        | ParkedPollEntry.Ignored _ -> None
                        | ParkedPollEntry.Watched (_, description, events) ->
                            // Through `uint16`, so that a request with its top bit
                            // set does not sign-extend into bits above `<poll.h>`.
                            let conditions = uint32 (uint16 events) ||| EpollEvents.Err ||| EpollEvents.Hup
                            Some (WakeCondition.Primitive (WakePrimitive.DescriptorReady (description, conditions)))
                    )

                let deadline =
                    poll.Deadline
                    |> Option.map (WakePrimitive.DeadlinePassed >> WakeCondition.Primitive)
                    |> Option.toList

                watched @ deadline
            | ParkedSyscall.KqueuePoll poll ->
                let deadline =
                    poll.Deadline
                    |> Option.map (WakePrimitive.DeadlinePassed >> WakeCondition.Primitive)
                    |> Option.toList

                WakeCondition.Primitive WakePrimitive.KqueuePollReportable :: deadline
            | ParkedSyscall.Accept accept ->
                match accept.Listener with
                | SleepTarget.EndedByClose _ -> [ ended ]
                | SleepTarget.Waiting (listener, _) ->
                    // No deadline: `SO_RCVTIMEO`, which bounds a Linux accept, is an
                    // option `setsockopt` refuses to set.
                    [ WakeCondition.Primitive (WakePrimitive.AcceptQueueNonEmpty listener) ; ended ]
            | ParkedSyscall.PipeRead read ->
                match read.Reader with
                | SleepTarget.EndedByClose _ -> [ ended ]
                | SleepTarget.Waiting (reader, _) ->
                    [
                        WakeCondition.Primitive (WakePrimitive.PipeHasBytes reader)
                        WakeCondition.Primitive (WakePrimitive.PipeWriteEndClosed reader)
                        ended
                    ]
            | ParkedSyscall.PipeWrite write ->
                match write.Writer with
                | SleepTarget.EndedByClose _ -> [ ended ]
                | SleepTarget.Waiting (writer, _) ->
                    [
                        WakeCondition.Primitive (WakePrimitive.PipeHasRoom (writer, write.Count, write.Written))
                        WakeCondition.Primitive (WakePrimitive.PipeReadEndClosed writer)
                        WakeCondition.Primitive (WakePrimitive.PipeReadWhileNonBlocking (writer, write.ReadsSeen))
                        ended
                    ]
            // A signal alone ends it.
            | ParkedSyscall.SigSuspend -> []

        let signal = WakeCondition.Primitive WakePrimitive.SignalDeliverable

        match own with
        | [] -> signal
        | first :: rest -> WakeCondition.AnyOf (first, rest @ [ signal ])

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
    /// `close` of the number this call was made through can leave that number
    /// naming something else entirely by the time the waiter wakes, while the
    /// object lives on for the waiter. `ParkedEpollWait` holds its epoll instance by
    /// description identity for exactly this reason.
    ///
    /// The system this rides with is the one a real kernel sleeps *in*, not the
    /// one the call arrived with: `flock` removes the caller's old lock before
    /// it establishes the new one, so a parked conversion is already holding
    /// nothing. That advance is the whole reason blocking is an outcome here
    /// rather than a refusal, which by design carries no system at all.
    | WouldBlock of WakeCondition
    /// The call was asleep, a signal with a handler interrupted it, and the call
    /// restarts (`SyscallInterruption.Restart`): it never returns. The task is
    /// no longer parked. Once the handlers have run, the client issues the call
    /// again with the arguments it was first made with, descriptor numbers
    /// included, as a real kernel re-executes it.
    ///
    /// Only a finishing call answers this, never the call as first made.
    | Restarts
