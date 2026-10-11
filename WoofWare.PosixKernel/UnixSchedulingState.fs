namespace WoofWare.PosixKernel

/// What the client's scheduling has done to the machine: how long it has been
/// up, and which task each of its processors is running.
///
/// Both are the client's decisions, which this library only records: the
/// client decides when time passes, and reports it with
/// `UnixSystem.advanceClock`; and it decides which task runs where, and
/// reports each choice with `UnixScheduling.dispatch`. So both change between
/// syscalls, as often as the client runs an instruction, where the rest of the
/// machine (`UnixMachineState`) changes only in a syscall. Held beside the
/// machine rather than in it, so that a client running instructions makes a
/// new copy of this small record alone, and the machine it shares with every
/// process stays the same value until a syscall changes it.
type internal UnixSchedulingState =
    internal
        {
            /// Time since this machine booted, in nanoseconds: what its monotonic
            /// clocks read. Never negative.
            ///
            /// Nothing in this library moves it. The client decides how fast its
            /// simulated machine runs and when time passes, and says so through
            /// `UnixSchedulingState.advanceClock`, the only way it changes; between two
            /// advances every clock stands still, so readings taken between them all
            /// name the same instant.
            NanosecondsSinceBoot : int64
            /// The task running on each busy logical processor, by its thread ID:
            /// what Linux calls a run queue's current task. An idle processor is
            /// absent.
            ///
            /// The client reports each task it runs with `UnixScheduling.dispatch`,
            /// which puts it here, displacing whichever task the processor ran
            /// before, whatever process that task belongs to. A task leaves it when
            /// it is dispatched elsewhere, when it parks in a syscall
            /// (`UnixWait.park`), and when it exits or its process ends. Keyed by
            /// processor, so no processor runs two tasks; the machine's rather
            /// than a process's, because the machine's processes share its
            /// processors; and by thread ID, which no two live tasks on the machine
            /// share.
            Occupants : Map<CpuId, OsThreadId>
        }

[<RequireQualifiedAccess>]
module internal UnixSchedulingState =

    /// A machine that has just booted: no time has passed, and no processor
    /// runs anything.
    let initial : UnixSchedulingState =
        {
            NanosecondsSinceBoot = 0L
            Occupants = Map.empty
        }

    /// Let `nanoseconds` pass on this machine: every clock it has moves forward
    /// by that much. Advancing by zero changes nothing.
    ///
    /// Refuses a negative amount, since no clock this kernel models runs
    /// backwards, and one that would take `NanosecondsSinceBoot` past
    /// `Int64.MaxValue` (about 292 years of uptime), beyond which no monotonic
    /// reading can be represented.
    let advanceClock (nanoseconds : int64) (scheduling : UnixSchedulingState) : UnixSchedulingState =
        if nanoseconds < 0L then
            failwith
                $"UnixSchedulingState.advanceClock: %d{nanoseconds} ns is negative, and every clock this kernel models is monotonic."

        // Unreachable through the public API, which builds no machine but through
        // this function and `UnixSystem.initial`; checked because
        // the overflow test below is only sound for a non-negative uptime.
        if scheduling.NanosecondsSinceBoot < 0L then
            failwith
                $"UnixSchedulingState.advanceClock: the machine has been up for %d{scheduling.NanosecondsSinceBoot} ns, which is negative. No uptime can be; the machine was assembled without this function."

        if nanoseconds > System.Int64.MaxValue - scheduling.NanosecondsSinceBoot then
            failwith
                $"UnixSchedulingState.advanceClock: advancing %d{scheduling.NanosecondsSinceBoot} ns of uptime by %d{nanoseconds} ns passes %d{System.Int64.MaxValue} ns (about 292 years), the longest uptime this kernel represents."

        { scheduling with
            NanosecondsSinceBoot = scheduling.NanosecondsSinceBoot + nanoseconds
        }

    /// How long this machine has been up, to the nanosecond.
    ///
    /// The client that decides when time passes reads this through
    /// `UnixSystem.nanosecondsSinceBoot`, to know how far to `advanceClock`. A
    /// process reads the same instant through `UnixClock.clockGettime`, at the
    /// granularity its flavour reports.
    let nanosecondsSinceBoot (scheduling : UnixSchedulingState) : int64 = scheduling.NanosecondsSinceBoot

    /// Whether `cpu` is running the task whose thread ID is `occupant`. See
    /// `UnixSchedulingState.Occupants`.
    let runs (cpu : CpuId) (occupant : OsThreadId) (scheduling : UnixSchedulingState) : bool =
        // Allocates nothing, unlike `Map.tryFind cpu scheduling.Occupants = Some occupant`, since
        // a client dispatches once per instruction.
        let mutable running = Unchecked.defaultof<OsThreadId>

        scheduling.Occupants.TryGetValue (cpu, &running)
        && OsThreadId.toUInt64 running = OsThreadId.toUInt64 occupant

    /// `scheduling` with the task whose thread ID is `occupant` taken off `cpu`,
    /// if it is running there, leaving the processor idle. See
    /// `UnixSchedulingState.Occupants`.
    let vacate (cpu : CpuId) (occupant : OsThreadId) (scheduling : UnixSchedulingState) : UnixSchedulingState =
        if runs cpu occupant scheduling then
            { scheduling with
                Occupants = Map.remove cpu scheduling.Occupants
            }
        else
            scheduling
