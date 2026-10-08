namespace WoofWare.PosixKernel

/// Where each flavour takes a new process's ID from.
[<RequireQualifiedAccess>]
type ProcessIdCounter =
    internal
    /// Linux takes a process's ID from the counter it takes thread IDs from
    /// (`ThreadIdAllocator`): the ID is the process's first task's thread ID.
    | ThreadIds
    /// Darwin takes it from a counter of its own, unrelated to its 64-bit
    /// thread IDs. `next` is the ID the next process gets.
    | Darwin of next : int32

/// The machine's process table, as far as this library keeps one: where the
/// next process's ID comes from, and the ID of every process on the machine,
/// which no new process is given.
type internal ProcessIdTable =
    {
        Counter : ProcessIdCounter
        /// Every process on the machine. A process joins it when it is
        /// launched and leaves it when it ends.
        Live : Set<ProcessId>
    }

[<RequireQualifiedAccess>]
module ProcessIdTable =

    /// Darwin's `PID_MAX`: a Darwin process ID is below it. Read from xnu's
    /// source (`bsd/sys/proc_internal.h`), not measured. xnu wraps its counter
    /// back to 100 on reaching it, skipping IDs in use; that has not been
    /// measured either, so this library refuses to go past it rather than wrap.
    [<Literal>]
    let darwinPidMax : int32 = 99999

    /// A Linux machine's table, with no process on it.
    let internal linux : ProcessIdTable =
        {
            Counter = ProcessIdCounter.ThreadIds
            Live = Set.empty
        }

    /// A Darwin machine's table whose first process is `first`, which is not
    /// on it yet: the counter is past `first`.
    let internal darwinAfter (first : ProcessId) : ProcessIdTable =
        {
            Counter = ProcessIdCounter.Darwin (ProcessId.toInt32 first + 1)
            Live = Set.empty
        }

    /// Every process on the machine.
    let internal live (table : ProcessIdTable) : Set<ProcessId> = table.Live

    /// `pid` is a process on the machine from now on. Loudly partial on one
    /// that already is: no kernel hands out an ID in use.
    let internal add (pid : ProcessId) (table : ProcessIdTable) : ProcessIdTable =
        if Set.contains pid table.Live then
            failwith
                $"ProcessIdTable.add: process ID %O{pid} is already a live process's (this is a bug in this library)."

        { table with
            Live = Set.add pid table.Live
        }

    /// The process `pid` has ended. Loudly partial on one not on the machine.
    let internal remove (pid : ProcessId) (table : ProcessIdTable) : ProcessIdTable =
        if not (Set.contains pid table.Live) then
            failwith $"ProcessIdTable.remove: process ID %O{pid} is no live process's (this is a bug in this library)."

        { table with
            Live = Set.remove pid table.Live
        }

    /// On a Darwin machine, the ID the next process gets, which the counter is
    /// then past; `None` once the counter has reached `darwinPidMax`. The ID is
    /// not on the machine until `add` puts it there.
    ///
    /// Loudly partial on a Linux machine's table, whose IDs come from its thread
    /// IDs.
    let internal nextDarwin (table : ProcessIdTable) : (ProcessId * ProcessIdTable) option =
        match table.Counter with
        | ProcessIdCounter.ThreadIds ->
            failwith
                "ProcessIdTable.nextDarwin: a Linux machine takes a process's ID from its thread IDs (this is a bug in this library)."
        | ProcessIdCounter.Darwin next ->
            if next >= darwinPidMax then
                None
            else
                Some (
                    ProcessId.parseOrFail "ProcessIdTable.nextDarwin" next,
                    { table with
                        Counter = ProcessIdCounter.Darwin (next + 1)
                    }
                )
