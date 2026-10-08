namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// Parks no syscall could make, for tests that `UnixSystem.checkInvariants`
/// reports them.
[<RequireQualifiedAccess>]
module ForgedPark =

    /// `system` with the unparked task `task` parked in `parked`, at the next
    /// park ordinal, holding those of the descriptions it names that the open
    /// file table holds. `UnixWait.park` refuses a park on a description the
    /// table does not hold, which is exactly the park a test of
    /// `ParkedOnAbsentDescription` needs.
    let onAbsent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (parked : ParkedSyscall)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        if (UnixTaskTable.parkOf task system.Tasks).IsSome then
            failwith $"ForgedPark.onAbsent: task %O{task} is parked already"

        let (ParkOrdinal.ParkOrdinal ordinal) = system.Machine.NextParkOrdinal

        let openFiles =
            ParkedSyscall.descriptions parked
            |> List.filter (fun id -> (OpenFileTable.tryFind id system.Machine.OpenFiles).IsSome)
            |> List.fold (fun openFiles id -> OpenFileTable.hold id openFiles) system.Machine.OpenFiles

        { system with
            Machine =
                { system.Machine with
                    OpenFiles = openFiles
                    NextParkOrdinal = ParkOrdinal.ParkOrdinal (ordinal + 1L)
                }
            Tasks =
                UnixTaskTable.withPark
                    task
                    {
                        Syscall = parked
                        Ordinal = system.Machine.NextParkOrdinal
                    }
                    system.Tasks
        }
