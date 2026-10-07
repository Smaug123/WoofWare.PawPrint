namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open WoofWare.PosixKernel

/// What the end of a process leaves of the machine it ran on.
[<RequireQualifiedAccess>]
module EndedMachine =

    /// Assert that `ended` is `before`'s machine with the process gone from it:
    /// no thread ID is live, no call holds a description, the process is not
    /// on the machine and stands in no directory, and nothing else has moved.
    /// Its descriptors are not closed, so each description is still counted as
    /// named by as many as before.
    let assertTasksGone<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (before : UnixSystem<'Task, 'Handler>)
        (ended : EndedProcess<'Task, 'Handler>)
        : unit
        =
        ThreadIdAllocator.live ended.Machine.ThreadIds |> shouldEqual Set.empty
        ProcessIdTable.live ended.Machine.ProcessIds |> shouldEqual Set.empty
        ended.Machine.CurrentDirectories |> shouldEqual Map.empty

        let openFiles = ended.Machine.OpenFiles

        OpenFileTable.descriptions openFiles
        |> shouldEqual (OpenFileTable.descriptions before.Machine.OpenFiles)

        for id in OpenFileTable.descriptions openFiles |> Map.keys do
            OpenFileTable.holdCount id openFiles |> shouldEqual (Some 0)

            OpenFileTable.descriptorCount id openFiles
            |> shouldEqual (OpenFileTable.descriptorCount id before.Machine.OpenFiles)

        { ended.Machine with
            ThreadIds = before.Machine.ThreadIds
            OpenFiles = before.Machine.OpenFiles
            ProcessIds = before.Machine.ProcessIds
            CurrentDirectories = before.Machine.CurrentDirectories
        }
        |> shouldEqual before.Machine
