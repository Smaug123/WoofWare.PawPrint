namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `EmulatedKernel` stores the POSIX state as three fields and hands it to
/// `UnixSystem.step` as one record. The two directions have to be inverses, and
/// nothing else in the suite can tell if they stop being: a syscall's answer is
/// silently lost if `withUnix` drops a part, and a state is silently resurrected
/// if it writes back a part the syscall did not touch.
///
/// The obligation grows: a fourth part of `UnixSystem` that `unix` fills but
/// `withUnix` forgets compiles, and only these rows notice.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSystemProjection =

    /// A kernel differing from the default in each of the three parts at once,
    /// so that a round trip which preserved only some of them fails. All-default
    /// inputs cannot tell "carried across" from "left alone".
    let private distinctive : EmulatedKernel =
        EmulatedKernel.initial
        |> EmulatedKernel.mapMachine (UnixMachineState.withProcessorCount 4)
        |> EmulatedKernel.mapProcess (UnixProcessState.withUserAndGroupId 7u 9u)
        |> EmulatedKernel.mapTasks (UnixTaskTable.register (ThreadId 3) (CpuId 2) (OsThreadId 11u))

    [<Test>]
    let ``writing back what was read changes nothing`` () : unit =
        distinctive
        |> EmulatedKernel.withUnix (EmulatedKernel.unix distinctive)
        |> shouldEqual distinctive

    [<Test>]
    let ``every part crosses in both directions`` () : unit =
        // Named individually rather than left to the record comparison above,
        // which a `withUnix` that ignored its argument entirely would also pass
        // when handed the kernel's own projection.
        let system = EmulatedKernel.unix distinctive

        system.Machine |> shouldEqual distinctive.Machine
        system.Process |> shouldEqual distinctive.Process
        system.Tasks |> shouldEqual distinctive.Tasks

        let restored = EmulatedKernel.withUnix system EmulatedKernel.initial

        restored.Machine |> shouldEqual distinctive.Machine
        restored.Process |> shouldEqual distinctive.Process
        restored.Tasks |> shouldEqual distinctive.Tasks

    [<Test>]
    let ``the CLR half is left alone`` () : unit =
        // `withUnix` must not be a whole-kernel replacement: the fields that
        // stay in PawPrint because a POSIX kernel would not have them belong to
        // the kernel being written into, not to the system being written back.
        let clrSide =
            EmulatedKernel.initial |> EmulatedKernel.withLastPInvokeError (ThreadId 0) 42

        let restored = EmulatedKernel.withUnix (EmulatedKernel.unix distinctive) clrSide

        EmulatedKernel.lastPInvokeErrorFor (ThreadId 0) restored |> shouldEqual 42
        restored.Machine |> shouldEqual distinctive.Machine

    [<Test>]
    let ``a system stepped from one kernel does not carry another's parts`` () : unit =
        // The hazard the round trip exists to catch, stated as a property over
        // arbitrary processor counts: read, change one part through the library,
        // write back, and only that part may differ.
        let property (count : int) : bool =
            let count = 1 + abs (count % 64)
            let before = distinctive
            let stepped = EmulatedKernel.unix before

            let stepped =
                { stepped with
                    Machine = UnixMachineState.withProcessorCount count stepped.Machine
                }

            let after = EmulatedKernel.withUnix stepped before

            after.Machine.ProcessorCount = count
            && after.Process = before.Process
            && after.Tasks = before.Tasks
            && after.Machine = UnixMachineState.withProcessorCount count before.Machine

        Check.QuickThrowOnFailure property
