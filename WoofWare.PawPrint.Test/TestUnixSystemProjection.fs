namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `EmulatedKernel` stores the POSIX state as four fields and hands it to
/// `UnixSystem.step` as one record. The two directions have to be inverses, and
/// nothing else in the suite can tell if they stop being: a syscall's answer is
/// silently lost if `withUnix` drops a part, and a state is silently resurrected
/// if it writes back a part the syscall did not touch.
///
/// The obligation grows: a fifth part of `UnixSystem` that `unix` fills but
/// `withUnix` forgets compiles, and only these rows notice.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSystemProjection =

    /// A kernel differing from the default in each of the four parts at once,
    /// so that a round trip which preserved only some of them fails. All-default
    /// inputs cannot tell "carried across" from "left alone".
    ///
    /// Its leader is a thread no process would have as one, because PawPrint's
    /// leader is always `ThreadId 0`; nothing here reads it but the projection.
    let private distinctive : EmulatedKernel =
        EmulatedKernel.initialImage
        |> UnixBootImage.withProcessorCount 4
        |> UnixBootImage.withCredentials
            "test"
            (Credentials.ofIds (UserId.parseOrFail "test" 7u) (GroupId.parseOrFail "test" 9u) [])
        |> EmulatedKernel.boot
        |> KernelTasks.ensure (ThreadId 3)
        |> fun kernel ->
            { kernel with
                Leader = ThreadId 3
            }

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
        system.Leader |> shouldEqual distinctive.Leader

        let restored = EmulatedKernel.withUnix system EmulatedKernel.initial

        restored.Machine |> shouldEqual distinctive.Machine
        restored.Process |> shouldEqual distinctive.Process
        restored.Tasks |> shouldEqual distinctive.Tasks
        restored.Leader |> shouldEqual distinctive.Leader

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
    let ``mapUnix applies the operation and writes back every part of it`` () : unit =
        // The composition of the two directions, which is how every library
        // operation that spans the three parts is called. Asserted separately
        // because a `mapUnix` that discarded its function's result, or that
        // wrote back the projection it read rather than the one it computed,
        // passes both round-trip rows above.
        let changed =
            distinctive
            |> EmulatedKernel.mapUnix (fun system ->
                let spawned =
                    match UnixTaskLifecycle.spawn system.Leader (ThreadId 4) (CpuId 1) system with
                    | Ok (_, spawned) -> spawned
                    | Error error -> failwith $"spawn failed: %O{error}"

                {
                    Machine =
                        { spawned.Machine with
                            ProcessorCount = 5
                        }
                    Process =
                        { spawned.Process with
                            Credentials =
                                Credentials.ofIds (UserId.parseOrFail "test" 11u) (GroupId.parseOrFail "test" 13u) []
                        }
                    Tasks = spawned.Tasks
                    Leader = ThreadId 4
                }
            )

        changed.Machine.ProcessorCount |> shouldEqual 5

        changed.Process.Credentials.EffectiveUser
        |> shouldEqual (UserId.parseOrFail "test" 11u)

        changed.Tasks.ContainsKey (ThreadId 4) |> shouldEqual true
        changed.Leader |> shouldEqual (ThreadId 4)

        // And the part the operation left alone is still the one it was handed,
        // rather than the default a whole-kernel replacement would restore.
        changed.Tasks.ContainsKey (ThreadId 3) |> shouldEqual true

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
                    Machine =
                        { stepped.Machine with
                            ProcessorCount = count
                        }
                }

            let after = EmulatedKernel.withUnix stepped before

            after.Machine.ProcessorCount = count
            && after.Process = before.Process
            && after.Tasks = before.Tasks
            && after.Leader = before.Leader
            && after.Machine = { before.Machine with
                                   ProcessorCount = count
                               }

        Check.QuickThrowOnFailure property
