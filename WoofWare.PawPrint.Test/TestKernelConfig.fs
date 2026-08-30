namespace WoofWare.PawPrint.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `KernelConfig` is everything a host may set about the simulated process
/// before a run, and `applyTo` is the only production path that writes those
/// fields onto a kernel. These are the rows about that layer itself — that its
/// defaults agree with the kernel's, that it reaches the field it names, and
/// that it validates rather than passing a bad value through — as opposed to
/// the rows about what any one field *means*, which belong with that field.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKernelConfig =

    [<Test>]
    let ``the instruction cost is configurable and validated`` () : unit =
        // The rate is guest-observable — a guest can measure it by counting work against
        // `Environment.TickCount64`, and it decides whether `SpinWait` reaches its blocking
        // rung — so it is part of the replay contract and belongs in `KernelConfig` rather than
        // being a constant a host cannot see.
        KernelConfig.Default.InstructionCostTicks
        |> shouldEqual EmulatedKernel.defaultInstructionCostTicks

        let configured =
            EmulatedKernel.initial
            |> KernelConfig.applyTo
                { KernelConfig.Default with
                    InstructionCostTicks = 10_000L
                }

        configured.InstructionCostTicks |> shouldEqual 10_000L

        // Zero would freeze the clock, so every guest waiting for time to pass would spin
        // forever: a hang rather than a wrong answer, and the sort of thing a host sweeping the
        // knob could reach by off-by-one. Rejected at the setter, like `ProcessorCount`.
        for bad in [ 0L ; -1L ] do
            let apply () =
                EmulatedKernel.initial
                |> KernelConfig.applyTo
                    { KernelConfig.Default with
                        InstructionCostTicks = bad
                    }
                |> ignore<EmulatedKernel>

            Assert.Throws<Exception> (TestDelegate apply) |> ignore<Exception>
