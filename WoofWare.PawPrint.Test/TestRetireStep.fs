namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedKernel.retireStep` is the per-instruction clock advance: bump `StepCounter` by one
/// and charge `InstructionCostTicks` of virtual time. It exists as its own function only so that
/// the two field writes cost one record copy instead of two — the interpreter performs it once
/// per retired IL instruction, and `EmulatedKernel` has 31 fields.
///
/// That makes the whole risk of the function a divergence from the composition it replaced, so
/// that composition is the oracle here: bumping `StepCounter` by record-copy and then calling the
/// validating setter `withVirtualClockTicks`. The properties below assert the two agree on the
/// resulting kernel *and* on which inputs are rejected, because collapsing the copies must not
/// quietly collapse the validation with it. `EmulatedKernel.withInstructionCostTicks` rejects a
/// cost below 1 and `KernelConfig.applyTo` is the only production path that writes the field, so
/// the kernels below — assembled by record-copy, which bypasses that setter — are reaching the
/// same hole `validateVirtualClockTicks`' own comment cites for its negative check.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRetireStep =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// The composition `retireStep` replaces, kept verbatim so it can act as the oracle.
    let private byComposition (kernel : EmulatedKernel) : EmulatedKernel =
        { kernel with
            StepCounter = kernel.StepCounter + 1L
        }
        |> EmulatedKernel.withVirtualClockTicks (kernel.VirtualClockTicks + kernel.InstructionCostTicks)

    /// Run `f`, reporting either the value or the fact that it threw. Which inputs are rejected is
    /// half of what is being compared, so a throw is an outcome rather than a test failure.
    let private outcome (f : unit -> 'a) : Result<'a, unit> =
        try
            Ok (f ())
        with _ ->
            Error ()

    let private kernelWith (clock : int64) (cost : int64) (step : int64) : EmulatedKernel =
        { EmulatedKernel.initial with
            VirtualClockTicks = clock
            InstructionCostTicks = cost
            StepCounter = step
        }

    /// Clock and cost both in the ordinary range: `retireStep` must produce exactly the kernel the
    /// composition does.
    [<Test>]
    let ``agrees with the composition it replaces`` () =
        let gen =
            gen {
                let! clock = Gen.choose64 (0L, 1_000_000L)
                let! cost = Gen.choose64 (1L, 1_000L)
                let! step = Gen.choose64 (0L, 1_000_000L)
                return clock, cost, step
            }

        let property =
            Prop.forAll
                (Arb.fromGen gen)
                (fun (clock, cost, step) ->
                    let kernel = kernelWith clock cost step

                    let expected = byComposition kernel
                    let actual = EmulatedKernel.retireStep kernel

                    actual.StepCounter |> shouldEqual expected.StepCounter
                    actual.VirtualClockTicks |> shouldEqual expected.VirtualClockTicks
                    actual |> shouldEqual expected
                )

        Check.One (propertyConfig, property)

    /// The advance is exactly one step and one instruction's worth of ticks. Stated directly as
    /// well as against the oracle, so that a mistake duplicated into `byComposition` is still
    /// caught.
    [<Test>]
    let ``advances by exactly one step and one instruction cost`` () =
        let gen =
            gen {
                let! clock = Gen.choose64 (0L, 1_000_000L)
                let! cost = Gen.choose64 (1L, 1_000L)
                let! step = Gen.choose64 (0L, 1_000_000L)
                return clock, cost, step
            }

        let property =
            Prop.forAll
                (Arb.fromGen gen)
                (fun (clock, cost, step) ->
                    let actual = EmulatedKernel.retireStep (kernelWith clock cost step)

                    actual.StepCounter |> shouldEqual (step + 1L)
                    actual.VirtualClockTicks |> shouldEqual (clock + cost)
                )

        Check.One (propertyConfig, property)

    /// Every other field must survive untouched. `EmulatedKernel` has 31 of them, and a `with`
    /// expression that named the wrong one would still satisfy the two properties above.
    [<Test>]
    let ``leaves every other field alone`` () =
        let kernel = kernelWith 500L 7L 11L
        let actual = EmulatedKernel.retireStep kernel

        actual
        |> shouldEqual
            { kernel with
                StepCounter = 12L
                VirtualClockTicks = 507L
            }

    /// A cost of zero freezes the clock and a negative one rewinds it. A record-copy can write
    /// either past `withInstructionCostTicks`, so `retireStep` must answer them exactly as the
    /// composition did — this is the property that fails if the fused copy skips validation.
    [<Test>]
    let ``rejects a non-advancing cost exactly as the composition does`` () =
        let gen =
            gen {
                let! clock = Gen.choose64 (0L, 1_000_000L)
                let! cost = Gen.choose64 (-1_000L, 0L)
                let! step = Gen.choose64 (0L, 1_000_000L)
                return clock, cost, step
            }

        let property =
            Prop.forAll
                (Arb.fromGen gen)
                (fun (clock, cost, step) ->
                    let kernel = kernelWith clock cost step

                    let expected = outcome (fun () -> byComposition kernel)
                    let actual = outcome (fun () -> EmulatedKernel.retireStep kernel)

                    // A zero cost leaves the clock where it is, which the monotonicity check permits; a
                    // negative one moves it backwards, which it does not. Whichever this input is, both
                    // implementations must agree.
                    match expected, actual with
                    | Ok e, Ok a -> a |> shouldEqual e
                    | Error (), Error () -> ()
                    | _ -> failwith $"disagreed on cost %d{cost}: composition %A{expected}, retireStep %A{actual}"
                )

        Check.One (propertyConfig, property)

    /// The clock horizon. A clock near `maxMonotonicTimestampClockTicks` must fault on the step
    /// that would cross it, and must fault in both implementations on the same input.
    [<Test>]
    let ``rejects crossing the clock horizon exactly as the composition does`` () =
        let horizon = EmulatedKernel.maxMonotonicTimestampClockTicks

        let gen =
            gen {
                let! belowHorizon = Gen.choose64 (0L, 1_000L)
                let! cost = Gen.choose64 (1L, 2_000L)
                return horizon - belowHorizon, cost
            }

        let property =
            Prop.forAll
                (Arb.fromGen gen)
                (fun (clock, cost) ->
                    let kernel = kernelWith clock cost 0L

                    let expected = outcome (fun () -> byComposition kernel)
                    let actual = outcome (fun () -> EmulatedKernel.retireStep kernel)

                    match expected, actual with
                    | Ok e, Ok a -> a |> shouldEqual e
                    | Error (), Error () -> ()
                    | _ ->
                        failwith
                            $"disagreed at clock %d{clock} cost %d{cost}: composition %A{expected}, retireStep %A{actual}"
                )

        Check.One (propertyConfig, property)

    /// Absolute statement of the monotonicity rejection, deliberately not phrased against
    /// `byComposition`.
    ///
    /// The oracle properties above cannot cover this. `byComposition` reaches the same shared
    /// `validateVirtualClockTicks` that `retireStep` does, so deleting the monotonicity check
    /// moves oracle and implementation together and they go on agreeing — a mutation battery
    /// confirmed exactly that: with `if ticks < kernel.VirtualClockTicks then` replaced by
    /// `if false then`, every comparison property stayed green. Only an assertion about what the
    /// clock is *absolutely* required to do can catch it.
    [<Test>]
    let ``a negative instruction cost is rejected outright`` () =
        let gen =
            gen {
                let! clock = Gen.choose64 (1L, 1_000_000L)
                let! cost = Gen.choose64 (-1_000L, -1L)
                return clock, cost
            }

        let property =
            Prop.forAll
                (Arb.fromGen gen)
                (fun (clock, cost) ->
                    match outcome (fun () -> EmulatedKernel.retireStep (kernelWith clock cost 0L)) with
                    | Error () -> ()
                    | Ok k ->
                        failwith
                            $"clock %d{clock} with cost %d{cost} was accepted, moving the clock to %d{k.VirtualClockTicks}; the virtual clock is monotonic by construction"
                )

        Check.One (propertyConfig, property)

    /// The other half of the boundary, so the test above is not passing because `retireStep`
    /// rejects everything. A zero cost leaves the clock exactly where it was, which monotonicity
    /// permits — it is `withInstructionCostTicks`' "must be >= 1" that a zero violates, and
    /// re-enforcing that rule is not this function's job.
    [<Test>]
    let ``a zero instruction cost is accepted and freezes the clock`` () =
        let actual = EmulatedKernel.retireStep (kernelWith 4_096L 0L 7L)

        actual.VirtualClockTicks |> shouldEqual 4_096L
        actual.StepCounter |> shouldEqual 8L

    /// Non-vacuity for the horizon property: the generator must actually straddle the boundary,
    /// or the test above passes by never reaching a rejection at all.
    [<Test>]
    let ``horizon generator straddles the boundary`` () =
        let horizon = EmulatedKernel.maxMonotonicTimestampClockTicks

        let outcomes =
            [ 0L .. 1000L ]
            |> List.map (fun belowHorizon ->
                let cost = 500L
                outcome (fun () -> EmulatedKernel.retireStep (kernelWith (horizon - belowHorizon) cost 0L))
            )

        outcomes |> List.filter Result.isOk |> List.isEmpty |> shouldEqual false
        outcomes |> List.filter Result.isError |> List.isEmpty |> shouldEqual false
