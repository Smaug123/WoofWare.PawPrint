namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ClockPal.firstTickAtOrAfter`, which turns a kernel park's deadline (nanoseconds since
/// boot) into the virtual-clock tick the idle jump may advance to. It must round up: a
/// jump that landed short of the deadline would leave the kernel's `DeadlinePassed`
/// false, and the parked call would not time out.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKernelDeadlineTicks =

    /// The least tick whose nanoseconds reach `nanoseconds`, computed in `decimal`, which
    /// holds every `int64` times 100 exactly.
    let private oracle (nanoseconds : int64) : int64 =
        System.Math.Ceiling (decimal nanoseconds / decimal ClockPal.nanosecondsPerTick)
        |> int64

    [<Test>]
    let ``the first tick at or after a deadline is its ceiling`` () : unit =
        let nearTicks =
            Gen.zip (Gen.choose (-1000, 1000)) (Gen.choose (-150, 150))
            |> Gen.map (fun (ticks, offset) -> int64 ticks * ClockPal.nanosecondsPerTick + int64 offset)

        let anywhere = Gen.oneof [ nearTicks ; ArbMap.defaults |> ArbMap.generate<int64> ]

        let property =
            Prop.forAll (
                Arb.fromGen (
                    Gen.oneof
                        [
                            anywhere
                            Gen.elements [ System.Int64.MaxValue ; System.Int64.MinValue ; 0L ]
                        ]
                )
            )
            <| fun nanoseconds -> ClockPal.firstTickAtOrAfter nanoseconds |> shouldEqual (oracle nanoseconds)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, property)

    [<Test>]
    let ``a deadline on a tick boundary is that tick, and one nanosecond past it is the next`` () : unit =
        ClockPal.firstTickAtOrAfter 500L |> shouldEqual 5L
        ClockPal.firstTickAtOrAfter 501L |> shouldEqual 6L
        ClockPal.firstTickAtOrAfter 499L |> shouldEqual 5L
