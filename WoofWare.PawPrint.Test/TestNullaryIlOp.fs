namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNullaryIlOp =

    type private Int32DivUnCase =
        {
            Numerator : int32
            Denominator : int32
        }

    type private Int64DivUnCase =
        {
            Numerator : int64
            Denominator : int64
        }

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private genNonZeroInt32Bits : Gen<int32> =
        gen {
            let! highBit = ArbMap.defaults |> ArbMap.generate<bool>
            let! lowBits = Gen.choose (1, System.Int32.MaxValue)

            if highBit then
                return int32 (uint32 lowBits ||| 0x80000000u)
            else
                return int32 lowBits
        }

    let private genInt32DivUnCase : Gen<Int32DivUnCase> =
        gen {
            let! numerator = ArbMap.defaults |> ArbMap.generate<int32>
            let! denominator = genNonZeroInt32Bits

            return
                {
                    Numerator = numerator
                    Denominator = denominator
                }
        }

    let private genNonZeroInt64Bits : Gen<int64> =
        gen {
            let! highBit = ArbMap.defaults |> ArbMap.generate<bool>
            let! raw = ArbMap.defaults |> ArbMap.generate<int64>
            let lowBits = raw &&& System.Int64.MaxValue

            let candidate =
                if highBit then
                    lowBits ||| System.Int64.MinValue
                else
                    lowBits

            return if candidate = 0L then 1L else candidate
        }

    let private genInt64DivUnCase : Gen<Int64DivUnCase> =
        gen {
            let! numerator = ArbMap.defaults |> ArbMap.generate<int64>
            let! denominator = genNonZeroInt64Bits

            return
                {
                    Numerator = numerator
                    Denominator = denominator
                }
        }

    [<Test>]
    let ``Div_un on int32 follows unsigned 32-bit division`` () : unit =
        let mutable highBitDenominators = 0
        let mutable lowBitDenominators = 0

        let property (case : Int32DivUnCase) : unit =
            if uint32 case.Denominator >= 0x80000000u then
                highBitDenominators <- highBitDenominators + 1
            else
                lowBitDenominators <- lowBitDenominators + 1

            let expected = uint32 case.Numerator / uint32 case.Denominator |> int32<uint32>

            match
                NullaryIlOp.divUnValues (EvalStackValue.Int32 case.Numerator) (EvalStackValue.Int32 case.Denominator)
            with
            | EvalStackValue.Int32 actual -> actual |> shouldEqual expected
            | other -> failwith $"Expected Int32 Div_un result, got %O{other}"

        Check.One (config, Prop.forAll (Arb.fromGen genInt32DivUnCase) property)

        if highBitDenominators < 100 || lowBitDenominators < 100 then
            failwith
                $"Div_un int32 generator was unbalanced: high-bit denominators %d{highBitDenominators}, low-bit denominators %d{lowBitDenominators}"

    [<Test>]
    let ``Div_un on native int follows unsigned native-width division`` () : unit =
        let mutable highBitDenominators = 0
        let mutable lowBitDenominators = 0

        let property (case : Int64DivUnCase) : unit =
            if case.Denominator < 0L then
                highBitDenominators <- highBitDenominators + 1
            else
                lowBitDenominators <- lowBitDenominators + 1

            let expected =
                uint64<int64> case.Numerator / uint64<int64> case.Denominator |> int64<uint64>

            match
                NullaryIlOp.divUnValues
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim case.Numerator))
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim case.Denominator))
            with
            | EvalStackValue.NativeInt (NativeIntSource.Verbatim actual) -> actual |> shouldEqual expected
            | other -> failwith $"Expected native int Div_un result, got %O{other}"

        Check.One (config, Prop.forAll (Arb.fromGen genInt64DivUnCase) property)

        if highBitDenominators < 100 || lowBitDenominators < 100 then
            failwith
                $"Div_un native-int generator was unbalanced: high-bit denominators %d{highBitDenominators}, low-bit denominators %d{lowBitDenominators}"
