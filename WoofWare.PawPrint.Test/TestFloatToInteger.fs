namespace WoofWare.PawPrint.Test

open System
open System.Numerics
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFloatToInteger =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 20000

    /// Every double the conversions treat specially, and its neighbours: each target's
    /// bounds (and the bounds of the targets whose result a small conversion wraps),
    /// half-integers either side of them, zeroes, infinities and NaNs of both signs.
    let private boundaryDoubles : float list =
        let powers =
            [ 0 ; 7 ; 8 ; 15 ; 16 ; 31 ; 32 ; 52 ; 53 ; 63 ; 64 ]
            |> List.map (fun k -> Math.Pow (2.0, float k))

        let around (b : float) : float list =
            [
                b
                b - 1.0
                b + 1.0
                b - 0.5
                b + 0.5
                Math.BitDecrement b
                Math.BitIncrement b
            ]

        let magnitudes = powers |> List.collect around

        [
            yield! magnitudes
            yield! magnitudes |> List.map (fun x -> -x)
            0.0
            -0.0
            0.5
            -0.5
            Double.Epsilon
            -Double.Epsilon
            Double.MaxValue
            Double.MinValue
            Double.PositiveInfinity
            Double.NegativeInfinity
            BitConverter.UInt64BitsToDouble 0x7FF8000000000000UL
            BitConverter.UInt64BitsToDouble 0xFFF8000000000000UL
            BitConverter.UInt64BitsToDouble 0x7FF0000000000001UL
            BitConverter.UInt64BitsToDouble 0xFFF0000000000001UL
        ]

    /// Doubles whose magnitude is spread evenly over the exponents at which the integer
    /// targets' ranges end, so that out-of-range, in-range and straddling values all occur
    /// often. A uniform bit pattern would almost always be far out of every range.
    let private genNearRange : Gen<float> =
        gen {
            let! exponent = Gen.choose (-2, 66)
            let! fraction = Gen.choose64 (0L, 0xF_FFFF_FFFF_FFFFL)
            let! negative = Gen.elements [ false ; true ]

            let magnitude =
                BitConverter.UInt64BitsToDouble ((uint64 (exponent + 1023) <<< 52) ||| uint64 fraction)

            return if negative then -magnitude else magnitude
        }

    let private genDouble : Gen<float> =
        Gen.frequency
            [
                3, genNearRange
                1, Gen.elements boundaryDoubles
                1,
                Gen.choose64 (Int64.MinValue, Int64.MaxValue)
                |> Gen.map BitConverter.Int64BitsToDouble
            ]

    let private genSingle : Gen<float32> =
        Gen.frequency
            [
                3, genNearRange |> Gen.map float32
                1, Gen.elements boundaryDoubles |> Gen.map float32
                1,
                Gen.choose (Int32.MinValue, Int32.MaxValue)
                |> Gen.map BitConverter.Int32BitsToSingle
            ]

    /// CoreCLR's rule for an unchecked conversion to a target of at least 32 bits, stated
    /// directly rather than executed: NaN is 0, and anything else is truncated toward zero
    /// and then clamped into the target's range.
    let private saturate (lower : BigInteger) (upper : BigInteger) (x : float) : BigInteger =
        if Double.IsNaN x then
            BigInteger.Zero
        elif Double.IsPositiveInfinity x then
            upper
        elif Double.IsNegativeInfinity x then
            lower
        else
            BigInteger.Min (upper, BigInteger.Max (lower, BigInteger (Math.Truncate x)))

    let private reference64 (x : float) : int64 =
        saturate (BigInteger Int64.MinValue) (BigInteger Int64.MaxValue) x |> int64

    let private referenceU64 (x : float) : uint64 =
        saturate BigInteger.Zero (BigInteger UInt64.MaxValue) x |> uint64

    let private reference32 (x : float) : int32 =
        saturate (BigInteger Int32.MinValue) (BigInteger Int32.MaxValue) x |> int32

    let private referenceU32 (x : float) : uint32 =
        saturate BigInteger.Zero (BigInteger UInt32.MaxValue) x |> uint32

    /// A target narrower than 32 bits takes the low bits of the int32 conversion; it does
    /// not saturate to its own range. `fgMorphExpandCast` splits every such conversion
    /// into a saturating one to int32 followed by a truncating integer narrowing.
    let private lowBits (bits : int) (x : float) : BigInteger =
        let modulus = BigInteger.One <<< bits
        let r = BigInteger.Remainder (BigInteger (reference32 x), modulus)
        if r.Sign < 0 then r + modulus else r

    let private signed (bits : int) (unsigned : BigInteger) : BigInteger =
        if unsigned >= (BigInteger.One <<< (bits - 1)) then
            unsigned - (BigInteger.One <<< bits)
        else
            unsigned

    let private referenceI8 (x : float) : int8 = lowBits 8 x |> signed 8 |> int8
    let private referenceU8 (x : float) : uint8 = lowBits 8 x |> uint8
    let private referenceI16 (x : float) : int16 = lowBits 16 x |> signed 16 |> int16
    let private referenceU16 (x : float) : uint16 = lowBits 16 x |> uint16

    /// Every conversion the interpreter offers, as (name, implementation, reference, host).
    /// The host column is the test host's own `conv.*`, which on .NET 9 and later is
    /// CoreCLR's saturating rule on every platform, and so a third, independent opinion.
    let private conversions : (string * (float -> Int128) * (float -> Int128) * (float -> Int128)) list =
        [
            "int64",
            (fun x -> Int128.op_Implicit (FloatToInteger.toInt64 x)),
            (fun x -> Int128.op_Implicit (reference64 x)),
            (fun x -> Int128.op_Implicit (int64 x))
            "uint64",
            (fun x -> Int128.op_Implicit (FloatToInteger.toUInt64 x)),
            (fun x -> Int128.op_Implicit (referenceU64 x)),
            (fun x -> Int128.op_Implicit (uint64 x))
            "int32",
            (fun x -> Int128.op_Implicit (FloatToInteger.toInt32 x)),
            (fun x -> Int128.op_Implicit (reference32 x)),
            (fun x -> Int128.op_Implicit (int32 x))
            "uint32",
            (fun x -> Int128.op_Implicit (FloatToInteger.toUInt32 x)),
            (fun x -> Int128.op_Implicit (referenceU32 x)),
            (fun x -> Int128.op_Implicit (uint32 x))
            "int16",
            (fun x -> Int128.op_Implicit (FloatToInteger.toInt16 x)),
            (fun x -> Int128.op_Implicit (referenceI16 x)),
            (fun x -> Int128.op_Implicit (int16 x))
            "uint16",
            (fun x -> Int128.op_Implicit (FloatToInteger.toUInt16 x)),
            (fun x -> Int128.op_Implicit (referenceU16 x)),
            (fun x -> Int128.op_Implicit (uint16 x))
            "int8",
            (fun x -> Int128.op_Implicit (FloatToInteger.toInt8 x)),
            (fun x -> Int128.op_Implicit (referenceI8 x)),
            (fun x -> Int128.op_Implicit (int8 x))
            "uint8",
            (fun x -> Int128.op_Implicit (FloatToInteger.toUInt8 x)),
            (fun x -> Int128.op_Implicit (referenceU8 x)),
            (fun x -> Int128.op_Implicit (uint8 x))
        ]

    let private describe (x : float) : string =
        $"%.17g{x} (0x%016x{BitConverter.DoubleToUInt64Bits x})"

    [<Test>]
    let ``every conversion matches the stated rule`` () : unit =
        let property (x : float) : bool =
            for name, implementation, reference, _ in conversions do
                let actual = implementation x
                let expected = reference x

                if actual <> expected then
                    failwith $"%s{name} of %s{describe x}: got %O{actual}, expected %O{expected}"

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genDouble) property)

    [<Test>]
    let ``every boundary value matches the stated rule`` () : unit =
        for x in boundaryDoubles do
            for name, implementation, reference, _ in conversions do
                let actual = implementation x
                let expected = reference x

                if actual <> expected then
                    failwith $"%s{name} of %s{describe x}: got %O{actual}, expected %O{expected}"

    /// This checks the statement of the rule, not the implementation: if it fails, the
    /// reference above has misdescribed CoreCLR. It holds only on a host whose own
    /// conversions saturate, which every .NET 9+ host does and which the test project's
    /// target framework guarantees.
    [<Test>]
    let ``the stated rule is the test host's own conversion`` () : unit =
        let property (x : float) : bool =
            for name, _, reference, host in conversions do
                let actual = host x
                let expected = reference x

                if actual <> expected then
                    failwith $"host %s{name} of %s{describe x}: got %O{actual}, reference says %O{expected}"

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genDouble) property)

    /// The interpreter converts a float32 by widening it to double first, which is exact.
    /// CoreCLR converts a float32 directly; this checks the two agree.
    [<Test>]
    let ``converting a float32 via its widening matches the host's direct float32 conversion`` () : unit =
        let property (x : float32) : bool =
            let wide = float x

            let pairs : (string * Int128 * Int128) list =
                [
                    "int64", Int128.op_Implicit (FloatToInteger.toInt64 wide), Int128.op_Implicit (int64 x)
                    "uint64", Int128.op_Implicit (FloatToInteger.toUInt64 wide), Int128.op_Implicit (uint64 x)
                    "int32", Int128.op_Implicit (FloatToInteger.toInt32 wide), Int128.op_Implicit (int32 x)
                    "uint32", Int128.op_Implicit (FloatToInteger.toUInt32 wide), Int128.op_Implicit (uint32 x)
                    "int16", Int128.op_Implicit (FloatToInteger.toInt16 wide), Int128.op_Implicit (int16 x)
                    "uint16", Int128.op_Implicit (FloatToInteger.toUInt16 wide), Int128.op_Implicit (uint16 x)
                    "int8", Int128.op_Implicit (FloatToInteger.toInt8 wide), Int128.op_Implicit (int8 x)
                    "uint8", Int128.op_Implicit (FloatToInteger.toUInt8 wide), Int128.op_Implicit (uint8 x)
                ]

            for name, actual, expected in pairs do
                if actual <> expected then
                    failwith
                        $"%s{name} of float32 %.9g{x} (0x%08x{BitConverter.SingleToUInt32Bits x}): got %O{actual}, host says %O{expected}"

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSingle) property)
