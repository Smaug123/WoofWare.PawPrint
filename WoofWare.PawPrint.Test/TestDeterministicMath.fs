namespace WoofWare.PawPrint.Test

open System
open System.Numerics
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDeterministicMath =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// A finite double drawn uniformly over the whole biased-exponent range, so that
    /// subnormals, tiny normals and near-overflow values all appear. FsCheck's own float
    /// generator concentrates on small "nice" values, which would leave the extreme ends
    /// of `roundToDouble`'s range — exactly where its subnormal and overflow branches live
    /// — untested.
    let private genFiniteDouble : Gen<float> =
        gen {
            let! sign = Gen.elements [ 0UL ; 1UL ]
            // 0 is the subnormal/zero exponent; 2047 is infinity/NaN. Include 0, exclude 2047.
            let! biasedExponent = Gen.choose (0, 2046)
            let! fraction = Gen.choose64 (0L, 0xF_FFFF_FFFF_FFFFL)

            return
                BitConverter.UInt64BitsToDouble ((sign <<< 63) ||| (uint64 biasedExponent <<< 52) ||| uint64 fraction)
        }

    let private genPositiveFiniteDouble : Gen<float> =
        genFiniteDouble
        |> Gen.map (fun d ->
            let magnitude = abs d
            if magnitude = 0.0 then Double.Epsilon else magnitude
        )

    /// Bases whose magnitude is close enough to 1 that raising them to an exponent in
    /// [0, 64] does not always overflow or underflow — otherwise the integer-power
    /// properties below would spend most of their draws comparing infinity to infinity.
    let private genModerateDouble : Gen<float> =
        gen {
            let! sign = Gen.elements [ 0UL ; 1UL ]
            let! biasedExponent = Gen.choose (1023 - 16, 1023 + 16)
            let! fraction = Gen.choose64 (0L, 0xF_FFFF_FFFF_FFFFL)

            return
                BitConverter.UInt64BitsToDouble ((sign <<< 63) ||| (uint64 biasedExponent <<< 52) ||| uint64 fraction)
        }

    /// The number of representable doubles strictly between `a` and `b`, plus one; 0 when
    /// they are the same double. Infinity when either is NaN or infinite and they differ.
    let private ulpDistance (a : float) (b : float) : float =
        if a = b then
            0.0
        elif Double.IsNaN a || Double.IsNaN b || Double.IsInfinity a || Double.IsInfinity b then
            infinity
        else

        // Map the IEEE bit patterns onto a monotone integer line, so that adjacent doubles
        // are adjacent integers even across the sign boundary.
        let toOrdered (v : float) : BigInteger =
            let bits = BitConverter.DoubleToInt64Bits v
            BigInteger (if bits < 0L then Int64.MinValue - bits else bits)

        // Subtract before converting: bit patterns above 2^53 do not survive a round trip
        // through `float`, so differencing them as doubles would quantise a one-ulp gap to
        // zero or four and make this metric useless exactly where doubles are largest.
        float (BigInteger.Abs (toOrdered a - toOrdered b))

    /// The exact value of `x` raised to a non-negative integer power, correctly rounded.
    /// `x` is a dyadic rational, so `x^n` is one too and `BigInteger` can hold it exactly;
    /// this is the reference implementation the logarithm-based path is checked against.
    let private exactIntegerPower (x : float) (n : int) : float =
        let mantissa, exponent = DeterministicMath.decompose x
        DeterministicMath.roundToDouble (BigInteger.Pow (mantissa, n)) (exponent * n)

    [<Test>]
    let ``decompose reconstructs the input exactly`` () : unit =
        let property (x : float) : bool =
            let mantissa, exponent = DeterministicMath.decompose x
            // Reconstructing through roundToDouble is exact by construction here: the
            // value being rounded is the input double itself, so no rounding happens.
            DeterministicMath.roundToDouble mantissa exponent = x

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``roundToDouble is the identity on exactly representable values`` () : unit =
        // Scaling the mantissa up by k bits and dropping k from the exponent names the same
        // real number, so the answer must not depend on which of those names it is given.
        let property (x : float, shift : int) : bool =
            let mantissa, exponent = DeterministicMath.decompose x
            let shift = abs shift % 64
            DeterministicMath.roundToDouble (mantissa <<< shift) (exponent - shift) = x

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genFiniteDouble (Gen.choose (0, 63)))) property)

    [<Test>]
    let ``roundToDouble breaks ties to even`` () : unit =
        // In [2^53, 2^54) the representable doubles are the even integers, so an odd
        // integer there is exactly halfway between two of them.
        let twoToThe53 = BigInteger.Pow (BigInteger 2, 53)

        // 2^53 + 1 sits between 2^53 (even mantissa) and 2^53 + 2 (odd mantissa).
        DeterministicMath.roundToDouble (twoToThe53 + BigInteger.One) 0
        |> shouldEqual 9007199254740992.0

        // 2^53 + 3 sits between 2^53 + 2 (odd) and 2^53 + 4 (even).
        DeterministicMath.roundToDouble (twoToThe53 + BigInteger 3) 0
        |> shouldEqual 9007199254740996.0

        // The same rule at the bottom of the subnormal range: 2^-1075 is exactly half of
        // the smallest subnormal, so it rounds down to (even) zero rather than up.
        DeterministicMath.roundToDouble BigInteger.One -1075 |> shouldEqual 0.0

        // Three halves of the smallest subnormal is nearer to two of them than to one.
        DeterministicMath.roundToDouble (BigInteger 3) -1075
        |> shouldEqual (2.0 * Double.Epsilon)

        // Negative values must break ties the same way (to even, not away from zero).
        DeterministicMath.roundToDouble (-(twoToThe53 + BigInteger.One)) 0
        |> shouldEqual -9007199254740992.0

    [<Test>]
    let ``roundToDouble saturates at the ends of the range`` () : unit =
        DeterministicMath.roundToDouble BigInteger.One 1024 |> shouldEqual infinity

        DeterministicMath.roundToDouble BigInteger.MinusOne 1024
        |> shouldEqual -infinity

        DeterministicMath.roundToDouble BigInteger.One 1023
        |> shouldEqual 8.98846567431158E+307

        // Just above the largest double: the true value rounds up past Double.MaxValue.
        let allOnes = BigInteger.Pow (BigInteger 2, 54) - BigInteger.One

        DeterministicMath.roundToDouble allOnes (1023 - 53) |> shouldEqual infinity

        DeterministicMath.roundToDouble BigInteger.One -1074
        |> shouldEqual Double.Epsilon

        DeterministicMath.roundToDouble BigInteger.One -1076 |> shouldEqual 0.0
        DeterministicMath.roundToDouble BigInteger.Zero 0 |> shouldEqual 0.0

    [<Test>]
    let ``exp2 inverts log2`` () : unit =
        // log2 carries far more precision than a double, so composing the two must land
        // back on exactly the input. This pins the whole fixed-point pipeline (the atanh
        // series, the ln 2 constant, the exponential series and the final rounding)
        // without needing any oracle beyond the input itself.
        let property (x : float) : bool =
            DeterministicMath.exp2 (DeterministicMath.log2 x) = x

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPositiveFiniteDouble) property)

    [<Test>]
    let ``log2 is exact on powers of two`` () : unit =
        // Powers of two are the inputs on which the range reduction must produce an atanh
        // argument of exactly zero; if it does not, `Math.Pow(2.0, n)` stops being exact.
        let property (exponent : int) : bool =
            let x = DeterministicMath.roundToDouble BigInteger.One exponent
            DeterministicMath.log2 x = (BigInteger exponent <<< DeterministicMath.fractionBits)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.choose (-1074, 1023))) property)

    [<Test>]
    let ``pow is exact on powers of two`` () : unit =
        let property (baseExponent : int, power : int) : bool =
            let x = DeterministicMath.roundToDouble BigInteger.One baseExponent
            let expected = DeterministicMath.roundToDouble BigInteger.One (baseExponent * power)
            DeterministicMath.pow x (float power) = expected

        // Keep the product in a range where the answer is neither infinite nor zero for
        // most draws, so the property is actually comparing finite values.
        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.choose (-40, 40)) (Gen.choose (-25, 25)))) property
        )

    [<Test>]
    let ``pow of one half equals the correctly rounded square root`` () : unit =
        // IEEE 754 mandates that `sqrt` be correctly rounded, and every platform's `sqrt`
        // is a hardware instruction that obeys that — so `Math.Sqrt` is an oracle that
        // does not vary between machines, unlike the platform `pow`. x^(1/2) can never be
        // an exact tie (a 54-bit odd result would need x to have ~107 significant bits),
        // so correct rounding here means exact agreement, not agreement to within an ulp.
        let property (x : float) : bool =
            DeterministicMath.pow x 0.5 = Math.Sqrt x

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPositiveFiniteDouble) property)

    [<Test>]
    let ``the logarithm path matches the exact rational value for integer exponents`` () : unit =
        // The logarithm path is what every non-integer exponent uses, and it has no
        // independent oracle there. For integer exponents the exact answer *is* computable
        // (in BigInteger), so checking the logarithm path against it measures the accuracy
        // of exactly the machinery the non-integer cases depend on.
        //
        // One ulp of slack: the logarithm path is accurate to far better than half an ulp,
        // so it agrees with the correctly rounded value except when the true result lies
        // within ~2^-200 of a tie, which the exact reference then breaks the other way.
        let property (x : float, power : int) : bool =
            let actual = DeterministicMath.powOfPositiveViaLogarithm x (float power)
            ulpDistance actual (exactIntegerPower x power) <= 1.0

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.map abs genModerateDouble) (Gen.choose (0, 64)))) property
        )

    [<Test>]
    let ``pow matches the exact rational value for integer exponents`` () : unit =
        // `pow` computes small integer exponents by this same exact route, so for those
        // this is a check of the dispatch and of the odd/even sign rule (hence the negative
        // bases) rather than of the arithmetic. The accuracy of the arithmetic is what the
        // preceding property measures, against the same reference but on the path that
        // non-integer exponents actually take.
        let property (x : float, power : int) : bool =
            DeterministicMath.pow x (float power) = exactIntegerPower x power

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genModerateDouble (Gen.choose (0, 64)))) property)

    [<Test>]
    let ``pow is monotone in the exponent`` () : unit =
        // For a base above 1, x^y is increasing in y; below 1 it is decreasing. This holds
        // of the true function, and correct rounding is monotone, so it must hold of ours.
        let property (x : float, y1 : float, y2 : float) : bool =
            let low, high = min y1 y2, max y1 y2
            let atLow = DeterministicMath.pow x low
            let atHigh = DeterministicMath.pow x high

            if x > 1.0 then atLow <= atHigh
            elif x < 1.0 then atLow >= atHigh
            else atLow = atHigh

        let genExponent = Gen.choose (-2000, 2000) |> Gen.map (fun i -> float i / 8.0)

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip3 genPositiveFiniteDouble genExponent genExponent)) property
        )

    [<Test>]
    let ``pow agrees with the host libm to within two ulps`` () : unit =
        // A coarse sanity check, deliberately *not* a specification: the whole point of
        // this module is that PawPrint must not depend on the host's `pow`, which is not
        // correctly rounded and is not required to agree bit-for-bit between platforms.
        // Two ulps comfortably covers the ~0.5 ulp error budget of a typical libm plus our
        // own rounding, while still catching any gross error (wrong constant, wrong series
        // term, misplaced range reduction).
        let property (x : float, y : float) : bool =
            ulpDistance (DeterministicMath.pow x y) (Math.Pow (x, y)) <= 2.0

        let genExponent = Gen.choose (-4096, 4096) |> Gen.map (fun i -> float i / 64.0)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genPositiveFiniteDouble genExponent)) property)

    /// The IEEE 754 / C99 `pow` special cases, as a table of (x, y, expected bits). Bits
    /// rather than values so that the signs of zero, and the distinction between the two
    /// NaN payloads, are actually asserted.
    let private specialCases : (float * float * uint64) list =
        let positiveNaN = 0x7FF8000000000000UL
        // `Double.NaN` in .NET is the *negative* quiet NaN.
        let negativeNaN = 0xFFF8000000000000UL
        let one = 0x3FF0000000000000UL
        let positiveZero = 0x0000000000000000UL
        let negativeZero = 0x8000000000000000UL
        let positiveInfinity = 0x7FF0000000000000UL
        let negativeInfinity = 0xFFF0000000000000UL

        [
            // Anything to the zeroth power is 1, including NaN.
            nan, 0.0, one
            nan, -0.0, one
            0.0, 0.0, one
            -0.0, -0.0, one
            infinity, 0.0, one
            -infinity, 0.0, one
            // 1 to any power is 1, including NaN.
            1.0, nan, one
            1.0, infinity, one
            1.0, -infinity, one
            // Otherwise NaN propagates, payload and all.
            nan, 2.0, negativeNaN
            2.0, nan, negativeNaN
            -1.0, nan, negativeNaN
            // A negative base with a non-integer exponent is a domain error.
            -2.0, 0.5, positiveNaN
            -2.0, 1.5, positiveNaN
            -0.5, -0.5, positiveNaN
            // |x| = 1 with an infinite exponent is 1, not NaN.
            -1.0, infinity, one
            -1.0, -infinity, one
            // Infinite exponents.
            0.5, infinity, positiveZero
            0.5, -infinity, positiveInfinity
            2.0, infinity, positiveInfinity
            2.0, -infinity, positiveZero
            0.0, infinity, positiveZero
            0.0, -infinity, positiveInfinity
            -0.0, infinity, positiveZero
            -0.0, -infinity, positiveInfinity
            // Zero bases: the sign survives only for odd integer exponents.
            0.0, 1.0, positiveZero
            -0.0, 1.0, negativeZero
            -0.0, 2.0, positiveZero
            -0.0, 3.0, negativeZero
            0.0, -1.0, positiveInfinity
            -0.0, -1.0, negativeInfinity
            -0.0, -2.0, positiveInfinity
            -0.0, -3.0, negativeInfinity
            // Infinite bases, same rule.
            infinity, 1.0, positiveInfinity
            infinity, -1.0, positiveZero
            -infinity, 1.0, negativeInfinity
            -infinity, 2.0, positiveInfinity
            -infinity, 3.0, negativeInfinity
            -infinity, -1.0, negativeZero
            -infinity, -2.0, positiveZero
            -infinity, 0.5, positiveInfinity
            // Ordinary negative bases with integer exponents.
            -2.0, 3.0, 0xC020000000000000UL
            -2.0, 2.0, 0x4010000000000000UL
            // A base of -1 with a huge (necessarily even) exponent.
            -1.0, 1e300, one
            // Overflow and underflow of the ordinary path.
            1e300, 3.0, positiveInfinity
            1e-300, 3.0, positiveZero
            2.0, 1024.0, positiveInfinity
            2.0, -1074.0, 0x0000000000000001UL
            2.0, -1075.0, positiveZero
        ]

    [<Test>]
    let ``pow matches the IEEE 754 special cases`` () : unit =
        for x, y, expected in specialCases do
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.pow x y)

            if actual <> expected then
                failwith $"pow(%.17g{x}, %.17g{y}): expected bits %016x{expected}, got %016x{actual}"

    [<Test>]
    let ``the host agrees about the IEEE 754 special cases`` () : unit =
        // The special-case table above is a *specification*, asserted independently of the
        // host. This test additionally records that the host we are differentially tested
        // against agrees with it — apart from the one place where IEEE 754 leaves the
        // answer open, and platforms genuinely differ: the payload of a freshly generated
        // NaN. x86 hardware produces the negative quiet NaN there and Arm the positive one,
        // so a mismatch on exactly those rows is expected, not a bug.
        let quietNaNBits = 0x7FF8000000000000UL

        for x, y, expected in specialCases do
            let host = BitConverter.DoubleToUInt64Bits (Math.Pow (x, y))

            if host <> expected && expected <> quietNaNBits then
                failwith $"host pow(%.17g{x}, %.17g{y}): expected bits %016x{expected}, got %016x{host}"

    [<Test>]
    let ``pow is a pure function of its arguments`` () : unit =
        // The reason this module exists: a replay must produce the same bits, so the result
        // may not depend on anything but the two arguments.
        let property (x : float, y : float) : bool =
            let first = DeterministicMath.pow x y
            let second = DeterministicMath.pow x y
            BitConverter.DoubleToUInt64Bits first = BitConverter.DoubleToUInt64Bits second

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genFiniteDouble genFiniteDouble)) property)
