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
        let ofBits (bits : uint64) : float = BitConverter.UInt64BitsToDouble bits
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
            // A quiet NaN's payload survives unchanged.
            ofBits 0x7FF8000000000123UL, 2.0, 0x7FF8000000000123UL
            2.0, ofBits 0x7FF8000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF8000000000123UL, 2.0, 0xFFF8000000000123UL
            // A *signaling* NaN must come back quieted -- IEEE 754 requires an operation
            // handed one to raise invalid-operation and deliver a quiet NaN -- but with its
            // sign and payload otherwise intact. Simply returning the operand unchanged is
            // observably wrong, and is what this arm originally did.
            ofBits 0x7FF0000000000123UL, 2.0, 0x7FF8000000000123UL
            2.0, ofBits 0x7FF0000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF0000000000123UL, 2.0, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000001UL, 2.0, 0x7FF8000000000001UL
            // ...and it beats the two overrides, which clause 9.2.1 grants only against a
            // *quiet* NaN. This is where the two hosts part company; see `hostAlternatives`.
            ofBits 0x7FF0000000000123UL, 0.0, 0x7FF8000000000123UL
            ofBits 0x7FF0000000000123UL, -0.0, 0x7FF8000000000123UL
            1.0, ofBits 0x7FF0000000000123UL, 0x7FF8000000000123UL
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

    /// Report every row that failed rather than only the first: a divergence found on a
    /// remote runner we cannot reproduce locally is much cheaper to diagnose whole.
    let private reportFailures (failures : string list) : unit =
        match failures with
        | [] -> ()
        | failures -> failwith (String.concat "\n" failures)

    [<Test>]
    let ``pow matches the IEEE 754 special cases`` () : unit =
        specialCases
        |> List.choose (fun (x, y, expected) ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.pow x y)

            if actual = expected then
                None
            else
                Some $"pow(%.17g{x}, %.17g{y}): expected bits %016x{expected}, got %016x{actual}"
        )
        |> reportFailures

    /// The rows of `specialCases` on which the host libm is allowed to disagree with the
    /// specification, together with the one *other* answer it may give. Keyed on the exact
    /// bits of both arguments rather than on the expected result, so that a later row which
    /// happens to produce those bits does not silently stop being checked; and stated as a
    /// specific alternative rather than a blanket exemption, so that a host doing something
    /// else entirely is still a failure.
    let private hostAlternatives : Map<uint64 * uint64, uint64> =
        let bitsOf (x : float) (y : float) : uint64 * uint64 =
            BitConverter.DoubleToUInt64Bits x, BitConverter.DoubleToUInt64Bits y

        let signallingNaN = BitConverter.UInt64BitsToDouble 0x7FF0000000000123UL
        let negativeNaN = 0xFFF8000000000000UL
        let one = 0x3FF0000000000000UL

        [
            // The payload of a *freshly generated* NaN is left to the implementation, and
            // hardware differs: x86 produces the negative quiet NaN (which is what
            // `Double.NaN` is) and Arm the positive one. glibc reaches these cases through
            // `__math_invalid`, i.e. (x - x) / (x - x), so the payload is the hardware's.
            bitsOf -2.0 0.5, negativeNaN
            bitsOf -2.0 1.5, negativeNaN
            bitsOf -0.5 -0.5, negativeNaN
            // Apple's libm applies pow(x, ±0) = 1 and pow(+1, y) = 1 even to a *signalling*
            // NaN operand, where glibc and clause 9.2.1 hand back a quiet NaN. See the
            // comment on `DeterministicMath.pow`: we specify glibc's answer, so it is macOS
            // that needs the exemption here.
            bitsOf signallingNaN 0.0, one
            bitsOf signallingNaN -0.0, one
            bitsOf 1.0 signallingNaN, one
        ]
        |> Map.ofList

    [<Test>]
    let ``the host agrees about the IEEE 754 special cases`` () : unit =
        // The special-case table above is a *specification*, asserted independently of the
        // host. This test additionally records that the host we are differentially tested
        // against agrees with it, except in the handful of places where IEEE 754 leaves the
        // answer open and real platforms genuinely differ. Those are enumerated in
        // `hostAlternatives`, which still pins the host to one of exactly two answers.
        specialCases
        |> List.choose (fun (x, y, expected) ->
            let host = BitConverter.DoubleToUInt64Bits (Math.Pow (x, y))

            let key = BitConverter.DoubleToUInt64Bits x, BitConverter.DoubleToUInt64Bits y

            let permitted =
                match Map.tryFind key hostAlternatives with
                | None -> [ expected ]
                | Some alternative -> [ expected ; alternative ]

            if List.contains host permitted then
                None
            else

            let permitted = permitted |> List.map (sprintf "%016x") |> String.concat " or "

            Some $"host pow(%.17g{x}, %.17g{y}): expected bits {permitted}, got %016x{host}"
        )
        |> reportFailures

    [<Test>]
    let ``pow is a pure function of its arguments`` () : unit =
        // The reason this module exists: a replay must produce the same bits, so the result
        // may not depend on anything but the two arguments.
        let property (x : float, y : float) : bool =
            let first = DeterministicMath.pow x y
            let second = DeterministicMath.pow x y
            BitConverter.DoubleToUInt64Bits first = BitConverter.DoubleToUInt64Bits second

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genFiniteDouble genFiniteDouble)) property)

    [<Test>]
    let ``pi agrees with its published decimal expansion`` () : unit =
        // `DeterministicMath` computes pi from Machin's formula rather than transcribing it,
        // so this is the one place the result is checked against an outside source. Sixty
        // digits is far more than the double-precision answers need and is enough to catch
        // any wrong arctangent coefficient, which would show up within the first few.
        let digits =
            (DeterministicMath.pi * BigInteger.Pow (BigInteger 10, 60))
            >>> DeterministicMath.piBits

        digits
        |> shouldEqual (BigInteger.Parse "3141592653589793238462643383279502884197169399375105820974944")

    /// Number of fractional bits the reference implementation below carries. Enough that
    /// even the most cancellation-prone double leaves it several hundred significant bits:
    /// an argument as large as 2^1024 consumes 1024 of these in the range reduction.
    let private referenceBits : int = 1400

    let private referenceScale : BigInteger = BigInteger.One <<< referenceBits

    let private referenceMultiply (a : BigInteger) (b : BigInteger) : BigInteger = a * b / referenceScale

    /// `atan(1/n)` at `referenceBits`, by its alternating series.
    let private referenceAtanReciprocal (n : int) : BigInteger =
        let nSquared = BigInteger (n * n)
        let mutable term = referenceScale / BigInteger n
        let mutable acc = BigInteger.Zero
        let mutable k = 0

        while not term.IsZero do
            let contribution = term / BigInteger ((2 * k) + 1)
            acc <- (if k % 2 = 0 then acc + contribution else acc - contribution)
            term <- term / nSquared
            k <- k + 1

        acc

    /// pi by *Euler's* formula, `pi/4 = atan(1/2) + atan(1/3)` — deliberately not the Machin
    /// formula the implementation uses, so that the two are independent evidence rather than
    /// the same arithmetic run twice.
    let private referencePi : BigInteger =
        BigInteger 4 * (referenceAtanReciprocal 2 + referenceAtanReciprocal 3)

    let private referenceTwoPi : BigInteger = referencePi <<< 1

    let private referenceOneOverTwoPi : BigInteger =
        (BigInteger.One <<< (2 * referenceBits)) / referenceTwoPi

    /// An independent cosine: reduce modulo 2 pi and evaluate the cosine series directly on
    /// the result. The implementation instead reduces modulo pi/2 and picks one of four
    /// quadrant formulae, so this reference shares neither the constant, nor the modulus,
    /// nor the quadrant table with what it is checking — a sign error in any of those shows
    /// up here as a gross disagreement rather than a last-bit one.
    let private referenceCos (x : float) : float =
        let mantissa, exponent = DeterministicMath.decompose (abs x)

        let scaled = mantissa * referenceOneOverTwoPi

        let turns =
            if exponent >= 0 then
                scaled <<< exponent
            else
                scaled >>> -exponent

        let k = (turns + (BigInteger.One <<< (referenceBits - 1))) >>> referenceBits
        let reduced = (mantissa <<< (exponent + referenceBits)) - (k * referenceTwoPi)

        // The cosine series converges on the whole of [-pi, pi], which is what reducing
        // modulo a full turn leaves; its intermediate terms peak around e^pi, so it loses
        // about five bits to cancellation and keeps the rest.
        let reducedSquared = referenceMultiply reduced reduced
        let mutable term = referenceScale
        let mutable acc = BigInteger.Zero
        let mutable j = 0

        while not term.IsZero do
            acc <- acc + term
            term <- -(referenceMultiply term reducedSquared) / BigInteger ((j + 1) * (j + 2))
            j <- j + 2

        DeterministicMath.roundToDouble acc -referenceBits

    [<Test>]
    let ``the two independently computed pis agree`` () : unit =
        // Machin's formula against Euler's. They are computed to different precisions, so
        // compare the top 1300 bits, which is well inside both.
        let machin = DeterministicMath.pi <<< (referenceBits - DeterministicMath.piBits)

        BigInteger.Abs (machin - referencePi) >>> (referenceBits - 1300)
        |> shouldEqual BigInteger.Zero

    [<Test>]
    let ``cos matches an independently computed reference`` () : unit =
        // The real specification of `cos`: bit-for-bit equality with a reference accurate to
        // several hundred bits. Both are correctly rounded far beyond a double's 53 bits, so
        // they can only differ if the true value lies within ~2^-190 of a midpoint, which no
        // draw is expected to hit.
        let property (x : float) : bool =
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.cos x)
            actual = BitConverter.DoubleToUInt64Bits (referenceCos x)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``cos agrees with the host libm to within one ulp`` () : unit =
        // As with `pow`: a coarse sanity check rather than a specification, since the host's
        // `cos` is not correctly rounded and PawPrint deliberately does not inherit it. On a
        // sample of 1500 random doubles the two differed on 29, always by exactly one ulp,
        // and on every one of those the exact value (computed to 1400 bits) was nearer to
        // PawPrint's answer — 0.4996 ulp at worst, against 0.5004 to 0.6206 for the host.
        let property (x : float) : bool =
            ulpDistance (DeterministicMath.cos x) (Math.Cos x) <= 1.0

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``cos is even`` () : unit =
        // Not merely a property of the real cosine that a good approximation inherits: IEEE
        // 754-2019 clause 9.2 requires it of the operation, for every rounding attribute and
        // over the whole domain. So this must hold exactly rather than to within a rounding —
        // which it does, because the reduction takes |x|.
        let property (x : float) : bool =
            let atX = BitConverter.DoubleToUInt64Bits (DeterministicMath.cos x)
            atX = BitConverter.DoubleToUInt64Bits (DeterministicMath.cos -x)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``cos stays within the unit interval`` () : unit =
        let property (x : float) : bool =
            let result = DeterministicMath.cos x
            result >= -1.0 && result <= 1.0

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``cos is a pure function of its argument`` () : unit =
        let property (x : float) : bool =
            let first = BitConverter.DoubleToUInt64Bits (DeterministicMath.cos x)
            first = BitConverter.DoubleToUInt64Bits (DeterministicMath.cos x)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``the quarter-turn reduction reconstructs its argument`` () : unit =
        // `|x| = k pi/2 + r` is the whole content of the reduction, so assert exactly that,
        // in `BigInteger` arithmetic at the reduction's own precision. `k` is only known
        // modulo 4 from the outside, but that is enough to pin the residue: recomputing k
        // from `(|x| - r) / (pi/2)` and checking its bottom two bits catches a reduction
        // that lands on the wrong quadrant, which is the failure that would otherwise show
        // up only as a sign or a swap of sine for cosine.
        let property (x : float) : bool =
            let quadrant, r = DeterministicMath.reduceModuloQuarterTurn x
            let mantissa, exponent = DeterministicMath.decompose (abs x)
            let xFixed = mantissa <<< (exponent + DeterministicMath.piBits)

            // Scale `r` back up; it was truncated on the way down, so allow that back.
            let rFull = r <<< (DeterministicMath.piBits - DeterministicMath.fractionBits)
            let piOverTwo = DeterministicMath.pi >>> 1
            let recoveredK = BigInteger.Divide (xFixed - rFull + (piOverTwo >>> 1), piOverTwo)

            // |r| <= pi/4, plus the one place in the last of `fractionBits` that truncating
            // it down to the working precision can add.
            let bound =
                (DeterministicMath.pi >>> 2)
                + (BigInteger.One
                   <<< (DeterministicMath.piBits - DeterministicMath.fractionBits + 1))

            BigInteger.Abs rFull <= bound && int (recoveredK &&& BigInteger 3) = quadrant

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    /// The number of significant bits a reduced argument retains: `r` is below 1, so its bit
    /// length as a fixed-point value at `fractionBits` places is exactly that count.
    let private reducedArgumentBits (x : float) : int =
        let _, r = DeterministicMath.reduceModuloQuarterTurn x
        int ((BigInteger.Abs r).GetBitLength ())

    [<Test>]
    let ``the reduction clears its precision floor by a wide margin`` () : unit =
        // `cos` answers ±sin(r) in the odd quadrants, where the result is proportional to `r`
        // itself, so the accuracy of the answer there is the accuracy of `r` — and `r` is a
        // difference of two nearly equal quantities. `cos` asserts that at least 128
        // significant bits survive; this measures how much room that assertion actually has.
        //
        // Kahan's witness for binary64 range reduction is the double nearest an odd multiple
        // of pi/2 over the whole range. It survives with about 195 bits, so the assertion is
        // roughly 67 bits away from firing on the worst input anyone has found.
        let kahansWitness =
            DeterministicMath.roundToDouble (BigInteger 6381956970095103L) 797

        // Confirm it really is a hard case before drawing any conclusion from it passing.
        let witnessBits = reducedArgumentBits kahansWitness
        witnessBits |> shouldBeSmallerThan 200
        witnessBits |> shouldBeGreaterThan 128

        // And sweep the near-multiples of pi/2 that a program is actually likely to produce.
        let sweptWorst =
            seq { 1..20000 }
            |> Seq.map (fun k -> float k * 1.5707963267948966)
            |> Seq.filter (fun x ->
                let quadrant, _ = DeterministicMath.reduceModuloQuarterTurn x
                quadrant % 2 = 1
            )
            |> Seq.map reducedArgumentBits
            |> Seq.min

        sweptWorst |> shouldBeGreaterThan 128

    /// The IEEE 754 / C99 `cos` special cases, as a table of (x, expected bits).
    let private cosSpecialCases : (float * uint64) list =
        let ofBits (bits : uint64) : float = BitConverter.UInt64BitsToDouble bits
        let positiveNaN = 0x7FF8000000000000UL
        let negativeNaN = 0xFFF8000000000000UL
        let one = 0x3FF0000000000000UL

        [
            // cos(±0) is exactly 1, with no rounding involved.
            0.0, one
            -0.0, one
            // So is cos(x) for any x small enough that 1 - x^2/2 rounds back to 1.
            ofBits 0x3E10000000000000UL, one // 2^-30
            // An infinite argument names no point on the circle: a domain error.
            infinity, positiveNaN
            -infinity, positiveNaN
            // A NaN propagates with its payload intact, which IEEE 754-2019 clause 6.2.3
            // recommends; the sign clause 6.3 leaves unspecified, and PawPrint keeps it for
            // consistency with its own `pow`. `Double.NaN` is the *negative* quiet NaN.
            nan, negativeNaN
            ofBits 0x7FF8000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF8000000000123UL, 0xFFF8000000000123UL
            // A *signalling* NaN comes back quieted, again keeping sign and payload.
            ofBits 0x7FF0000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF0000000000123UL, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000001UL, 0x7FF8000000000001UL
        ]

    [<Test>]
    let ``cos matches the IEEE 754 special cases`` () : unit =
        cosSpecialCases
        |> List.choose (fun (x, expected) ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.cos x)

            if actual = expected then
                None
            else
                Some $"cos(%.17g{x}): expected bits %016x{expected}, got %016x{actual}"
        )
        |> reportFailures

    /// The rows of `cosSpecialCases` on which the host is allowed to disagree, with the one
    /// other answer it may give. Every row here is a NaN payload or sign, which IEEE 754
    /// leaves to the implementation; the finite rows admit no alternative.
    let private cosHostAlternatives : Map<uint64, uint64> =
        let positiveNaN = 0x7FF8000000000000UL
        let negativeNaN = 0xFFF8000000000000UL

        [
            // The payload of a *freshly generated* NaN is the hardware's: x86 produces the
            // negative quiet NaN and Arm the positive one, and glibc reaches this case
            // through an arithmetic operation on the infinity rather than by naming a bit
            // pattern. macOS/Arm gives the positive one this table specifies.
            BitConverter.DoubleToUInt64Bits infinity, negativeNaN
            BitConverter.DoubleToUInt64Bits -infinity, negativeNaN
            // macOS/Arm's `cos` clears the sign of a NaN argument rather than propagating it.
            // Clause 6.3 does not specify the sign of a NaN result, so both conform; the
            // payload survives either way, so the alternative differs from the specified
            // answer only in the sign bit.
            0xFFF8000000000000UL, positiveNaN
            0xFFF8000000000123UL, 0x7FF8000000000123UL
            0xFFF0000000000123UL, 0x7FF8000000000123UL
        ]
        |> Map.ofList

    [<Test>]
    let ``the host agrees about the cos special cases`` () : unit =
        cosSpecialCases
        |> List.choose (fun (x, expected) ->
            let host = BitConverter.DoubleToUInt64Bits (Math.Cos x)
            let key = BitConverter.DoubleToUInt64Bits x

            let permitted =
                match Map.tryFind key cosHostAlternatives with
                | None -> [ expected ]
                | Some alternative -> [ expected ; alternative ]

            if List.contains host permitted then
                None
            else

            let permitted = permitted |> List.map (sprintf "%016x") |> String.concat " or "

            Some $"host cos(%.17g{x}): expected bits {permitted}, got %016x{host}"
        )
        |> reportFailures

    /// An independent sine, built the same way `referenceCos` is: reduce modulo 2 pi against
    /// Euler's pi and evaluate the sine series directly on the result, sharing neither the
    /// constant, nor the modulus, nor the quadrant table with the implementation.
    ///
    /// This stays valid all the way down to `Double.Epsilon`, because it carries the reduced
    /// argument at `referenceBits` fractional bits without ever narrowing it — which is what
    /// lets it act as the oracle for the implementation's small-argument shortcut as well as
    /// for the series.
    let private referenceSin (x : float) : float =
        let mantissa, exponent = DeterministicMath.decompose (abs x)

        let scaled = mantissa * referenceOneOverTwoPi

        let turns =
            if exponent >= 0 then
                scaled <<< exponent
            else
                scaled >>> -exponent

        let k = (turns + (BigInteger.One <<< (referenceBits - 1))) >>> referenceBits
        let reduced = (mantissa <<< (exponent + referenceBits)) - (k * referenceTwoPi)

        // As in `referenceCos`, the series converges on the whole of [-pi, pi]. Its worst
        // cancellation is at an argument near ±pi, where the terms peak around pi^3/6 and the
        // sum comes back near 2^-53: about 55 bits lost out of `referenceBits`.
        let reducedSquared = referenceMultiply reduced reduced
        let mutable term = reduced
        let mutable acc = BigInteger.Zero
        let mutable j = 1

        while not term.IsZero do
            acc <- acc + term
            term <- -(referenceMultiply term reducedSquared) / BigInteger ((j + 1) * (j + 2))
            j <- j + 2

        let magnitude = DeterministicMath.roundToDouble acc -referenceBits

        // The reduction above took |x|; sine is odd, and `Double.IsNegative` rather than
        // `< 0.0` so that the sign of a zero survives.
        if Double.IsNegative x then -magnitude else magnitude

    [<Test>]
    let ``sin matches an independently computed reference`` () : unit =
        // The real specification of `sin`, as `cos` has above: bit-for-bit equality with a
        // reference accurate to several hundred bits.
        let property (x : float) : bool =
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.sin x)
            actual = BitConverter.DoubleToUInt64Bits (referenceSin x)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``sin agrees with the host libm to within one ulp`` () : unit =
        // A sanity check rather than a specification: the host's `sin` is not correctly
        // rounded either, so where the two disagree it is PawPrint that is expected to be
        // right, and `sin matches an independently computed reference` is what says so.
        // Measured on a sample of 1500 random doubles, the two differed on 25, always by
        // exactly one ulp, and on every one of those the exact value (computed to 1400 bits)
        // was nearer to PawPrint's answer — 0.4996 ulp at worst, against 0.5004 to 0.6554
        // for the host.
        let property (x : float) : bool =
            ulpDistance (DeterministicMath.sin x) (Math.Sin x) <= 1.0

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``sin is odd`` () : unit =
        // IEEE 754-2019 clause 9.2 requires this of the operation, not merely of the real
        // sine: "For operations f defined by odd mathematical functions, f(-x) is -f(x) for
        // roundTiesToEven, roundTiesToAway, and roundTowardZero for their entire domain and
        // range." Note the narrower scope than the evenness rule `cos is even` asserts, which
        // holds for every rounding attribute; under the directed roundings an odd function
        // need not be exactly odd. We round ties to even throughout, so it binds here.
        //
        // It holds exactly rather than to within a rounding because the reduction takes |x|
        // and the sign is reapplied to the result.
        let property (x : float) : bool =
            let atMinusX = BitConverter.DoubleToUInt64Bits (DeterministicMath.sin -x)
            atMinusX = BitConverter.DoubleToUInt64Bits (-(DeterministicMath.sin x))

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``sin stays within the unit interval`` () : unit =
        let property (x : float) : bool =
            let result = DeterministicMath.sin x
            result >= -1.0 && result <= 1.0

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``sin is a pure function of its argument`` () : unit =
        let property (x : float) : bool =
            let first = BitConverter.DoubleToUInt64Bits (DeterministicMath.sin x)
            first = BitConverter.DoubleToUInt64Bits (DeterministicMath.sin x)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``sin and cos satisfy the Pythagorean identity`` () : unit =
        // The one check that ties the two quadrant tables to each other. `sin` and `cos` read
        // the same reduction and the same series, differing only in which residues call for
        // the sine and which of them negate; rotating one table relative to the other leaves
        // both functions individually plausible — still bounded, still the right parity — but
        // breaks this immediately.
        //
        // Both operands are correctly rounded, so the two squarings and the addition are the
        // only error: a few ulps of 1, far inside this bound.
        let property (x : float) : bool =
            let s = DeterministicMath.sin x
            let c = DeterministicMath.cos x
            abs ((s * s) + (c * c) - 1.0) < 1e-15

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    /// A double whose magnitude is below 2^-27, spanning the subnormals up to the largest
    /// octave on which `sin x` is exactly `x`. Biased exponent 995 is the octave
    /// [2^-28, 2^-27), so every draw is strictly below the bound.
    let private genTinyDouble : Gen<float> =
        gen {
            let! sign = Gen.elements [ 0UL ; 1UL ]
            let! biasedExponent = Gen.choose (0, 995)
            let! fraction = Gen.choose64 (0L, 0xF_FFFF_FFFF_FFFFL)

            return
                BitConverter.UInt64BitsToDouble ((sign <<< 63) ||| (uint64 biasedExponent <<< 52) ||| uint64 fraction)
        }

    [<Test>]
    let ``sin returns its argument unchanged on the octaves where that is correct`` () : unit =
        // `sin x` differs from `x` in binary64 only once `x^3/6` reaches half an ulp of `x`,
        // i.e. once |x| exceeds about 2^-25.2; below that the correctly rounded answer is the
        // argument itself.
        //
        // This range straddles `smallArgumentThreshold`, which is 2^-128: draws above it go
        // through the reduction and the series, draws below it take the shortcut. So this is
        // simultaneously the specification of the shortcut and the evidence that the
        // threshold sits a hundred octaves inside the region where the shortcut is valid,
        // rather than at the edge of it.
        let property (x : float) : bool =
            BitConverter.DoubleToUInt64Bits (DeterministicMath.sin x) = BitConverter.DoubleToUInt64Bits x

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genTinyDouble) property)

    [<Test>]
    let ``the small-argument shortcut agrees with the series across the boundary`` () : unit =
        // The two doubles either side of `smallArgumentThreshold` take different code paths
        // and must not disagree. The one at the threshold is the hardest input the series is
        // ever asked for: its reduced argument has exactly `reducedArgumentFloor` significant
        // bits, so it is the point at which the floor assertion is closest to firing.
        let threshold =
            DeterministicMath.roundToDouble
                BigInteger.One
                (DeterministicMath.reducedArgumentFloor - DeterministicMath.fractionBits)

        let below = Math.BitDecrement threshold

        [ below, "below" ; threshold, "at" ; Math.BitIncrement threshold, "above" ]
        |> List.collect (fun (x, where) ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.sin x)

            [
                if actual <> BitConverter.DoubleToUInt64Bits x then
                    yield
                        $"sin(%.17g{x}), %s{where} the threshold: expected the argument back, got %.17g{DeterministicMath.sin x}"
                if actual <> BitConverter.DoubleToUInt64Bits (referenceSin x) then
                    yield
                        $"sin(%.17g{x}), %s{where} the threshold: got bits %016x{actual}, reference says %016x{BitConverter.DoubleToUInt64Bits (referenceSin x)}"
            ]
        )
        |> reportFailures

    /// The IEEE 754 / C99 `sin` special cases, as a table of (x, expected bits).
    let private sinSpecialCases : (float * uint64) list =
        let ofBits (bits : uint64) : float = BitConverter.UInt64BitsToDouble bits
        let positiveNaN = 0x7FF8000000000000UL
        let negativeNaN = 0xFFF8000000000000UL
        let positiveZero = 0x0000000000000000UL
        let negativeZero = 0x8000000000000000UL

        [
            // IEEE 754-2019 clause 9.2.1: "For the operations sin, tan, ... f(+0) is +0 and
            // f(-0) is -0 with no exception." So the sign of a zero is specified, unlike the
            // sign of a NaN below.
            0.0, positiveZero
            -0.0, negativeZero
            // sin(x) is exactly x for any x small enough that x - x^3/6 rounds back to x.
            ofBits 0x3E10000000000000UL, 0x3E10000000000000UL // 2^-30
            ofBits 0xBE10000000000000UL, 0xBE10000000000000UL // -2^-30
            // An infinite argument names no point on the circle: a domain error.
            infinity, positiveNaN
            -infinity, positiveNaN
            // A NaN propagates with its payload intact, which IEEE 754-2019 clause 6.2.3
            // recommends; the sign clause 6.3 leaves unspecified, and PawPrint keeps it for
            // consistency with its own `pow` and `cos`. `Double.NaN` is the *negative* quiet
            // NaN.
            nan, negativeNaN
            ofBits 0x7FF8000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF8000000000123UL, 0xFFF8000000000123UL
            // A *signalling* NaN comes back quieted, again keeping sign and payload.
            ofBits 0x7FF0000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF0000000000123UL, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000001UL, 0x7FF8000000000001UL
        ]

    [<Test>]
    let ``sin matches the IEEE 754 special cases`` () : unit =
        sinSpecialCases
        |> List.choose (fun (x, expected) ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.sin x)

            if actual = expected then
                None
            else
                Some $"sin(%.17g{x}): expected bits %016x{expected}, got %016x{actual}"
        )
        |> reportFailures

    /// The rows of `sinSpecialCases` on which the host is allowed to disagree, with the one
    /// other answer it may give. As with `cos`, every row here is a NaN payload or sign; the
    /// zeros and the finite rows admit no alternative, because clause 9.2.1 fixes them.
    let private sinHostAlternatives : Map<uint64, uint64> =
        let positiveNaN = 0x7FF8000000000000UL
        let negativeNaN = 0xFFF8000000000000UL

        [
            // The payload of a *freshly generated* NaN is the hardware's: x86 produces the
            // negative quiet NaN and Arm the positive one.
            BitConverter.DoubleToUInt64Bits infinity, negativeNaN
            BitConverter.DoubleToUInt64Bits -infinity, negativeNaN
            // macOS/Arm's libm clears the sign of a NaN argument rather than propagating it.
            0xFFF8000000000000UL, positiveNaN
            0xFFF8000000000123UL, 0x7FF8000000000123UL
            0xFFF0000000000123UL, 0x7FF8000000000123UL
        ]
        |> Map.ofList

    [<Test>]
    let ``the host agrees about the sin special cases`` () : unit =
        sinSpecialCases
        |> List.choose (fun (x, expected) ->
            let host = BitConverter.DoubleToUInt64Bits (Math.Sin x)
            let key = BitConverter.DoubleToUInt64Bits x

            let permitted =
                match Map.tryFind key sinHostAlternatives with
                | None -> [ expected ]
                | Some alternative -> [ expected ; alternative ]

            if List.contains host permitted then
                None
            else

            let permitted = permitted |> List.map (sprintf "%016x") |> String.concat " or "

            Some $"host sin(%.17g{x}): expected bits {permitted}, got %016x{host}"
        )
        |> reportFailures

    /// `x` written exactly as an integer count of `2^exponent`. Every double is a dyadic
    /// rational, so this is lossless as long as `exponent` is at or below the exponent of the
    /// smallest subnormal.
    let private atExponent (exponent : int) (x : float) : BigInteger =
        let mantissa, ownExponent = DeterministicMath.decompose x
        mantissa <<< (ownExponent - exponent)

    /// Whether `r` really is the double nearest to the exact square root of `x`, decided
    /// without reference to any other square-root implementation: `r` is nearest exactly when
    /// `x` lies between the squares of the two midpoints bracketing `r`. Everything in sight
    /// is a dyadic rational, so `BigInteger` settles it exactly.
    ///
    /// The comparisons are non-strict at both ends, which would admit either neighbour at a
    /// tie — but no tie exists to admit. One would need the exact square root of a double to
    /// be an odd 54-bit value, whose square has 107 significant bits and so is not a double.
    let private isCorrectlyRoundedSqrt (x : float) (r : float) : bool =
        // One octave below the smallest subnormal, so that doubling a midpoint (see below)
        // cannot run out of room at the bottom either.
        let fixedPoint = -1080
        let asFixed = atExponent fixedPoint

        // Twice each midpoint, which keeps the arithmetic in integers with no division.
        let twiceLower = asFixed r + asFixed (Math.BitDecrement r)
        let twiceUpper = asFixed r + asFixed (Math.BitIncrement r)

        // `twiceLower^2 <= 4x <= twiceUpper^2`, with `4x` carried at the squared scale.
        let fourX = asFixed x <<< (2 - fixedPoint)

        twiceLower * twiceLower <= fourX && fourX <= twiceUpper * twiceUpper

    /// Wide `BigInteger`s spanning many bit lengths: the Newton iteration below derives its
    /// starting point from the bit length, so that is the axis worth varying.
    let private genWideInteger : Gen<BigInteger> =
        gen {
            let! shift = Gen.choose (0, 400)
            let! seed = Gen.choose64 (0L, Int64.MaxValue)
            let! low = Gen.choose64 (0L, Int64.MaxValue)
            return ((BigInteger seed) <<< shift) + BigInteger low
        }

    [<Test>]
    let ``integerSqrt brackets its argument`` () : unit =
        // The defining property, and the only thing `sqrt` assumes of it.
        let property (n : BigInteger) : bool =
            let r = DeterministicMath.integerSqrt n
            r * r <= n && n < (r + BigInteger.One) * (r + BigInteger.One)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genWideInteger) property)

    [<Test>]
    let ``integerSqrt is exact on squares`` () : unit =
        // Perfect squares are where the bracket above is tightest, and they are the inputs on
        // which `sqrt`'s sticky bit has to come out zero. A random draw almost never hits one.
        let property (r : BigInteger) : bool =
            DeterministicMath.integerSqrt (r * r) = r

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genWideInteger) property)

    /// Doubles whose exponent leaves room to square them without overflowing or reaching the
    /// subnormals, where the squaring below would lose bits of its own.
    let private genSquarableDouble : Gen<float> =
        gen {
            let! biasedExponent = Gen.choose (1023 - 500, 1023 + 500)
            let! fraction = Gen.choose64 (0L, 0xF_FFFF_FFFF_FFFFL)
            return BitConverter.UInt64BitsToDouble ((uint64 biasedExponent <<< 52) ||| uint64 fraction)
        }

    /// Arguments whose exact square root sits within about a quarter of an ulp of a midpoint
    /// between two representable results — near enough that which way the answer goes is
    /// decided by bits a uniform draw leaves nowhere near the boundary.
    ///
    /// These are constructed rather than searched for. The midpoint `m` between a double and
    /// its successor is exactly representable in 54 bits, so `m^2` is exact in `BigInteger`,
    /// and the double nearest `m^2` has a square root within a relative 2^-54 of `m`.
    ///
    /// A quarter of an ulp is as close as this construction gets, which is *not* close enough
    /// to make the discarded remainder decide the answer — that needs 2^-91, and is what the
    /// constructive test below is for. This generator covers the surrounding regime instead:
    /// every draw exercises the rounding boundary itself, where a uniform draw lands within a
    /// quarter-ulp of it about one time in two million.
    let private genNearMidpointSquare : Gen<float> =
        genSquarableDouble
        |> Gen.map (fun r ->
            let fixedPoint = -1080

            let twiceMidpoint =
                atExponent fixedPoint r + atExponent fixedPoint (Math.BitIncrement r)

            // Squaring twice the midpoint gives 4 m^2, hence the compensating -2 below.
            DeterministicMath.roundToDouble (twiceMidpoint * twiceMidpoint) ((2 * fixedPoint) - 2)
        )

    /// Whether the exact integer `v` is a double: at most 53 significant bits, however many
    /// trailing zeros it takes to get there.
    let private isExactlyRepresentable (v : BigInteger) : bool =
        let bitLength = int (v.GetBitLength ())
        bitLength <= 53 || ((v >>> (bitLength - 53)) <<< (bitLength - 53)) = v

    /// An `m` with `m * m = target` modulo 2^`bits`, found by lifting one bit at a time. Every
    /// odd square is 1 modulo 8, and conversely any `target` that is 1 modulo 8 has such an
    /// `m`; each new bit of the answer is then forced, so the lift never has to backtrack.
    let private squareRootModuloPowerOfTwo (bits : int) (target : BigInteger) : BigInteger =
        let mutable m = BigInteger.One

        for k in 3..bits do
            let modulus = BigInteger.One <<< k

            if (((m * m) - target) % modulus + modulus) % modulus <> BigInteger.Zero then
                m <- m + (BigInteger.One <<< (k - 2))

        m

    [<Test>]
    let ``sqrt rounds correctly where the remainder alone decides the answer`` () : unit =
        // The one input class that random draws cannot reach, and the reason `sqrt` carries a
        // sticky bit rather than just rounding its truncated integer root.
        //
        // The integer root is taken to 128 guard bits, so for a normal argument it agrees with
        // the true root to 38 bits below the last one the answer keeps. Only when *all* 38 of
        // those bits read 1000...0 -- the true root sitting just above the midpoint between two
        // doubles, by less than 2^-91 relative -- does the discarded remainder change the
        // answer: with it the value is above the midpoint and rounds up, without it the value
        // is exactly on the midpoint and ties to even. That is about one double in 2^37, so a
        // uniform search finds none: 400 000 random arguments produced no disagreement at all,
        // and neither did the quarter-ulp generator above.
        //
        // These are constructed instead. Take an odd 54-bit `m`, which is exactly a midpoint
        // between two representable results, and ask for `m * m` to sit just *below* a double:
        // that is `m^2 = -c (mod 2^54)` for a small `c`, which the bitwise lift above solves
        // whenever `-c` is 1 modulo 8. Then `x = m^2 + c` is exactly a double, and its root
        // exceeds the midpoint `m` by about `c / 2m`, which for the `c` below is between 2^-104
        // and 2^-100 in relative terms -- nine octaves or more inside the 2^-91 window where
        // the remainder decides.
        //
        // Every such `x` lands on an even exponent by construction, so this says nothing about
        // the parity adjustment; the general properties above cover that.
        let constructed =
            [ 7..8..199 ]
            |> List.collect (fun c ->
                let modulus = BigInteger.One <<< 54
                let root = squareRootModuloPowerOfTwo 54 (modulus - BigInteger c)

                // `m` and `m + 2^53` are both solutions modulo 2^54; a midpoint needs the one
                // that is odd and has 54 bits.
                [ root ; root + (BigInteger.One <<< 53) ]
                |> List.filter (fun m -> not m.IsEven && m > (BigInteger.One <<< 53) && m < modulus)
                |> List.map (fun m -> c, m, (m * m) + BigInteger c)
            )
            // Not every `m` the lift produces yields a *double*: `m^2 + c` is a multiple of
            // 2^54 by construction, but it also has to fit a 53-bit significand, which needs
            // `m` below 2^53.5 and the quotient even above it. Rows that miss are dropped
            // rather than silently weakening the check, and the count assertion at the end is
            // what stops that from emptying the test.
            |> List.filter (fun (_, _, exact) -> isExactlyRepresentable exact)
            |> List.map (fun (c, m, exact) -> c, m, exact, DeterministicMath.roundToDouble exact 0)

        // The construction is only meaningful if `x` really is `m^2 + c` exactly; a value that
        // had to be rounded would not sit where the argument above says it does.
        constructed
        |> List.choose (fun (c, m, exact, x) ->
            let mantissa, exponent = DeterministicMath.decompose x

            if (mantissa <<< exponent) = exact then
                None
            else
                Some $"the constructed argument for c = %i{c}, m = %O{m} is not exactly representable"
        )
        |> reportFailures

        constructed
        |> List.choose (fun (c, m, _, x) ->
            let actual = DeterministicMath.sqrt x
            let host = Math.Sqrt x

            if
                isCorrectlyRoundedSqrt x actual
                && BitConverter.DoubleToUInt64Bits actual = BitConverter.DoubleToUInt64Bits host
            then
                None
            else
                Some
                    $"sqrt of the constructed argument for c = %i{c}, m = %O{m} (%.17g{x}): got %016x{BitConverter.DoubleToUInt64Bits actual}, host %016x{BitConverter.DoubleToUInt64Bits host}"
        )
        |> reportFailures

        // Guards against the construction quietly producing nothing to check. Nineteen of the
        // twenty-five rows above survive the filter as this is written.
        if List.length constructed < 8 then
            failwith $"the near-tie construction produced only %i{List.length constructed} arguments to check"

    [<Test>]
    let ``sqrt is correctly rounded where the answer is nearly a tie`` () : unit =
        // Checked twice over: against the exact predicate, and against the host, which must
        // resolve these the same way for the same reason.
        let property (x : float) : bool =
            let root = DeterministicMath.sqrt x

            isCorrectlyRoundedSqrt x root
            && BitConverter.DoubleToUInt64Bits root = BitConverter.DoubleToUInt64Bits (Math.Sqrt x)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genNearMidpointSquare) property)

    [<Test>]
    let ``sqrt is correctly rounded`` () : unit =
        // Not "to within an ulp", as the corresponding pow, sin and cos properties have to
        // say. IEEE 754 clause 5.4.1 makes squareRoot a *required* operation and requires it
        // to be correctly rounded, so this is exact — and it is checked against an exact
        // predicate rather than against another implementation.
        let property (x : float) : bool =
            isCorrectlyRoundedSqrt x (DeterministicMath.sqrt x)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPositiveFiniteDouble) property)

    [<Test>]
    let ``sqrt agrees with the host bit-for-bit`` () : unit =
        // The one place in this module where the host is an exact oracle rather than a bound.
        // `pow`, `sin` and `cos` are clause 9.2 operations, which libms round to about
        // 0.5 + epsilon ulp, so their tests can only limit how far the two may drift apart;
        // squareRoot must be correctly rounded and every platform emits a hardware instruction
        // that is, so any disagreement here at all is a bug in one of the two.
        let property (x : float) : bool =
            BitConverter.DoubleToUInt64Bits (DeterministicMath.sqrt x) = BitConverter.DoubleToUInt64Bits (Math.Sqrt x)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPositiveFiniteDouble) property)

    [<Test>]
    let ``sqrt is exact on squares of exactly representable values`` () : unit =
        // A double with at most 26 significant bits has a square that is still a double, so
        // the root is recoverable exactly and no rounding may intervene. This is the case in
        // which `sqrt`'s sticky bit is zero, which a random draw would essentially never hit.
        let property (significand : int, exponent : int) : bool =
            // Exponents kept well inside the range where the square neither overflows nor
            // reaches the subnormals, since squaring would there lose bits of its own.
            let y = DeterministicMath.roundToDouble (BigInteger significand) exponent
            DeterministicMath.sqrt (y * y) = y

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.choose (1, (1 <<< 26) - 1)) (Gen.choose (-400, 400)))) property
        )

    [<Test>]
    let ``sqrt inverts squaring to within a rounding`` () : unit =
        // Squaring a general double loses bits, so `sqrt (x * x)` cannot always be `x` — but
        // the true root of the rounded square is within an ulp of `x`, and `sqrt` rounds
        // correctly, so the answer is `x` or one of its immediate neighbours.
        let property (x : float) : bool =
            let squared = x * x

            // A square that overflows, or that lands in the subnormals where squaring
            // discards far more than a rounding, says nothing about `sqrt`.
            if not (Double.IsNormal squared) then
                true
            else
                ulpDistance (DeterministicMath.sqrt squared) x <= 1.0

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPositiveFiniteDouble) property)

    [<Test>]
    let ``sqrt is monotone`` () : unit =
        // The correct rounding of a monotone function is monotone, so this holds exactly
        // rather than up to an error term.
        let property (a : float, b : float) : bool =
            let smaller, larger = if a <= b then a, b else b, a
            DeterministicMath.sqrt smaller <= DeterministicMath.sqrt larger

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip genPositiveFiniteDouble genPositiveFiniteDouble)) property
        )

    [<Test>]
    let ``sqrt is a pure function of its argument`` () : unit =
        let property (x : float) : bool =
            let first = DeterministicMath.sqrt x
            let second = DeterministicMath.sqrt x
            BitConverter.DoubleToUInt64Bits first = BitConverter.DoubleToUInt64Bits second

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    /// `(argument, expected bits)` for the arguments on which IEEE 754 fixes an answer
    /// exactly. Stated in bits so that the sign of a zero and the payload of a NaN are pinned
    /// rather than compared by an equality that ignores them.
    ///
    /// The irrational rows are not transcribed on trust: the test below re-derives that each
    /// one is the correctly rounded root, so a mistyped constant fails whatever `sqrt` does.
    let private sqrtSpecialCases : (float * uint64) list =
        let ofBits (b : uint64) : float = BitConverter.UInt64BitsToDouble b
        let positiveNaN = 0x7FF8000000000000UL

        [
            // Clause 5.4.1: squareRoot(+/-0) is that same zero, and squareRoot(+infinity) is
            // +infinity. Exact, not merely correctly rounded.
            0.0, 0x0000000000000000UL
            -0.0, 0x8000000000000000UL
            infinity, 0x7FF0000000000000UL

            // Every *other* negative argument is outside the domain: invalid operation, and a
            // quiet NaN. -0 above is not, which is why it has to be handled before them.
            -1.0, positiveNaN
            -0.5, positiveNaN
            -Double.Epsilon, positiveNaN
            -infinity, positiveNaN
            Double.MinValue, positiveNaN

            // Roots that are themselves representable: every correctly rounded implementation
            // returns these bit-for-bit, and so must an exact one.
            1.0, 0x3FF0000000000000UL
            4.0, 0x4000000000000000UL
            9.0, 0x4008000000000000UL
            2.25, 0x3FF8000000000000UL
            0.25, 0x3FE0000000000000UL

            // Irrational roots, correctly rounded, including both ends of the range: the
            // smallest subnormal and the largest finite double, where the argument's exponent
            // is furthest from even and the widening in `sqrt` does the most work.
            2.0, 0x3FF6A09E667F3BCDUL
            3.0, 0x3FFBB67AE8584CAAUL
            10.0, 0x40094C583ADA5B53UL
            0.5, 0x3FE6A09E667F3BCDUL
            1e-300, 0x20CA2FE76A3F9475UL
            1e300, 0x5F138D352E5096AFUL
            Double.Epsilon, 0x1E60000000000000UL
            Double.MaxValue, 0x5FEFFFFFFFFFFFFFUL

            // A NaN argument comes back with its payload and sign intact, and quietened if it
            // was signalling. Unlike the NaN *generated* above for a negative argument, this
            // is what both x86 and Arm hardware do, so no host exemption is needed for it.
            ofBits 0x7FF8000000000000UL, 0x7FF8000000000000UL
            ofBits 0xFFF8000000000000UL, 0xFFF8000000000000UL
            ofBits 0x7FF8000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF8000000000123UL, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF0000000000123UL, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000001UL, 0x7FF8000000000001UL
        ]

    [<Test>]
    let ``the sqrt special cases are correctly rounded`` () : unit =
        // Keeps the table above honest independently of `sqrt`: for every row whose argument
        // and expected result are both finite and positive, the expected bits really are the
        // nearest double to the true square root.
        sqrtSpecialCases
        |> List.choose (fun (x, expected) ->
            let result = BitConverter.UInt64BitsToDouble expected

            if
                Double.IsNaN x
                || x <= 0.0
                || Double.IsInfinity x
                || Double.IsNaN result
                || result = 0.0
                || Double.IsInfinity result
            then
                None
            elif isCorrectlyRoundedSqrt x result then
                None
            else
                Some $"the table's sqrt(%.17g{x}) = %016x{expected} is not the correctly rounded root"
        )
        |> reportFailures

    [<Test>]
    let ``sqrt matches the IEEE 754 special cases`` () : unit =
        sqrtSpecialCases
        |> List.choose (fun (x, expected) ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.sqrt x)

            if actual = expected then
                None
            else
                Some $"sqrt(%.17g{x}): expected bits %016x{expected}, got %016x{actual}"
        )
        |> reportFailures

    /// The rows of `sqrtSpecialCases` on which the host is allowed to disagree, with the one
    /// other answer it may give.
    ///
    /// This map is much shorter than its `pow` and `sin` counterparts, and that is the point:
    /// squareRoot is correctly rounded everywhere, so no *finite* row can appear here. Only
    /// the NaN generated for a negative argument is open, and only in its sign.
    let private sqrtHostAlternatives : Map<uint64, uint64> =
        let negativeNaN = 0xFFF8000000000000UL

        [
            // The payload of a freshly generated NaN is the hardware's: x86's `sqrtsd` yields
            // the negative quiet NaN (the "indefinite" value, which is what `Double.NaN` is)
            // and Arm's `fsqrt` the positive one.
            BitConverter.DoubleToUInt64Bits -1.0, negativeNaN
            BitConverter.DoubleToUInt64Bits -0.5, negativeNaN
            BitConverter.DoubleToUInt64Bits -Double.Epsilon, negativeNaN
            BitConverter.DoubleToUInt64Bits -infinity, negativeNaN
            BitConverter.DoubleToUInt64Bits Double.MinValue, negativeNaN
        ]
        |> Map.ofList

    [<Test>]
    let ``the host agrees about the sqrt special cases`` () : unit =
        sqrtSpecialCases
        |> List.choose (fun (x, expected) ->
            let host = BitConverter.DoubleToUInt64Bits (Math.Sqrt x)
            let key = BitConverter.DoubleToUInt64Bits x

            let permitted =
                match Map.tryFind key sqrtHostAlternatives with
                | None -> [ expected ]
                | Some alternative -> [ expected ; alternative ]

            if List.contains host permitted then
                None
            else

            let permitted = permitted |> List.map (sprintf "%016x") |> String.concat " or "

            Some $"host sqrt(%.17g{x}): expected bits {permitted}, got %016x{host}"
        )
        |> reportFailures

    /// Whether `r` really is the smallest integral value at or above `x`, decided without
    /// reference to any other ceiling implementation: `r` must be an integer, at or above
    /// `x`, and `r - 1` must be strictly below it, so that no smaller integer would have
    /// done. Every double is a dyadic rational, so `BigInteger` settles all three exactly.
    ///
    /// Note what this deliberately does not see: `atExponent` maps both zeros to 0, so the
    /// predicate cannot tell `ceiling(-0.5) = -0` from `+0`. That sign is pinned by the
    /// special-case table and by the bit-for-bit host property instead.
    let private isCeilingOf (x : float) (r : float) : bool =
        // One octave below the smallest subnormal, so that every double is an exact integer
        // count of `2^fixedPoint` and `one` below is representable in the same units.
        let fixedPoint = -1080
        let one = BigInteger.One <<< -fixedPoint

        let xFixed = atExponent fixedPoint x
        let rFixed = atExponent fixedPoint r

        (rFixed % one).IsZero && rFixed >= xFixed && rFixed - one < xFixed

    /// Doubles whose exponent puts them in the only range where `ceiling` has anything to
    /// do: `[1, 2^52)` in magnitude, where a value can have both an integer part and a
    /// fractional one. A uniform draw over the whole exponent range spends about half its
    /// budget on values that are integral already and almost all the rest on values below 1,
    /// leaving roughly one draw in forty for the interesting case.
    let private genFractionalDouble : Gen<float> =
        gen {
            let! sign = Gen.elements [ 0UL ; 1UL ]
            // Biased exponent 1023 is the binade [1, 2); 1074 is [2^51, 2^52), the last one
            // in which a double can be non-integral at all.
            let! biasedExponent = Gen.choose (1023, 1074)
            let! fraction = Gen.choose64 (0L, 0xF_FFFF_FFFF_FFFFL)

            return
                BitConverter.UInt64BitsToDouble ((sign <<< 63) ||| (uint64 biasedExponent <<< 52) ||| uint64 fraction)
        }

    /// Doubles within one ulp of an integer, where the answer turns on whether a single
    /// discarded bit was set — the tightest boundary this function has, and one a uniform
    /// draw over a binade essentially never lands on.
    let private genNearIntegerDouble : Gen<float> =
        gen {
            let! magnitude = Gen.choose64 (0L, (1L <<< 52) - 1L)
            let! sign = Gen.elements [ 1.0 ; -1.0 ]
            let! offset = Gen.elements [ -1 ; 0 ; 1 ]

            let integral = sign * float magnitude

            return
                match offset with
                | -1 -> Math.BitDecrement integral
                | 1 -> Math.BitIncrement integral
                | _ -> integral
        }

    [<Test>]
    let ``ceiling matches its exact definition`` () : unit =
        // Not "to within an ulp", as the pow, sin and cos properties have to say, nor even
        // "correctly rounded", as sqrt's does: `roundToIntegralTowardPositive` is exact, so
        // there is a single right answer and it is checked against the definition itself
        // rather than against another implementation.
        let property (x : float) : bool =
            isCeilingOf x (DeterministicMath.ceiling x)

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``ceiling agrees with the host bit-for-bit`` () : unit =
        // As with `sqrt`, the host is an exact oracle here rather than a bound — and more so:
        // `sqrt` must merely be correctly rounded, whereas this operation is exact, so any
        // disagreement at all, including about the sign of a zero, is a bug in one of the two.
        let property (x : float) : bool =
            let actual = DeterministicMath.ceiling x
            BitConverter.DoubleToUInt64Bits actual = BitConverter.DoubleToUInt64Bits (Math.Ceiling x)

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``ceiling is idempotent`` () : unit =
        // The result is integral, so a second application must change nothing at all — not
        // even the sign of the negative zero the first one may have produced.
        let property (x : float) : bool =
            let once = DeterministicMath.ceiling x
            let twice = DeterministicMath.ceiling once
            BitConverter.DoubleToUInt64Bits twice = BitConverter.DoubleToUInt64Bits once

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``ceiling is the identity on doubles that are already integral`` () : unit =
        // At or above 2^52 in magnitude a double's ulp is at least 1, so it is an integer and
        // must come back untouched. This is the `exponent >= 0` fast path, which about half
        // of `genFiniteDouble`'s draws reach; the rest pass vacuously, and are covered by the
        // properties above instead.
        let property (x : float) : bool =
            if abs x < 4503599627370496.0 then
                true
            else
                BitConverter.DoubleToUInt64Bits (DeterministicMath.ceiling x) = BitConverter.DoubleToUInt64Bits x

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``ceiling moves its argument by less than one, and never downwards`` () : unit =
        // The bound a caller actually relies on, stated on its own rather than as a corollary
        // of the definition above: `0 <= ceiling(x) - x < 1`, in exact arithmetic so that the
        // subtraction cannot itself round.
        let property (x : float) : bool =
            let fixedPoint = -1080
            let one = BigInteger.One <<< -fixedPoint

            let difference =
                atExponent fixedPoint (DeterministicMath.ceiling x) - atExponent fixedPoint x

            difference.Sign >= 0 && difference < one

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``ceiling is monotone`` () : unit =
        // A non-decreasing function of a non-decreasing argument, exactly rather than up to
        // an error term.
        let property (a : float, b : float) : bool =
            let smaller, larger = if a <= b then a, b else b, a
            DeterministicMath.ceiling smaller <= DeterministicMath.ceiling larger

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genFiniteDouble genFiniteDouble)) property)

    [<Test>]
    let ``ceiling is a pure function of its argument`` () : unit =
        let property (x : float) : bool =
            let first = DeterministicMath.ceiling x
            let second = DeterministicMath.ceiling x
            BitConverter.DoubleToUInt64Bits first = BitConverter.DoubleToUInt64Bits second

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``ceiling agrees with the host on a dense sweep`` () : unit =
        // Quarter-integers either side of zero, which is where the sign rules bite and where
        // a random draw over a binade would spend nothing at all: every fractional part in
        // sight is one a generator of uniform mantissas essentially never produces.
        [ -2000 .. 2000 ]
        |> List.collect (fun n ->
            [ -0.75 ; -0.5 ; -0.25 ; 0.0 ; 0.25 ; 0.5 ; 0.75 ]
            |> List.map (fun offset -> float n + offset)
        )
        |> List.choose (fun x ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.ceiling x)
            let host = BitConverter.DoubleToUInt64Bits (Math.Ceiling x)

            if actual = host && isCeilingOf x (DeterministicMath.ceiling x) then
                None
            else
                Some $"ceiling(%.17g{x}): got %016x{actual}, host %016x{host}"
        )
        |> reportFailures

    /// `(argument, expected bits)` for the arguments on which IEEE 754 fixes an answer
    /// exactly — which for this operation is all of them, so this table is a spread of
    /// interesting shapes rather than an enumeration of exceptions. Stated in bits so that
    /// the sign of a zero and the payload of a NaN are pinned rather than compared by an
    /// equality that ignores them.
    let private ceilingSpecialCases : (float * uint64) list =
        let ofBits (b : uint64) : float = BitConverter.UInt64BitsToDouble b
        let bits (v : float) : uint64 = BitConverter.DoubleToUInt64Bits v

        // The last binade in which a double can be non-integral: its ulp is 1/2, so the
        // value below is exactly representable and its ceiling is 2^52 itself.
        let justBelowTwoToThe52 = 4503599627370495.5

        [
            // Zeros and infinities are integral already and come back with their signs.
            0.0, bits 0.0
            -0.0, bits (-0.0)
            infinity, bits infinity
            -infinity, bits (-infinity)

            // Anything strictly between 0 and 1 rounds up to 1...
            Double.Epsilon, bits 1.0
            1e-320, bits 1.0
            0.25, bits 1.0
            0.5, bits 1.0
            0.75, bits 1.0

            // ...and anything strictly between -1 and 0 rounds up to *negative* zero, which
            // is the one sign rule of this operation that an implementation is likely to get
            // wrong: the natural integer arithmetic produces a zero with no sign attached.
            -Double.Epsilon, bits (-0.0)
            -1e-320, bits (-0.0)
            -0.25, bits (-0.0)
            -0.5, bits (-0.0)
            -0.75, bits (-0.0)

            // Integers are their own ceiling, with no sign surprises.
            1.0, bits 1.0
            -1.0, bits (-1.0)
            2.0, bits 2.0
            -2.0, bits (-2.0)

            // Ordinary fractional arguments, both signs. Rounding *up* means the negative
            // rows truncate towards zero and the positive ones do not, which is the
            // asymmetry a floor-shaped implementation would get backwards.
            1.5, bits 2.0
            -1.5, bits (-1.0)
            2.5, bits 3.0
            -2.5, bits (-2.0)
            123.456, bits 124.0
            -123.456, bits (-123.0)

            // The boundary of the integral range: 2^52 is the smallest magnitude whose ulp
            // is 1, and the row below it is the largest non-integral double there is.
            justBelowTwoToThe52, bits 4503599627370496.0
            -justBelowTwoToThe52, bits (-4503599627370495.0)
            4503599627370496.0, bits 4503599627370496.0
            -4503599627370496.0, bits (-4503599627370496.0)

            // Beyond it nothing can be fractional, right out to the ends of the range.
            1e300, bits 1e300
            -1e300, bits (-1e300)
            Double.MaxValue, bits Double.MaxValue
            Double.MinValue, bits Double.MinValue

            // A NaN argument comes back with its payload and sign intact, quietened if it was
            // signalling. Both `roundsd` and `frintp` do this, and so does C's `ceil`, so
            // unlike the NaN `sqrt` *generates* for a negative argument no host exemption is
            // needed for any of these.
            ofBits 0x7FF8000000000000UL, 0x7FF8000000000000UL
            ofBits 0xFFF8000000000000UL, 0xFFF8000000000000UL
            ofBits 0x7FF8000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF8000000000123UL, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF0000000000123UL, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000001UL, 0x7FF8000000000001UL
        ]

    [<Test>]
    let ``the ceiling special cases satisfy the exact definition`` () : unit =
        // Keeps the table above honest independently of `ceiling`, so that a mistyped
        // constant fails whatever the implementation does. Covers every finite row; the
        // infinities and NaNs have no integer to compare against and are pinned by the table
        // alone.
        ceilingSpecialCases
        |> List.choose (fun (x, expected) ->
            let result = BitConverter.UInt64BitsToDouble expected

            if Double.IsNaN x || Double.IsInfinity x then
                None
            elif isCeilingOf x result then
                None
            else
                Some $"the table's ceiling(%.17g{x}) = %016x{expected} is not the smallest integer at or above it"
        )
        |> reportFailures

    [<Test>]
    let ``ceiling matches the IEEE 754 special cases`` () : unit =
        ceilingSpecialCases
        |> List.choose (fun (x, expected) ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.ceiling x)

            if actual = expected then
                None
            else
                Some $"ceiling(%.17g{x}): expected bits %016x{expected}, got %016x{actual}"
        )
        |> reportFailures

    [<Test>]
    let ``the host agrees about the ceiling special cases`` () : unit =
        // No table of permitted alternatives accompanies this one, unlike its `pow`, `sin`
        // and `sqrt` counterparts. `roundToIntegralTowardPositive` is exact and generates no
        // NaN of its own, so there is nothing left for a platform to choose: every row must
        // match on every host, and a failure here is a real disagreement rather than a known
        // latitude.
        ceilingSpecialCases
        |> List.choose (fun (x, expected) ->
            let host = BitConverter.DoubleToUInt64Bits (Math.Ceiling x)

            if host = expected then
                None
            else
                Some $"host ceiling(%.17g{x}): expected bits %016x{expected}, got %016x{host}"
        )
        |> reportFailures

    /// Whether `r` really is the integral value nearest to `x` with ties going to the even one,
    /// decided without reference to any other rounding implementation: `r` must be an integer,
    /// no other integer may be strictly nearer to `x`, and where `r` is exactly half a unit away
    /// — so that its other neighbour is equally near — `r` must be the even one. Every double is
    /// a dyadic rational, so `BigInteger` settles all of this exactly.
    ///
    /// Note what this deliberately does not see: `atExponent` maps both zeros to 0, so the
    /// predicate cannot tell `round(-0.25) = -0` from `+0`. That sign is pinned by the special
    /// case table and by the bit-for-bit host property instead.
    let private isRoundOf (x : float) (r : float) : bool =
        // One octave below the smallest subnormal, so that every double is an exact integer
        // count of `2^fixedPoint` and `one` below is representable in the same units.
        let fixedPoint = -1080
        let one = BigInteger.One <<< -fixedPoint
        let half = one >>> 1

        let xFixed = atExponent fixedPoint x
        let rFixed = atExponent fixedPoint r
        let distance = BigInteger.Abs (rFixed - xFixed)

        // `r` is an integer, and no integer is nearer: the nearest integer to anything is within
        // half a unit of it, with equality only at a midpoint.
        (rFixed % one).IsZero
        && distance <= half
        // At a midpoint the other neighbour is equally near, so the tie-break decides. `r / 1`
        // is the integer `r` names; it must be even.
        && (distance < half || ((rFixed / one) % BigInteger 2).IsZero)

    [<Test>]
    let ``round matches its exact definition`` () : unit =
        // As with `ceiling`: `roundToIntegralTiesToEven` is exact, so there is a single right
        // answer and it is checked against the definition itself rather than against another
        // implementation.
        let property (x : float) : bool = isRoundOf x (DeterministicMath.round x)

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``round agrees with the host bit-for-bit`` () : unit =
        // The host is an exact oracle here rather than a bound, as for `ceiling`: any
        // disagreement at all, including about the sign of a zero, is a bug in one of the two.
        let property (x : float) : bool =
            let actual = DeterministicMath.round x
            BitConverter.DoubleToUInt64Bits actual = BitConverter.DoubleToUInt64Bits (Math.Round x)

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``round is idempotent`` () : unit =
        // The result is integral, so a second application must change nothing at all — not even
        // the sign of the negative zero the first one may have produced.
        let property (x : float) : bool =
            let once = DeterministicMath.round x
            let twice = DeterministicMath.round once
            BitConverter.DoubleToUInt64Bits twice = BitConverter.DoubleToUInt64Bits once

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``round is the identity on doubles that are already integral`` () : unit =
        // At or above 2^52 in magnitude a double's ulp is at least 1, so it is an integer and
        // must come back untouched. This is the `exponent >= 0` fast path, which about half of
        // `genFiniteDouble`'s draws reach; the rest pass vacuously, and are covered by the
        // properties above instead.
        let property (x : float) : bool =
            if abs x < 4503599627370496.0 then
                true
            else
                BitConverter.DoubleToUInt64Bits (DeterministicMath.round x) = BitConverter.DoubleToUInt64Bits x

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``round is odd`` () : unit =
        // `round(-x) = -round(x)` on the nose, including at the zeros. Ties-to-even is symmetric
        // about zero — unlike, say, round-half-up — so the negation may be taken before or after
        // the rounding. Compared on bits, so that the two zeros are distinguished.
        let property (x : float) : bool =
            let negatedAfter = -(DeterministicMath.round x)
            let negatedBefore = DeterministicMath.round -x

            BitConverter.DoubleToUInt64Bits negatedAfter = BitConverter.DoubleToUInt64Bits negatedBefore

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``round moves its argument by at most half a unit`` () : unit =
        // The bound a caller actually relies on, stated on its own rather than as a corollary of
        // the definition above, and in exact arithmetic so that the subtraction cannot itself
        // round.
        let property (x : float) : bool =
            let fixedPoint = -1080
            let half = BigInteger.One <<< (-fixedPoint - 1)

            let difference =
                atExponent fixedPoint (DeterministicMath.round x) - atExponent fixedPoint x

            BigInteger.Abs difference <= half

        for generator in [ genFiniteDouble ; genFractionalDouble ; genNearIntegerDouble ] do
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``round is monotone`` () : unit =
        // A non-decreasing function of a non-decreasing argument, exactly rather than up to an
        // error term.
        let property (a : float, b : float) : bool =
            let smaller, larger = if a <= b then a, b else b, a
            DeterministicMath.round smaller <= DeterministicMath.round larger

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genFiniteDouble genFiniteDouble)) property)

    [<Test>]
    let ``round is a pure function of its argument`` () : unit =
        let property (x : float) : bool =
            let first = DeterministicMath.round x
            let second = DeterministicMath.round x
            BitConverter.DoubleToUInt64Bits first = BitConverter.DoubleToUInt64Bits second

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genFiniteDouble) property)

    [<Test>]
    let ``round agrees with the host on a dense sweep`` () : unit =
        // Quarter-integers either side of zero, which is where the sign rules and the tie-break
        // bite and where a random draw over a binade would spend nothing at all: every
        // fractional part in sight is one a generator of uniform mantissas essentially never
        // produces, and every other whole number below carries the opposite parity.
        [ -2000 .. 2000 ]
        |> List.collect (fun n ->
            [ -0.75 ; -0.5 ; -0.25 ; 0.0 ; 0.25 ; 0.5 ; 0.75 ]
            |> List.map (fun offset -> float n + offset)
        )
        |> List.choose (fun x ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.round x)
            let host = BitConverter.DoubleToUInt64Bits (Math.Round x)

            if actual = host && isRoundOf x (DeterministicMath.round x) then
                None
            else
                Some $"round(%.17g{x}): got %016x{actual}, host %016x{host}"
        )
        |> reportFailures

    /// `(argument, expected bits)` for the arguments on which IEEE 754 fixes an answer exactly —
    /// which for this operation is all of them, so this table is a spread of interesting shapes
    /// rather than an enumeration of exceptions. Stated in bits so that the sign of a zero and
    /// the payload of a NaN are pinned rather than compared by an equality that ignores them.
    let private roundSpecialCases : (float * uint64) list =
        let ofBits (b : uint64) : float = BitConverter.UInt64BitsToDouble b
        let bits (v : float) : uint64 = BitConverter.DoubleToUInt64Bits v

        // The last binade in which a double can be non-integral: its ulp is 1/2, so the value
        // below is exactly representable, and it is a midpoint between the odd 2^52 - 1 and the
        // even 2^52.
        let justBelowTwoToThe52 = 4503599627370495.5

        [
            // Zeros and infinities are integral already and come back with their signs.
            0.0, bits 0.0
            -0.0, bits (-0.0)
            infinity, bits infinity
            -infinity, bits (-infinity)

            // Anything of magnitude at most 1/2 rounds to a zero carrying the operand's sign.
            // The negative rows are the sign rule an implementation is likely to get wrong: the
            // natural integer arithmetic produces a zero with no sign attached.
            Double.Epsilon, bits 0.0
            1e-320, bits 0.0
            0.25, bits 0.0
            0.5, bits 0.0
            -Double.Epsilon, bits (-0.0)
            -1e-320, bits (-0.0)
            -0.25, bits (-0.0)
            -0.5, bits (-0.0)

            // 0.49999999999999994 is the double immediately below 1/2: the classic trap for an
            // implementation written as floor(x + 1/2), where the addition itself rounds up to
            // exactly 1 and the answer comes out one too large.
            0.49999999999999994, bits 0.0
            -0.49999999999999994, bits (-0.0)
            Math.BitIncrement 0.5, bits 1.0
            -(Math.BitIncrement 0.5), bits (-1.0)

            // Ties go to the even neighbour, in both directions and on both sides of zero —
            // which is what separates this operation from every "half away from zero" rule.
            1.5, bits 2.0
            2.5, bits 2.0
            3.5, bits 4.0
            4.5, bits 4.0
            -1.5, bits (-2.0)
            -2.5, bits (-2.0)
            -3.5, bits (-4.0)
            -4.5, bits (-4.0)

            // Away from a midpoint the nearer integer wins, symmetrically in the sign — unlike
            // `ceiling`, whose two columns differ.
            0.75, bits 1.0
            -0.75, bits (-1.0)
            1.25, bits 1.0
            -1.25, bits (-1.0)
            123.456, bits 123.0
            -123.456, bits (-123.0)
            123.567, bits 124.0
            -123.567, bits (-124.0)

            // Integers are their own rounding, with no sign surprises.
            1.0, bits 1.0
            -1.0, bits (-1.0)
            2.0, bits 2.0
            -2.0, bits (-2.0)

            // The boundary of the integral range. 2^52 is the smallest magnitude whose ulp is 1;
            // the row below it is the largest non-integral double there is, and is a tie whose
            // even neighbour is the one *away* from zero...
            justBelowTwoToThe52, bits 4503599627370496.0
            -justBelowTwoToThe52, bits (-4503599627370496.0)
            // ...while one ulp lower is a tie whose even neighbour is the one *towards* zero, so
            // between them the pair pins the tie-break rather than a fixed direction.
            4503599627370494.5, bits 4503599627370494.0
            -4503599627370494.5, bits (-4503599627370494.0)
            4503599627370496.0, bits 4503599627370496.0
            -4503599627370496.0, bits (-4503599627370496.0)

            // Beyond it nothing can be fractional, right out to the ends of the range.
            1e300, bits 1e300
            -1e300, bits (-1e300)
            Double.MaxValue, bits Double.MaxValue
            Double.MinValue, bits Double.MinValue

            // A NaN argument comes back with its payload and sign intact, quietened if it was
            // signalling — the same rule as `ceiling`, and likewise not a place where platforms
            // differ, so no host exemption is needed for any of these.
            ofBits 0x7FF8000000000000UL, 0x7FF8000000000000UL
            ofBits 0xFFF8000000000000UL, 0xFFF8000000000000UL
            ofBits 0x7FF8000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF8000000000123UL, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000123UL, 0x7FF8000000000123UL
            ofBits 0xFFF0000000000123UL, 0xFFF8000000000123UL
            ofBits 0x7FF0000000000001UL, 0x7FF8000000000001UL
        ]

    [<Test>]
    let ``the round special cases satisfy the exact definition`` () : unit =
        // Keeps the table above honest independently of `round`, so that a mistyped constant
        // fails whatever the implementation does. Covers every finite row; the infinities and
        // NaNs have no integer to compare against and are pinned by the table alone.
        roundSpecialCases
        |> List.choose (fun (x, expected) ->
            let result = BitConverter.UInt64BitsToDouble expected

            if Double.IsNaN x || Double.IsInfinity x then
                None
            elif isRoundOf x result then
                None
            else
                Some $"the table's round(%.17g{x}) = %016x{expected} is not the nearest integer, ties to even"
        )
        |> reportFailures

    [<Test>]
    let ``round matches the IEEE 754 special cases`` () : unit =
        roundSpecialCases
        |> List.choose (fun (x, expected) ->
            let actual = BitConverter.DoubleToUInt64Bits (DeterministicMath.round x)

            if actual = expected then
                None
            else
                Some $"round(%.17g{x}): expected bits %016x{expected}, got %016x{actual}"
        )
        |> reportFailures

    [<Test>]
    let ``the host agrees about the round special cases`` () : unit =
        // As for `ceiling`, no table of permitted alternatives accompanies this one:
        // `roundToIntegralTiesToEven` is exact and generates no NaN of its own, so there is
        // nothing left for a platform to choose and a failure here is a real disagreement.
        roundSpecialCases
        |> List.choose (fun (x, expected) ->
            let host = BitConverter.DoubleToUInt64Bits (Math.Round x)

            if host = expected then
                None
            else
                Some $"host round(%.17g{x}): expected bits %016x{expected}, got %016x{host}"
        )
        |> reportFailures
