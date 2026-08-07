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
        // Cosine is even as a real function, and the implementation reduces |x|, so this must
        // hold exactly rather than to within a rounding.
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
            // A NaN propagates with its sign and payload intact, as IEEE 754 clause 7.2
            // recommends; `Double.NaN` is the *negative* quiet NaN.
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
            // macOS/Arm's `cos` clears the sign of a NaN argument rather than propagating it,
            // where clause 7.2 recommends returning the input NaN quieted. Payload survives
            // either way, so the alternative differs from the specified answer only in the
            // sign bit.
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
