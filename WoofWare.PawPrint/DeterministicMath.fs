namespace WoofWare.PawPrint

open System
open System.Numerics

/// Host-independent implementations of the transcendental `System.Math` primitives that
/// CoreCLR implements as `InternalCall`s straight down to the platform C library.
///
/// The interpreter cannot simply forward these to the host's `System.Math`. `pow` and its
/// relatives are not correctly rounded and are not required to agree bit-for-bit between
/// libm implementations, so a run recorded on one machine could replay differently on
/// another — silently, and only in the last bit, which is the worst failure mode this
/// project has. Everything here is therefore computed in-tree, from integer arithmetic
/// only, and depends on nothing but its arguments.
///
/// The strategy is to carry far more precision than a double needs and round once at the
/// end. Intermediate reals are held as `BigInteger` fixed-point values scaled by
/// 2^-`fractionBits`, giving roughly 200 bits of headroom below the 53 a double keeps.
/// The result is therefore the correctly rounded one unless the true value lies within
/// about 2^-200 of the midpoint between two doubles — see `pow` for when that can happen.
[<RequireQualifiedAccess>]
module DeterministicMath =

    /// Number of fractional bits in the fixed-point representation used throughout.
    /// Everything below needs about 130 of these (the error analysis in `pow`); the rest
    /// is slack, and costs only a little `BigInteger` work in an already slow interpreter.
    let internal fractionBits : int = 256

    /// The fixed-point representation of 1.0.
    let private scale : BigInteger = BigInteger.One <<< fractionBits

    /// Fixed-point multiply. Division rather than a right shift because `>>>` on
    /// `BigInteger` floors, which would leave a negative operand stuck at -1 forever
    /// instead of shrinking to zero, and the series below terminate on reaching zero.
    let private mulFixed (a : BigInteger) (b : BigInteger) : BigInteger = a * b / scale

    /// Fixed-point divide.
    let private divFixed (a : BigInteger) (b : BigInteger) : BigInteger = (a <<< fractionBits) / b

    /// The fixed-point value of `numerator / denominator`, for an exact integer ratio.
    let private ofRatio (numerator : BigInteger) (denominator : BigInteger) : BigInteger =
        (numerator <<< fractionBits) / denominator

    /// atanh, by its Taylor series `s + s^3/3 + s^5/5 + ...`. Callers keep |s| below
    /// (sqrt 2 - 1)/(sqrt 2 + 1) ≈ 0.172, so successive terms shrink by a factor of at
    /// least 34 and the loop runs about 50 times before the term underflows to zero.
    let private atanh (s : BigInteger) : BigInteger =
        let sSquared = mulFixed s s
        let mutable term = s
        let mutable acc = BigInteger.Zero
        let mutable k = 1

        while not term.IsZero do
            acc <- acc + term / BigInteger k
            term <- mulFixed term sSquared
            k <- k + 2

        acc

    /// ln 2 = 2 * atanh(1/3), computed rather than transcribed so that there is no
    /// hand-copied constant to get wrong.
    let private lnTwo : BigInteger = BigInteger 2 * atanh (scale / BigInteger 3)

    /// exp, by its Taylor series. Callers keep `u` in [0, ln 2), so this converges.
    let private expSmall (u : BigInteger) : BigInteger =
        let mutable term = scale
        let mutable acc = BigInteger.Zero
        let mutable k = 1

        while not term.IsZero do
            acc <- acc + term
            term <- mulFixed term u / BigInteger k
            k <- k + 1

        acc

    /// Split a finite `x` into an exact `(mantissa, exponent)` with
    /// `x = mantissa * 2^exponent`. The mantissa carries the sign.
    let internal decompose (x : float) : BigInteger * int =
        let bits = BitConverter.DoubleToUInt64Bits x
        let biasedExponent = int ((bits >>> 52) &&& 0x7FFUL)
        let fraction = bits &&& 0xF_FFFF_FFFF_FFFFUL

        let magnitude, exponent =
            if biasedExponent = 0 then
                // Subnormal (or zero): no implicit leading bit, and a fixed exponent.
                BigInteger fraction, -1074
            else
                // Normal: restore the implicit leading bit. The value is
                // (2^52 + fraction) * 2^(biasedExponent - 1023 - 52).
                BigInteger (fraction ||| 0x10_0000_0000_0000UL), biasedExponent - 1075

        (if Double.IsNegative x then -magnitude else magnitude), exponent

    /// 2^k as a double, for -1074 <= k <= 1023. Built from bits rather than by
    /// multiplication so that the subnormal end is exact too.
    let private twoPow (k : int) : float =
        if k >= -1022 then
            BitConverter.UInt64BitsToDouble (uint64 (k + 1023) <<< 52)
        else
            BitConverter.UInt64BitsToDouble (1UL <<< (k + 1074))

    /// The double nearest to the exact value `mantissa * 2^exponent`, breaking ties to
    /// even, saturating to an infinity on overflow and to a zero on underflow. This is the
    /// single place where precision is lost, which is what lets the callers above reason
    /// about correct rounding by an error budget alone.
    let internal roundToDouble (mantissa : BigInteger) (exponent : int) : float =
        if mantissa.IsZero then
            0.0
        else

        let isNegative = mantissa.Sign < 0
        let magnitude = BigInteger.Abs mantissa

        // The exact value lies in [2^valueExponent, 2^(valueExponent + 1)).
        let valueExponent = int (magnitude.GetBitLength ()) - 1 + exponent

        if valueExponent >= 1024 then
            // Every double is strictly below 2^1024.
            if isNegative then -infinity else infinity
        else

        // The exponent of the last bit the result can represent: one ulp of a normal at
        // this magnitude, or the fixed ulp of the subnormal range, whichever is coarser.
        let ulpExponent = max (valueExponent - 52) -1074
        let shift = ulpExponent - exponent

        let rounded =
            if shift <= 0 then
                // The result is finer-grained than the input; no bits are discarded.
                magnitude <<< -shift
            else
                let truncated = magnitude >>> shift
                let remainder = magnitude - (truncated <<< shift)
                let half = BigInteger.One <<< (shift - 1)

                if remainder > half || (remainder = half && not truncated.IsEven) then
                    truncated + BigInteger.One
                else
                    truncated

        if rounded.IsZero then
            if isNegative then -0.0 else 0.0
        else

        // `rounded` is at most 2^53 and so converts exactly; `ulpExponent` is in
        // [-1074, 971] here, so `twoPow` is in range and the product is a single exact
        // IEEE multiply (or an overflow to infinity, which is the right answer).
        let result = float rounded * twoPow ulpExponent
        if isNegative then -result else result

    /// log2 of a strictly positive finite `x`, in fixed point.
    let internal log2 (x : float) : BigInteger =
        let mantissa, exponent = decompose x
        let bitLength = int (mantissa.GetBitLength ())

        // x = m * 2^e, where m = mantissa / 2^(bitLength - 1) lies in [1, 2).
        let mutable denominator = BigInteger.One <<< (bitLength - 1)
        let mutable e = exponent + bitLength - 1

        // Halve m if it is at least sqrt 2, bringing it into [sqrt(1/2), sqrt 2) so that
        // the atanh argument below stays under 0.172. The test `m >= sqrt 2` is
        // `mantissa^2 >= 2^(2 * bitLength - 1)`, which is exact in integers — no square
        // root, and no constant to round.
        if mantissa * mantissa >= (BigInteger.One <<< (2 * bitLength - 1)) then
            denominator <- denominator <<< 1
            e <- e + 1

        // ln m = 2 * atanh((m - 1) / (m + 1)), and the ratio is exact in integers.
        let s = ofRatio (mantissa - denominator) (mantissa + denominator)
        let lnMantissa = BigInteger 2 * atanh s

        // A power of two takes s = 0 exactly, so log2 comes out exactly integral there.
        (BigInteger e <<< fractionBits) + divFixed lnMantissa lnTwo

    /// 2 raised to the fixed-point power `t`, rounded to a double.
    let internal exp2 (t : BigInteger) : float =
        // Bound the work before splitting `t`: `t` can legitimately be astronomically
        // large (a base near 1 with a huge exponent), and everything outside this band
        // rounds to an infinity or a zero regardless of its fractional part.
        if t >= BigInteger 1024 * scale then
            infinity
        elif t < BigInteger -1080 * scale then
            0.0
        else

        // Arithmetic shift floors, which is what is wanted: `fraction` lands in [0, 1).
        let integerPart = t >>> fractionBits
        let fraction = t - (integerPart <<< fractionBits)

        // 2^fraction = exp(fraction * ln 2), and the argument is in [0, ln 2).
        let significand = expSmall (mulFixed fraction lnTwo)

        roundToDouble significand (int integerPart - fractionBits)

    /// `x^y` for strictly positive finite `x` and finite `y`, via logarithms. Exposed
    /// separately from `pow` so that the accuracy of this path — the one every
    /// non-integer exponent takes — can be measured against an exact reference.
    let internal powOfPositiveViaLogarithm (x : float) (y : float) : float =
        let logarithm = log2 x

        // Multiply by `y` exactly: `y` is a dyadic rational, so this is an integer
        // multiply and a shift.
        let mantissa, exponent = decompose y
        let product = logarithm * mantissa

        let scaled =
            if exponent >= 0 then
                product <<< exponent
            else
                product >>> -exponent

        exp2 scaled

    [<RequireQualifiedAccess>]
    type private IntegerKind =
        | NotAnInteger
        | EvenInteger
        | OddInteger

    /// Whether `y` is a mathematical integer and, if so, its parity — which is what
    /// decides the sign of a negative base raised to it.
    let private classifyInteger (y : float) : IntegerKind =
        if not (Double.IsFinite y) then
            IntegerKind.NotAnInteger
        elif y = 0.0 then
            IntegerKind.EvenInteger
        else

        let mantissa, exponent = decompose (abs y)

        if exponent >= 1 then
            // A mantissa shifted left by at least one bit is even.
            IntegerKind.EvenInteger
        elif exponent = 0 then
            if mantissa.IsEven then
                IntegerKind.EvenInteger
            else
                IntegerKind.OddInteger
        else

        // An integer iff the bits the exponent would shift out are all zero.
        let discarded = mantissa &&& ((BigInteger.One <<< -exponent) - BigInteger.One)

        if not discarded.IsZero then
            IntegerKind.NotAnInteger
        elif (mantissa >>> -exponent).IsEven then
            IntegerKind.EvenInteger
        else
            IntegerKind.OddInteger

    /// The quiet NaN produced for a domain error. IEEE 754 leaves the payload of a
    /// generated NaN to the implementation and real platforms differ — x86 hardware
    /// produces the negative quiet NaN (which is what `Double.NaN` is), Arm the positive
    /// one — so a host-independent runtime has to pick. This is IEEE 754's own
    /// recommendation, and the one Arm produces.
    let private quietNaN : float = BitConverter.UInt64BitsToDouble 0x7FF8000000000000UL

    /// A NaN operand may not simply be handed back: IEEE 754 requires an operation given a
    /// *signaling* NaN to raise the invalid-operation exception and deliver a quiet one, so
    /// the result must have the quiet bit set with the sign and payload otherwise preserved.
    /// The platform libm CoreCLR calls does exactly this, and unlike the choice of payload
    /// for a freshly generated NaN it does not vary between platforms, so matching it costs
    /// nothing in determinism. Setting the quiet bit cannot turn a NaN into an infinity: the
    /// remaining payload bits are what made it a NaN, and they are untouched.
    let private quieted (x : float) : float =
        BitConverter.UInt64BitsToDouble (BitConverter.DoubleToUInt64Bits x ||| 0x0008000000000000UL)

    /// The largest integer exponent handled by exact integer exponentiation. Chosen so
    /// that the intermediate `BigInteger` stays under about 54 000 bits.
    let private maxExactPower = 1024

    /// `x` raised to the power `y`, with the semantics of IEEE 754 / C99 `pow` — which is
    /// what CoreCLR's `Math.Pow` inherits from the platform C library.
    ///
    /// Accuracy: a non-negative integer exponent up to `maxExactPower` is computed
    /// exactly and rounded once, so it is correctly rounded without qualification. Every
    /// other case goes through `powOfPositiveViaLogarithm`, whose fixed-point error is
    /// under 2^-128 relative (about 75 bits below a double's own precision), so it too is
    /// correctly rounded unless the true result lies within that of a tie. That can only
    /// happen when the true result is an exact midpoint between two doubles, and:
    ///
    ///  - for a negative integer exponent, `x^-n` is a midpoint only if `x` is a power of
    ///    two, and those the logarithm path already gets exactly right (log2 of a power of
    ///    two is an exact integer, so no rounding happens at all);
    ///  - for a non-integer exponent, no case is known; it would need `x^(p/2^k)` to be an
    ///    odd 54-bit integer for a double `x`, which the obvious constructions rule out.
    ///
    /// Where a tie does arise the break is arbitrary rather than to-even. That is a
    /// divergence from a correctly rounded implementation, but not one from the real
    /// runtime, whose libm is not correctly rounded there either.
    ///
    /// Measured against macOS/Arm's libm over 200 000 random (base, exponent) pairs
    /// spanning the whole exponent range: 199 975 bit-identical, and all 25 disagreements
    /// were one ulp apart on inputs whose true value sits just under a midpoint — this
    /// implementation was between 0.4898 and 0.4993 ulp from the true value on every one
    /// of them, and the host between 0.5008 and 0.5102. In other words the residual
    /// disagreement is the host's rounding error, not ours; there was no input on which
    /// this implementation was the further of the two.
    let pow (x : float) (y : float) : float =
        // Ordered to match IEEE 754's own case analysis: the two cases that override a
        // NaN operand come first.
        if y = 0.0 then
            // Including x = NaN.
            1.0
        elif x = 1.0 then
            // Including y = NaN.
            1.0
        elif Double.IsNaN x then
            quieted x
        elif Double.IsNaN y then
            quieted y
        else

        let yKind = classifyInteger y
        let signIsNegative = Double.IsNegative x && yKind = IntegerKind.OddInteger

        if Double.IsInfinity y then
            // x is not NaN and not 1 here, so only |x| = 1 is left to special-case.
            let magnitude = abs x

            if magnitude = 1.0 then 1.0
            elif (magnitude > 1.0) = (y > 0.0) then infinity
            else 0.0
        elif x = 0.0 then
            // y is finite and non-zero.
            if y < 0.0 then
                (if signIsNegative then -infinity else infinity)
            else
                (if signIsNegative then -0.0 else 0.0)
        elif Double.IsInfinity x then
            if y < 0.0 then
                (if signIsNegative then -0.0 else 0.0)
            else
                (if signIsNegative then -infinity else infinity)
        elif Double.IsNegative x && yKind = IntegerKind.NotAnInteger then
            // A negative base has no real non-integer power.
            quietNaN
        else

        let magnitude = abs x

        let result =
            if yKind <> IntegerKind.NotAnInteger && y > 0.0 && y <= float maxExactPower then
                // `x` is a dyadic rational, so `x^n` is one too and fits exactly in a
                // `BigInteger`. Computing it that way makes even exact ties — a result
                // that is an odd 54-bit integer, such as 3^34 — round to even correctly,
                // which an approximation of any finite precision cannot guarantee.
                let mantissa, exponent = decompose magnitude
                let power = int y
                roundToDouble (BigInteger.Pow (mantissa, power)) (exponent * power)
            else
                powOfPositiveViaLogarithm magnitude y

        if signIsNegative then -result else result
