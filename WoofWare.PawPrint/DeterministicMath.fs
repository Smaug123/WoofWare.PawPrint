namespace WoofWare.PawPrint

open System
open System.Numerics

/// Host-independent implementations of the `System.Math` primitives that CoreCLR's JIT
/// lowers either to a call into the platform C library or to a machine instruction, rather
/// than executing as ordinary managed IL.
///
/// Most of them CoreCLR declares as `InternalCall`s with no IL body at all. `round` is the
/// exception: it has a body, but that body is a managed emulation of the instruction the JIT
/// emits rather than a definition — see its own comment.
///
/// The interpreter cannot forward these to the host's `System.Math`. `pow` and its
/// transcendental relatives are not correctly rounded and are not required to agree
/// bit-for-bit between libm implementations, so a run recorded on one machine could replay
/// differently on another — silently, and only in the last bit, which is the worst failure
/// mode this project has. Everything here is therefore computed in-tree, from integer
/// arithmetic only, and depends on nothing but its arguments. (`sqrt`, `ceiling` and `round`
/// are exceptions to the *motivation* rather than to the rule: all three are exactly
/// specified, so the host would have agreed anyway. See their own comments for why they are
/// here.)
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

    /// A NaN operand may not be handed back unchanged: IEEE 754 requires an operation given a
    /// *signaling* NaN to raise the invalid-operation exception and deliver a quiet one, so
    /// the result must have the quiet bit set with the sign and payload otherwise preserved.
    /// The platform libm CoreCLR calls does exactly this, and unlike the choice of payload
    /// for a freshly generated NaN it does not vary between platforms, so matching it costs
    /// nothing in determinism. Setting the quiet bit cannot turn a NaN into an infinity: the
    /// remaining payload bits are what made it a NaN, and they are untouched.
    let private quieted (x : float) : float =
        BitConverter.UInt64BitsToDouble (BitConverter.DoubleToUInt64Bits x ||| 0x0008000000000000UL)

    /// A NaN whose leading significand bit is clear: IEEE 754's *signaling* NaN, the one an
    /// operation must not silently swallow.
    let private isSignallingNaN (x : float) : bool =
        Double.IsNaN x
        && BitConverter.DoubleToUInt64Bits x &&& 0x0008000000000000UL = 0UL

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
        // Ordered to match IEEE 754's own case analysis. Clause 9.2.1 gives two cases that
        // override a NaN operand -- pow(x, ±0) is 1 "for any x, even a zero, quiet NaN, or
        // infinity", and pow(+1, y) is 1 "for any y, even a quiet NaN" -- but both say
        // *quiet* NaN, where the rest of the table just says NaN. A signalling NaN therefore
        // falls back to the general rule of clause 7.2 (raise invalid-operation, deliver a
        // quiet NaN) and beats the overrides instead.
        //
        // Platforms differ on this. glibc implements the reading above
        // (`return issignaling_inline (x) ? x + y : 1.0;` in
        // sysdeps/ieee754/dbl-64/e_pow.c, where the addition is what quietens the operand),
        // and Apple's libm returns 1 unconditionally. We follow the standard, which is also
        // the behaviour of the linux-x64 host this is differentially tested against in CI.
        if isSignallingNaN x then
            quieted x
        elif isSignallingNaN y then
            quieted y
        elif y = 0.0 then
            // Including x = quiet NaN.
            1.0
        elif x = 1.0 then
            // Including y = quiet NaN.
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

    /// The integer square root of a non-negative `n`: the greatest `r` with `r * r <= n`.
    ///
    /// Newton's iteration, run entirely in integers so that it is exact rather than
    /// approximate. Starting at or above the answer, `x -> (x + n/x) / 2` decreases strictly
    /// until it reaches `floor(sqrt n)` and never goes below it, so the first non-decrease is
    /// the termination condition and the value before it is the answer. `2^ceil(b/2)` is such
    /// a starting point: `n < 2^b` gives `sqrt n < 2^(b/2)`.
    let internal integerSqrt (n : BigInteger) : BigInteger =
        if n.Sign < 0 then
            failwith $"DeterministicMath: integerSqrt of the negative value %O{n}"
        elif n.IsZero then
            // The iteration below would divide by its own zero starting point.
            BigInteger.Zero
        else

        let mutable current = BigInteger.One <<< ((int (n.GetBitLength ()) + 1) / 2)
        let mutable next = (current + (n / current)) >>> 1

        while next < current do
            current <- next
            next <- (current + (n / current)) >>> 1

        current

    /// How far `sqrt` widens the mantissa before taking its integer square root. Two
    /// constraints: it must be even, so that halving the exponent that comes with it stays
    /// exact; and it must leave the root wider than the 54 significant bits a rounding
    /// midpoint has, which is what the argument on `sqrt` below turns on. A mantissa of `L`
    /// bits gives a root of `ceil((L + 128) / 2)`, so with `L` between 1 (the smallest
    /// subnormal) and 54 (a normal after the parity adjustment) the root runs from 65 bits to
    /// 91 — comfortably clear at both ends.
    let private sqrtGuardBits : int = 128

    /// The square root of `x`, with the semantics of IEEE 754's `squareRoot` operation —
    /// which is what CoreCLR's `Math.Sqrt` inherits from the hardware instruction the JIT
    /// emits for it.
    ///
    /// This function is the odd one out in this module. `pow`, `sin` and
    /// `cos` are clause 9.2 *recommended* operations that no mainstream libm rounds correctly,
    /// so computing them here changes the answer in the last bit; `squareRoot` is a clause
    /// 5.4.1 *required* operation, every platform implements it as a correctly rounded
    /// hardware instruction, and so this implementation agrees with the host bit-for-bit on
    /// every argument on which IEEE 754 fixes an answer at all. Computing it in-tree is
    /// therefore not about changing the result but about not having to trust that the host
    /// conforms — and about giving the tests an exact oracle, which the other three lack.
    ///
    /// Correct rounding here is a proof rather than an error budget. `integerSqrt` returns
    /// `floor(sqrt scaled)` exactly, so the true root lies in `[root, root + 1)` and equals
    /// `root` precisely when the remainder vanishes; `2 * root` with a sticky bit therefore
    /// names a value on the same side of every 53-bit rounding boundary as the truth. It can
    /// never sit *on* one: with the sticky bit set the value is odd and has at least 66 bits,
    /// while a midpoint has at most 54, and without it the root is `2^(sqrtGuardBits / 2)`
    /// times an integer of at most 27 bits and so is representable exactly. (Nor can the true
    /// square root of a double ever be a midpoint: that would need a 54-bit odd square root,
    /// whose square has 107 bits and is not a double.)
    let sqrt (x : float) : float =
        if Double.IsNaN x then
            quieted x
        elif x = 0.0 then
            // Both zeros are their own square root; clause 5.4.1 gives squareRoot(-0) = -0,
            // so this must come before the negative case below rather than after it.
            x
        elif Double.IsNegative x then
            // Every other negative, including -infinity, is a domain error.
            quietNaN
        elif Double.IsPositiveInfinity x then
            infinity
        else

        // x = mantissa * 2^exponent exactly, with mantissa > 0. Halving the exponent needs it
        // to be even, so a spare factor of two moves into the mantissa when it is not.
        let mantissa, exponent = decompose x

        let mantissa, exponent =
            if exponent % 2 = 0 then
                mantissa, exponent
            else
                mantissa <<< 1, exponent - 1

        let scaled = mantissa <<< sqrtGuardBits
        let root = integerSqrt scaled

        let sticky =
            if root * root = scaled then
                BigInteger.Zero
            else
                BigInteger.One

        // sqrt(x) = sqrt(scaled) * 2^((exponent - sqrtGuardBits) / 2), and the bracketing
        // value is (2 * root + sticky) / 2 -- hence the extra -1 on the exponent. Both terms
        // of the subtraction are even, so the halving is exact.
        roundToDouble ((root <<< 1) + sticky) (((exponent - sqrtGuardBits) / 2) - 1)

    /// The smallest integral double at or above `x`, with the semantics of IEEE 754's
    /// `roundToIntegralTowardPositive` (clause 5.9) — which is what CoreCLR's `Math.Ceiling`
    /// inherits from the `roundsd`/`frintp` instruction the JIT emits for it.
    ///
    /// This is the least approximate function in the module: no rounding is involved at all.
    /// Clause 5.9 makes the result exact, and every double's ceiling is itself a double
    /// (a non-integral double has magnitude below 2^52, so adding one to its truncation
    /// cannot leave the exactly-representable integers), so there is no error term to budget
    /// and nothing for the fixed-point machinery above to do. It lives here beside `sqrt` for
    /// the same reason that one does: to keep the promise this runtime's own rather than a
    /// property of the host that recorded the run, and to give the tests an exact oracle.
    ///
    /// The two signs that are easy to get wrong are both specified rather than open: the
    /// `roundToIntegral` operations take the sign of a zero result from the operand. So the
    /// ceiling of an argument strictly between -1 and 0 is *negative* zero rather than
    /// positive, and the ceiling of a negative zero is that same negative zero. C says the
    /// same of `ceil` (C17 F.10.6.1), and the hardware obeys, so these are asserted against
    /// the host in `TestDeterministicMath` rather than merely chosen here.
    let ceiling (x : float) : float =
        if Double.IsNaN x then
            // Both hardware instructions propagate a NaN operand with its sign and payload,
            // quietening a signalling one -- the same rule as `sqrt`, and likewise not a
            // place where platforms differ.
            quieted x
        elif Double.IsInfinity x || x = 0.0 then
            // Already integral, and their signs are part of the answer. `x = 0.0` catches
            // -0 as well as +0, which is exactly what is wanted: both are returned unchanged.
            x
        else

        // x = mantissa * 2^exponent exactly, with the sign carried by the mantissa.
        let mantissa, exponent = decompose x

        if exponent >= 0 then
            // An integer times a non-negative power of two is already integral.
            x
        else

        // `>>>` on a `BigInteger` is an arithmetic shift, so `truncated` is the floor of
        // `mantissa / 2^-exponent` for a negative mantissa as well as a positive one. The
        // ceiling is one more than the floor exactly when something was discarded.
        let shift = -exponent
        let truncated = mantissa >>> shift

        let ceiled =
            if (truncated <<< shift) = mantissa then
                truncated
            else
                truncated + BigInteger.One

        if ceiled.IsZero then
            // Only reachable from -1 < x < 0, whose ceiling clause 5.9 gives as -0. This case
            // has to be handled here because `roundToDouble` takes the sign from its mantissa
            // and so cannot tell a zero that came from below from one that came from above.
            -0.0
        else
            // Exact: `ceiled` is an integer of at most 53 bits, since `exponent < 0` bounds
            // `|x|` below 2^52.
            roundToDouble ceiled 0

    /// The integral double nearest to `x`, ties going to the even one, with the semantics of
    /// IEEE 754's `roundToIntegralTiesToEven` (clause 5.9) — which is what CoreCLR's
    /// `Math.Round(double)` means.
    ///
    /// Exact, like `ceiling` and for the same reasons: clause 5.9 fixes the result, and every
    /// double's rounding is itself a double (a non-integral double has magnitude below 2^52, so
    /// stepping to the next integer cannot leave the exactly-representable integers). There is
    /// no error term to budget and nothing for the fixed-point machinery above to do.
    ///
    /// `Math.Round` is the odd one out among the `System.Math` primitives implemented here in
    /// that CoreCLR gives it an IL body rather than declaring it `InternalCall`. That body is
    /// not a definition, though: it is a managed emulation of the instruction the JIT actually
    /// emits (`roundsd` with mode 0, or `frintn`), and it obtains ties-to-even from the ambient
    /// rounding mode by computing `(a + 2^52) - 2^52`. Running it would make the answer a
    /// property of whatever performed that addition, which is the class of dependency this
    /// module exists to remove, so the operation is named here instead of inherited.
    ///
    /// The two signs that are easy to get wrong are both specified rather than open. As for
    /// `ceiling`, the `roundToIntegral` operations take the sign of a zero result from the
    /// operand — so the rounding of an argument in [-1/2, 0) is *negative* zero, and the
    /// rounding of a negative zero is that same negative zero. And the tie-break is towards the
    /// even neighbour in both directions: 2.5 and 3.5 both give 2.0 and 4.0 respectively rather
    /// than moving consistently away from zero. Both are asserted against the host in
    /// `TestDeterministicMath`.
    let round (x : float) : float =
        if Double.IsNaN x then
            // Both hardware instructions propagate a NaN operand with its sign and payload,
            // quietening a signalling one -- the same rule as `ceiling`.
            quieted x
        elif Double.IsInfinity x || x = 0.0 then
            // Already integral, and their signs are part of the answer. `x = 0.0` catches -0 as
            // well as +0, which is exactly what is wanted: both are returned unchanged.
            x
        else

        // x = mantissa * 2^exponent exactly, with the sign carried by the mantissa.
        let mantissa, exponent = decompose x

        if exponent >= 0 then
            // An integer times a non-negative power of two is already integral.
            x
        else

        // `>>>` on a `BigInteger` is an arithmetic shift, so `truncated` is the floor of
        // `mantissa / 2^-exponent` for a negative mantissa as well as a positive one, and
        // `remainder` is therefore the non-negative distance up from it. The two candidate
        // answers are `truncated` and `truncated + 1`; `remainder` against half the divisor
        // says which is nearer, and the parity of `truncated` breaks an exact tie. (Exactly
        // one of the two candidates is even, so "keep `truncated` when it is even" is the
        // same rule as "choose the even one".)
        let shift = -exponent
        let truncated = mantissa >>> shift
        let remainder = mantissa - (truncated <<< shift)
        // `shift` is at least 1 here, since `exponent < 0`.
        let half = BigInteger.One <<< (shift - 1)

        let rounded =
            if remainder > half || (remainder = half && not truncated.IsEven) then
                truncated + BigInteger.One
            else
                truncated

        if rounded.IsZero then
            // Only reachable from -1/2 <= x < 0 and from 0 < x <= 1/2. Clause 5.9 gives the
            // result the operand's sign; this case has to be handled here because
            // `roundToDouble` takes the sign from its mantissa and so cannot tell a zero that
            // came from below from one that came from above.
            if Double.IsNegative x then -0.0 else 0.0
        else
            // Exact: `exponent < 0` bounds `|x|` below 2^52, so `|rounded|` is at most 2^52 and
            // is an integer of at most 53 bits.
            roundToDouble rounded 0

    /// Number of fractional bits carried by the value of pi used for trigonometric range
    /// reduction. This is not the accuracy of the answer, which `fractionBits` governs; it
    /// is the accuracy needed to *subtract* a multiple of pi/2 from an argument that may be
    /// as large as 2^1024 without the difference becoming meaningless. Every bit of the
    /// argument's exponent is a bit of pi consumed by that subtraction, so this must exceed
    /// 1024 by whatever margin the reduced argument is then wanted to.
    let internal piBits : int = 1500

    /// `atan(1/n)` in fixed point with `bits` fractional bits, by its alternating series
    /// `1/n - 1/(3n^3) + 1/(5n^5) - ...`. Callers use n >= 5, so successive terms shrink by
    /// a factor of at least 25 and the loop runs about `bits / 4.6` times.
    ///
    /// Each division truncates towards zero, so the running `term` carries an error under
    /// `n^2 / (n^2 - 1)` units and each contribution adds under two more; over the few
    /// hundred terms that bounds the loss at under ten bits at the bottom (five, measured
    /// against a 600-digit decimal evaluation), which the margin in `piBits` absorbs many
    /// times over.
    let private atanReciprocal (bits : int) (n : int) : BigInteger =
        let nSquared = BigInteger (n * n)
        let mutable term = (BigInteger.One <<< bits) / BigInteger n
        let mutable acc = BigInteger.Zero
        let mutable k = 0

        while not term.IsZero do
            let contribution = term / BigInteger (2 * k + 1)
            acc <- (if k % 2 = 0 then acc + contribution else acc - contribution)
            term <- term / nSquared
            k <- k + 1

        acc

    /// pi, in fixed point with `piBits` fractional bits, by Machin's formula
    /// `pi = 16 atan(1/5) - 4 atan(1/239)`. Computed rather than transcribed so that there
    /// is no hand-copied constant to get wrong; `TestDeterministicMath` checks the first
    /// sixty decimal digits against the published expansion.
    let internal pi : BigInteger =
        (BigInteger 16 * atanReciprocal piBits 5)
        - (BigInteger 4 * atanReciprocal piBits 239)

    /// pi/2, the period of the quadrant reduction below.
    let private piOverTwo : BigInteger = pi >>> 1

    /// 2/pi, used to find how many quarter-turns an argument contains. Dividing once here
    /// turns the per-call reduction into a multiplication. The exponent is `2 piBits + 1`
    /// rather than `2 piBits` because `pi` is itself scaled by 2^`piBits`: the quotient of
    /// 2^(2 piBits + 1) by `pi` is 2^`piBits` times 2/pi, which is what the callers want.
    let private twoOverPi : BigInteger = (BigInteger.One <<< ((2 * piBits) + 1)) / pi

    /// `sin` or `cos` of a fixed-point argument, by the shared alternating series
    /// `t0 - t0 r^2/((k+1)(k+2)) + ...`; seeding it with `(r, 1)` gives sin and with
    /// `(1, 0)` gives cos. Callers keep |r| below about pi/4, so the factorial denominators
    /// dominate immediately and the loop runs about 30 times.
    let private trigSeries (initialTerm : BigInteger) (initialIndex : int) (rSquared : BigInteger) : BigInteger =
        let mutable term = initialTerm
        let mutable acc = BigInteger.Zero
        let mutable k = initialIndex

        while not term.IsZero do
            acc <- acc + term
            term <- -(mulFixed term rSquared) / BigInteger ((k + 1) * (k + 2))
            k <- k + 2

        acc

    /// The number of significant bits a reduced argument must retain for the accuracy
    /// argument on `sin` and `cos` to hold. A double's significand is 53 of those, so this
    /// leaves a factor of two in hand over what correct rounding needs.
    let internal reducedArgumentFloor : int = 128

    /// Reduce a finite `x` modulo pi/2. Returns `(quadrant, r)` where `quadrant` is
    /// `k % 4` for the nearest integer `k` of quarter-turns in |x|, and `r = |x| - k pi/2`
    /// is a fixed-point value with `fractionBits` fractional bits satisfying |r| <= pi/4.
    ///
    /// Sign is not part of the result: both `sin` and `cos` are determined on all of the
    /// reals by their behaviour on |x| together with a parity rule, so folding the sign in
    /// here would only give each caller a second thing to undo.
    ///
    /// This is Payne–Hanek reduction with the table replaced by a single wide constant.
    /// The subtraction `|x| - k pi/2` cancels every bit of `|x|`'s exponent, so `pi` must be
    /// known to `piBits` places for the difference to retain `piBits - 1024` of them; that
    /// is what makes a naive `x % (2 pi)` in double arithmetic useless above a few dozen
    /// bits and why `piBits` is what it is.
    let internal reduceModuloQuarterTurn (x : float) : int * BigInteger =
        let mantissa, exponent = decompose (abs x)

        // |x| (2/pi), with `piBits` fractional bits. The error is under |x| 2^-piBits,
        // i.e. under 2^-476 for any double, which decides `k` correctly unless |x| (2/pi)
        // is within that of a half-integer — and a `k` off by one is harmless anyway, since
        // it only widens |r| past pi/4 by the same negligible amount.
        let scaledByTwoOverPi = mantissa * twoOverPi

        let quarterTurns =
            if exponent >= 0 then
                scaledByTwoOverPi <<< exponent
            else
                scaledByTwoOverPi >>> -exponent

        let k = (quarterTurns + (BigInteger.One <<< (piBits - 1))) >>> piBits

        // `exponent` is at least -1074 and `piBits` far exceeds that, so this shift is
        // left and `|x|` is represented exactly.
        let xFixed = mantissa <<< (exponent + piBits)

        // The error here is under `k` 2^-piBits, again under 2^-476.
        let reduced = xFixed - (k * piOverTwo)

        // |r| <= pi/4 is what makes the series in `cos` converge, and the arithmetic above
        // delivers it without the caller arranging anything. Checked anyway, because the
        // failure mode is not a wrong answer: a reduction that
        // returns a large `r` makes the alternating series grow for as many terms as it
        // takes the factorial to overtake `r^2`, which for a badly wrong `r` is longer than
        // anyone will wait. A loud failure beats a hang.
        // The slack is 2^-64: far above the 2^-475 by which a `k` off by one can
        // overshoot pi/4, and far below the pi/4 itself, so this catches only real breakage.
        let quarterTurnCeiling = (pi >>> 2) + (BigInteger.One <<< (piBits - 64))

        if BigInteger.Abs reduced > quarterTurnCeiling then
            failwith
                $"DeterministicMath: reducing %.17g{x} modulo pi/2 left a remainder outside [-pi/4, pi/4]; the reduction is broken, not merely imprecise"

        // Down to the working precision. The shift floors rather than rounds, which costs
        // at most one unit in the last place of the result.
        (int (k &&& BigInteger 3)), (reduced >>> (piBits - fractionBits))

    /// `sin r` or `cos r` for a reduced argument `r`, which both entry points below want
    /// depending on which residue their argument fell into. `x` is passed only so that a
    /// failure can name the argument that produced it.
    ///
    /// The precision floor is checked on the sine branch alone: `cos r` is at least
    /// cos(pi/4) in magnitude for any `r` the reduction can return, so it is insensitive to
    /// how many bits `r` kept, whereas `sin r` is proportional to `r` itself and inherits
    /// whatever cancellation the reduction suffered.
    let private evaluateReduced (x : float) (isSine : bool) (r : BigInteger) : float =
        if isSine then
            // `r` is less than 1, so its bit length as a fixed-point value *is* its number
            // of significant bits.
            let significantBits = int ((BigInteger.Abs r).GetBitLength ())

            if significantBits < reducedArgumentFloor then
                failwith
                    $"DeterministicMath: reducing %.17g{x} modulo pi/2 left a remainder with only %i{significantBits} significant bits, below the %i{reducedArgumentFloor} this implementation's accuracy argument assumes; the reduced argument needs to be carried at more than %i{fractionBits} places"

        let rSquared = mulFixed r r

        let value =
            if isSine then
                trigSeries r 1 rSquared
            else
                trigSeries scale 0 rSquared

        roundToDouble value -fractionBits

    /// The cosine of `x` in radians, with the semantics of IEEE 754 / C99 `cos` — which is
    /// what CoreCLR's `Math.Cos` inherits from the platform C library.
    ///
    /// Accuracy: the argument is reduced modulo pi/2 against a `piBits`-place pi, leaving a
    /// reduced argument whose absolute error is under 2^-475, and the series that follows
    /// is evaluated to `fractionBits` places. For the two quadrants answering ±cos(r) the
    /// result is at least cos(pi/4) in magnitude, so its relative error is under 2^-255. For
    /// the two answering ±sin(r) the result is proportional to `r` itself, so the relative
    /// error is set by how much cancellation the reduction suffered — 256 bits less the
    /// number of leading zeros in `r`.
    ///
    /// The reduced argument therefore has to stay clear of zero, and it does: the doubles
    /// nearest an odd multiple of pi/2 leave |r| near 2^-61 (Kahan's worst case for binary64
    /// reduction, 6381956970095103 * 2^797, is the standard witness and `TestDeterministicMath`
    /// measures it), which leaves about 195 significant bits. `reducedArgumentFloor` asserts
    /// a bound two-and-a-half times weaker than that rather than trusting the claim: if it
    /// ever fires, the fix is to carry the reduced argument at more than `fractionBits`
    /// places, not to relax the bound.
    let cos (x : float) : float =
        // No case overrides a NaN operand, so unlike `pow` this needs no separate
        // signalling test: `quieted` is the identity on a NaN that is already quiet.
        if Double.IsNaN x then
            quieted x
        elif Double.IsInfinity x then
            // A domain error: an infinite argument names no point on the circle.
            quietNaN
        else

        let quadrant, r = reduceModuloQuarterTurn x

        // cos(k pi/2 + r) is cos(r), -sin(r), -cos(r), sin(r) as k runs through the
        // residues mod 4.
        let magnitude = evaluateReduced x (quadrant % 2 = 1) r

        // Quadrants 1 and 2 are the half-turn on which cosine is negative.
        if quadrant = 1 || quadrant = 2 then
            -magnitude
        else
            magnitude

    /// Below this magnitude `sin x` is answered by `x` itself rather than by the reduction
    /// and the series.
    ///
    /// Two facts meet here. Downwards: the reduction narrows its result to `fractionBits`
    /// fractional bits, so an argument below 2^-`fractionBits` reduces to nothing at all and
    /// one below 2^-(`fractionBits` - `reducedArgumentFloor`) keeps too few bits for the
    /// accuracy argument — this is exactly the magnitude at which `evaluateReduced` would
    /// start to complain, hence the definition in terms of those two constants rather than
    /// a literal. `cos` needs no such bound because its answer near zero is 1 regardless.
    ///
    /// Upwards: `x - sin x` is about x^3/6, which stays under half an ulp of `x` until |x|
    /// reaches roughly 2^-25.2, so returning the argument is the correctly rounded answer
    /// anywhere below that. The threshold sits a hundred octaves inside that region, and
    /// `TestDeterministicMath` checks the whole of the gap rather than the boundary alone.
    ///
    /// It also disposes of the zeros: clause 9.2.1 of IEEE 754-2019 asks for sin(+0) = +0 and
    /// sin(-0) = -0, and returning the argument gives both.
    let private smallArgumentThreshold : float =
        roundToDouble BigInteger.One (reducedArgumentFloor - fractionBits)

    /// The sine of `x` in radians, with the semantics of IEEE 754 / C99 `sin` — which is what
    /// CoreCLR's `Math.Sin` inherits from the platform C library.
    ///
    /// The accuracy argument is the one written out over `cos`, with the two branches
    /// exchanged: here it is the *even* residues that answer ±sin(r) and so depend on how
    /// much of `r` survived the reduction.
    let sin (x : float) : float =
        if Double.IsNaN x then
            quieted x
        elif Double.IsInfinity x then
            // A domain error, as for `cos`.
            quietNaN
        elif abs x < smallArgumentThreshold then
            x
        else

        let quadrant, r = reduceModuloQuarterTurn x

        // sin(k pi/2 + r) is sin(r), cos(r), -sin(r), -cos(r) as k runs through the
        // residues mod 4 — the cosine table rotated by one, since the reduction is shared.
        let magnitude = evaluateReduced x (quadrant % 2 = 0) r

        // Quadrants 2 and 3 are the half-turn on which the sine of |x| is negative.
        let atMagnitude =
            if quadrant = 2 || quadrant = 3 then
                -magnitude
            else
                magnitude

        // The reduction took |x|, so the oddness of sine is applied here. `Double.IsNegative`
        // rather than `< 0.0`: the latter is false for -0.0, and clause 9.2.1 wants -0 back.
        // (A negative zero never reaches this line, being below the threshold above, but a
        // sign rule that is wrong on it is a trap for anyone who moves the threshold.)
        if Double.IsNegative x then -atMagnitude else atMagnitude
