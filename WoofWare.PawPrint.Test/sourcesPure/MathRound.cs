using System;

// Math.Round(double) is [Intrinsic] in CoreCLR. Unlike its Math.Ceiling / Math.Sqrt siblings it
// *does* have an IL body, but that body is a managed emulation of the instruction the JIT
// actually emits (roundsd with mode 0 on x86, frintn on Arm): it leans on the add-and-subtract
// trick `(a + 2^52) - 2^52`, which is only ties-to-even because the ambient rounding mode is.
// PawPrint implements the operation itself instead (DeterministicMath.round), so that the
// semantics are named rather than inherited from the rounding mode of whatever performed the
// addition.
//
// This file runs under both PawPrint and the real runtime and the two exit codes are compared,
// so it may only assert things *both* must agree on. Here that is everything except a NaN
// payload: roundToIntegralTiesToEven is an *exact* operation (IEEE 754 clause 5.9), so as with
// MathCeiling -- and unlike MathPow / MathCos / MathSin, which approximate clause 9.2 operations
// that no mainstream libm rounds correctly -- every result below is the single value the
// standard names. Nothing here is asserted to within a tolerance.
//
// The one thing left out is the bits of a NaN, as in MathCeiling.cs: this operation generates no
// NaN of its own, but a NaN *argument* comes back carrying its payload, and a guest cannot write
// a NaN payload in C# anyway. The bit-exact specification of that lives in
// TestDeterministicMath.fs.
//
// Every failure code below is distinct and stays under 128. A process exit code is eight bits,
// so a code of 256 would reach the harness as 0 and a wrong answer would read as success; and
// codes from 128 up are indistinguishable from the 128+signo of a guest that died on a signal.
// That is why the loops report a bucket of their index rather than the index itself.
public static class MathRound
{
    // 2^52: the smallest magnitude whose ulp is 1, so every double at or above it is already an
    // integer and is its own rounding.
    const double TwoToThe52 = 4503599627370496.0;

    static int ZeroAndInfinity()
    {
        // A zero is integral already and keeps its sign.
        if (Math.Round(0.0) != 0.0) return 1;
        if (double.IsNegative(Math.Round(0.0))) return 2;
        if (Math.Round(-0.0) != 0.0) return 3;
        if (!double.IsNegative(Math.Round(-0.0))) return 4;

        if (!double.IsPositiveInfinity(Math.Round(double.PositiveInfinity))) return 5;
        if (!double.IsNegativeInfinity(Math.Round(double.NegativeInfinity))) return 6;

        // NaN propagates. Which NaN is deliberately not asserted; see the file comment.
        if (!double.IsNaN(Math.Round(double.NaN))) return 7;

        return 0;
    }

    static int TiesGoToEven()
    {
        // The whole point of this operation, and the thing that separates it from every
        // "round half away from zero" implementation a reader might expect: at an exact
        // midpoint the *even* neighbour wins, on both sides of zero.
        if (Math.Round(0.5) != 0.0) return 11;
        if (Math.Round(1.5) != 2.0) return 12;
        if (Math.Round(2.5) != 2.0) return 13;
        if (Math.Round(3.5) != 4.0) return 14;
        if (Math.Round(4.5) != 4.0) return 15;

        if (Math.Round(-0.5) != 0.0) return 16;
        if (Math.Round(-1.5) != -2.0) return 17;
        if (Math.Round(-2.5) != -2.0) return 18;
        if (Math.Round(-3.5) != -4.0) return 19;
        if (Math.Round(-4.5) != -4.0) return 20;

        // 1e15 has an ulp of 1/8, so these halves are exactly representable and are genuine
        // midpoints rather than values that merely print as one. 1e15 is even.
        if (Math.Round(1e15 + 0.5) != 1e15) return 21;
        if (Math.Round(1e15 + 1.5) != 1e15 + 2.0) return 22;
        if (Math.Round(-(1e15 + 0.5)) != -1e15) return 23;
        if (Math.Round(-(1e15 + 1.5)) != -(1e15 + 2.0)) return 24;

        return 0;
    }

    static int SmallMagnitudesGiveSignedZero()
    {
        // An argument of magnitude at most 1/2 rounds to zero, and IEEE 754 makes that zero
        // carry the *operand's* sign. The natural integer arithmetic produces a zero with no
        // sign attached, so this is a real check rather than a formality -- and `== 0.0` cannot
        // see it, hence IsNegative.
        if (Math.Round(0.5) != 0.0) return 31;
        if (double.IsNegative(Math.Round(0.5))) return 32;
        if (double.IsNegative(Math.Round(0.25))) return 33;
        if (double.IsNegative(Math.Round(double.Epsilon))) return 34;
        if (double.IsNegative(Math.Round(1e-320))) return 35;

        if (Math.Round(-0.5) != 0.0) return 36;
        if (!double.IsNegative(Math.Round(-0.5))) return 37;
        if (!double.IsNegative(Math.Round(-0.25))) return 38;
        if (!double.IsNegative(Math.Round(-double.Epsilon))) return 39;
        if (!double.IsNegative(Math.Round(-1e-320))) return 40;

        // Just past the midpoint the answer is a genuine one, with the operand's sign.
        if (Math.Round(0.5000000000000001) != 1.0) return 41;
        if (Math.Round(-0.5000000000000001) != -1.0) return 42;
        if (Math.Round(0.75) != 1.0) return 43;
        if (Math.Round(-0.75) != -1.0) return 44;
        if (Math.Round(0.9999999999999999) != 1.0) return 45;
        if (Math.Round(-0.9999999999999999) != -1.0) return 46;

        // 0.49999999999999994 is the double immediately below 1/2, and is the classic trap:
        // an implementation written as floor(x + 0.5) gets 1 here, because the addition itself
        // rounds up to exactly 1. The right answer is zero.
        if (Math.Round(0.49999999999999994) != 0.0) return 47;
        if (double.IsNegative(Math.Round(0.49999999999999994))) return 48;
        if (Math.Round(-0.49999999999999994) != 0.0) return 49;
        if (!double.IsNegative(Math.Round(-0.49999999999999994))) return 50;

        return 0;
    }

    static int FractionalCases()
    {
        // Away from the midpoints there is nothing to break a tie over, and the answer is
        // simply the nearer integer -- symmetrically in the sign, unlike Math.Ceiling.
        if (Math.Round(1.25) != 1.0) return 61;
        if (Math.Round(-1.25) != -1.0) return 62;
        if (Math.Round(1.75) != 2.0) return 63;
        if (Math.Round(-1.75) != -2.0) return 64;
        if (Math.Round(1.0000000000000002) != 1.0) return 65;
        if (Math.Round(-1.0000000000000002) != -1.0) return 66;
        if (Math.Round(123.456) != 123.0) return 67;
        if (Math.Round(-123.456) != -123.0) return 68;
        if (Math.Round(123.567) != 124.0) return 69;
        if (Math.Round(-123.567) != -124.0) return 70;

        // Integers are their own rounding.
        if (Math.Round(1.0) != 1.0) return 71;
        if (Math.Round(-1.0) != -1.0) return 72;
        if (Math.Round(1e15) != 1e15) return 73;
        if (Math.Round(-1e15) != -1e15) return 74;

        return 0;
    }

    static int TheIntegralBoundary()
    {
        // The last binade in which a double can be fractional at all is [2^51, 2^52), whose ulp
        // is 1/2. Half a unit below 2^52 is therefore representable, and is the largest
        // non-integral double there is -- and it is an exact midpoint, between the odd
        // 2^52 - 1 and the even 2^52, so ties-to-even sends it *away* from zero.
        double justBelow = TwoToThe52 - 0.5;

        if (Math.Round(justBelow) != TwoToThe52) return 81;
        if (Math.Round(-justBelow) != -TwoToThe52) return 82;

        // One ulp lower is 2^52 - 1, an odd integer, which is its own rounding.
        if (Math.Round(TwoToThe52 - 1.0) != TwoToThe52 - 1.0) return 83;
        if (Math.Round(-(TwoToThe52 - 1.0)) != -(TwoToThe52 - 1.0)) return 84;

        // And one ulp lower again is a midpoint whose even neighbour is 2^52 - 2, i.e. the one
        // *towards* zero. Together with `justBelow` above this pins the tie-break rather than
        // any fixed direction.
        if (Math.Round(TwoToThe52 - 1.5) != TwoToThe52 - 2.0) return 85;
        if (Math.Round(-(TwoToThe52 - 1.5)) != -(TwoToThe52 - 2.0)) return 86;

        // At and above 2^52 nothing is fractional, so every argument is its own rounding --
        // right out to the ends of the range, where an implementation that reconstructed the
        // result from its exponent rather than returning the argument could overflow.
        if (Math.Round(TwoToThe52) != TwoToThe52) return 87;
        if (Math.Round(-TwoToThe52) != -TwoToThe52) return 88;
        if (Math.Round(1e300) != 1e300) return 89;
        if (Math.Round(-1e300) != -1e300) return 90;
        if (Math.Round(double.MaxValue) != double.MaxValue) return 91;
        if (Math.Round(double.MinValue) != double.MinValue) return 92;

        return 0;
    }

    static int DenseSweep()
    {
        // Quarter-integers either side of zero. The expected value is computed from the offset
        // rather than looked up, so this covers 8004 arguments without a table -- and every one
        // of them is exact, since the whole part stays under 2^11. The `whole + 0.5` row is the
        // only one whose answer depends on the parity of `whole`, which is precisely the
        // property under test.
        for (int i = 0; i <= 2000; i++)
        {
            int n = i - 1000;
            double whole = n;
            double towardsEven = (n % 2 == 0) ? whole : whole + 1.0;

            if (Math.Round(whole) != whole) return 100 + (i % 4);
            if (Math.Round(whole + 0.25) != whole) return 104 + (i % 4);
            if (Math.Round(whole + 0.5) != towardsEven) return 108 + (i % 4);
            if (Math.Round(whole + 0.75) != whole + 1.0) return 112 + (i % 4);
        }

        return 0;
    }

    static int TheDefiningProperty()
    {
        // round(x) is an integer within half a unit of x, and at exactly half a unit it is the
        // even one. Stated over a spread that reaches both ends of the exponent range.
        double[] arguments =
        {
            0.0, -0.0, double.Epsilon, -double.Epsilon, 1e-320, -1e-320, 1e-30, -1e-30,
            0.25, -0.25, 0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 2.0, -2.0, 2.5, -2.5, 3.7, -3.7,
            123.456, -123.456, 1e5 + 0.5, -(1e5 + 0.5), 1e15 + 0.25, -(1e15 + 0.25),
            TwoToThe52 - 0.5, -(TwoToThe52 - 0.5), TwoToThe52, -TwoToThe52,
            1e100, -1e100, 1e300, -1e300, double.MaxValue, double.MinValue,
        };

        for (int i = 0; i < arguments.Length; i++)
        {
            double x = arguments[i];
            double r = Math.Round(x);

            // The result is integral, so applying the operation again changes nothing.
            if (Math.Round(r) != r) return 116 + (i % 5);

            // Below 2^52 the difference is exact -- both operands are integer multiples of the
            // smaller one's ulp -- so this really does say "within half a unit". At or above
            // 2^52, `r == x` already and there is nothing left to say.
            if (Math.Abs(x) < TwoToThe52)
            {
                double difference = r - x;

                if (!(difference <= 0.5) || !(difference >= -0.5)) return 121 + (i % 5);

                // At exactly half a unit, the answer must be the even integer. `r` is integral
                // and at most 2^52 in magnitude here, so the conversion to long is exact and
                // its parity is the parity of the answer.
                if (Math.Abs(difference) == 0.5 && ((long)r % 2L) != 0L)
                {
                    return 126 + (i % 5);
                }
            }
            else if (r != x)
            {
                return 131 + (i % 5);
            }
        }

        return 0;
    }

    static int IsMonotone()
    {
        // A non-decreasing function of a non-decreasing argument, exactly rather than up to an
        // error term. The array is written in increasing order, which this also checks.
        double[] increasing =
        {
            double.NegativeInfinity, double.MinValue, -1e300, -1e15, -123.456, -2.5, -1.5, -1.0,
            -0.75, -0.5, -double.Epsilon, -0.0, 0.0, double.Epsilon, 0.5, 0.75, 1.0, 1.5, 2.5,
            123.456, 1e15, 1e300, double.MaxValue, double.PositiveInfinity,
        };

        double previous = double.NegativeInfinity;

        for (int i = 0; i < increasing.Length; i++)
        {
            double r = Math.Round(increasing[i]);

            if (r < previous) return 136 + (i % 5);

            previous = r;
        }

        return 0;
    }

    static int IsOdd()
    {
        // round(-x) = -round(x), including at the zeros -- the symmetry that the ties-to-even
        // rule preserves and that a round-half-away-from-zero or round-half-up rule would too,
        // but which an implementation built on a floor plus a correction typically breaks.
        double[] arguments =
        {
            0.0, double.Epsilon, 1e-320, 0.25, 0.5, 0.75, 1.0, 1.5, 2.5, 3.5, 123.456,
            1e15 + 0.5, TwoToThe52 - 0.5, TwoToThe52, 1e300, double.MaxValue,
        };

        for (int i = 0; i < arguments.Length; i++)
        {
            double x = arguments[i];
            double positive = Math.Round(x);
            double negative = Math.Round(-x);

            if (negative != -positive) return 141 + (i % 5);

            // -0.0 is where `!=` stops seeing the difference, so check the sign too.
            if (double.IsNegative(negative) == double.IsNegative(positive)) return 146 + (i % 5);
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = ZeroAndInfinity();
        if (result != 0) return result;

        result = TiesGoToEven();
        if (result != 0) return result;

        result = SmallMagnitudesGiveSignedZero();
        if (result != 0) return result;

        result = FractionalCases();
        if (result != 0) return result;

        result = TheIntegralBoundary();
        if (result != 0) return result;

        result = DenseSweep();
        if (result != 0) return result;

        result = TheDefiningProperty();
        if (result != 0) return result;

        result = IsMonotone();
        if (result != 0) return result;

        result = IsOdd();
        if (result != 0) return result;

        return 0;
    }
}
