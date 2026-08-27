using System;

// Math.Truncate(double) is [Intrinsic] in CoreCLR, and the JIT lowers it to a round-toward-zero
// machine instruction (roundsd with mode 3 on x86, frintz on Arm). Unlike Math.Ceiling it is not
// also InternalCall: it has an IL body. That body is not a definition either, though -- it is
// `ModF(d, &d); return d;`, and ModF *is* InternalCall with no IL, bottoming out in the platform
// C library's modf. PawPrint implements the operation in-tree instead
// (DeterministicMath.truncate).
//
// This file runs under both PawPrint and the real runtime and the two exit codes are compared,
// so it may only assert things *both* must agree on. Here that is everything except a NaN
// payload: roundToIntegralTowardZero is an *exact* operation, so unlike its MathPow / MathCos /
// MathSin siblings -- which approximate IEEE 754 clause 9.2 operations that no mainstream libm
// rounds correctly -- and unlike even MathSqrt, which is correctly rounded but is still a
// rounding, every result below is the single value the standard names. Nothing here is asserted
// to within a tolerance.
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
public static class MathTruncate
{
    // 2^52: the smallest magnitude whose ulp is 1, so every double at or above it is already an
    // integer and is its own truncation.
    const double TwoToThe52 = 4503599627370496.0;

    static int ZeroAndInfinity()
    {
        // A zero is integral already and keeps its sign.
        if (Math.Truncate(0.0) != 0.0) return 1;
        if (double.IsNegative(Math.Truncate(0.0))) return 2;
        if (Math.Truncate(-0.0) != 0.0) return 3;
        if (!double.IsNegative(Math.Truncate(-0.0))) return 4;

        if (!double.IsPositiveInfinity(Math.Truncate(double.PositiveInfinity))) return 5;
        if (!double.IsNegativeInfinity(Math.Truncate(double.NegativeInfinity))) return 6;

        // NaN propagates. Which NaN is deliberately not asserted; see the file comment.
        if (!double.IsNaN(Math.Truncate(double.NaN))) return 7;

        return 0;
    }

    static int FractionsKeepTheirSign()
    {
        // The sign rule an implementation is most likely to get wrong. Truncating towards zero
        // collapses everything in (-1, 1) to a zero, and IEEE 754 takes that zero's sign from
        // the *operand* -- so the negative rows give -0 and the positive rows +0. The natural
        // integer arithmetic produces a zero with no sign attached, and `== 0.0` cannot see the
        // difference either, hence IsNegative.
        if (Math.Truncate(-0.5) != 0.0) return 11;
        if (!double.IsNegative(Math.Truncate(-0.5))) return 12;

        if (!double.IsNegative(Math.Truncate(-0.25))) return 13;
        if (!double.IsNegative(Math.Truncate(-0.75))) return 14;
        if (!double.IsNegative(Math.Truncate(-double.Epsilon))) return 15;
        if (!double.IsNegative(Math.Truncate(-1e-320))) return 16;
        if (!double.IsNegative(Math.Truncate(-0.9999999999999999))) return 17;

        if (Math.Truncate(0.5) != 0.0) return 18;
        if (double.IsNegative(Math.Truncate(0.5))) return 19;

        if (double.IsNegative(Math.Truncate(0.25))) return 20;
        if (double.IsNegative(Math.Truncate(0.75))) return 21;
        if (double.IsNegative(Math.Truncate(double.Epsilon))) return 22;
        if (double.IsNegative(Math.Truncate(1e-320))) return 23;
        if (double.IsNegative(Math.Truncate(0.9999999999999999))) return 24;

        return 0;
    }

    static int FractionalCases()
    {
        // Truncating towards zero is the one directed rounding whose two columns are mirror
        // images: an implementation built on a floor gets the negative column wrong, and one
        // built on a ceiling gets the positive column wrong.
        if (Math.Truncate(1.5) != 1.0) return 31;
        if (Math.Truncate(-1.5) != -1.0) return 32;
        if (Math.Truncate(2.5) != 2.0) return 33;
        if (Math.Truncate(-2.5) != -2.0) return 34;
        if (Math.Truncate(1.0000000000000002) != 1.0) return 35;
        if (Math.Truncate(-1.0000000000000002) != -1.0) return 36;
        if (Math.Truncate(123.456) != 123.0) return 37;
        if (Math.Truncate(-123.456) != -123.0) return 38;

        // 1e15 has an ulp of 1/8, so the halves below are exactly representable.
        if (Math.Truncate(1e15 + 0.5) != 1e15) return 39;
        if (Math.Truncate(-(1e15 + 0.5)) != -1e15) return 40;

        // Integers are their own truncation.
        if (Math.Truncate(1.0) != 1.0) return 41;
        if (Math.Truncate(-1.0) != -1.0) return 42;
        if (Math.Truncate(1e15) != 1e15) return 43;
        if (Math.Truncate(-1e15) != -1e15) return 44;

        return 0;
    }

    static int TheIntegralBoundary()
    {
        // The last binade in which a double can be fractional at all is [2^51, 2^52), whose ulp
        // is 1/2. Half a unit below 2^52 is therefore representable, and is the largest
        // non-integral double there is.
        double justBelow = TwoToThe52 - 0.5;

        if (Math.Truncate(justBelow) != TwoToThe52 - 1.0) return 61;
        if (Math.Truncate(-justBelow) != -(TwoToThe52 - 1.0)) return 62;

        // At and above 2^52 nothing is fractional, so every argument is its own truncation --
        // right out to the ends of the range, where an implementation that reconstructed the
        // result from its exponent rather than returning the argument could overflow.
        if (Math.Truncate(TwoToThe52) != TwoToThe52) return 63;
        if (Math.Truncate(-TwoToThe52) != -TwoToThe52) return 64;
        if (Math.Truncate(1e300) != 1e300) return 65;
        if (Math.Truncate(-1e300) != -1e300) return 66;
        if (Math.Truncate(double.MaxValue) != double.MaxValue) return 67;
        if (Math.Truncate(double.MinValue) != double.MinValue) return 68;

        return 0;
    }

    static int DenseSweep()
    {
        // Quarter-integers either side of zero, where the sign rules bite. The expected value is
        // computed from the offset rather than looked up, so this covers 8004 arguments without
        // a table -- and every one of them is exact, since the whole part stays under 2^11.
        //
        // The expected answer is `whole` for a non-negative argument and `whole + 1` for a
        // negative one, which is what makes this a check on the *direction* of the rounding
        // rather than only on its magnitude.
        for (int i = 0; i <= 2000; i++)
        {
            double whole = i - 1000;
            double towardsZero = whole < 0.0 ? whole + 1.0 : whole;

            if (Math.Truncate(whole) != whole) return 70 + (i % 4);
            if (Math.Truncate(whole + 0.25) != towardsZero) return 74 + (i % 4);
            if (Math.Truncate(whole + 0.5) != towardsZero) return 78 + (i % 4);
            if (Math.Truncate(whole + 0.75) != towardsZero) return 82 + (i % 4);
        }

        return 0;
    }

    static int TheDefiningProperty()
    {
        // trunc(x) is an integer, it is no further from zero than x, and moving it one further
        // from zero would overshoot -- so no integer nearer to x would have done. Stated over a
        // spread that reaches both ends of the exponent range.
        double[] arguments =
        {
            0.0, -0.0, double.Epsilon, -double.Epsilon, 1e-320, -1e-320, 1e-30, -1e-30,
            0.25, -0.25, 0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 2.0, -2.0, 3.7, -3.7,
            123.456, -123.456, 1e5 + 0.5, -(1e5 + 0.5), 1e15 + 0.25, -(1e15 + 0.25),
            TwoToThe52 - 0.5, -(TwoToThe52 - 0.5), TwoToThe52, -TwoToThe52,
            1e100, -1e100, 1e300, -1e300, double.MaxValue, double.MinValue,
        };

        for (int i = 0; i < arguments.Length; i++)
        {
            double x = arguments[i];
            double r = Math.Truncate(x);

            if (Math.Abs(r) > Math.Abs(x)) return 90 + (i % 5);

            // The result is integral, so applying the operation again changes nothing.
            if (Math.Truncate(r) != r) return 95 + (i % 5);

            // Below 2^52 the successor of an integer is exact, so `|r| + 1 > |x|` is a real
            // assertion; together with `|r| <= |x|` above it says the move was by less than a
            // whole unit. At or above 2^52, `r == x` already and there is nothing left to say --
            // and `Math.Abs(r) + 1.0` would round straight back, making the check vacuous rather
            // than true.
            if (Math.Abs(r) < TwoToThe52 && !(Math.Abs(r) + 1.0 > Math.Abs(x))) return 100 + (i % 5);
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
            double r = Math.Truncate(increasing[i]);

            if (r < previous) return 115 + (i % 5);

            previous = r;
        }

        return 0;
    }

    static int AgreesWithCeiling()
    {
        // Truncation is the ceiling below zero and the floor above it. PawPrint already
        // implements Math.Ceiling, so the negative column can be cross-checked against a second
        // in-tree implementation rather than only against the tables above; and above zero the
        // complementary relationship holds, `ceil(x) - trunc(x)` being 1 exactly when x is
        // fractional.
        double[] negatives =
        {
            -0.75, -0.5, -0.25, -1.5, -2.5, -123.456, -3.7, -1e-30, -(1e15 + 0.5),
            -1.0, -2.0, -1e15, -1e300,
        };

        for (int i = 0; i < negatives.Length; i++)
        {
            double x = negatives[i];

            // Both are zero for x in (-1, 0), and `!=` cannot see the sign; they agree there
            // too, since both are -0, but only IsNegative says so.
            if (Math.Truncate(x) != Math.Ceiling(x)) return 121 + (i % 3);
            if (double.IsNegative(Math.Truncate(x)) != double.IsNegative(Math.Ceiling(x))) return 124 + (i % 3);
        }

        double[] positives = { 0.75, 0.5, 0.25, 1.5, 2.5, 123.456, 3.7, 1e15 + 0.5 };

        for (int i = 0; i < positives.Length; i++)
        {
            double x = positives[i];

            // Every one of these is fractional, so the two differ by exactly one.
            if (Math.Ceiling(x) - Math.Truncate(x) != 1.0) return 118 + (i % 3);
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = ZeroAndInfinity();
        if (result != 0) return result;

        result = FractionsKeepTheirSign();
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

        result = AgreesWithCeiling();
        if (result != 0) return result;

        return 0;
    }
}
