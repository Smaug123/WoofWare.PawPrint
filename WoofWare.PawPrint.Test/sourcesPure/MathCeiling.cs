using System;

// Math.Ceiling(double) is [Intrinsic] + InternalCall in CoreCLR: it has no IL body, and the JIT
// lowers it to a round-toward-positive machine instruction (roundsd on x86, frintp on Arm) or to
// the platform C library's ceil. PawPrint implements it in-tree instead
// (DeterministicMath.ceiling).
//
// This file runs under both PawPrint and the real runtime and the two exit codes are compared,
// so it may only assert things *both* must agree on. Here that is everything except a NaN
// payload: roundToIntegralTowardPositive is an *exact* operation, so unlike its MathPow /
// MathCos / MathSin siblings -- which approximate IEEE 754 clause 9.2 operations that no
// mainstream libm rounds correctly -- and unlike even MathSqrt, which is correctly rounded but
// is still a rounding, every result below is the single value the standard names. Nothing here
// is asserted to within a tolerance.
//
// The one thing left out is the bits of a NaN, as in MathSqrt.cs: this operation generates no
// NaN of its own, but a NaN *argument* comes back carrying its payload, and a guest cannot write
// a NaN payload in C# anyway. The bit-exact specification of that lives in
// TestDeterministicMath.fs.
//
// Every failure code below is distinct and stays under 128. A process exit code is eight bits,
// so a code of 256 would reach the harness as 0 and a wrong answer would read as success; and
// codes from 128 up are indistinguishable from the 128+signo of a guest that died on a signal.
// That is why the loops report a bucket of their index rather than the index itself.
public static class MathCeiling
{
    // 2^52: the smallest magnitude whose ulp is 1, so every double at or above it is already an
    // integer and is its own ceiling.
    const double TwoToThe52 = 4503599627370496.0;

    static int ZeroAndInfinity()
    {
        // A zero is integral already and keeps its sign.
        if (Math.Ceiling(0.0) != 0.0) return 1;
        if (double.IsNegative(Math.Ceiling(0.0))) return 2;
        if (Math.Ceiling(-0.0) != 0.0) return 3;
        if (!double.IsNegative(Math.Ceiling(-0.0))) return 4;

        if (!double.IsPositiveInfinity(Math.Ceiling(double.PositiveInfinity))) return 5;
        if (!double.IsNegativeInfinity(Math.Ceiling(double.NegativeInfinity))) return 6;

        // NaN propagates. Which NaN is deliberately not asserted; see the file comment.
        if (!double.IsNaN(Math.Ceiling(double.NaN))) return 7;

        return 0;
    }

    static int NegativeFractionsGiveNegativeZero()
    {
        // The sign rule an implementation is most likely to get wrong: an argument strictly
        // between -1 and 0 rounds *up* to zero, and IEEE 754 makes that zero negative. The
        // natural integer arithmetic produces a zero with no sign attached, so this is a real
        // check rather than a formality -- and `== 0.0` cannot see it, hence IsNegative.
        if (Math.Ceiling(-0.5) != 0.0) return 11;
        if (!double.IsNegative(Math.Ceiling(-0.5))) return 12;

        if (!double.IsNegative(Math.Ceiling(-0.25))) return 13;
        if (!double.IsNegative(Math.Ceiling(-0.75))) return 14;
        if (!double.IsNegative(Math.Ceiling(-double.Epsilon))) return 15;
        if (!double.IsNegative(Math.Ceiling(-1e-320))) return 16;
        if (!double.IsNegative(Math.Ceiling(-0.9999999999999999))) return 17;

        // The mirror image: a positive fraction rounds up to a positive one, not to zero.
        if (Math.Ceiling(0.5) != 1.0) return 18;
        if (Math.Ceiling(double.Epsilon) != 1.0) return 19;
        if (Math.Ceiling(1e-320) != 1.0) return 20;
        if (Math.Ceiling(0.9999999999999999) != 1.0) return 21;

        return 0;
    }

    static int FractionalCases()
    {
        // Rounding *up* means the negative rows truncate towards zero and the positive ones do
        // not; an implementation built on a floor, or on a truncation, gets exactly one of the
        // two columns wrong.
        if (Math.Ceiling(1.5) != 2.0) return 31;
        if (Math.Ceiling(-1.5) != -1.0) return 32;
        if (Math.Ceiling(2.5) != 3.0) return 33;
        if (Math.Ceiling(-2.5) != -2.0) return 34;
        if (Math.Ceiling(1.0000000000000002) != 2.0) return 35;
        if (Math.Ceiling(-1.0000000000000002) != -1.0) return 36;
        if (Math.Ceiling(123.456) != 124.0) return 37;
        if (Math.Ceiling(-123.456) != -123.0) return 38;

        // 1e15 has an ulp of 1/8, so the halves below are exactly representable.
        if (Math.Ceiling(1e15 + 0.5) != 1e15 + 1.0) return 39;
        if (Math.Ceiling(-(1e15 + 0.5)) != -1e15) return 40;

        // Integers are their own ceiling.
        if (Math.Ceiling(1.0) != 1.0) return 41;
        if (Math.Ceiling(-1.0) != -1.0) return 42;
        if (Math.Ceiling(1e15) != 1e15) return 43;
        if (Math.Ceiling(-1e15) != -1e15) return 44;

        return 0;
    }

    static int TheIntegralBoundary()
    {
        // The last binade in which a double can be fractional at all is [2^51, 2^52), whose ulp
        // is 1/2. Half a unit below 2^52 is therefore representable, and is the largest
        // non-integral double there is.
        double justBelow = TwoToThe52 - 0.5;

        if (Math.Ceiling(justBelow) != TwoToThe52) return 61;
        if (Math.Ceiling(-justBelow) != -(TwoToThe52 - 1.0)) return 62;

        // At and above 2^52 nothing is fractional, so every argument is its own ceiling --
        // right out to the ends of the range, where an implementation that reconstructed the
        // result from its exponent rather than returning the argument could overflow.
        if (Math.Ceiling(TwoToThe52) != TwoToThe52) return 63;
        if (Math.Ceiling(-TwoToThe52) != -TwoToThe52) return 64;
        if (Math.Ceiling(1e300) != 1e300) return 65;
        if (Math.Ceiling(-1e300) != -1e300) return 66;
        if (Math.Ceiling(double.MaxValue) != double.MaxValue) return 67;
        if (Math.Ceiling(double.MinValue) != double.MinValue) return 68;

        return 0;
    }

    static int DenseSweep()
    {
        // Quarter-integers either side of zero, where the sign rules bite. The expected value is
        // computed from the offset rather than looked up, so this covers 8004 arguments without
        // a table -- and every one of them is exact, since the whole part stays under 2^11.
        for (int i = 0; i <= 2000; i++)
        {
            double whole = i - 1000;

            if (Math.Ceiling(whole) != whole) return 70 + (i % 4);
            if (Math.Ceiling(whole + 0.25) != whole + 1.0) return 74 + (i % 4);
            if (Math.Ceiling(whole + 0.5) != whole + 1.0) return 78 + (i % 4);
            if (Math.Ceiling(whole + 0.75) != whole + 1.0) return 82 + (i % 4);
        }

        return 0;
    }

    static int TheDefiningProperty()
    {
        // ceil(x) is an integer, it is at or above x, and one less than it is strictly below x
        // -- so no smaller integer would have done. Stated over a spread that reaches both ends
        // of the exponent range.
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
            double r = Math.Ceiling(x);

            if (r < x) return 90 + (i % 5);

            // The result is integral, so applying the operation again changes nothing.
            if (Math.Ceiling(r) != r) return 95 + (i % 5);

            // Below 2^52 the predecessor of an integer is exact, so `r - 1 < x` is a real
            // assertion; together with `r >= x` above it says the move was by less than a whole
            // unit. At or above 2^52, `r == x` already and there is nothing left to say -- and
            // `r - 1.0` would round straight back to `r`, making the check vacuous rather than
            // true. (`r - x < 1.0` is *not* an alternative phrasing: the subtraction rounds. It
            // is exactly 1.0 for x = double.Epsilon, where the true difference is under it.)
            if (Math.Abs(r) < TwoToThe52 && !(r - 1.0 < x)) return 100 + (i % 5);
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
            double r = Math.Ceiling(increasing[i]);

            if (r < previous) return 115 + (i % 5);

            previous = r;
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = ZeroAndInfinity();
        if (result != 0) return result;

        result = NegativeFractionsGiveNegativeZero();
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

        return 0;
    }
}
