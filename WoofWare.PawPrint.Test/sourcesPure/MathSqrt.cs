using System;

// Math.Sqrt is [Intrinsic] + InternalCall in CoreCLR: it has no IL body, and the JIT lowers it
// to the platform's hardware square-root instruction. PawPrint implements it in-tree instead
// (DeterministicMath.sqrt).
//
// This file runs under both PawPrint and the real runtime and the two exit codes are compared,
// so it may only assert things *both* must agree on -- and here that is very nearly everything,
// which makes this case stronger than its MathPow / MathCos / MathSin siblings. Those three
// approximate IEEE 754 clause 9.2 *recommended* operations, which no mainstream libm rounds
// correctly, so they may only assert identities and slack bounds. squareRoot is a clause 5.4.1
// *required* operation and must be correctly rounded, so its result is a single specified
// double for every argument, and the irrational cases below are pinned exactly rather than to
// within a tolerance.
//
// The one thing left open is the payload and sign of the NaN produced for a negative argument:
// x86's `sqrtsd` yields the negative quiet NaN and Arm's `fsqrt` the positive one. So the NaN
// cases below check double.IsNaN and never the bits. (A NaN *argument* is propagated with its
// sign and payload intact by both, but a guest cannot write a NaN payload in C# anyway; the
// bit-exact specification of that lives in TestDeterministicMath.fs.)
public static class MathSqrt
{
    // A spread covering the whole exponent range, including both ends where the argument's
    // exponent is furthest from the even one that halving wants.
    static readonly double[] Arguments =
    {
        0.0, double.Epsilon, 1e-320, 1e-300, 1e-30, 1e-8, 0.25, 0.5, 1.0, 2.0, 3.0, 4.0,
        6.25, 10.0, 123.456, 1000.0, 12345.6789, 1e5, 1e8, 1e15, 1e30, 1e100, 1e300,
        double.MaxValue,
    };

    static int ExactCases()
    {
        // IEEE 754-2019 clause 5.4.1: squareRoot(+/-0) is that same zero. The sign is
        // specified, so unlike the sign of a NaN it can be asserted against a host.
        if (Math.Sqrt(0.0) != 0.0) return 1;
        if (double.IsNegative(Math.Sqrt(0.0))) return 2;
        if (Math.Sqrt(-0.0) != 0.0) return 3;
        if (!double.IsNegative(Math.Sqrt(-0.0))) return 4;

        if (!double.IsPositiveInfinity(Math.Sqrt(double.PositiveInfinity))) return 5;

        // Roots that are themselves representable.
        if (Math.Sqrt(1.0) != 1.0) return 10;
        if (Math.Sqrt(4.0) != 2.0) return 11;
        if (Math.Sqrt(9.0) != 3.0) return 12;
        if (Math.Sqrt(2.25) != 1.5) return 13;
        if (Math.Sqrt(0.25) != 0.5) return 14;
        if (Math.Sqrt(6.25) != 2.5) return 15;
        if (Math.Sqrt(1e100) != 1e50) return 16;

        return 0;
    }

    static int CorrectlyRoundedCases()
    {
        // Correct rounding is *required* of squareRoot, so these irrational roots have one
        // right answer and every conforming implementation returns it bit-for-bit. Each
        // literal below is the shortest decimal that round-trips to that answer. This is the
        // assertion the sibling Math cases cannot make about their own functions.
        if (Math.Sqrt(2.0) != 1.4142135623730951) return 1;
        if (Math.Sqrt(3.0) != 1.7320508075688772) return 2;
        if (Math.Sqrt(5.0) != 2.23606797749979) return 3;
        if (Math.Sqrt(10.0) != 3.1622776601683795) return 4;
        if (Math.Sqrt(0.5) != 0.7071067811865476) return 5;

        // Both ends of the range, where the widening the implementation does before taking an
        // integer root has the most work to do: the smallest subnormal and the largest finite
        // double. A subnormal argument in particular has no implicit leading significand bit,
        // which is the case an implementation is most likely to get wrong.
        if (Math.Sqrt(double.Epsilon) != 2.2227587494850775E-162) return 10;
        if (Math.Sqrt(1e-320) != 9.99994433575849E-161) return 11;
        if (Math.Sqrt(double.MaxValue) != 1.3407807929942596E+154) return 12;

        return 0;
    }

    static int NaNRules()
    {
        // Every negative argument other than -0 is outside the domain.
        if (!double.IsNaN(Math.Sqrt(-1.0))) return 1;
        if (!double.IsNaN(Math.Sqrt(-0.5))) return 2;
        if (!double.IsNaN(Math.Sqrt(-double.Epsilon))) return 3;
        if (!double.IsNaN(Math.Sqrt(double.MinValue))) return 4;
        if (!double.IsNaN(Math.Sqrt(double.NegativeInfinity))) return 5;

        // NaN propagates.
        if (!double.IsNaN(Math.Sqrt(double.NaN))) return 6;

        return 0;
    }

    static int PerfectSquares()
    {
        // An integer below 2^26 has a square that is still exactly a double, so the root is
        // recoverable exactly and no rounding may intervene at all. This is the branch on
        // which the implementation's remainder vanishes, which random arguments essentially
        // never reach.
        for (int i = 0; i <= 3000; i++)
        {
            double n = i;

            if (Math.Sqrt(n * n) != n) return 100 + (i % 100);
        }

        // ...and the same at magnitudes a plain loop cannot reach, where the exponent's
        // parity is what the implementation has to get right. Scaling by a power of four
        // keeps the argument a perfect square exactly.
        double square = 4.0;
        double root = 2.0;

        for (int i = 0; i < 100; i++)
        {
            if (Math.Sqrt(square) != root) return 300 + i;

            square *= 16.0;
            root *= 4.0;
        }

        return 0;
    }

    static int RoundTrips()
    {
        for (int i = 0; i < Arguments.Length; i++)
        {
            double x = Arguments[i];
            double r = Math.Sqrt(x);

            if (double.IsNaN(r)) return 100 + i;
            if (r < 0.0) return 200 + i;

            // Below the smallest normal, squaring the root lands back in the subnormals and
            // discards most of its significand, so the round trip says nothing. (Those
            // arguments are covered exactly by CorrectlyRoundedCases instead.)
            if (x < 2.2250738585072014E-308) continue;

            // Squaring the root recovers the argument to within the two roundings involved,
            // which is a relative error far under 2^-50. Expressed as a ratio rather than as
            // an absolute difference so that the tolerance cannot itself underflow. This is a
            // real check, not a formality: an implementation that mishandled the parity of the
            // argument's exponent would be out by a factor of two rather than by an ulp.
            double ratio = (r * r) / x;

            if (ratio < 0.99999999999999 || ratio > 1.00000000000001) return 300 + i;
        }

        return 0;
    }

    static int IsMonotone()
    {
        // The correct rounding of a monotone function is monotone, so this holds exactly.
        // Arguments is written in increasing order, which this also checks.
        double previous = -1.0;

        for (int i = 0; i < Arguments.Length; i++)
        {
            double r = Math.Sqrt(Arguments[i]);

            if (r < previous) return 100 + i;

            previous = r;
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = ExactCases();
        if (result != 0) return 1000 + result;

        result = CorrectlyRoundedCases();
        if (result != 0) return 2000 + result;

        result = NaNRules();
        if (result != 0) return 3000 + result;

        result = PerfectSquares();
        if (result != 0) return 4000 + result;

        result = RoundTrips();
        if (result != 0) return 5000 + result;

        result = IsMonotone();
        if (result != 0) return 6000 + result;

        return 0;
    }
}
