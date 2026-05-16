using System;

public class DoubleAbsTests
{
    public static int TestFinite()
    {
        if (double.Abs(1.5) != 1.5) return 1;
        if (double.Abs(-1.5) != 1.5) return 2;
        if (double.Abs(0.0) != 0.0) return 3;
        if (double.Abs(double.MaxValue) != double.MaxValue) return 4;
        if (double.Abs(-double.MaxValue) != double.MaxValue) return 5;
        if (double.Abs(double.Epsilon) != double.Epsilon) return 6;
        if (double.Abs(-double.Epsilon) != double.Epsilon) return 7;
        return 0;
    }

    public static int TestNegativeZero()
    {
        // 0.0 == -0.0 per IEEE 754, so direct equality won't catch a sign-bit bug.
        // Compare the bit pattern: Abs(-0.0) must be +0.0 (all-zero bits).
        double r = double.Abs(-0.0);
        long bits = BitConverter.DoubleToInt64Bits(r);
        if (bits != 0L) return 1;
        return 0;
    }

    public static int TestInfinities()
    {
        if (double.Abs(double.PositiveInfinity) != double.PositiveInfinity) return 1;
        if (double.Abs(double.NegativeInfinity) != double.PositiveInfinity) return 2;
        return 0;
    }

    public static int TestNaN()
    {
        if (!double.IsNaN(double.Abs(double.NaN))) return 1;
        if (!double.IsNaN(double.Abs(-double.NaN))) return 2;
        return 0;
    }

    public static int TestAgreesWithMathAbs()
    {
        // double.Abs is documented to delegate to Math.Abs; exercise that the two
        // produce identical results on a handful of finite values.
        double[] samples = { 0.0, -0.0, 1.0, -1.0, 3.14159, -2.71828, 1e300, -1e-300 };
        for (int i = 0; i < samples.Length; i++)
        {
            double a = double.Abs(samples[i]);
            double b = Math.Abs(samples[i]);
            // Bit-exact comparison so we catch any divergence on -0.0 specifically.
            if (BitConverter.DoubleToInt64Bits(a) != BitConverter.DoubleToInt64Bits(b))
                return i + 1;
        }
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;

        result = DoubleAbsTests.TestFinite();
        if (result != 0) return 1000 + result;

        result = DoubleAbsTests.TestNegativeZero();
        if (result != 0) return 2000 + result;

        result = DoubleAbsTests.TestInfinities();
        if (result != 0) return 3000 + result;

        result = DoubleAbsTests.TestNaN();
        if (result != 0) return 4000 + result;

        result = DoubleAbsTests.TestAgreesWithMathAbs();
        if (result != 0) return 5000 + result;

        return 0;
    }
}
