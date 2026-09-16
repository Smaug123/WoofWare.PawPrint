using System;

// PawPrint's own contract for the NaN a float opcode makes from non-NaN operands, and for
// which operand wins when both are NaN. Real x64 delivers the negative quiet NaN and real
// arm64 the positive one, so a recorded run would replay differently across hosts if the
// host CPU chose; PawPrint fixes the positive quiet NaN (IEEE 754's recommendation) and
// propagates the first NaN operand. No real runtime is an oracle for this.
public class Program
{
    private static long B(double d) => BitConverter.DoubleToInt64Bits(d);
    private static int B(float f) => BitConverter.SingleToInt32Bits(f);
    private static double D(long bits) => BitConverter.Int64BitsToDouble(bits);

    public static int Main(string[] args)
    {
        double zero = args.Length;
        double one = 1.0 + args.Length;
        double inf = one / zero;
        long canonical = 0x7FF8000000000000L;
        int canonicalF = 0x7FC00000;

        if (B(zero / zero) != canonical) return 1;
        if (B(inf - inf) != canonical) return 2;
        if (B(zero * inf) != canonical) return 3;
        if (B(one % zero) != canonical) return 4;
        if (B(inf % one) != canonical) return 5;
        if (B(-inf + inf) != canonical) return 6;

        float zeroF = args.Length;
        float oneF = 1.0f + args.Length;
        float infF = oneF / zeroF;
        if (B(zeroF / zeroF) != canonicalF) return 7;
        if (B(infF - infF) != canonicalF) return 8;
        if (B(zeroF * infF) != canonicalF) return 9;
        if (B(oneF % zeroF) != canonicalF) return 10;

        // Two NaN operands: the first wins, quieted.
        long first = unchecked((long)0xFFF8000000000AAAUL);
        long second = 0x7FF8000000000BBBL;
        long firstSignalling = unchecked((long)0xFFF0000000000AAAUL);
        if (B(D(first) + D(second)) != first) return 11;
        if (B(D(second) * D(first)) != second) return 12;
        if (B(D(firstSignalling) - D(second)) != first) return 13;

        return 0;
    }
}
