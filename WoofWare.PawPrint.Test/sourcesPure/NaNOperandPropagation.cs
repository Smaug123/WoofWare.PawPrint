using System;

// Facts about NaN operands that hold on every CoreCLR platform, so they can be checked
// against the real runtime: a lone NaN operand comes through an arithmetic operation with
// its sign and payload, and a signalling one comes through with its quiet bit set. Which
// operand wins when both are NaN, and the payload of a NaN made from non-NaN operands,
// differ between x64 and arm64, so those are asserted only under PawPrint (see the impure
// case `NaNGeneratedPayloadCanonical.cs`).
public class Program
{
    private static double D(long bits) => BitConverter.Int64BitsToDouble(bits);
    private static long B(double d) => BitConverter.DoubleToInt64Bits(d);
    private static float F(int bits) => BitConverter.Int32BitsToSingle(bits);
    private static int B(float f) => BitConverter.SingleToInt32Bits(f);

    public static int Main(string[] args)
    {
        double one = 1.0 + args.Length;
        float oneF = 1.0f + args.Length;

        // A quiet NaN with a payload, positive and negative, through every operation.
        long qPayload = unchecked((long)0x7FF8000000001234UL);
        long qNegPayload = unchecked((long)0xFFF8000000005678UL);
        if (B(D(qPayload) + one) != qPayload) return 1;
        if (B(one - D(qPayload)) != qPayload) return 2;
        if (B(D(qNegPayload) * one) != qNegPayload) return 3;
        if (B(one / D(qNegPayload)) != qNegPayload) return 4;
        if (B(D(qPayload) % one) != qPayload) return 5;
        if (B(one % D(qNegPayload)) != qNegPayload) return 6;

        // A signalling NaN is delivered quiet, with everything else preserved.
        long sPayload = unchecked((long)0x7FF0000000001234UL);
        long sNegPayload = unchecked((long)0xFFF0000000005678UL);
        if (B(D(sPayload) + one) != qPayload) return 7;
        if (B(one * D(sNegPayload)) != qNegPayload) return 8;

        // float32 likewise.
        int qPayloadF = 0x7FC01234;
        int qNegPayloadF = unchecked((int)0xFFC05678);
        int sPayloadF = 0x7F801234;
        if (B(F(qPayloadF) + oneF) != qPayloadF) return 9;
        if (B(oneF - F(qNegPayloadF)) != qNegPayloadF) return 10;
        if (B(F(sPayloadF) * oneF) != qPayloadF) return 11;
        if (B(F(qPayloadF) / oneF) != qPayloadF) return 12;

        // A NaN generated from non-NaN operands is a NaN, whatever its payload.
        double zero = args.Length;
        double inf = one / zero;
        if (!double.IsNaN(zero / zero)) return 13;
        if (!double.IsNaN(inf - inf)) return 14;
        if (!double.IsNaN(zero * inf)) return 15;
        if (!double.IsNaN(one % zero)) return 16;
        if (!double.IsNaN(inf % one)) return 17;

        // Negation flips the sign of a NaN and keeps its payload.
        if (B(-D(qPayload)) != qNegPayload + (0x1234 - 0x5678)) return 18;

        return 0;
    }
}
