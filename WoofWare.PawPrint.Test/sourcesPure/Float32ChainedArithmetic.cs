using System;

// float32 arithmetic must be evaluated in single precision at every step, the way
// CoreCLR's JIT evaluates TYP_FLOAT operations, rather than widened to double on the
// evaluation stack and only rounded back to float32 when stored. A single operation
// cannot tell the two apart (double rounding is innocuous for + - * / when the wide
// format has at least 2p+2 bits), so every check here chains two operations, or reads
// the raw bits of a value that never went through a float32 store.
public class Program
{
    private static int Fail(int code)
    {
        return code;
    }

    // A value the compiler cannot fold: `args.Length` is 0 at runtime.
    private static float Opaque(float f, int zero)
    {
        return f + zero;
    }

    public static int Main(string[] args)
    {
        int zero = args.Length;

        // `a + b + c` is `ldloc a; ldloc b; add; ldloc c; add; stloc`: the intermediate
        // 16777217 is not representable as float32, so the first add rounds to 16777216
        // and the second add rounds again. A double intermediate keeps 16777217 exactly
        // and the final store rounds 16777218 up.
        float a = Opaque(16777216f, zero);
        float b = Opaque(1f, zero);
        float c = Opaque(1f, zero);
        float chained = a + b + c;
        if (chained != 16777216f) return Fail(1);

        // `(double)(x * y)` widens the float32 product; a double product has more bits.
        float x = Opaque(1.1f, zero);
        float y = Opaque(1.1f, zero);
        float storedProduct = x * y;
        double widenedProduct = (double)(x * y);
        if (widenedProduct != (double)storedProduct) return Fail(2);
        if (BitConverter.DoubleToInt64Bits(widenedProduct) != 0x3FF35C2900000000L) return Fail(3);

        // Mixed widths: a float32 operand against a double operand is computed in double.
        double mixed = x * 3.0;
        if (mixed != (double)x * 3.0) return Fail(4);

        // Widening explicitly with conv.r8 must be exact.
        double widened = (double)x;
        if (BitConverter.DoubleToInt64Bits(widened) != BitConverter.DoubleToInt64Bits((double)1.1f)) return Fail(5);

        // A signalling NaN's payload survives a round trip that never leaves float32.
        int signallingNaN = 0x7F800001 + zero;
        float sNaN = BitConverter.Int32BitsToSingle(signallingNaN);
        if (BitConverter.SingleToInt32Bits(sNaN) != 0x7F800001) return Fail(6);

        // Negative zero survives too (`-0.0f + 0` would be `+0.0f`, so it is built from bits).
        float negativeZero = BitConverter.Int32BitsToSingle(unchecked((int)0x80000000) + zero) * -1f * -1f;
        if (BitConverter.SingleToInt32Bits(negativeZero) != unchecked((int)0x80000000)) return Fail(7);

        // Division and remainder chains round at every step as well.
        float p = Opaque(1f, zero);
        float q = Opaque(3f, zero);
        float divChain = p / q * q;
        float divStored = p / q;
        float divStoredChain = divStored * q;
        if (BitConverter.SingleToInt32Bits(divChain) != BitConverter.SingleToInt32Bits(divStoredChain)) return Fail(8);

        // Search: pseudo-random float32 triples, comparing `a * b + c` computed in one
        // expression against the same computation with the product stored between the
        // two operations. Both are single precision on CoreCLR, so the bits agree.
        uint state = 0x9E3779B9u + (uint)zero;
        int mismatches = 0;
        for (int i = 0; i < 2000; i++)
        {
            float ra = NextFloat(ref state);
            float rb = NextFloat(ref state);
            float rc = NextFloat(ref state);
            float oneExpression = ra * rb + rc;
            float product = ra * rb;
            float twoStatements = product + rc;
            if (BitConverter.SingleToInt32Bits(oneExpression) != BitConverter.SingleToInt32Bits(twoStatements))
            {
                mismatches++;
            }

            float oneExpressionSub = ra - rb - rc;
            float difference = ra - rb;
            float twoStatementsSub = difference - rc;
            if (BitConverter.SingleToInt32Bits(oneExpressionSub) != BitConverter.SingleToInt32Bits(twoStatementsSub))
            {
                mismatches++;
            }

            float oneExpressionDiv = ra / rb / rc;
            float quotient = ra / rb;
            float twoStatementsDiv = quotient / rc;
            if (BitConverter.SingleToInt32Bits(oneExpressionDiv) != BitConverter.SingleToInt32Bits(twoStatementsDiv))
            {
                mismatches++;
            }
        }

        if (mismatches != 0)
        {
            Console.WriteLine("mismatches: " + mismatches);
            return Fail(9);
        }

        return 0;
    }

    // A finite, normal float32 with a random sign, an exponent within [-20, 20] and random
    // mantissa bits, from an xorshift generator so the sequence is the same on every runtime.
    private static float NextFloat(ref uint state)
    {
        state ^= state << 13;
        state ^= state >> 17;
        state ^= state << 5;
        uint mantissa = state & 0x007FFFFFu;
        uint exponent = 127u + (state >> 23) % 41u - 20u;
        uint sign = (state >> 31) << 31;
        return BitConverter.UInt32BitsToSingle(sign | (exponent << 23) | mantissa);
    }
}
