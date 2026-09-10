using System;

// There is no `conv.r4.un`, so C# compiles `(float)someUlong` to `conv.r.un; conv.r4`.
// CoreCLR's importer recognises that pair and performs one rounding, straight from the
// 64-bit integer to float32. Rounding to double first and then to float32 differs
// whenever the first rounding lands exactly on a float32 tie that the integer was not on.
public class Program
{
    // The number of significant bits in `u`, without BitOperations.
    private static int SignificantBits(ulong u)
    {
        int n = 0;
        while (u != 0)
        {
            n++;
            u >>= 1;
        }
        return n;
    }

    // Correctly rounded ulong -> float32, independently of any fused conversion: drop the
    // bits below the 53 a double can hold, folding them into a sticky bit, so that the
    // double is exact and the single remaining rounding sees whether anything was below.
    private static float Reference(ulong u)
    {
        int n = SignificantBits(u);
        if (n <= 53)
        {
            double exact = u;
            return (float)exact;
        }

        int shift = n - 53;
        ulong kept = u >> shift;
        if ((u & ((1UL << shift) - 1)) != 0)
        {
            kept |= 1;
        }

        double scaled = (double)kept * (double)(1UL << shift);
        return (float)scaled;
    }

    private static ulong Next(ref ulong state)
    {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        return state;
    }

    public static int Main(string[] args)
    {
        ulong zero = (ulong)args.Length;

        // 2^63 + 2^39 + 1: the +1 is below a double's precision, so a double rounds to
        // exactly the float32 tie 2^63 + 2^39 and then rounds to even, losing the bit.
        ulong u = 0x8000008000000001UL + zero;
        float fused = (float)u;
        if (BitConverter.SingleToInt32Bits(fused) != 0x5F000001) return 1;

        double viaDouble = u;
        if (BitConverter.DoubleToInt64Bits(viaDouble) != 0x43E0000010000000L) return 2;
        float roundedTwice = (float)viaDouble;
        if (BitConverter.SingleToInt32Bits(roundedTwice) != 0x5F000000) return 3;

        // A 32-bit unsigned source takes the same opcode pair; every uint is exact in a
        // double, so this checks only that the pair still works on an int32 stack slot.
        uint w = 0xFFFFFFFFu - (uint)zero;
        float fromUInt = (float)w;
        if (BitConverter.SingleToInt32Bits(fromUInt) != 0x4F800000) return 4;

        // Search: values built to sit just above a float32 tie with the excess below the
        // double's precision, at every bit length from 54 to 64, plus raw random values.
        ulong state = 0x2545F4914F6CDD1DUL + zero;
        int mismatches = 0;
        for (int i = 0; i < 3000; i++)
        {
            ulong r = Next(ref state);
            ulong candidate;
            if (i % 4 == 3)
            {
                candidate = r;
            }
            else
            {
                ulong top24 = (r >> 40) | 0x800000UL;
                ulong low11 = (r & 0x7FFUL) | 1UL;
                ulong full = (top24 << 40) | (1UL << 39) | low11;
                int drop = (int)((r >> 24) % 11UL);
                candidate = full >> drop;
            }

            float converted = (float)candidate;
            if (BitConverter.SingleToInt32Bits(converted) != BitConverter.SingleToInt32Bits(Reference(candidate)))
            {
                mismatches++;
            }
        }

        if (mismatches != 0)
        {
            Console.WriteLine("mismatches: " + mismatches);
            return 5;
        }

        return 0;
    }
}
