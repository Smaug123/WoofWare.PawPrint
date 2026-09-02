using System.Numerics;

internal static class Program
{
    private struct Pair
    {
        public int X;
        public int Y;
    }

    private static int Main()
    {
        // Vector<T>.IsSupported asks whether T is a valid vector element type. It is
        // independent of hardware acceleration: real .NET answers true for the twelve
        // primitive element types whatever the hardware can do, and
        // ThrowHelper.ThrowForUnsupportedNumericsVectorBaseType relies on that to no-op.
        if (!Vector<byte>.IsSupported)
        {
            return 1;
        }

        if (!Vector<sbyte>.IsSupported)
        {
            return 2;
        }

        if (!Vector<short>.IsSupported)
        {
            return 3;
        }

        if (!Vector<ushort>.IsSupported)
        {
            return 4;
        }

        if (!Vector<int>.IsSupported)
        {
            return 5;
        }

        if (!Vector<uint>.IsSupported)
        {
            return 6;
        }

        if (!Vector<long>.IsSupported)
        {
            return 7;
        }

        if (!Vector<ulong>.IsSupported)
        {
            return 8;
        }

        if (!Vector<nint>.IsSupported)
        {
            return 9;
        }

        if (!Vector<nuint>.IsSupported)
        {
            return 10;
        }

        if (!Vector<float>.IsSupported)
        {
            return 11;
        }

        if (!Vector<double>.IsSupported)
        {
            return 12;
        }

        // Every other element type answers false, including the primitives that are
        // not numeric and structs of the right size.
        if (Vector<bool>.IsSupported)
        {
            return 13;
        }

        if (Vector<char>.IsSupported)
        {
            return 14;
        }

        if (Vector<decimal>.IsSupported)
        {
            return 15;
        }

        if (Vector<Pair>.IsSupported)
        {
            return 16;
        }

        return 0;
    }
}
