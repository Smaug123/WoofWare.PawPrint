using System;

// Exercises `Array.Clear` over element types with no GC pointers, i.e. everything that
// routes through `SpanHelpers.ClearWithoutReferences(ref byte, nuint)`.
//
// Rather than hand-picking cases, each element type is swept exhaustively: for every array
// length 0..MaxLen and every valid (index, length) pair, the array is seeded with a pattern
// whose every slot is non-default, cleared, and then checked slot by slot -- cleared slots
// must be `default`, untouched slots must still hold their seed. That covers the off-by-one
// and byte-count-derivation failure modes (a wrong `ComponentSize` clears the wrong number of
// bytes, and shows up as either a surviving non-zero inside the range or a clobbered slot
// outside it) for every element width the interpreter models.
//
// Failure codes are `typeBase + ((len * 10 + index) * 10 + length) * 10 + kind`, so a
// mismatch identifies the element type, the exact swept case, and which check failed.
// Reference-typed element arrays are deliberately absent: those route through
// `SpanHelpers.ClearWithReferences` instead, which is a separate boundary.
public class TestArrayClear
{
    private const int MaxLen = 8;

    private enum Colour
    {
        None = 0,
        Red = 1,
        Green = 2,
        Blue = 3,
    }

    private struct Pair
    {
        public int X;
        public long Y;
        public byte Z;
    }

    private struct Nested
    {
        public Pair Inner;
        public double D;
    }

    private static int Case(int len, int index, int length, int kind)
    {
        return (((len * 10) + index) * 10 + length) * 10 + kind;
    }

    // kind 1: a slot inside the cleared range is not default.
    // kind 2: a slot outside the cleared range no longer holds its seed.

    private static int SweepInt()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    int[] a = new int[len];
                    for (int i = 0; i < len; i++) a[i] = (i * 7) + 1;

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != (i * 7) + 1) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepByte()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    byte[] a = new byte[len];
                    for (int i = 0; i < len; i++) a[i] = (byte)(i + 1);

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != (byte)(i + 1)) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepSByte()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    sbyte[] a = new sbyte[len];
                    for (int i = 0; i < len; i++) a[i] = (sbyte)(-i - 1);

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != (sbyte)(-i - 1)) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepShort()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    short[] a = new short[len];
                    for (int i = 0; i < len; i++) a[i] = (short)((i * 300) + 1);

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != (short)((i * 300) + 1)) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepUShort()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    ushort[] a = new ushort[len];
                    for (int i = 0; i < len; i++) a[i] = (ushort)(0xF000 + i + 1);

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != (ushort)(0xF000 + i + 1)) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepLong()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    long[] a = new long[len];
                    for (int i = 0; i < len; i++) a[i] = ((long)i << 40) + 1;

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != ((long)i << 40) + 1) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepULong()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    ulong[] a = new ulong[len];
                    for (int i = 0; i < len; i++) a[i] = 0xF000000000000000UL + (ulong)i + 1;

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != 0xF000000000000000UL + (ulong)i + 1) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepDouble()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    double[] a = new double[len];
                    for (int i = 0; i < len; i++) a[i] = i + 1.5;

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0.0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != i + 1.5) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepFloat()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    float[] a = new float[len];
                    for (int i = 0; i < len; i++) a[i] = i + 1.25f;

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0.0f) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != i + 1.25f) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepChar()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    char[] a = new char[len];
                    for (int i = 0; i < len; i++) a[i] = (char)('A' + i);

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != '\0') return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != (char)('A' + i)) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepBool()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    bool[] a = new bool[len];
                    for (int i = 0; i < len; i++) a[i] = true;

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i]) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (!a[i]) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepEnum()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    Colour[] a = new Colour[len];
                    for (int i = 0; i < len; i++) a[i] = (Colour)((i % 3) + 1);

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != Colour.None) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != (Colour)((i % 3) + 1)) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepNativeInt()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    nint[] a = new nint[len];
                    for (int i = 0; i < len; i++) a[i] = (nint)(i + 1);

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i] != (nint)(i + 1)) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepStruct()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    Pair[] a = new Pair[len];
                    for (int i = 0; i < len; i++)
                    {
                        a[i].X = i + 1;
                        a[i].Y = ((long)i << 33) + 1;
                        a[i].Z = (byte)(i + 1);
                    }

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i].X != 0 || a[i].Y != 0 || a[i].Z != 0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i].X != i + 1) return Case(len, index, length, 2);
                            if (a[i].Y != ((long)i << 33) + 1) return Case(len, index, length, 2);
                            if (a[i].Z != (byte)(i + 1)) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    private static int SweepNestedStruct()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    Nested[] a = new Nested[len];
                    for (int i = 0; i < len; i++)
                    {
                        a[i].Inner.X = i + 1;
                        a[i].Inner.Y = ((long)i << 20) + 1;
                        a[i].Inner.Z = (byte)(i + 1);
                        a[i].D = i + 2.5;
                    }

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i].Inner.X != 0 || a[i].Inner.Y != 0 || a[i].Inner.Z != 0)
                            {
                                return Case(len, index, length, 1);
                            }

                            if (a[i].D != 0.0) return Case(len, index, length, 1);
                        }
                        else
                        {
                            if (a[i].Inner.X != i + 1) return Case(len, index, length, 2);
                            if (a[i].Inner.Y != ((long)i << 20) + 1) return Case(len, index, length, 2);
                            if (a[i].Inner.Z != (byte)(i + 1)) return Case(len, index, length, 2);
                            if (a[i].D != i + 2.5) return Case(len, index, length, 2);
                        }
                    }
                }
            }
        }

        return 0;
    }

    // The single-argument overload takes a different route through `Array.Clear`'s own IL:
    // it derives the byte count from `pMT->ComponentSize * array.NativeLength` and takes its
    // data reference from `MemoryMarshal.GetArrayDataReference(Array)` rather than from
    // `Unsafe.As<RawArrayData>(array).Data`.
    private static int SweepWholeArrayOverload()
    {
        for (int len = 0; len <= MaxLen; len++)
        {
            int[] ints = new int[len];
            for (int i = 0; i < len; i++) ints[i] = i + 1;
            Array.Clear(ints);
            for (int i = 0; i < len; i++)
            {
                if (ints[i] != 0) return Case(len, 0, len, 1);
            }

            byte[] bytes = new byte[len];
            for (int i = 0; i < len; i++) bytes[i] = (byte)(i + 1);
            Array.Clear(bytes);
            for (int i = 0; i < len; i++)
            {
                if (bytes[i] != 0) return Case(len, 0, len, 3);
            }

            Pair[] pairs = new Pair[len];
            for (int i = 0; i < len; i++)
            {
                pairs[i].X = i + 1;
                pairs[i].Y = i + 2;
                pairs[i].Z = (byte)(i + 3);
            }

            Array.Clear(pairs);
            for (int i = 0; i < len; i++)
            {
                if (pairs[i].X != 0 || pairs[i].Y != 0 || pairs[i].Z != 0) return Case(len, 0, len, 4);
            }
        }

        return 0;
    }

    // A clear that is long enough for the BCL's block-at-a-time paths (`len > 64`, and the
    // `len >= 256` opportunistic-alignment prologue) rather than only its small-size unrolls.
    private static int TestLargeArray()
    {
        const int N = 400;
        int[] a = new int[N];
        for (int i = 0; i < N; i++) a[i] = i + 1;

        Array.Clear(a, 3, N - 5);

        for (int i = 0; i < N; i++)
        {
            bool cleared = i >= 3 && i < N - 2;
            if (cleared)
            {
                if (a[i] != 0) return 1;
            }
            else
            {
                if (a[i] != i + 1) return 2;
            }
        }

        return 0;
    }

    // Argument validation happens in `Array.Clear` itself, before it reaches either
    // `SpanHelpers` helper. Pinned here so a future rewrite of the clear path cannot quietly
    // start clamping instead of throwing.
    private static int TestArgumentValidation()
    {
        int[] a = new int[3];
        for (int i = 0; i < 3; i++) a[i] = i + 1;

        try
        {
            Array.Clear(null, 0, 0);
            return 1;
        }
        catch (ArgumentNullException)
        {
        }

        try
        {
            Array.Clear(a, 0, 4);
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            Array.Clear(a, -1, 1);
            return 3;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            Array.Clear(a, 2, -1);
            return 4;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            Array.Clear(a, 4, 0);
            return 5;
        }
        catch (IndexOutOfRangeException)
        {
        }

        // A rejected clear must not have touched anything.
        for (int i = 0; i < 3; i++)
        {
            if (a[i] != i + 1) return 6;
        }

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = SweepInt();
        if (result != 0) return 1000000 + result;

        result = SweepByte();
        if (result != 0) return 2000000 + result;

        result = SweepSByte();
        if (result != 0) return 3000000 + result;

        result = SweepShort();
        if (result != 0) return 4000000 + result;

        result = SweepUShort();
        if (result != 0) return 5000000 + result;

        result = SweepLong();
        if (result != 0) return 6000000 + result;

        result = SweepULong();
        if (result != 0) return 7000000 + result;

        result = SweepDouble();
        if (result != 0) return 8000000 + result;

        result = SweepFloat();
        if (result != 0) return 9000000 + result;

        result = SweepChar();
        if (result != 0) return 10000000 + result;

        result = SweepBool();
        if (result != 0) return 11000000 + result;

        result = SweepEnum();
        if (result != 0) return 12000000 + result;

        result = SweepNativeInt();
        if (result != 0) return 13000000 + result;

        result = SweepStruct();
        if (result != 0) return 14000000 + result;

        result = SweepNestedStruct();
        if (result != 0) return 15000000 + result;

        result = SweepWholeArrayOverload();
        if (result != 0) return 16000000 + result;

        result = TestLargeArray();
        if (result != 0) return 17000000 + result;

        result = TestArgumentValidation();
        if (result != 0) return 18000000 + result;

        return 0;
    }
}
