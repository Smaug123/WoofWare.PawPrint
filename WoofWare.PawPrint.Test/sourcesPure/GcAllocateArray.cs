using System;

// `GC.AllocateArray<T>` and `GC.AllocateUninitializedArray<T>`: the two public APIs over the
// `GCInterface_AllocateNewArray` QCall.
//
// `AllocateArray<T>` always reaches the QCall (GC.CoreCLR.cs:841-853 has no shortcut), so every
// case below exercises it. `AllocateUninitializedArray<T>` does not: a release-built CoreLib
// returns `new T[length]` instead whenever the array is unpinned and either holds references or
// is smaller than `2048 / sizeof(T)` (GC.CoreCLR.cs:800-815). The uninitialized cases here are
// therefore sized at or past that threshold — 2048 for `byte`, 512 for `int` — so that they
// reach the QCall rather than testing `newarr` under a different spelling.
//
// `AllocateArray<T>` is documented as zero-filled and CoreCLR never passes
// GC_ALLOC_ZEROING_OPTIONAL for it, so asserting that its contents are `default` is a real
// cross-runtime claim. `AllocateUninitializedArray<T>`'s contents are explicitly unspecified, so
// nothing here reads an element of one before writing it.
public class TestGcAllocateArray
{
    private enum Colour
    {
        None = 0,
        Red = 1,
        Green = 2,
    }

    private struct Pair
    {
        public int X;
        public long Y;
        public byte Z;
    }

    // Failure codes are `kind * 10 + step`, so a failure names both the element type and which
    // property of the array was wrong.
    // step 1: null;  2: wrong Length;  3: wrong runtime type;  4: element not default;
    // step 5: element does not survive a write.

    private static int CheckInt(int len)
    {
        int[] a = GC.AllocateArray<int>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        if (a.GetType() != typeof(int[])) return 3;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != 0) return 4;
        }

        for (int i = 0; i < len; i++) a[i] = (i * 7) + 1;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != (i * 7) + 1) return 5;
        }

        return 0;
    }

    private static int CheckByte(int len)
    {
        byte[] a = GC.AllocateArray<byte>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        if (a.GetType() != typeof(byte[])) return 3;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != 0) return 4;
        }

        for (int i = 0; i < len; i++) a[i] = (byte)(i + 1);
        for (int i = 0; i < len; i++)
        {
            if (a[i] != (byte)(i + 1)) return 5;
        }

        return 0;
    }

    private static int CheckLong(int len)
    {
        long[] a = GC.AllocateArray<long>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != 0L) return 4;
        }

        for (int i = 0; i < len; i++) a[i] = ((long)i << 40) + 1;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != ((long)i << 40) + 1) return 5;
        }

        return 0;
    }

    private static int CheckDouble(int len)
    {
        double[] a = GC.AllocateArray<double>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != 0.0) return 4;
        }

        for (int i = 0; i < len; i++) a[i] = i + 1.5;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != i + 1.5) return 5;
        }

        return 0;
    }

    private static int CheckChar(int len)
    {
        char[] a = GC.AllocateArray<char>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != '\0') return 4;
        }

        for (int i = 0; i < len; i++) a[i] = (char)('A' + i);
        for (int i = 0; i < len; i++)
        {
            if (a[i] != (char)('A' + i)) return 5;
        }

        return 0;
    }

    private static int CheckBool(int len)
    {
        bool[] a = GC.AllocateArray<bool>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        for (int i = 0; i < len; i++)
        {
            if (a[i]) return 4;
        }

        for (int i = 0; i < len; i++) a[i] = (i % 2) == 0;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != ((i % 2) == 0)) return 5;
        }

        return 0;
    }

    private static int CheckEnum(int len)
    {
        Colour[] a = GC.AllocateArray<Colour>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        if (a.GetType() != typeof(Colour[])) return 3;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != Colour.None) return 4;
        }

        for (int i = 0; i < len; i++) a[i] = (Colour)((i % 2) + 1);
        for (int i = 0; i < len; i++)
        {
            if (a[i] != (Colour)((i % 2) + 1)) return 5;
        }

        return 0;
    }

    private static int CheckStruct(int len)
    {
        Pair[] a = GC.AllocateArray<Pair>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        for (int i = 0; i < len; i++)
        {
            if (a[i].X != 0 || a[i].Y != 0L || a[i].Z != 0) return 4;
        }

        for (int i = 0; i < len; i++)
        {
            a[i].X = i + 1;
            a[i].Y = ((long)i << 33) + 1;
            a[i].Z = (byte)(i + 1);
        }

        for (int i = 0; i < len; i++)
        {
            if (a[i].X != i + 1) return 5;
            if (a[i].Y != ((long)i << 33) + 1) return 5;
            if (a[i].Z != (byte)(i + 1)) return 5;
        }

        return 0;
    }

    // A reference element type. `AllocateUninitializedArray` refuses to take the QCall for these
    // (`IsReferenceOrContainsReferences<T>()` sends it to `new T[]`), but `AllocateArray` does
    // take it, so this is the only route by which the QCall sees a reference-containing array.
    private static int CheckString(int len)
    {
        string[] a = GC.AllocateArray<string>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        if (a.GetType() != typeof(string[])) return 3;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != null) return 4;
        }

        // Literals rather than `"x" + i`: int-to-string formatting is a large, unrelated BCL path,
        // and nothing here needs distinct values beyond telling neighbouring slots apart.
        string[] words = new string[] { "alpha", "beta", "gamma" };
        for (int i = 0; i < len; i++) a[i] = words[i % 3];
        for (int i = 0; i < len; i++)
        {
            if (!ReferenceEquals(a[i], words[i % 3])) return 5;
        }

        return 0;
    }

    private static int CheckObject(int len)
    {
        object[] a = GC.AllocateArray<object>(len);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != null) return 4;
        }

        object marker = new object();
        for (int i = 0; i < len; i++) a[i] = marker;
        for (int i = 0; i < len; i++)
        {
            if (!ReferenceEquals(a[i], marker)) return 5;
        }

        return 0;
    }

    // `pinned: true` sets GC_ALLOC_PINNED_OBJECT_HEAP (64) rather than 0. PawPrint never moves an
    // object, so the request changes nothing it can observe; what this pins is that the flag is
    // accepted rather than rejected as an unrecognised bit.
    private static int CheckPinned(int len)
    {
        int[] a = GC.AllocateArray<int>(len, true);
        if (a == null) return 1;
        if (a.Length != len) return 2;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != 0) return 4;
        }

        for (int i = 0; i < len; i++) a[i] = i + 3;
        for (int i = 0; i < len; i++)
        {
            if (a[i] != i + 3) return 5;
        }

        return 0;
    }

    // 2048 == 2048 / sizeof(byte), the first unpinned length for which a release CoreLib does not
    // short-circuit to `new byte[length]`.
    private static int CheckUninitializedBytes()
    {
        const int N = 2048;
        byte[] a = GC.AllocateUninitializedArray<byte>(N);
        if (a == null) return 1;
        if (a.Length != N) return 2;
        if (a.GetType() != typeof(byte[])) return 3;

        // No read before write: the contents of an uninitialized array are unspecified.
        for (int i = 0; i < N; i++) a[i] = (byte)((i * 3) + 1);
        for (int i = 0; i < N; i++)
        {
            if (a[i] != (byte)((i * 3) + 1)) return 5;
        }

        return 0;
    }

    // 512 == 2048 / sizeof(int), the same threshold at a different element width.
    private static int CheckUninitializedInts()
    {
        const int N = 512;
        int[] a = GC.AllocateUninitializedArray<int>(N);
        if (a == null) return 1;
        if (a.Length != N) return 2;

        for (int i = 0; i < N; i++) a[i] = (i * 5) + 2;
        for (int i = 0; i < N; i++)
        {
            if (a[i] != (i * 5) + 2) return 5;
        }

        return 0;
    }

    // `pinned: true` skips the whole `if (!pinned)` block, so this reaches the QCall at a length
    // that would otherwise have been short-circuited, with flags 80
    // (ZEROING_OPTIONAL | PINNED_OBJECT_HEAP).
    private static int CheckUninitializedPinnedSmall()
    {
        const int N = 4;
        int[] a = GC.AllocateUninitializedArray<int>(N, true);
        if (a == null) return 1;
        if (a.Length != N) return 2;

        for (int i = 0; i < N; i++) a[i] = i + 9;
        for (int i = 0; i < N; i++)
        {
            if (a[i] != i + 9) return 5;
        }

        return 0;
    }

    private static int Sweep(int len)
    {
        int result;

        result = CheckInt(len);
        if (result != 0) return 10 + result;

        result = CheckByte(len);
        if (result != 0) return 20 + result;

        result = CheckLong(len);
        if (result != 0) return 30 + result;

        result = CheckDouble(len);
        if (result != 0) return 40 + result;

        result = CheckChar(len);
        if (result != 0) return 50 + result;

        result = CheckBool(len);
        if (result != 0) return 60 + result;

        result = CheckEnum(len);
        if (result != 0) return 70 + result;

        result = CheckStruct(len);
        if (result != 0) return 80 + result;

        result = CheckString(len);
        if (result != 0) return 90 + result;

        result = CheckObject(len);
        if (result != 0) return 100 + result;

        result = CheckPinned(len);
        if (result != 0) return 110 + result;

        return 0;
    }

    public static int Main(string[] argv)
    {
        // Zero is included deliberately: an empty array is the one length at which a
        // length-derived allocation bug cannot show up as a wrong element, only as a wrong Length.
        int[] lengths = new int[] { 0, 1, 7, 64 };

        for (int i = 0; i < lengths.Length; i++)
        {
            int result = Sweep(lengths[i]);
            if (result != 0) return (i + 1) * 1000 + result;
        }

        int r;

        r = CheckUninitializedBytes();
        if (r != 0) return 9000 + r;

        r = CheckUninitializedInts();
        if (r != 0) return 9100 + r;

        r = CheckUninitializedPinnedSmall();
        if (r != 0) return 9200 + r;

        return 0;
    }
}
