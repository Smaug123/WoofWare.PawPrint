using System;
using System.Runtime.CompilerServices;

// Subtracting and comparing two byrefs into one array where only one of them carries a
// byte view.
//
// The two spellings differ only in how the byref got onto the native-int stack:
// `Unsafe.AsPointer` transports a plain array-element byref, while `fixed` goes through
// `conv.u`, which anchors a byte view on it. The distance between them must be the byte
// distance the array's layout implies, whatever the element's size, whichever side carries
// the view, and whether either side is at a nonzero index or mid-cell.
//
// Returns 0 on success, or the number of the first check that failed.
public class Program
{
    struct ThreeBytes
    {
        public byte A;
        public byte B;
        public byte C;
    }

    struct Triple
    {
        public int A;
        public int B;
        public int C;
    }

    // `unanchoredIndex` is reached through `Unsafe.AsPointer`, `anchoredIndex` through `fixed`.
    static unsafe int Check<T> (T[] a, int unanchoredIndex, int anchoredIndex, int failBase) where T : unmanaged
    {
        T* unanchored = (T*) Unsafe.AsPointer (ref a[unanchoredIndex]);
        int cells = anchoredIndex - unanchoredIndex;
        long bytes = (long) cells * sizeof (T);

        fixed (T* anchored = &a[anchoredIndex])
        {
            // Element-denominated, as C# pointer subtraction is: `sub; sizeof T; div`.
            if (anchored - unanchored != cells)
                return failBase + 1;
            if (unanchored - anchored != -cells)
                return failBase + 2;

            // Byte-denominated: the raw `sub` with no division after it.
            if ((byte*) anchored - (byte*) unanchored != bytes)
                return failBase + 3;
            if ((byte*) unanchored - (byte*) anchored != -bytes)
                return failBase + 4;

            // The byte-view side mid-cell (when `T` is wider than a byte), so the cursor
            // carries an in-cell offset as well as a cell index.
            byte* midCell = (byte*) anchored + 1;
            if (midCell - (byte*) unanchored != bytes + 1)
                return failBase + 5;
            if ((byte*) unanchored - midCell != -(bytes + 1))
                return failBase + 6;

            // Unsigned ordering and equality of the same mixed pair.
            if ((anchored == unanchored) != (cells == 0))
                return failBase + 7;
            if ((anchored > unanchored) != (cells > 0))
                return failBase + 8;
            if ((anchored < unanchored) != (cells < 0))
                return failBase + 9;
            if ((unanchored > anchored) != (cells < 0))
                return failBase + 10;
            if (!(midCell > (byte*) unanchored) && cells >= 0)
                return failBase + 11;
        }

        return 0;
    }

    public static unsafe int Main (string[] args)
    {
        int[] a = new int[4] { 10, 20, 30, 40 };

        int* unanchored = (int*) Unsafe.AsPointer (ref a[0]);

        fixed (int* anchored = a)
        {
            // 1: the same address by two routes, so the distance between them is zero.
            if (anchored - unanchored != 0)
                return 1;

            // 2: and the distance is measured in elements, as C# pointer subtraction is.
            if ((anchored + 2) - unanchored != 2)
                return 2;

            // 3: the other direction of the same pair.
            if (unanchored - (anchored + 3) != -3)
                return 3;
        }

        int result;

        byte[] bytes = new byte[6];
        short[] shorts = new short[6];
        int[] ints = new int[6];
        long[] longs = new long[6];
        ThreeBytes[] threes = new ThreeBytes[6];
        Triple[] triples = new Triple[6];

        int failBase = 100;

        for (int i = 0; i < 6; i++)
        {
            for (int j = 0; j < 6; j++)
            {
                if ((result = Check (bytes, i, j, failBase)) != 0)
                    return result;
                if ((result = Check (shorts, i, j, failBase + 20)) != 0)
                    return result;
                if ((result = Check (ints, i, j, failBase + 40)) != 0)
                    return result;
                if ((result = Check (longs, i, j, failBase + 60)) != 0)
                    return result;
                if ((result = Check (threes, i, j, failBase + 80)) != 0)
                    return result;
                if ((result = Check (triples, i, j, failBase + 100)) != 0)
                    return result;
            }
        }

        return 0;
    }
}
