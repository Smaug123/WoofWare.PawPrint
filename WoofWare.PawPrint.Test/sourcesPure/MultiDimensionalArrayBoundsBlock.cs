using System;

// CoreLib reaches a multi-dimensional array's per-dimension bounds by a raw walk from
// `Unsafe.As<RawArrayData>(array).Data`, which on such an array is the start of the
// `2 * rank` int32 bounds block (lengths, then lower bounds) that CoreCLR lays out
// between the length header and element 0. `Array.Clear(Array, int, int)` reads the
// first lower bound there and then steps over the block to find element 0;
// `Array.GetValue` / `SetValue` read every length and lower bound in `GetFlattenedIndex`.
// Neither is intercepted, so this guest pins that PawPrint answers those reads from the
// array's shape, and that the step past the block lands on element 0.
//
// `Array.Clear` is swept over every (index, length) pair of each array, seeded with a
// pattern whose every slot is non-default: a lower bound read from element data, or a
// step that lands past element 0, shows up as a surviving seed inside the range or a
// clobbered slot outside it. The element widths cover a block that ends on a cell
// boundary (int), two bounds entries per cell (long), one byte per cell (byte) and
// entries that straddle cells (a 3-byte struct).
//
// Failure codes are `kindBase + (index * 100 + length) * 10 + check`, so a mismatch names
// the array, the swept pair, and which check failed:
//   check 1: a slot inside the cleared range is not default
//   check 2: a slot outside the cleared range no longer holds its seed
public class TestMultiDimensionalArrayBoundsBlock
{
    private struct Triple
    {
        public byte A;
        public byte B;
        public byte C;
    }

    private static int Code(int index, int length, int check)
    {
        return (index * 100 + length) * 10 + check;
    }

    // The scenario from the report: clearing the first two cells of an int[2,3] must clear
    // a[0,0] and a[0,1] and nothing else.
    private static int ClearFirstRowPrefix()
    {
        int[,] a = new int[2, 3];
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                a[i, j] = 10 * i + j + 1;

        Array.Clear(a, 0, 2);

        if (a[0, 0] != 0) return 1;
        if (a[0, 1] != 0) return 2;
        if (a[0, 2] != 3) return 3;
        if (a[1, 0] != 11) return 4;
        if (a[1, 1] != 12) return 5;
        if (a[1, 2] != 13) return 6;
        return 0;
    }

    private static int SweepInt2D()
    {
        const int d0 = 2, d1 = 3;
        int total = d0 * d1;
        for (int index = 0; index <= total; index++)
        {
            for (int length = 0; index + length <= total; length++)
            {
                int[,] a = new int[d0, d1];
                for (int i = 0; i < d0; i++)
                    for (int j = 0; j < d1; j++)
                        a[i, j] = 100 + i * d1 + j;

                Array.Clear(a, index, length);

                for (int i = 0; i < d0; i++)
                {
                    for (int j = 0; j < d1; j++)
                    {
                        int flat = i * d1 + j;
                        bool cleared = flat >= index && flat < index + length;
                        if (cleared && a[i, j] != 0) return Code(index, length, 1);
                        if (!cleared && a[i, j] != 100 + flat) return Code(index, length, 2);
                    }
                }
            }
        }
        return 0;
    }

    private static int SweepLong3D()
    {
        const int d0 = 2, d1 = 2, d2 = 2;
        int total = d0 * d1 * d2;
        for (int index = 0; index <= total; index++)
        {
            for (int length = 0; index + length <= total; length++)
            {
                long[,,] a = new long[d0, d1, d2];
                for (int i = 0; i < d0; i++)
                    for (int j = 0; j < d1; j++)
                        for (int k = 0; k < d2; k++)
                            a[i, j, k] = 0x1_0000_0000L + (i * d1 + j) * d2 + k;

                Array.Clear(a, index, length);

                for (int i = 0; i < d0; i++)
                {
                    for (int j = 0; j < d1; j++)
                    {
                        for (int k = 0; k < d2; k++)
                        {
                            int flat = (i * d1 + j) * d2 + k;
                            bool cleared = flat >= index && flat < index + length;
                            if (cleared && a[i, j, k] != 0) return Code(index, length, 1);
                            if (!cleared && a[i, j, k] != 0x1_0000_0000L + flat) return Code(index, length, 2);
                        }
                    }
                }
            }
        }
        return 0;
    }

    private static int SweepByte3D()
    {
        const int d0 = 3, d1 = 1, d2 = 2;
        int total = d0 * d1 * d2;
        for (int index = 0; index <= total; index++)
        {
            for (int length = 0; index + length <= total; length++)
            {
                byte[,,] a = new byte[d0, d1, d2];
                for (int i = 0; i < d0; i++)
                    for (int j = 0; j < d1; j++)
                        for (int k = 0; k < d2; k++)
                            a[i, j, k] = (byte)(200 + (i * d1 + j) * d2 + k);

                Array.Clear(a, index, length);

                for (int i = 0; i < d0; i++)
                {
                    for (int j = 0; j < d1; j++)
                    {
                        for (int k = 0; k < d2; k++)
                        {
                            int flat = (i * d1 + j) * d2 + k;
                            bool cleared = flat >= index && flat < index + length;
                            if (cleared && a[i, j, k] != 0) return Code(index, length, 1);
                            if (!cleared && a[i, j, k] != (byte)(200 + flat)) return Code(index, length, 2);
                        }
                    }
                }
            }
        }
        return 0;
    }

    private static int SweepTriple2D()
    {
        const int d0 = 2, d1 = 3;
        int total = d0 * d1;
        for (int index = 0; index <= total; index++)
        {
            for (int length = 0; index + length <= total; length++)
            {
                Triple[,] a = new Triple[d0, d1];
                for (int i = 0; i < d0; i++)
                {
                    for (int j = 0; j < d1; j++)
                    {
                        int flat = i * d1 + j;
                        a[i, j].A = (byte)(1 + flat);
                        a[i, j].B = (byte)(50 + flat);
                        a[i, j].C = (byte)(100 + flat);
                    }
                }

                Array.Clear(a, index, length);

                for (int i = 0; i < d0; i++)
                {
                    for (int j = 0; j < d1; j++)
                    {
                        int flat = i * d1 + j;
                        bool cleared = flat >= index && flat < index + length;
                        Triple t = a[i, j];
                        if (cleared && (t.A != 0 || t.B != 0 || t.C != 0)) return Code(index, length, 1);
                        if (!cleared && (t.A != (byte)(1 + flat) || t.B != (byte)(50 + flat) || t.C != (byte)(100 + flat)))
                            return Code(index, length, 2);
                    }
                }
            }
        }
        return 0;
    }

    // Reference elements take the `ClearWithReferences` branch, after the same bounds read
    // and skip.
    private static int SweepString2D()
    {
        const int d0 = 2, d1 = 2;
        int total = d0 * d1;
        string[] seeds = { "a", "b", "c", "d" };
        for (int index = 0; index <= total; index++)
        {
            for (int length = 0; index + length <= total; length++)
            {
                string[,] a = new string[d0, d1];
                for (int i = 0; i < d0; i++)
                    for (int j = 0; j < d1; j++)
                        a[i, j] = seeds[i * d1 + j];

                Array.Clear(a, index, length);

                for (int i = 0; i < d0; i++)
                {
                    for (int j = 0; j < d1; j++)
                    {
                        int flat = i * d1 + j;
                        bool cleared = flat >= index && flat < index + length;
                        if (cleared && a[i, j] != null) return Code(index, length, 1);
                        if (!cleared && !ReferenceEquals(a[i, j], seeds[flat])) return Code(index, length, 2);
                    }
                }
            }
        }
        return 0;
    }

    private static bool ClearThrowsIndexOutOfRange(Array a, int index, int length)
    {
        try
        {
            Array.Clear(a, index, length);
            return false;
        }
        catch (IndexOutOfRangeException)
        {
            return true;
        }
    }

    // Out-of-range pairs are rejected against the lower bound and total length read
    // through the same walk, and a rejected call leaves the array untouched.
    private static int ClearRejectsOutOfRange()
    {
        int[,] a = new int[2, 3];
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                a[i, j] = 10 * i + j + 1;

        if (!ClearThrowsIndexOutOfRange(a, -1, 1)) return 1;
        if (!ClearThrowsIndexOutOfRange(a, 0, 7)) return 2;
        if (!ClearThrowsIndexOutOfRange(a, 6, 1)) return 3;
        if (!ClearThrowsIndexOutOfRange(a, 3, -1)) return 4;
        if (!ClearThrowsIndexOutOfRange(a, 7, 0)) return 5;
        if (!ClearThrowsIndexOutOfRange(a, int.MinValue, 1)) return 6;
        if (!ClearThrowsIndexOutOfRange(a, 1, int.MaxValue)) return 7;

        // A zero-length clear at the very end is in range.
        if (ClearThrowsIndexOutOfRange(a, 6, 0)) return 8;

        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                if (a[i, j] != 10 * i + j + 1) return 9;

        // An array with a zero-length dimension has no elements but still has a bounds
        // block to read.
        int[,,] empty = new int[2, 0, 5];
        if (ClearThrowsIndexOutOfRange(empty, 0, 0)) return 10;
        if (!ClearThrowsIndexOutOfRange(empty, 0, 1)) return 11;
        if (!ClearThrowsIndexOutOfRange(empty, 1, 0)) return 12;

        return 0;
    }

    private static int GetSetValueInt2D()
    {
        int[,] a = new int[2, 3];
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                a[i, j] = 10 * i + j + 1;

        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                if ((int)a.GetValue(i, j) != 10 * i + j + 1) return 1 + i * 3 + j;
                if ((int)a.GetValue(new int[] { i, j }) != 10 * i + j + 1) return 10 + i * 3 + j;
            }
        }

        a.SetValue(77, 1, 2);
        if (a[1, 2] != 77) return 20;
        a.SetValue(78, new int[] { 0, 1 });
        if (a[0, 1] != 78) return 21;
        if ((int)a.GetValue(1, 2) != 77) return 22;

        // Every other cell survives the two stores.
        if (a[0, 0] != 1 || a[0, 2] != 3 || a[1, 0] != 11 || a[1, 1] != 12) return 23;

        return 0;
    }

    private static int GetSetValueLong3D()
    {
        long[,,] a = new long[2, 3, 4];
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                for (int k = 0; k < 4; k++)
                    a[i, j, k] = 1000 + 100 * i + 10 * j + k;

        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                for (int k = 0; k < 4; k++)
                    if ((long)a.GetValue(i, j, k) != 1000 + 100 * i + 10 * j + k) return 1 + (i * 3 + j) * 4 + k;

        a.SetValue(-5L, 1, 2, 3);
        if (a[1, 2, 3] != -5L) return 30;
        if ((long)a.GetValue(1, 2, 3) != -5L) return 31;
        if (a[1, 2, 2] != 1122 || a[0, 0, 0] != 1000) return 32;

        return 0;
    }

    private static int GetSetValueString2D()
    {
        string[,] a = new string[2, 2];
        a[0, 0] = "p";
        a[0, 1] = "q";
        a[1, 0] = "r";
        a[1, 1] = "s";

        if (!ReferenceEquals(a.GetValue(0, 0), a[0, 0])) return 1;
        if (!ReferenceEquals(a.GetValue(0, 1), a[0, 1])) return 2;
        if (!ReferenceEquals(a.GetValue(1, 0), a[1, 0])) return 3;
        if (!ReferenceEquals(a.GetValue(1, 1), a[1, 1])) return 4;

        string t = "t";
        a.SetValue(t, 1, 0);
        if (!ReferenceEquals(a[1, 0], t)) return 5;
        a.SetValue(null, 0, 1);
        if (a[0, 1] != null) return 6;
        if (a[0, 0] != "p" || a[1, 1] != "s") return 7;

        return 0;
    }

    private static bool GetValueThrowsIndexOutOfRange(Array a, int i, int j)
    {
        try
        {
            a.GetValue(i, j);
            return false;
        }
        catch (IndexOutOfRangeException)
        {
            return true;
        }
    }

    // `GetFlattenedIndex` checks each index against the length read from the bounds block
    // for that dimension, not against the total length.
    private static int GetValueRejectsOutOfRange()
    {
        int[,] a = new int[2, 3];

        if (!GetValueThrowsIndexOutOfRange(a, 2, 0)) return 1;
        if (!GetValueThrowsIndexOutOfRange(a, 0, 3)) return 2;
        if (!GetValueThrowsIndexOutOfRange(a, -1, 0)) return 3;
        if (!GetValueThrowsIndexOutOfRange(a, 0, -1)) return 4;
        if (!GetValueThrowsIndexOutOfRange(a, 1, int.MinValue)) return 5;

        // In range at the far corner, and (0, 2) is not confused with (2, 0).
        if (GetValueThrowsIndexOutOfRange(a, 1, 2)) return 6;
        if (GetValueThrowsIndexOutOfRange(a, 0, 2)) return 7;

        try
        {
            a.SetValue(1, 0, 3);
            return 8;
        }
        catch (IndexOutOfRangeException)
        {
        }

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = ClearFirstRowPrefix();
        if (result != 0) return 100 + result;

        result = SweepInt2D();
        if (result != 0) return 10000 + result;

        result = SweepLong3D();
        if (result != 0) return 20000 + result;

        result = SweepByte3D();
        if (result != 0) return 30000 + result;

        result = SweepTriple2D();
        if (result != 0) return 40000 + result;

        result = SweepString2D();
        if (result != 0) return 50000 + result;

        result = ClearRejectsOutOfRange();
        if (result != 0) return 200 + result;

        result = GetSetValueInt2D();
        if (result != 0) return 300 + result;

        result = GetSetValueLong3D();
        if (result != 0) return 400 + result;

        result = GetSetValueString2D();
        if (result != 0) return 500 + result;

        result = GetValueRejectsOutOfRange();
        if (result != 0) return 600 + result;

        return 0;
    }
}
