// `System.Array.GetLength` / `GetLowerBound` / `GetUpperBound` — the `[Intrinsic]` bound
// accessors. Upstream reads a raw inline bounds block; PawPrint answers from
// `AllocatedArray.Lengths` directly instead of walking its rendering of that block.
//
// `GetLongLength` is exercised too: it is not itself intrinsic, it just widens `GetLength`,
// so it should start working for free.

using System;

class Program
{
    static int TestSzArray()
    {
        int[] arr = new int[4];

        if (arr.GetLength(0) != 4) return 1;
        if (arr.GetLowerBound(0) != 0) return 2;
        if (arr.GetUpperBound(0) != 3) return 3;
        if (arr.GetLongLength(0) != 4L) return 4;
        if (arr.Rank != 1) return 5;

        // A zero-length array has an upper bound one below its lower bound.
        int[] empty = new int[0];
        if (empty.GetLength(0) != 0) return 6;
        if (empty.GetLowerBound(0) != 0) return 7;
        if (empty.GetUpperBound(0) != -1) return 8;

        // Reference-element and struct-element arrays take the same path.
        string[] strs = new string[2];
        if (strs.GetLength(0) != 2) return 9;
        if (strs.GetUpperBound(0) != 1) return 10;

        return 0;
    }

    static int TestRank2()
    {
        int[,] md = new int[2, 3];

        if (md.GetLength(0) != 2) return 20;
        if (md.GetLength(1) != 3) return 21;
        if (md.GetLowerBound(0) != 0) return 22;
        if (md.GetLowerBound(1) != 0) return 23;
        if (md.GetUpperBound(0) != 1) return 24;
        if (md.GetUpperBound(1) != 2) return 25;
        if (md.GetLongLength(1) != 3L) return 26;
        if (md.Rank != 2) return 27;
        if (md.Length != 6) return 28;

        return 0;
    }

    static int TestRank3WithZeroDimension()
    {
        // A zero-length dimension makes the whole array empty but must not disturb the
        // per-dimension answers.
        int[,,] md = new int[2, 0, 5];

        if (md.GetLength(0) != 2) return 40;
        if (md.GetLength(1) != 0) return 41;
        if (md.GetLength(2) != 5) return 42;
        if (md.GetUpperBound(0) != 1) return 43;
        if (md.GetUpperBound(1) != -1) return 44;
        if (md.GetUpperBound(2) != 4) return 45;
        if (md.GetLowerBound(2) != 0) return 46;
        if (md.Rank != 3) return 47;
        if (md.Length != 0) return 48;

        return 0;
    }

    // Dimensions outside [0, rank) throw IndexOutOfRangeException. Upstream's check is an
    // unsigned compare, so negative dimensions — including int.MinValue — throw rather than
    // wrapping into range.
    static int TestOutOfRangeDimension()
    {
        int[] sz = new int[3];
        int[,] md = new int[2, 2];

        if (!ThrowsIndexOutOfRange(() => sz.GetLength(1))) return 60;
        if (!ThrowsIndexOutOfRange(() => sz.GetLength(-1))) return 61;
        if (!ThrowsIndexOutOfRange(() => sz.GetLength(int.MinValue))) return 62;
        if (!ThrowsIndexOutOfRange(() => sz.GetLowerBound(1))) return 63;
        if (!ThrowsIndexOutOfRange(() => sz.GetUpperBound(1))) return 64;

        if (!ThrowsIndexOutOfRange(() => md.GetLength(2))) return 65;
        if (!ThrowsIndexOutOfRange(() => md.GetLength(-1))) return 66;
        if (!ThrowsIndexOutOfRange(() => md.GetLowerBound(2))) return 67;
        if (!ThrowsIndexOutOfRange(() => md.GetUpperBound(int.MinValue))) return 68;

        // The CLR passes SR.IndexOutOfRange_ArrayRankIndex to the constructor; PawPrint
        // synthesises the exception without running a ctor, so it must populate _message
        // itself for a guest that inspects it.
        try
        {
            md.GetLength(7);
            return 71;
        }
        catch (IndexOutOfRangeException e)
        {
            if (e.Message != "Array does not have that many dimensions.") return 72;
        }

        // In-range dimensions on the same arrays must still succeed afterwards.
        if (sz.GetLength(0) != 3) return 69;
        if (md.GetLength(1) != 2) return 70;

        return 0;
    }

    static bool ThrowsIndexOutOfRange(Func<int> f)
    {
        try
        {
            f();
            return false;
        }
        catch (IndexOutOfRangeException)
        {
            return true;
        }
    }

    // Roslyn lowers `foreach` over a multi-dimensional array into GetLowerBound/GetUpperBound
    // calls per dimension (it does NOT use GetLength), so this exercises the accessors the way
    // ordinary C# actually reaches them.
    static int TestForeachOverRank2()
    {
        int[,] md = new int[2, 3];
        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                md[i, j] = (i * 3) + j;
            }
        }

        int total = 0;
        int count = 0;
        foreach (int x in md)
        {
            total += x;
            count++;
        }

        if (count != 6) return 80;
        if (total != 15) return 81;

        // Empty multi-dimensional array: the loop body must not run.
        int[,] emptyMd = new int[0, 4];
        foreach (int x in emptyMd)
        {
            return 82;
        }

        return 0;
    }

    static int Main(string[] args)
    {
        int result;

        result = TestSzArray();
        if (result != 0) return 1000 + result;

        result = TestRank2();
        if (result != 0) return 2000 + result;

        result = TestRank3WithZeroDimension();
        if (result != 0) return 3000 + result;

        result = TestOutOfRangeDimension();
        if (result != 0) return 4000 + result;

        result = TestForeachOverRank2();
        if (result != 0) return 5000 + result;

        return 0;
    }
}
