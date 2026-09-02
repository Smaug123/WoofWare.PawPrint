// An array index on the evaluation stack may be a native int as well as an int32, and
// ECMA-335 III.4.8 (`ldelem.*`) and III.4.26 (`stelem.*`) compare it against the array's
// length at its full width. An index whose low 32 bits name a valid element but which is
// itself out of range must therefore raise `IndexOutOfRangeException`, rather than reading
// or writing the element its truncation names.
//
// C# indexes an array with a `long` by `conv.ovf.i` followed by the element-typed
// `ldelem.*`/`stelem.*`, and with a `ulong` by `conv.ovf.i.un`, so those are the two spellings
// that put a native int in front of the opcode. The int32 spelling is covered alongside them,
// since a native-int check that lost the int32 one would be no improvement.

using System;

public class Program
{
    // Keep every index opaque so nothing can fold the bounds check away.
    private static long Opaque(long i)
    {
        return i;
    }

    private static ulong Opaque(ulong i)
    {
        return i;
    }

    private static int Opaque(int i)
    {
        return i;
    }

    private static int NativeIntIndexAboveInt32Range()
    {
        int[] a = { 10, 20, 30 };

        // Low 32 bits are 1, so a 32-bit truncation would read element 1.
        try
        {
            int x = a[Opaque(0x1_0000_0001L)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        // Low 32 bits are 0, so a 32-bit truncation would read element 0.
        try
        {
            int x = a[Opaque(0x1_0000_0000L)];
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        // Low 32 bits are 1, so a 32-bit truncation would overwrite element 1.
        try
        {
            a[Opaque(0x1_0000_0001L)] = 99;
            return 3;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (a[0] != 10 || a[1] != 20 || a[2] != 30)
        {
            return 4;
        }

        // A native int that is in range reads and writes the element it names.
        if (a[Opaque(2L)] != 30)
        {
            return 5;
        }

        a[Opaque(1L)] = 21;
        if (a[1] != 21)
        {
            return 6;
        }

        return 0;
    }

    private static int NegativeNativeIntIndex()
    {
        int[] a = { 10, 20, 30 };

        try
        {
            int x = a[Opaque(-1L)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            a[Opaque(-1L)] = 99;
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        // Low 32 bits are 2, so a 32-bit truncation would read element 2.
        try
        {
            int x = a[Opaque(unchecked((long)0xFFFF_FFFF_0000_0002UL))];
            return 3;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (a[0] != 10 || a[1] != 20 || a[2] != 30)
        {
            return 4;
        }

        return 0;
    }

    private static int UnsignedNativeIntIndex()
    {
        int[] a = { 10, 20, 30 };

        try
        {
            int x = a[Opaque(0x1_0000_0001UL)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            a[Opaque(0x1_0000_0002UL)] = 99;
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (a[0] != 10 || a[1] != 20 || a[2] != 30)
        {
            return 3;
        }

        if (a[Opaque(1UL)] != 20)
        {
            return 4;
        }

        return 0;
    }

    // `ldelem.ref`/`stelem.ref` rather than a numeric element width.
    private static int ReferenceElements()
    {
        string[] s = { "a", "b", "c" };

        try
        {
            string x = s[Opaque(0x1_0000_0001L)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            s[Opaque(0x1_0000_0001L)] = "z";
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (s[0] != "a" || s[1] != "b" || s[2] != "c")
        {
            return 3;
        }

        if (s[Opaque(2L)] != "c")
        {
            return 4;
        }

        return 0;
    }

    // The other element widths share one path with `ldelem.i4`/`stelem.i4`, so one of the
    // narrow ones and one of the wide ones stand in for the rest.
    private static int OtherElementWidths()
    {
        byte[] b = { 1, 2, 3 };
        long[] l = { 100L, 200L, 300L };

        try
        {
            byte x = b[Opaque(0x1_0000_0001L)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            b[Opaque(0x1_0000_0001L)] = 9;
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            long x = l[Opaque(0x1_0000_0001L)];
            return 3;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            l[Opaque(0x1_0000_0001L)] = 9L;
            return 4;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (b[1] != 2 || l[1] != 200L)
        {
            return 5;
        }

        return 0;
    }

    // The int32 spelling: the index is already 32 bits wide, so there is nothing to truncate,
    // and what is being checked is that an out-of-range int32 index raises the guest-visible
    // exception too.
    private static int Int32Index()
    {
        int[] a = { 10, 20, 30 };

        try
        {
            int x = a[Opaque(3)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            int x = a[Opaque(-1)];
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            a[Opaque(3)] = 99;
            return 3;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            a[Opaque(-1)] = 99;
            return 4;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (a[0] != 10 || a[1] != 20 || a[2] != 30)
        {
            return 5;
        }

        // An empty array rejects index 0.
        int[] empty = new int[0];

        try
        {
            int x = empty[Opaque(0)];
            return 6;
        }
        catch (IndexOutOfRangeException)
        {
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = NativeIntIndexAboveInt32Range();
        if (result != 0)
        {
            return 10 + result;
        }

        result = NegativeNativeIntIndex();
        if (result != 0)
        {
            return 20 + result;
        }

        result = UnsignedNativeIntIndex();
        if (result != 0)
        {
            return 30 + result;
        }

        result = ReferenceElements();
        if (result != 0)
        {
            return 40 + result;
        }

        result = OtherElementWidths();
        if (result != 0)
        {
            return 50 + result;
        }

        result = Int32Index();
        if (result != 0)
        {
            return 60 + result;
        }

        return 0;
    }
}
