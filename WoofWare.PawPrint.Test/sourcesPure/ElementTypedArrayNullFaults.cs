// The element-typed `ldelem.*` and `stelem.*` opcodes, and `ldlen`, raise
// `NullReferenceException` when the array is null (ECMA-335 III.4.8, III.4.12, III.4.26). The
// null check comes before the bounds check, so a null array with an out-of-range index, of
// either int32 or native-int width, raises `NullReferenceException` too.
//
// C# reaches the element-typed opcodes by indexing an array of a primitive or reference type,
// and `ldlen` through `Length`. The token forms `ldelem <T>`/`stelem <T>`/`ldelema <T>` are
// covered by LdelemTokenFaults.

using System;

public class Program
{
    // Keep every operand opaque so nothing can fold the checks away.
    private static T Opaque<T>(T x)
    {
        return x;
    }

    private static int Loads()
    {
        try
        {
            int x = Opaque<int[]>(null)[0];
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            byte x = Opaque<byte[]>(null)[0];
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            long x = Opaque<long[]>(null)[0];
            return 3;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            double x = Opaque<double[]>(null)[0];
            return 4;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            string x = Opaque<string[]>(null)[0];
            return 5;
        }
        catch (NullReferenceException)
        {
        }

        // The null check comes before the bounds check.
        try
        {
            int x = Opaque<int[]>(null)[Opaque(5)];
            return 6;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            int x = Opaque<int[]>(null)[Opaque(0x1_0000_0001L)];
            return 7;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            string x = Opaque<string[]>(null)[Opaque(-1)];
            return 8;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
    }

    private static int Stores()
    {
        try
        {
            Opaque<int[]>(null)[0] = 1;
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Opaque<byte[]>(null)[0] = 1;
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Opaque<long[]>(null)[0] = 1L;
            return 3;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Opaque<double[]>(null)[0] = 1.0;
            return 4;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Opaque<string[]>(null)[0] = "z";
            return 5;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Opaque<int[]>(null)[Opaque(5)] = 1;
            return 6;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Opaque<int[]>(null)[Opaque(0x1_0000_0001L)] = 1;
            return 7;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
    }

    private static int Length()
    {
        try
        {
            int x = Opaque<int[]>(null).Length;
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            int x = Opaque<string[]>(null).Length;
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        if (Opaque(new int[3]).Length != 3)
        {
            return 3;
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = Loads();
        if (result != 0)
        {
            return 10 + result;
        }

        result = Stores();
        if (result != 0)
        {
            return 20 + result;
        }

        result = Length();
        if (result != 0)
        {
            return 30 + result;
        }

        return 0;
    }
}
