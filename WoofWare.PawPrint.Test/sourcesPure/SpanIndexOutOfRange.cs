// `Span<T>`/`ReadOnlySpan<T>` indexers bounds-check with `(uint)index >= (uint)_length` and
// call `ThrowHelper.ThrowIndexOutOfRangeException()`, which throws the parameterless
// `IndexOutOfRangeException` — so the message is the type's default and PawPrint reproduces
// it exactly.
//
// Note the unsigned comparison: a negative index is caught by the same branch, because it
// reinterprets as a huge unsigned value. Both directions are covered below.

using System;

public class Program
{
    // Keep indices opaque so nothing can fold the bounds check away.
    private static int Opaque(int i)
    {
        return i;
    }

    private static int TestSpan()
    {
        Span<int> s = new int[3];

        try
        {
            int x = s[Opaque(7)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            int x = s[Opaque(-1)];
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        // The boundary index is one past the end.
        try
        {
            int x = s[Opaque(3)];
            return 3;
        }
        catch (IndexOutOfRangeException)
        {
        }

        // In-range indices must still work.
        s[Opaque(2)] = 42;
        if (s[Opaque(2)] != 42)
        {
            return 4;
        }

        return 0;
    }

    private static int TestReadOnlySpan()
    {
        ReadOnlySpan<int> r = new int[2];

        try
        {
            int y = r[Opaque(2)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            int y = r[Opaque(-5)];
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (r[Opaque(1)] != 0)
        {
            return 3;
        }

        return 0;
    }

    private static int TestEmptySpan()
    {
        // A zero-length span rejects every index, including 0.
        Span<int> empty = new int[0];

        try
        {
            int z = empty[Opaque(0)];
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = TestSpan();
        if (result != 0)
        {
            return 10 + result;
        }

        result = TestReadOnlySpan();
        if (result != 0)
        {
            return 20 + result;
        }

        result = TestEmptySpan();
        if (result != 0)
        {
            return 30 + result;
        }

        return 0;
    }
}
