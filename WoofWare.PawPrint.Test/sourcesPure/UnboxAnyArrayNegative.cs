// The failure modes of `unbox.any` with an array-typed token. Because III.4.33 makes
// it `castclass` for reference types, all of these must raise InvalidCastException
// rather than aborting or silently succeeding:
//   - element type mismatch (int[] is not string[]);
//   - a non-array operand for an array token;
//   - rank mismatch (int[] is not int[,] and vice versa).
// A null operand, by contrast, always succeeds and yields null.

using System;

public class TestUnboxAnyArrayNegative
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    private static bool CastThrows<T>(object o)
    {
        try
        {
            T _ = Cast<T>(o);
            return false;
        }
        catch (InvalidCastException)
        {
            return true;
        }
    }

    public static int Main(string[] argv)
    {
        // Null passes straight through for any reference-typed token, including arrays.
        if (Cast<int[]>(null) != null) return 1;
        if (Cast<int[,]>(null) != null) return 2;

        object ints = new int[] { 1, 2, 3 };

        // Value-typed elements are invariant: int[] is not string[] and not long[].
        if (!CastThrows<string[]>(ints)) return 3;
        if (!CastThrows<long[]>(ints)) return 4;

        // Rank is part of array type identity in both directions.
        if (!CastThrows<int[,]>(ints)) return 5;

        object grid = new int[1, 1];
        if (!CastThrows<int[]>(grid)) return 6;

        // A non-array operand cannot satisfy an array-typed token.
        object boxedInt = 7;
        if (!CastThrows<int[]>(boxedInt)) return 7;

        object str = "hello";
        if (!CastThrows<char[]>(str)) return 8;

        // ... and an array operand cannot satisfy a value-typed token.
        if (!CastThrows<int>(ints)) return 9;

        return 0;
    }
}
