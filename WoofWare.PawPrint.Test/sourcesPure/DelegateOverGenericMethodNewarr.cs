using System;
using System.Reflection;

// A delegate over a generic method instantiation whose body allocates `new T[n]`. The `newarr !!0`
// inside the target resolves `!!0` through the *frame's* method generics, so the frame that
// delegate invocation builds for the target must carry the target's own instantiation, not the
// (always empty) generics of `Func<...>.Invoke`, and not those of whichever method invoked the
// delegate.
//
// Returns 0 on success, or the number of the first check that failed. Every code is below 128, so
// that none can be mistaken for the 128+signo a signalled guest reports.

public class Pair<T>
{
    public static T[] Left<U> (int n)
    {
        return new T[n];
    }

    public static U[] Right<U> (int n)
    {
        return new U[n];
    }
}

public class DelegateOverGenericMethodNewarr
{
    public static T[] Make<T> (int n)
    {
        return new T[n];
    }

    private int _seed;

    private T[] Fill<T> (T value, int n)
    {
        T[] result = new T[n + _seed];
        for (int i = 0; i < result.Length; i++)
        {
            result[i] = value;
        }
        return result;
    }

    // Invokes the delegate from inside a method whose own instantiation (`V = string`) differs
    // from the target's (`T = int`), so a frame built from the *caller's* generics would allocate
    // a `string[]` and be caught below.
    private static int InvokeFromGenericCaller<V> (Func<int, int[]> f)
    {
        int[] arr = f (2);
        if (arr == null) return 30;
        if (arr.Length != 2) return 31;
        if (arr.GetType () != typeof (int[])) return 32;
        if (arr.GetType () == typeof (V[])) return 33;
        return 0;
    }

    public static int Main (string[] argv)
    {
        // The issue's own shape: a static generic method at a primitive instantiation.
        Func<int, int[]> makeInt = Make<int>;
        int[] ints = makeInt (3);
        if (ints == null) return 1;
        if (ints.Length != 3) return 2;
        if (ints.GetType () != typeof (int[])) return 3;

        // A nominal instantiation, which cannot be confused with a primitive's default.
        Func<int, string[]> makeString = Make<string>;
        string[] strings = makeString (4);
        if (strings == null) return 10;
        if (strings.Length != 4) return 11;
        if (strings.GetType () != typeof (string[])) return 12;
        object asObject = strings;
        if (!(asObject is string[])) return 13;
        if (asObject is int[]) return 14;

        // A closed delegate over an instance generic method, so the receiver's field and the
        // method's instantiation are both live in the target frame.
        DelegateOverGenericMethodNewarr self = new DelegateOverGenericMethodNewarr ();
        self._seed = 1;
        Func<long, int, long[]> fill = self.Fill<long>;
        long[] longs = fill (7L, 2);
        if (longs == null) return 20;
        if (longs.Length != 3) return 21;
        if (longs[0] != 7L || longs[1] != 7L || longs[2] != 7L) return 22;
        if (longs.GetType () != typeof (long[])) return 23;

        int fromCaller = InvokeFromGenericCaller<string> (makeInt);
        if (fromCaller != 0) return fromCaller;

        // A generic method on a generic type: the declaring type's generics travel with the bound
        // method, and the method's own must travel too. `Left` uses only the type's `T`, `Right`
        // only the method's `U`, so each frame half is checked on its own.
        Func<int, char[]> left = Pair<char>.Left<string>;
        char[] chars = left (5);
        if (chars == null) return 40;
        if (chars.Length != 5) return 41;
        if (chars.GetType () != typeof (char[])) return 42;

        Func<int, string[]> right = Pair<char>.Right<string>;
        string[] rights = right (6);
        if (rights == null) return 43;
        if (rights.Length != 6) return 44;
        if (rights.GetType () != typeof (string[])) return 45;

        // The same target bound through reflection rather than `ldftn`, so the method pointer
        // comes from `Delegate.CreateDelegate` over a `MakeGenericMethod` result.
        MethodInfo open = typeof (DelegateOverGenericMethodNewarr).GetMethod ("Make");
        if (open == null) return 50;
        MethodInfo closed = open.MakeGenericMethod (typeof (short));
        Func<int, short[]> viaReflection =
            (Func<int, short[]>) Delegate.CreateDelegate (typeof (Func<int, short[]>), closed);
        short[] shorts = viaReflection (2);
        if (shorts == null) return 51;
        if (shorts.Length != 2) return 52;
        if (shorts.GetType () != typeof (short[])) return 53;

        return 0;
    }
}
