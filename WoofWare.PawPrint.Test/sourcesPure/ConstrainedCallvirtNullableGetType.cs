using System;

public class Program
{
    struct Pair
    {
        public int A;
        public int B;
    }

    enum Colour : byte
    {
        Red = 1,
        Green = 2,
    }

    // Inside a generic method Roslyn emits `ldarga.s t; constrained. !!T; callvirt Object::GetType()`.
    // `Object.GetType` is not virtual, so no value type overrides it: for a value-type T the runtime
    // takes ECMA III.2.1 case 3, boxing `t` before the call. Boxing follows ECMA III.4.1, so a
    // `Nullable<T>` boxes as a `T` (or as null), never as a `Nullable<T>`.
    static Type GT<T>(T t)
    {
        return t.GetType();
    }

    static bool ThrowsNre<T>(T t)
    {
        try
        {
            GT(t);
            return false;
        }
        catch (NullReferenceException)
        {
            return true;
        }
    }

    // These three *are* overridden by Nullable<T>, so the same prefix takes case 2 (the method is
    // called on the Nullable<T> itself, unboxed). Controls that the fix to case 3 leaves case 2 alone.
    static int HashOf<T>(T t)
    {
        return t.GetHashCode();
    }

    static string StringOf<T>(T t)
    {
        return t.ToString();
    }

    static bool EqualsNull<T>(T t)
    {
        return t.Equals(null);
    }

    static bool Same(Type actual, Type expected)
    {
        return object.ReferenceEquals(actual, expected);
    }

    public static int Main(string[] args)
    {
        // Case 3 on a plain value type boxes it as itself.
        if (!Same(GT(new Pair { A = 1, B = 2 }), typeof(Pair))) return 1;
        if (!Same(GT(5), typeof(int))) return 2;
        if (!Same(GT(Colour.Green), typeof(Colour))) return 3;
        // Case 1 on a reference type.
        if (!Same(GT("s"), typeof(string))) return 4;
        if (!ThrowsNre<string>(null)) return 5;

        // A Nullable<T> that has a value boxes as T, so GetType answers T.
        if (!Same(GT<int?>(5), typeof(int))) return 6;
        if (!Same(GT<Pair?>(new Pair { A = 3, B = 4 }), typeof(Pair))) return 7;
        if (!Same(GT<Colour?>(Colour.Red), typeof(Colour))) return 8;

        // A Nullable<T> without a value boxes to null, and callvirt's null check throws.
        if (!ThrowsNre<int?>(null)) return 9;
        if (!ThrowsNre<Pair?>(null)) return 10;
        if (!ThrowsNre<Colour?>(null)) return 11;

        // Case 2 controls: Nullable<T>'s own overrides run on the unboxed Nullable<T>, so a
        // value-less one does not throw.
        int? none = null;
        if (HashOf(none) != 0) return 12;
        if (StringOf(none) != "") return 13;
        if (!EqualsNull(none)) return 14;

        int? some = 7;
        if (HashOf(some) != 7.GetHashCode()) return 15;
        if (StringOf(some) != "7") return 16;
        if (EqualsNull(some)) return 17;

        return 0;
    }
}
