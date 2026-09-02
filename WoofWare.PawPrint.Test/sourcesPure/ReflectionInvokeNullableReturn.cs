using System;
using System.Reflection;

public class Program
{
    public struct Pair
    {
        public int A;
        public int B;
    }

    public static int? NoInt()
    {
        return null;
    }

    public static int? SomeInt()
    {
        return 5;
    }

    public static Pair? NoPair()
    {
        return null;
    }

    public static Pair? SomePair()
    {
        return new Pair { A = 3, B = 4 };
    }

    public static int PlainInt()
    {
        return 9;
    }

    static object Call(string name)
    {
        return typeof(Program).GetMethod(name, BindingFlags.Public | BindingFlags.Static).Invoke(null, null);
    }

    public static int Main(string[] args)
    {
        // Control: a plain value-type return comes back boxed as itself.
        object plain = Call("PlainInt");
        if (plain == null) return 1;
        if (!object.ReferenceEquals(plain.GetType(), typeof(int))) return 2;
        if ((int)plain != 9) return 3;

        // A Nullable<T> return is boxed by the runtime, so it follows ECMA III.4.1: null when it
        // has no value, and a boxed T (never a boxed Nullable<T>) when it does.
        if (Call("NoInt") != null) return 4;
        object someInt = Call("SomeInt");
        if (someInt == null) return 5;
        if (!object.ReferenceEquals(someInt.GetType(), typeof(int))) return 6;
        if ((int)someInt != 5) return 7;

        if (Call("NoPair") != null) return 8;
        object somePair = Call("SomePair");
        if (somePair == null) return 9;
        if (!object.ReferenceEquals(somePair.GetType(), typeof(Pair))) return 10;
        Pair p = (Pair)somePair;
        if (p.A != 3 || p.B != 4) return 11;

        return 0;
    }
}
