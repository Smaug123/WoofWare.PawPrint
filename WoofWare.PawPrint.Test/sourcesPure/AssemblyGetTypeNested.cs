using System;
using System.Collections.Specialized;

public class Outer
{
    public class Inner { }

    // Derives from a type in a framework assembly nothing else loads, so the lookup must load
    // it while priming the nested type's base chain.
    public class NestedFromNameValueCollection : NameValueCollection { }
}

public class Program
{
    public static int Main (string[] args)
    {
        var asm = typeof (Program).Assembly;

        Type inner = asm.GetType ("Outer+Inner");
        if (inner == null) return 1;
        if (!object.ReferenceEquals (inner, typeof (Outer.Inner))) return 2;

        Type nested = asm.GetType ("Outer+NestedFromNameValueCollection");
        if (nested == null) return 3;
        if (nested.BaseType.Name != "NameValueCollection") return 4;

        return 0;
    }
}
