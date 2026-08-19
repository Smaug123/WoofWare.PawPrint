using System;
using System.Reflection;

// `RuntimeConstructorInfo.Invoke(obj, ...)` — running a constructor against an instance the caller
// already has — with more than four arguments. This is the only route to the five-argument overload
// of `MethodBaseInvoker.InvokeConstructorWithoutAlloc` (MethodBaseInvoker.Constructor.cs:15), which
// is one of the four CoreLib frames `NativeGcFrameRegistration` permits to register a GC frame and
// the only one with no other caller.
//
// It reaches the QCall with `isConstructor: obj is null` — i.e. false — so it allocates nothing and
// answers null, exactly as the no-argument shape in `ReflectionInvokeConstructor.cs` does.
public class Program
{
    private static int runs;

    public class SixArgs
    {
        public int Sum;
        public string Joined;

        public SixArgs (int a, string b, int c, string d, int e, string f)
        {
            runs = runs + 1;
            Sum = a + c + e;
            Joined = b + d + f;
        }
    }

    public static int Main (string[] args)
    {
        ConstructorInfo[] ctors = typeof (SixArgs).GetConstructors ();

        if (ctors.Length != 1)
            return 1;

        SixArgs existing = new SixArgs (0, "", 0, "", 0, "");

        if (runs != 1)
            return 2;

        object[] parameters = new object[] { 4, "x", 5, "y", 6, "z" };
        object result = ctors[0].Invoke (existing, parameters);

        if (result != null)
            return 3;

        if (runs != 2)
            return 4;

        if (existing.Sum != 15)
            return 5;

        if (existing.Joined != "xyz")
            return 6;

        return 0;
    }
}
