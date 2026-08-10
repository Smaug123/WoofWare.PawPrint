using System;

// When a type initializer throws, the `TypeInitializationException`'s stack trace names the
// method whose call triggered the initialization. Real .NET reports `Bad.M` and then
// `Program.Main`, because the initializer check happens inside `Bad.M`'s own prologue, so
// that frame is already established when the `.cctor` runs.
//
// PawPrint runs the initializer *before* establishing the target's frame — every call path
// does `loadClass` and only then pushes the callee — so the target method never appears.
// The direct call below involves no delegate at all and diverges identically, which is what
// makes this a general property of class initialization rather than anything about
// delegates: both routes report only `Program.Main`.
//
// The delegate route is included precisely so the two are checked to agree. An earlier state
// of the delegate route was worse than the direct one, reporting a `System.Action.Invoke`
// stub frame that no real trace contains; `DelegateCctorFailureTraceHasNoStubFrame.cs` is
// the active test that keeps that from coming back.

class BadDirect
{
    static BadDirect()
    {
        throw new InvalidOperationException ("boom");
    }

    public static void M() { }
}

class BadDelegate
{
    static BadDelegate()
    {
        throw new InvalidOperationException ("boom");
    }

    public static void M() { }
}

class Program
{
    static int Main(string[] args)
    {
        try
        {
            BadDirect.M ();
            return 1;
        }
        catch (TypeInitializationException e)
        {
            string st = e.StackTrace;

            if (st == null)
            {
                return 2;
            }

            if (!st.Contains ("BadDirect"))
            {
                return 3;
            }
        }

        Action f = BadDelegate.M;

        try
        {
            f ();
            return 4;
        }
        catch (TypeInitializationException e)
        {
            string st = e.StackTrace;

            if (st == null)
            {
                return 5;
            }

            if (!st.Contains ("BadDelegate"))
            {
                return 6;
            }
        }

        return 0;
    }
}
