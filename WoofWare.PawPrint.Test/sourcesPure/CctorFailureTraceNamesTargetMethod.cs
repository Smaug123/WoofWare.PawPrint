using System;

// When a type initializer throws, the `TypeInitializationException`'s stack trace names the
// method whose call triggered the initialization. Real .NET reports `Bad.M` and then
// `Program.Main`, because the initializer check happens inside `Bad.M`'s own prologue, so
// that frame is already established when the `.cctor` runs.
//
// PawPrint likewise arms the check on the callee's frame and runs it as that frame's
// prologue, which is what makes the name available to report.
//
// The direct call below involves no delegate at all, which is what makes this a general
// property of class initialization rather than anything about delegates; the delegate route is
// included so the two are checked to agree. A delegate route that ran the initializer before
// popping the `System.Action.Invoke` stub frame would report a frame that no real trace
// contains; `DelegateCctorFailureTraceHasNoStubFrame.cs` is the active test for that.

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
