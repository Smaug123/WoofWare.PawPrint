using System;

// A delegate's `Invoke` is a stub, not a managed method, so no real stack trace contains a
// frame for it: an exception crossing a delegate call reports the target and then whoever
// called `Invoke`.
//
// PawPrint gets that for free whenever the target itself throws, because
// `dispatchDelegateInvoke` pops its synthetic frame before calling the target — by the time
// anything can throw, the frame is gone. Class initialization is the one exception: it runs
// while that frame is deliberately still active, so that the invocation can be retried once
// the `.cctor` returns. Both routes are checked below, since it is the *contrast* that
// makes the point — the ordinary route is clean by construction, and only the initializer
// route could regress.
//
// What this file does not check is that the trace names the target method. It does not, on
// either route, including a direct call that involves no delegate at all; see the parked
// `CctorFailureTraceNamesTargetMethod.cs`.

class ThrowingCctor
{
    static ThrowingCctor()
    {
        throw new InvalidOperationException ("boom");
    }

    public static void M() { }
}

class ThrowingBody
{
    public static void M()
    {
        throw new InvalidOperationException ("boom");
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Route 1: the target's type initializer throws, so the failure happens while the
        // synthetic `Invoke` frame is still on the stack.
        Action viaCctor = ThrowingCctor.M;

        try
        {
            viaCctor ();
            return 1;
        }
        catch (TypeInitializationException e)
        {
            string st = e.StackTrace;

            if (st == null)
            {
                return 2;
            }

            if (st.Contains ("Invoke"))
            {
                return 3;
            }

            // The caller of `Invoke` is still reported.
            if (!st.Contains ("Main"))
            {
                return 4;
            }
        }

        // Route 2: an ordinary throw from the target body, where the synthetic frame has
        // already been popped. This is the control.
        Action viaBody = ThrowingBody.M;

        try
        {
            viaBody ();
            return 5;
        }
        catch (InvalidOperationException e)
        {
            string st = e.StackTrace;

            if (st == null)
            {
                return 6;
            }

            if (st.Contains ("Invoke"))
            {
                return 7;
            }

            if (!st.Contains ("Main"))
            {
                return 8;
            }
        }

        return 0;
    }
}
