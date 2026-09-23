using System;
using System.Reflection;

// A reflective field read whose declaring type's initialiser throws. CoreCLR's
// InvokeUtil::GetFieldValue runs the initialiser inside an EX_TRY and rethrows the failure wrapped
// in a fresh TargetInvocationException (CreateTargetExcept, invokeutil.cpp), exactly as
// SetValidField does for a write. The second read finds the type already failed; the runtime
// rethrows the TypeInitializationException it cached the first time, and that is wrapped too --
// in a new TargetInvocationException around the same inner instance. The inner exception never
// reaches a managed frame, so it reports no stack trace.

static class Boom
{
    public static int Value;

    static Boom()
    {
        throw new InvalidOperationException("boom");
    }
}

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    static TargetInvocationException Read(FieldInfo field)
    {
        try
        {
            field.GetValue(null);
        }
        catch (TargetInvocationException e)
        {
            return e;
        }

        return null;
    }

    static int Main()
    {
        FieldInfo field = typeof(Boom).GetField("Value");

        TargetInvocationException first = Read(field);
        Check(first != null);
        Check(first != null && first.InnerException is TypeInitializationException);
        Check(first != null && first.InnerException.InnerException is InvalidOperationException);
        // The TypeInitializationException is caught in native code where it is raised, so no
        // frame is ever appended to it; the wrapper is thrown through the managed callers.
        Check(first != null && first.InnerException.StackTrace == null);
        Check(first != null && first.StackTrace != null);

        TargetInvocationException second = Read(field);
        Check(second != null);
        Check(second != null && first != null && !ReferenceEquals(first, second));
        Check(second != null && first != null && ReferenceEquals(first.InnerException, second.InnerException));
        Check(second != null && second.InnerException.StackTrace == null);

        return firstFailure;
    }
}
