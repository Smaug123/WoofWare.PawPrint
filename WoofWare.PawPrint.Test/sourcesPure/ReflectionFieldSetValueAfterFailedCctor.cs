using System;
using System.Reflection;

// A reflective field write to a type whose initialiser has *already* failed, by an ordinary static
// field access. That access sees the TypeInitializationException bare; the reflective write finds
// the type failed on entry, and CoreCLR's InvokeUtil::SetValidField rethrows the cached
// TypeInitializationException wrapped in a fresh TargetInvocationException (CreateTargetExcept,
// invokeutil.cpp). Every such write wraps the same cached instance in a new wrapper.

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

    static TargetInvocationException Write(FieldInfo field)
    {
        try
        {
            field.SetValue(null, 1);
        }
        catch (TargetInvocationException e)
        {
            return e;
        }

        return null;
    }

    static int Main()
    {
        TypeInitializationException direct = null;

        try
        {
            Boom.Value = 1;
        }
        catch (TypeInitializationException e)
        {
            direct = e;
        }

        Check(direct != null);
        Check(direct != null && direct.StackTrace != null);

        FieldInfo field = typeof(Boom).GetField("Value");

        TargetInvocationException first = Write(field);
        Check(first != null);
        Check(first != null && ReferenceEquals(first.InnerException, direct));
        // The runtime clears the cached exception's trace before rethrowing it, and the native
        // catch intercepts it before any frame is appended, so the instance the direct access
        // caught now reports no trace at all.
        Check(direct != null && direct.StackTrace == null);

        TargetInvocationException second = Write(field);
        Check(second != null);
        Check(second != null && first != null && !ReferenceEquals(first, second));
        Check(second != null && ReferenceEquals(second.InnerException, direct));

        return firstFailure;
    }
}
