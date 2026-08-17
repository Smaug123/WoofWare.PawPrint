using System;
using System.Reflection;

// A reflective field set whose declaring type's initialiser throws. CoreCLR's SetValidField runs
// the initialiser inside an EX_TRY and rethrows the failure wrapped in a fresh
// TargetInvocationException (CreateTargetExcept, invokeutil.cpp) — unlike
// ReflectionInvocation_RunClassConstructor, which lets a TypeInitializationException through
// unwrapped. So the exception a guest sees here is TargetInvocationException, whose InnerException
// is the TypeInitializationException.

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

    static int Main()
    {
        FieldInfo field = typeof(Boom).GetField("Value");

        bool wrapped = false;
        bool innerWasTypeInit = false;

        try
        {
            field.SetValue(null, 1);
        }
        catch (TargetInvocationException e)
        {
            wrapped = true;
            innerWasTypeInit = e.InnerException is TypeInitializationException;
        }

        Check(wrapped);
        Check(innerWasTypeInit);

        return firstFailure;
    }
}
