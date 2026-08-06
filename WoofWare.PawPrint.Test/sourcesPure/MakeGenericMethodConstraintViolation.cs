using System;
using System.Reflection;

class NoParameterlessCtor
{
    private NoParameterlessCtor ()
    {
    }
}

class Program
{
    static void MStruct<T> () where T : struct
    {
    }

    static void MClass<T> () where T : class
    {
    }

    static void MNew<T> () where T : new()
    {
    }

    static MethodInfo Get (string name) =>
        typeof (Program).GetMethod (name, BindingFlags.Static | BindingFlags.NonPublic);

    static bool Throws (MethodInfo m, Type argument)
    {
        try
        {
            m.MakeGenericMethod (argument);
            return false;
        }
        catch (ArgumentException)
        {
            return true;
        }
    }

    static int Main (string[] args)
    {
        // CoreCLR checks a generic method's constraints while *binding* the instantiation, inside
        // the RuntimeMethodHandle_GetStubIfNeededSlow QCall's callee. The managed
        // SanityCheckGenericArguments that runs before it only screens nulls, non-RuntimeType
        // arguments, and arity, so a constraint violation is not caught until then. Reflection
        // surfaces all three as ArgumentException.

        if (!Throws (Get ("MStruct"), typeof (string)))
            return 1;

        if (!Throws (Get ("MClass"), typeof (int)))
            return 2;

        if (!Throws (Get ("MNew"), typeof (NoParameterlessCtor)))
            return 3;

        // Nullable<T> does not satisfy `where T : struct`, which is the non-nullable-value
        // constraint rather than plain "is a value type".
        if (!Throws (Get ("MStruct"), typeof (int?)))
            return 4;

        // The *satisfying* cases are deliberately not asserted here. Binding one succeeds, and
        // reflection then continues into RuntimeType.GetMethodBase, which reaches the unimplemented
        // RuntimeMethodHandle.IsDynamicMethod InternalCall -- an unrelated gap that would make this
        // case fail for the wrong reason. `sourcesPure/MethodOnClosedGenericType.cs` covers a
        // successful rebinding through the same QCall by a route that does not reach that call, and
        // `TestNativeRuntimeMethodHandle.fs` pins the outcome table directly.

        return 0;
    }
}
