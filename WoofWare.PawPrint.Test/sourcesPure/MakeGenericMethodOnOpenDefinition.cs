using System;
using System.Collections.Generic;
using System.Reflection;

public class Holder<T> where T : class
{
    public void Unconstrained<U>()
    {
    }

    public void RequiresT<U>() where U : T
    {
    }

    public void RequiresComparerOfT<U>() where U : IComparer<T>
    {
    }
}

public sealed class ObjectComparer : IComparer<object>
{
    public int Compare(object x, object y) => 0;
}

public static class Program
{
    // `MakeGenericMethod` on a generic method of an open generic type *definition*. CoreCLR binds it
    // (genmeth.cpp:1256-1270) and validates the method's constraints against the declaring type's
    // *unbound formals*, which is a variance-aware assignability question rather than a syntactic
    // one -- so PawPrint's `RuntimeMethodHandle_GetStubIfNeededSlow` refuses the shape rather than
    // handing back a handle real .NET would have rejected.
    //
    // The measured rows below are why the refusal cannot be replaced by a blanket "a constraint
    // mentioning a type formal admits no closed argument": `IComparer<in T>` is contravariant and
    // `T : class` bounds `T` above by `object`, so `IComparer<object>` is assignable to
    // `IComparer<T>` for every legal `T` and is accepted, while `IComparer<string>` is not.
    //
    // Un-park when constraint validation can run against a declaring type's formals.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        MethodInfo unconstrained = typeof(Holder<>).GetMethod("Unconstrained");

        if (unconstrained == null) return 1;
        if (!unconstrained.IsGenericMethodDefinition) return 2;

        // An unconstrained parameter binds whatever it is given, even though the declaring type is
        // still open. The result is inspectable but not invokable: its declaring type has no
        // instantiation, so it still contains generic parameters.
        MethodInfo bound = unconstrained.MakeGenericMethod(typeof(int));

        if (bound.GetGenericArguments()[0] != typeof(int)) return 3;
        if (bound.DeclaringType != typeof(Holder<>)) return 4;
        if (!bound.ContainsGenericParameters) return 5;
        if (bound.IsGenericMethodDefinition) return 6;

        // `U : T` admits no closed argument at all: nothing is assignable to an unbound formal.
        if (!Throws(typeof(Holder<>).GetMethod("RequiresT"), typeof(int))) return 7;
        if (!Throws(typeof(Holder<>).GetMethod("RequiresT"), typeof(string))) return 8;

        // `U : IComparer<T>` does, by contravariance.
        MethodInfo requiresComparer = typeof(Holder<>).GetMethod("RequiresComparerOfT");

        if (Throws(requiresComparer, typeof(IComparer<object>))) return 9;
        if (Throws(requiresComparer, typeof(ObjectComparer))) return 10;
        if (!Throws(requiresComparer, typeof(IComparer<string>))) return 11;

        // The accepted binding really is bound, rather than having been waved through.
        if (requiresComparer.MakeGenericMethod(typeof(IComparer<object>)).GetGenericArguments()[0] != typeof(IComparer<object>)) return 12;

        return 0;
    }

    static bool Throws(MethodInfo method, Type argument)
    {
        try
        {
            method.MakeGenericMethod(argument);
            return false;
        }
        catch (ArgumentException)
        {
            return true;
        }
    }
}
