using System;
using System.Reflection;

class Container<T>
{
    public int Instance () => 1;
    public static int Stat () => 2;
}

struct SContainer<T>
{
    public int Instance () => 3;
}

interface IContainer<T>
{
    int Iface ();
}

class Program
{
    static int Main (string[] args)
    {
        // Every one of these lookups routes through RuntimeMethodHandle.GetStubIfNeeded, whose
        // fast-path FCall declines for a declaring type that is a bound generic or a value type,
        // handing off to the RuntimeMethodHandle_GetStubIfNeededSlow QCall.

        // Instance method on a closed generic *class*: CoreCLR's
        // FindOrCreateAssociatedMethodDescForReflection wants no stub here (not a value type, not
        // an interface, method not static), so the QCall returns the MethodDesc unchanged.
        if (typeof (Container<int>).GetMethod ("Instance") == null)
            return 1;

        // Static method on a closed generic class: needs an instantiating stub.
        if (typeof (Container<int>).GetMethod ("Stat") == null)
            return 2;

        // Method on a generic *struct*: value types always need one.
        if (typeof (SContainer<int>).GetMethod ("Instance") == null)
            return 3;

        // Method on a closed generic *interface*: needs one.
        if (typeof (IContainer<int>).GetMethod ("Iface") == null)
            return 4;

        // The rebound handle must still describe the same method.
        MethodInfo stat = typeof (Container<int>).GetMethod ("Stat");

        if (stat.Name != "Stat")
            return 5;

        if (!stat.IsStatic)
            return 6;

        // A non-generic method's instantiation stays empty through the rebinding.
        if (stat.GetGenericArguments ().Length != 0)
            return 7;

        // A method on a *non*-generic reference type never reaches the slow path at all (the
        // fast-path FCall short-circuits); included so the two paths are exercised side by side.
        if (typeof (Program).GetMethod ("Main", BindingFlags.Static | BindingFlags.NonPublic) == null)
            return 8;

        return 0;
    }
}
