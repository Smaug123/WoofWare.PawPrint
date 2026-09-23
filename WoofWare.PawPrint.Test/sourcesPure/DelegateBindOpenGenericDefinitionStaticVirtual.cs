using System;
using System.Reflection;

// `CreateDelegate` over a static virtual method of an open generic *interface definition* —
// `typeof(I<>).GetMethod("M")`. Unlike every other method of an open definition, this one binds:
// `BindToMethod` sends a virtual target on a non-value type to a virtual call stub over the typical
// instantiation (comdelegate.cpp:1237-1244), or virtualises it when closed over an object, and
// neither asks for a code address. The delegate then raises `EntryPointNotFoundException` when
// invoked. Closed over null, it does ask for a code address and raises `InvalidOperationException`
// like the rest of `DelegateBindOpenGenericDefinitionMethod.cs`.
//
// Returns 0 on success, or the number of the first check that failed. Every expectation was
// measured on real .NET.

public interface IOpenStaticVirtual<T>
{
    static abstract string Abstract ();

    static virtual string Virtual ()
    {
        return "virtual";
    }

    static abstract string Closed (string s);
}

public static class Program
{
    static int ExpectEntryPointNotFound (Func<string> invoke)
    {
        try
        {
            invoke ();
            return 1;
        }
        catch (EntryPointNotFoundException)
        {
            return 0;
        }
    }

    public static int Main ()
    {
        Type definition = typeof (IOpenStaticVirtual<>);

        // 1, 2: a static abstract method binds open, and fails only when invoked.
        Func<string> onAbstract = (Func<string>) definition.GetMethod ("Abstract").CreateDelegate (typeof (Func<string>));

        if (onAbstract == null || onAbstract.Target != null)
        {
            return 1;
        }

        if (ExpectEntryPointNotFound (onAbstract) != 0)
        {
            return 2;
        }

        // 3, 4: so does a static virtual method with a body.
        Func<string> onVirtual = (Func<string>) definition.GetMethod ("Virtual").CreateDelegate (typeof (Func<string>));

        if (onVirtual == null)
        {
            return 3;
        }

        if (ExpectEntryPointNotFound (onVirtual) != 0)
        {
            return 4;
        }

        // 5: closed over an object, binding itself raises.
        try
        {
            definition.GetMethod ("Closed").CreateDelegate (typeof (Func<string>), "x");
            return 5;
        }
        catch (EntryPointNotFoundException)
        {
        }

        // 6: closed over null, it asks for a code address.
        try
        {
            definition.GetMethod ("Closed").CreateDelegate (typeof (Func<string>), null);
            return 6;
        }
        catch (InvalidOperationException)
        {
        }

        return 0;
    }
}
