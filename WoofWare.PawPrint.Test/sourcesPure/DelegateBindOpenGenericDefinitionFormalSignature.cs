using System;
using System.Reflection;

// `CreateDelegate` over a method of an open generic *definition* where deciding compatibility means
// comparing a type that names the definition's own variables: a parameter or return spelled with
// `T`, or an instance target's receiver, which is the typical instantiation `G<T>`. CoreCLR reads
// the target's signature against that typical instantiation and compares each such type as a
// `TypeVarTypeDesc` under its constraints (`IsLocationAssignable`, comdelegate.cpp:2367-2489), so
// the answer is not "incompatible because it mentions `T`": check 3 binds only because `T : class`,
// check 6 because a nested enum's verifier type is its underlying integer, and check 10 hands back
// a working delegate through interface contravariance.
//
// `DelegateBindOpenGenericDefinitionMethod.cs` covers the cases that never compare such a type.
//
// Returns 0 on success, or the number of the first check that failed. Every expectation was
// measured on real .NET.

public class Formal<T>
{
    public string Instance (int n)
    {
        return n.ToString ();
    }

    public static string TakesT (T x)
    {
        return "";
    }

    public static T ReturnsT ()
    {
        return default (T);
    }

    public static Formal<T> ReturnsSelf ()
    {
        return null;
    }

    public enum Nested
    {
        A,
    }

    public static string TakesNestedEnum (Nested e)
    {
        return "";
    }
}

public class FormalRef<T> where T : class
{
    public static T ReturnsT ()
    {
        return null;
    }

    public static string FirstT (T s, int n)
    {
        return "";
    }
}

public interface IContra<in T> where T : class
{
    string M ();
}

public class ContraImpl : IContra<object>
{
    public string M ()
    {
        return "impl";
    }
}

public static class Program
{
    const string NotFullyInstantiated =
        "Could not execute the method because either the method itself or the containing type is not fully instantiated.";

    static int ExpectNotInstantiated (Func<Delegate> create)
    {
        try
        {
            create ();
            return 1;
        }
        catch (InvalidOperationException e)
        {
            return e.Message == NotFullyInstantiated ? 0 : 2;
        }
    }

    static int ExpectBindFailure (Func<Delegate> create)
    {
        try
        {
            create ();
            return 1;
        }
        catch (ArgumentException e)
        {
            return e.GetType () == typeof (ArgumentException) ? 0 : 2;
        }
    }

    public static int Main ()
    {
        Type formal = typeof (Formal<>);
        Type formalRef = typeof (FormalRef<>);

        // 1: `T` is not an `int`.
        if (ExpectBindFailure (() => formal.GetMethod ("TakesT").CreateDelegate (typeof (Func<int, string>))) != 0)
        {
            return 1;
        }

        // 2: an unconstrained `T` might be a value type, so it cannot be returned as `object`.
        if (ExpectBindFailure (() => formal.GetMethod ("ReturnsT").CreateDelegate (typeof (Func<object>))) != 0)
        {
            return 2;
        }

        // 3: a `T : class` can, so this is compatible and reaches the code-address throw.
        if (ExpectNotInstantiated (() => formalRef.GetMethod ("ReturnsT").CreateDelegate (typeof (Func<object>))) != 0)
        {
            return 3;
        }

        // 4, 5: a static closed over its `T : class` first parameter. Over a string, the string is
        // not a `T`; over null, nothing is compared and the rest of the signature is compatible.
        if (ExpectBindFailure (() => formalRef.GetMethod ("FirstT").CreateDelegate (typeof (Func<int, string>), "x")) != 0)
        {
            return 4;
        }

        if (ExpectNotInstantiated (() => formalRef.GetMethod ("FirstT").CreateDelegate (typeof (Func<int, string>), null)) != 0)
        {
            return 5;
        }

        // 6: `Formal<T>.Nested` names `T`, but it is an enum over `int`, which the enum rule admits.
        if (ExpectNotInstantiated (() => formal.GetMethod ("TakesNestedEnum").CreateDelegate (typeof (Func<int, string>))) != 0)
        {
            return 6;
        }

        // 7: `Formal<T>` is a class, so it can be returned as `object`.
        if (ExpectNotInstantiated (() => formal.GetMethod ("ReturnsSelf").CreateDelegate (typeof (Func<object>))) != 0)
        {
            return 7;
        }

        MethodInfo instance = formal.GetMethod ("Instance");

        // 8: a closed instantiation's object is not a `Formal<T>`.
        if (Delegate.CreateDelegate (typeof (Func<int, string>), new Formal<string> (), instance, false) != null)
        {
            return 8;
        }

        // 9: nor is a `Formal<string>` parameter an open receiver.
        if (ExpectBindFailure (() => instance.CreateDelegate (typeof (Func<Formal<string>, int, string>))) != 0)
        {
            return 9;
        }

        // 10: but a contravariant interface's closed implementation *is* an `IContra<T>` for a
        // `T : class`, and the binding virtualises onto the implementation, which has code.
        Func<string> contra = (Func<string>)
            typeof (IContra<>).GetMethod ("M").CreateDelegate (typeof (Func<string>), new ContraImpl ());

        if (contra () != "impl")
        {
            return 10;
        }

        // 11: closed over null, `Action<T>.Invoke`'s `T` parameter still has to accept a `string`,
        // and it does not.
        if (ExpectBindFailure (() => typeof (Action<>).GetMethod ("Invoke").CreateDelegate (typeof (Action<string>), null)) != 0)
        {
            return 11;
        }

        return 0;
    }
}
