using System;
using System.Reflection;

// `CreateDelegate` over a method whose declaring type is an open generic *definition* —
// `typeof(G<>).GetMethod("M")`. Legal to ask, and real .NET never answers with a delegate here:
// which exception the guest sees depends on where binding stops.
//
// The compatibility check (`COMDelegate::IsMethodDescCompatible`) runs first, reading the target's
// signature against the definition's typical instantiation. If it fails, the QCall reports a bind
// failure, which the managed caller turns into `ArgumentException` or, where
// `throwOnBindFailure: false` was passed, into a null return. If it succeeds, `BindToMethod` asks
// the target for a code address and `MethodDesc::TryGetMultiCallableAddrOfCode`
// (method.cpp:2091-2093) throws `InvalidOperationException` — from the QCall itself, so
// `throwOnBindFailure: false` does not suppress it.
//
// Every signature here is spelled without `T`, and every instance binding is either rejected by
// arity before any types are compared or closed over null, which skips the receiver comparison.
// So nothing below needs to compare a type that names the definition's variables; the cases that
// do are in `DelegateBindOpenGenericDefinitionFormalSignature.cs`.
//
// Returns 0 on success, or the number of the first check that failed. Every expectation was
// measured on real .NET.

public class OpenDefnHolder<T>
{
    public string Instance (int n)
    {
        return n.ToString ();
    }

    public virtual string VirtualInstance (int n)
    {
        return n.ToString ();
    }

    public static string Static (int n)
    {
        return n.ToString ();
    }

    public static string StaticFirstRef (string s, int n)
    {
        return s + n;
    }
}

public struct OpenDefnStruct<T>
{
    public string Instance (int n)
    {
        return n.ToString ();
    }
}

public static class Program
{
    const string NotFullyInstantiated =
        "Could not execute the method because either the method itself or the containing type is not fully instantiated.";

    // 0 if `create` threw InvalidOperationException with CoreCLR's message; otherwise nonzero.
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

    // 0 if `create` threw ArgumentException (and nothing more derived); otherwise nonzero.
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
        // 1, 2: controls. The closed instantiation's methods bind and run, so what follows is
        // about the definition rather than about these signatures.
        Func<int, string> closedStatic = (Func<int, string>)
            typeof (OpenDefnHolder<string>).GetMethod ("Static").CreateDelegate (typeof (Func<int, string>));

        if (closedStatic (7) != "7")
        {
            return 1;
        }

        Func<int, string> closedInstance = (Func<int, string>)
            typeof (OpenDefnHolder<string>)
                .GetMethod ("Instance")
                .CreateDelegate (typeof (Func<int, string>), new OpenDefnHolder<string> ());

        if (closedInstance (8) != "8")
        {
            return 2;
        }

        Type definition = typeof (OpenDefnHolder<>);
        MethodInfo definitionStatic = definition.GetMethod ("Static");
        MethodInfo definitionStaticFirstRef = definition.GetMethod ("StaticFirstRef");
        MethodInfo definitionInstance = definition.GetMethod ("Instance");
        MethodInfo definitionVirtual = definition.GetMethod ("VirtualInstance");
        MethodInfo definitionStructInstance = typeof (OpenDefnStruct<>).GetMethod ("Instance");

        // 3: the lookups themselves succeed, so the refusals are about binding.
        if (definitionStatic == null
            || definitionStaticFirstRef == null
            || definitionInstance == null
            || definitionVirtual == null
            || definitionStructInstance == null)
        {
            return 3;
        }

        // 4-6: a compatible open static binding dies looking for a code address, whichever
        // overload asks, and `throwOnBindFailure: false` does not turn that into null.
        if (ExpectNotInstantiated (() => definitionStatic.CreateDelegate (typeof (Func<int, string>))) != 0)
        {
            return 4;
        }

        if (ExpectNotInstantiated (() => Delegate.CreateDelegate (typeof (Func<int, string>), definitionStatic, false)) != 0)
        {
            return 5;
        }

        if (ExpectNotInstantiated (
                () => Delegate.CreateDelegate (typeof (Func<int, string>), null, definitionStatic, false)) != 0)
        {
            return 6;
        }

        // 7: relaxed return matching still applies, so `string` to `object` is compatible and
        // reaches the code-address throw.
        if (ExpectNotInstantiated (() => definitionStatic.CreateDelegate (typeof (Func<int, object>))) != 0)
        {
            return 7;
        }

        // 8, 9: an incompatible parameter is a bind failure — the check runs before the
        // code-address fetch.
        if (ExpectBindFailure (() => definitionStatic.CreateDelegate (typeof (Func<long, string>))) != 0)
        {
            return 8;
        }

        if (Delegate.CreateDelegate (typeof (Func<long, string>), definitionStatic, false) != null)
        {
            return 9;
        }

        // 10: so is an arity that fits neither an open nor a closed binding.
        if (Delegate.CreateDelegate (typeof (Func<string>), definitionStatic, false) != null)
        {
            return 10;
        }

        // 11, 12: a static closed over its first argument, whether that is an object or null.
        if (ExpectNotInstantiated (() => definitionStaticFirstRef.CreateDelegate (typeof (Func<int, string>), "x")) != 0)
        {
            return 11;
        }

        if (ExpectNotInstantiated (() => definitionStaticFirstRef.CreateDelegate (typeof (Func<int, string>), null)) != 0)
        {
            return 12;
        }

        // 13: the v1 overloads accept only an open binding, and this one is closed by arity.
        if (Delegate.CreateDelegate (typeof (Func<int, string>), definitionStaticFirstRef, false) != null)
        {
            return 13;
        }

        // 14, 15: an instance target is closed by arity, which the v1 overloads refuse before any
        // types are compared.
        if (ExpectBindFailure (() => definitionInstance.CreateDelegate (typeof (Func<int, string>))) != 0)
        {
            return 14;
        }

        if (Delegate.CreateDelegate (typeof (Func<int, string>), definitionInstance, false) != null)
        {
            return 15;
        }

        // 16, 17: closed over null, there is no receiver to compare, so the rest of the signature
        // decides and a compatible one reaches the code-address throw.
        if (ExpectNotInstantiated (() => definitionInstance.CreateDelegate (typeof (Func<int, string>), null)) != 0)
        {
            return 16;
        }

        if (ExpectNotInstantiated (
                () => Delegate.CreateDelegate (typeof (Func<int, string>), null, definitionInstance, false)) != 0)
        {
            return 17;
        }

        // 18: and an incompatible one is a bind failure.
        if (Delegate.CreateDelegate (typeof (Func<long, string>), null, definitionInstance, false) != null)
        {
            return 18;
        }

        // 19: a virtual target closed over null is not virtualised, so it too asks for code.
        if (ExpectNotInstantiated (() => definitionVirtual.CreateDelegate (typeof (Func<int, string>), null)) != 0)
        {
            return 19;
        }

        // 20: likewise an instance method of a value-type definition.
        if (ExpectNotInstantiated (() => definitionStructInstance.CreateDelegate (typeof (Func<int, string>), null)) != 0)
        {
            return 20;
        }

        return 0;
    }
}
