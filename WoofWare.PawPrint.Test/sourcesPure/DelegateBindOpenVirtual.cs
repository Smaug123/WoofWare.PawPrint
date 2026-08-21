using System;
using System.Reflection;

// An *open* delegate over a virtual instance method declared on a reference type: the shape
// CoreCLR resolves at invocation rather than at binding, through a virtual call stub in
// `_methodPtrAux` with `_invocationCount` holding the `MethodDesc`
// (`COMDelegate::BindToMethod`, comdelegate.cpp:1236-1245).
//
// PawPrint writes neither field, and `AbstractMachine.dispatchDelegateInvoke` calls whatever
// `_methodPtr` names without virtualising, so binding the declared method would silently ignore an
// override. `Delegate_BindToMethodInfo` therefore refuses the shape.
//
// C# cannot spell this with method-group syntax — there is no receiver to write — so
// `Delegate.CreateDelegate` is the only route to it, and this file is what pins that PawPrint
// refuses rather than answers wrongly. Checks 4 and 5 are what stop the refusal being satisfiable
// by binding the declared method: they invoke through a derived receiver, where a
// declared-method binding returns the base's answer.
//
// Returns 0 on success, or the number of the first check that failed.

public class OpenVirtualBase
{
    public virtual string Name ()
    {
        return "base";
    }

    // `final` in IL because Roslyn marks a `sealed` member so, and a final method's slot always
    // resolves to itself — so binding it directly is what CoreCLR's stub would have found anyway.
    public virtual string Sealed ()
    {
        return "sealedBase";
    }

    public string NotVirtual ()
    {
        return "notVirtual";
    }
}

public class OpenVirtualDerived : OpenVirtualBase
{
    public override string Name ()
    {
        return "derived";
    }

    public sealed override string Sealed ()
    {
        return "sealedDerived";
    }
}

public static class Program
{
    public static int Main ()
    {
        MethodInfo notVirtual = typeof (OpenVirtualBase).GetMethod ("NotVirtual");

        // 1: control. An open delegate over a *non*-virtual instance method needs no stub, and is
        // bound directly by CoreCLR too — so this half is served.
        Func<OpenVirtualBase, string> open =
            (Func<OpenVirtualBase, string>) notVirtual.CreateDelegate (typeof (Func<OpenVirtualBase, string>));

        if (open (new OpenVirtualBase ()) != "notVirtual")
        {
            return 1;
        }

        if (open (new OpenVirtualDerived ()) != "notVirtual")
        {
            return 2;
        }

        // 3: control. A `sealed override` is `final`, so its slot resolves to itself and binding it
        // directly is answer-preserving; this half is served too.
        Func<OpenVirtualDerived, string> openSealed = (Func<OpenVirtualDerived, string>)
            typeof (OpenVirtualDerived).GetMethod ("Sealed").CreateDelegate (typeof (Func<OpenVirtualDerived, string>));

        if (openSealed (new OpenVirtualDerived ()) != "sealedDerived")
        {
            return 3;
        }

        // 4: the refused shape. Binding the declared method rather than dispatching would answer
        // "base" here.
        MethodInfo virtualMethod = typeof (OpenVirtualBase).GetMethod ("Name");

        Func<OpenVirtualBase, string> openVirtual =
            (Func<OpenVirtualBase, string>) virtualMethod.CreateDelegate (typeof (Func<OpenVirtualBase, string>));

        if (openVirtual (new OpenVirtualDerived ()) != "derived")
        {
            return 4;
        }

        // 5: and the same delegate still answers the base's body for a base receiver, so the
        // dispatch is per-argument rather than fixed at binding.
        if (openVirtual (new OpenVirtualBase ()) != "base")
        {
            return 5;
        }

        return 0;
    }
}
