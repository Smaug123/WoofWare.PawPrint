using System;
using System.Reflection;

// `Delegate.DynamicInvoke`, the ordinary guest route to the
// `Delegate::GetInvokeMethod(MethodTable*)` InternalCall (`COMDelegate::GetInvokeMethod`,
// comdelegate.cpp:2156). `DynamicInvokeImpl` asks the runtime for the *delegate type's* `Invoke`
// MethodDesc, turns it into a `RuntimeMethodInfo` via `RuntimeType.GetMethodBase`, and reflectively
// invokes it with the delegate as the receiver (Delegate.CoreCLR.cs:80-86). Every check below
// therefore also depends on `RuntimeMethodHandle_InvokeMethod` and on ordinary delegate dispatch;
// what is new here is which method the runtime names. The last block reaches the InternalCall a
// second way, by reflecting on its managed wrapper, in order to observe the answer's identity
// rather than only its effect.
//
// This file deliberately breaks the "invoke each MethodInfo exactly once" rule that
// `ReflectionInvokeMethod.cs` and its siblings keep, because the whole point of the issue-849 row
// this covers is that a *repeated* `DynamicInvoke` must work. `DynamicInvokeImpl` calls
// `GetInvokeMethod()` afresh each time, but `RuntimeType.GetMethodBase` caches per method handle,
// so the second call reuses one `MethodInfo` and takes
// `MethodInvokerCommon.DetermineStrategy_*`'s second-invocation branch. On the oracle that branch
// builds a Reflection.Emit stub; under PawPrint, which seeds
// `RuntimeFeature.IsDynamicCodeSupported` false, it stays interpreted. The two runtimes therefore
// agree on the *answer* while reaching it differently, which is all this file asserts.
//
// Returns 0 on success, or the number of the first check that failed. Every code is below 128, so
// that none can be mistaken for the 128+signo a signalled guest reports.

public struct Pt
{
    public int X;
    public int Y;
}

public delegate Pt MakePt (int a, int b);

public delegate T Ident<T> (T v);

public class VirtBase
{
    public virtual int Virt ()
    {
        return 1;
    }
}

public class VirtDerived : VirtBase
{
    public override int Virt ()
    {
        return 2;
    }
}

public class DelegateDynamicInvoke
{
    private static int _voidCalls;

    private int _k;

    private static int Twice (int x)
    {
        return x * 2;
    }

    private static int Length (string s)
    {
        return s.Length;
    }

    private int Add (int x)
    {
        return x + _k;
    }

    private static Pt Make (int a, int b)
    {
        Pt p = new Pt ();
        p.X = a;
        p.Y = b;
        return p;
    }

    private static void Bump (int by)
    {
        _voidCalls = _voidCalls + by;
    }

    private static string Decorate (string s)
    {
        return "[" + s + "]";
    }

    private static int CallVirt (VirtBase b)
    {
        return b.Virt ();
    }

    public static int Main (string[] argv)
    {
        // The plainest shape: an open static target, invoked twice through one delegate. The
        // second call is the one issue 849 records as failing.
        Func<int, int> f = Twice;
        object a = f.DynamicInvoke (3);
        if (!(a is int)) return 1;
        if ((int) a != 6) return 2;
        object b = f.DynamicInvoke (4);
        if (!(b is int)) return 3;
        if ((int) b != 8) return 4;

        // A *closed* delegate over an instance method. This is what pins that the method named is
        // the delegate type's `Invoke` rather than the target: `DynamicInvokeImpl` passes the
        // delegate itself as the receiver, so an implementation that answered `Add` would be
        // invoking it against a receiver of the wrong type.
        DelegateDynamicInvoke p = new DelegateDynamicInvoke ();
        p._k = 5;
        Func<int, int> closed = p.Add;
        object c = closed.DynamicInvoke (1);
        if ((int) c != 6) return 10;
        object d = closed.DynamicInvoke (2);
        if ((int) d != 7) return 11;

        // A second instantiation of the same generic delegate definition. An implementation that
        // named `Invoke` on the open `Func<,>`, or that cached one answer per definition, would
        // hand `Func<string, int>`'s call `Func<int, int>`'s signature.
        Func<string, int> len = Length;
        object e = len.DynamicInvoke ("abcd");
        if ((int) e != 4) return 20;
        object g = len.DynamicInvoke ("xyz");
        if ((int) g != 3) return 21;

        // A user-declared generic delegate type, and a non-generic one, so the shape is not only
        // `Func<>`.
        Ident<string> id = Decorate;
        if ((string) id.DynamicInvoke ("q") != "[q]") return 30;
        if ((string) id.DynamicInvoke ("r") != "[r]") return 31;

        // Two arguments, and a value-type return, which has to come back boxed.
        MakePt mk = Make;
        object pt = mk.DynamicInvoke (7, 9);
        if (!(pt is Pt)) return 40;
        if (((Pt) pt).X != 7) return 41;
        if (((Pt) pt).Y != 9) return 42;
        object pt2 = mk.DynamicInvoke (1, 2);
        if (((Pt) pt2).X != 1) return 43;
        if (((Pt) pt2).Y != 2) return 44;

        // A void return is reported as null, and the call really happened -- a no-op
        // implementation would pass the null check alone.
        Action<int> act = Bump;
        if (act.DynamicInvoke (3) != null) return 50;
        if (_voidCalls != 3) return 51;
        if (act.DynamicInvoke (4) != null) return 52;
        if (_voidCalls != 7) return 53;

        // Virtual dispatch happens inside the target, not at the delegate, so this checks that a
        // real call was made rather than a signature merely matched.
        Func<VirtBase, int> v = CallVirt;
        if ((int) v.DynamicInvoke (new VirtDerived ()) != 2) return 60;
        if ((int) v.DynamicInvoke (new VirtBase ()) != 1) return 61;

        // The wrong number of arguments is checked against `Invoke`'s signature, above the QCall,
        // in `MethodBase.Invoke`.
        try
        {
            f.DynamicInvoke (1, 2);
            return 70;
        }
        catch (TargetParameterCountException)
        {
        }

        // As is the argument's type.
        try
        {
            f.DynamicInvoke ("not an int");
            return 71;
        }
        catch (ArgumentException)
        {
        }

        // Passing no arguments at all where one is required.
        try
        {
            f.DynamicInvoke (null);
            return 72;
        }
        catch (TargetParameterCountException)
        {
        }

        // The delegate still works normally afterwards: `DynamicInvoke` must not have disturbed
        // the fields ordinary dispatch reads.
        if (f (10) != 20) return 80;
        if (closed (10) != 15) return 81;

        // And the method the runtime named is reachable by ordinary reflection too, agreeing on
        // the answer. `Invoke` has a MethodDef row like any other method; only its body is
        // supplied by the runtime.
        MethodInfo invoke = typeof (Func<int, int>).GetMethod ("Invoke");
        if (invoke == null) return 90;
        if (invoke.DeclaringType != typeof (Func<int, int>)) return 91;
        if (invoke.ReturnType != typeof (int)) return 92;
        if (invoke.GetParameters ().Length != 1) return 93;

        // The InternalCall itself, called directly. Everything above observes it only through the
        // answer `Invoke` computes, which several wrong implementations would still get right;
        // these check the handle's *identity*. Real .NET returns a raw `MethodDesc*` here, so only
        // equality is asserted and never a value.
        MethodInfo getInvokeMethod = typeof (Delegate).GetMethod (
            "GetInvokeMethod",
            BindingFlags.NonPublic | BindingFlags.Instance,
            null,
            Type.EmptyTypes,
            null);
        if (getInvokeMethod == null) return 100;

        object h1 = getInvokeMethod.Invoke (f, null);
        if (!(h1 is IntPtr)) return 101;
        if (h1.Equals (IntPtr.Zero)) return 102;

        // Two delegates of one type name one method, and asking twice gives one answer: CoreCLR
        // reads a slot the type loader filled in, so there is exactly one `MethodDesc` per
        // delegate type however many delegates or calls there are.
        Func<int, int> f3 = Twice;
        if (!h1.Equals (getInvokeMethod.Invoke (f, null))) return 103;
        if (!h1.Equals (getInvokeMethod.Invoke (f3, null))) return 104;

        // A value-type and a reference-type instantiation of one generic delegate definition name
        // *different* methods. An implementation that answered per definition rather than per
        // instantiation would pass every check above that happens to coerce its arguments
        // successfully.
        //
        // Do not generalise this to two *reference-type* instantiations: CoreCLR shares one
        // `DelegateEEClass`, and so one `Invoke` MethodDesc, across all of them, so real .NET
        // answers `Func<string, int>` and `Func<object, int>` alike where PawPrint answers two
        // registry ids. That pair is a recorded divergence, not an assertion this file can make --
        // see docs/divergences.md, "A generic delegate type's `Invoke` handle is
        // per-instantiation". The pair below spans the value/reference boundary, where CoreCLR
        // shares nothing and the two runtimes agree.
        if (h1.Equals (getInvokeMethod.Invoke (len, null))) return 105;

        // As do two unrelated delegate types. Neither shares a generic definition with `f`, so
        // neither is at risk of the canonical sharing above.
        if (h1.Equals (getInvokeMethod.Invoke (act, null))) return 106;
        if (h1.Equals (getInvokeMethod.Invoke (id, null))) return 107;

        // And a delegate over an instance method answers the same as one over a static method,
        // because what is named is the delegate type's `Invoke` and not the target.
        if (!h1.Equals (getInvokeMethod.Invoke (closed, null))) return 108;

        return 0;
    }
}
