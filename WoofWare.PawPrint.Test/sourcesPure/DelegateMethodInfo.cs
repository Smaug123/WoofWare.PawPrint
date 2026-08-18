using System;
using System.Reflection;

// `Delegate.Method`, the only guest route to the QCall `Delegate_FindMethodHandle`.
// `MulticastDelegate.GetMethodImpl` falls through to `Delegate.GetMethodImpl`, which finds the
// `_methodBase` cache empty and asks the runtime which method the delegate points at.
//
// Returns 0 on success, or the number of the first check that failed. Every code is below 128, so
// that none can be mistaken for the 128+signo a signalled guest reports.

public class Box<T>
{
    public T Value;

    public Box (T v)
    {
        Value = v;
    }

    public string Describe (int n)
    {
        return n.ToString ();
    }

    public static string StaticDescribe (int n)
    {
        return n.ToString ();
    }

    public static U Roundtrip<U> (U x)
    {
        return x;
    }
}

// Inherits an instance method from a generic base. `Delegate.GetMethodImpl` has a special path for
// exactly this: when the declaring type is generic and the target is an instance, it walks
// `_target.GetType()`'s base chain for the generic type definition, so the declaring type reported
// must be `Box<string>` rather than `SubBox`.
public class SubBox : Box<string>
{
    public SubBox (string v)
        : base (v)
    {
    }
}

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

public interface IThing
{
    int Go ();
}

public class Thing : IThing
{
    public int Go ()
    {
        return 9;
    }
}

public struct SThing : IThing
{
    public int N;

    public int Go ()
    {
        return N;
    }
}

public struct SBox
{
    public int N;

    public int Get ()
    {
        return N;
    }
}

public class DelegateMethodInfo
{
    private static int Twice (int x)
    {
        return x * 2;
    }

    private int _k;

    private int Add (int x)
    {
        return x + _k;
    }

    private static T Ident<T> (T x)
    {
        return x;
    }

    public static int Main (string[] argv)
    {
        // A static target: the plainest shape, and the one that pins that the answer is the
        // target rather than the delegate type's own `Invoke`.
        Func<int, int> f = Twice;
        MethodInfo m = f.Method;
        if (m == null) return 1;
        if (m.Name != "Twice") return 2;
        if (m.DeclaringType != typeof (DelegateMethodInfo)) return 3;
        if (!m.IsStatic) return 4;

        // A closed delegate over an instance method.
        DelegateMethodInfo p = new DelegateMethodInfo ();
        p._k = 5;
        Func<int, int> g = p.Add;
        if (g.Method.Name != "Add") return 10;
        if (g.Method.IsStatic) return 11;
        if (g.Target != p) return 12;
        if (g (1) != 6) return 13;

        // A generic declaring type. CoreCLR can reach `FindMethodHandle` with a shared (`__Canon`)
        // MethodDesc for these, which is why it normalises before allocating the stub; the
        // declaring type reported must be the exact instantiation.
        Box<string> b = new Box<string> ("hi");
        Func<int, string> h = b.Describe;
        if (h.Method.Name != "Describe") return 20;
        if (h.Method.DeclaringType != typeof (Box<string>)) return 21;

        Func<int, string> hs = Box<string>.StaticDescribe;
        if (hs.Method.Name != "StaticDescribe") return 22;
        if (hs.Method.DeclaringType != typeof (Box<string>)) return 23;

        Func<int, int> hg = Box<string>.Roundtrip<int>;
        if (hg.Method.Name != "Roundtrip") return 24;
        if (hg.Method.DeclaringType != typeof (Box<string>)) return 25;

        // Inherited from a generic base, so the declaring type and the target's type differ.
        SubBox sb = new SubBox ("x");
        Func<int, string> hb = sb.Describe;
        if (hb.Method.Name != "Describe") return 30;
        if (hb.Method.DeclaringType != typeof (Box<string>)) return 31;

        // A virtual target, bound eagerly by `ldvirtftn`, so the override is what is reported.
        VirtBase v = new VirtDerived ();
        Func<int> vd = v.Virt;
        if (vd.Method.Name != "Virt") return 40;
        if (vd.Method.DeclaringType != typeof (VirtDerived)) return 41;

        // An interface method through an interface-typed receiver.
        IThing t = new Thing ();
        Func<int> ti = t.Go;
        if (ti.Method.Name != "Go") return 42;
        if (ti.Method.DeclaringType != typeof (Thing)) return 43;

        // The same, but the implementer is a struct, so CoreCLR binds an *unboxing stub* — one of
        // the two flavours `FindOrCreateAssociatedMethodDescForReflection` exists to normalise
        // away before reflection sees them.
        SThing st = new SThing ();
        st.N = 4;
        IThing sti = st;
        Func<int> stf = sti.Go;
        if (stf.Method.Name != "Go") return 44;
        if (stf.Method.DeclaringType != typeof (SThing)) return 45;
        if (stf () != 4) return 46;

        // An instance method of a value type reached directly, so the receiver is a fresh box.
        SBox s = new SBox ();
        s.N = 7;
        Func<int> sv = s.Get;
        if (sv.Method.Name != "Get") return 50;
        if (sv.Method.DeclaringType != typeof (SBox)) return 51;

        // A generic method at a value instantiation. `IsGenericMethod` alone is true of the
        // generic method *definition* too, so the arguments are checked: dropping the method
        // instantiation anywhere on this path would otherwise pass.
        Func<int, int> gi = Ident<int>;
        if (gi.Method.Name != "Ident") return 60;
        if (!gi.Method.IsGenericMethod) return 61;
        if (gi.Method.IsGenericMethodDefinition) return 62;
        if (gi.Method.GetGenericArguments ().Length != 1) return 63;
        if (gi.Method.GetGenericArguments ()[0] != typeof (int)) return 64;

        // The same generic method at a reference instantiation, which on CoreCLR is served by a
        // `__Canon`-shared MethodDesc — the other flavour that gets normalised.
        Func<string, string> gs = Ident<string>;
        if (gs.Method.Name != "Ident") return 65;
        if (gs.Method.GetGenericArguments ()[0] != typeof (string)) return 66;

        // A lambda, whose target is a compiler-generated display class.
        int captured = 3;
        Func<int, int> lam = delegate (int x) { return x + captured; };
        if (lam.Method == null) return 70;
        if (lam.Method.GetParameters ().Length != 1) return 71;

        // A delegate over another delegate's `Invoke`. `Invoke` has a MethodDef row like any other
        // method — the runtime supplies only its body — so it round-trips like one.
        Func<int, int> fi = f.Invoke;
        if (fi.Method.Name != "Invoke") return 80;
        if (fi.Method.DeclaringType != typeof (Func<int, int>)) return 81;

        // `GetMethodImpl` caches into `_methodBase`, so a second read of one delegate is the same
        // object; and two delegates over one method agree, which is the guest-visible half of the
        // stub deduplication.
        if (!ReferenceEquals (f.Method, f.Method)) return 90;
        Func<int, int> f2 = Twice;
        if (!ReferenceEquals (f.Method, f2.Method)) return 91;

        // The MethodInfo a delegate reports is the one ordinary reflection reports.
        MethodInfo direct = typeof (DelegateMethodInfo).GetMethod (
            "Twice",
            BindingFlags.NonPublic | BindingFlags.Static);
        if (direct == null) return 100;
        if (!f.Method.Equals (direct)) return 101;

        // A usable MethodInfo, not merely a well-named one: an implementation that handed back a
        // stub naming some other method of the same name would pass everything above.
        object r = f.Method.Invoke (null, new object[] { 21 });
        if (!(r is int)) return 110;
        if ((int) r != 42) return 111;
        if (f.Method.ReturnType != typeof (int)) return 112;
        if (f.Method.GetParameters ().Length != 1) return 113;
        if (f.Method.GetParameters ()[0].ParameterType != typeof (int)) return 114;

        return 0;
    }
}
