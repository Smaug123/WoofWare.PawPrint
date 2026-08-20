using System;

// Dispatch through the slot an override in a *generic* hierarchy really did take. Every call here
// names the declaration whose slot the most-derived override occupies, so the correct answer is that
// override in each case.
//
// These shapes were extracted from `VirtualDispatchGenericDefinitionSlots.cs` when that file was
// parked, being the half PawPrint answered correctly at the time. Both files now pass, dispatch
// reading the slot table rather than reconstructing the slot from declarations, so this one is no
// longer guarding an exclusion -- it is a narrower case of its sibling, kept because a regression that
// broke only the slots an override *did* take would fail here first and so name itself more precisely.
//
// What it pins about generics specifically: slots are laid out on the generic *definition*, where
// `A3<T>.M(T)` and `B3<T>.M(string)` are distinct declarations owning distinct slots. Closing them at
// `T = string` first makes them coincide, and an override of one then appears to fill the other.
//
// Exit code is the index of the first failing check, so a failure names itself.

public class Program
{
    public class A3<T>
    {
        public virtual int M (T x) => 1;
    }

    public class B3<T> : A3<T>
    {
        public virtual int M (string x) => 2;
    }

    public class C3<T> : B3<T>
    {
        public override int M (T x) => 3;
    }

    public class Ka<T>
    {
        public virtual int M (T x) => 11;
    }

    public class Kb<T> : Ka<string>
    {
        public virtual int M (T x) => 12;
    }

    public class Kc<T> : Kb<T>
    {
        public override int M (string x) => 13;
    }

    public class Ha<T>
    {
        public virtual int M (T x) => 21;
    }

    public class Hb<T> : Ha<T>
    {
        public new virtual int M (T x) => 22;
    }

    public class Hc<T> : Hb<T>
    {
        public override int M (T x) => 23;
    }

    public static int Main ()
    {
        // C3.M overrode A3's slot, so a call spelled A3<string>::M reaches it.
        A3<string> viaA3 = new C3<string> ();
        if (viaA3.M ("x") != 3) return 1;

        // Kc.M(string) overrode Ka<string>'s slot.
        Ka<string> viaKa = new Kc<string> ();
        if (viaKa.M ("x") != 13) return 2;

        // Hc.M overrode the slot Hb introduced with `new virtual`.
        Hb<int> viaHb = new Hc<int> ();
        if (viaHb.M (5) != 23) return 3;

        return 0;
    }
}
