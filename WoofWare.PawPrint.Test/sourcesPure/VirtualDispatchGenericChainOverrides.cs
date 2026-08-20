using System;

// Dispatch through the slot an override in a *generic* hierarchy really did take. Every call here
// names the declaration whose slot the most-derived override occupies, so the correct answer is that
// override in each case.
//
// These shapes are the half of `VirtualDispatchGenericDefinitionSlots.cs` that PawPrint answers
// correctly, extracted so that they are actively guarded. What they guard is
// `tryResolveByOverrideChain`'s refusal to serve a chain containing a generic definition: serving one
// needs each ancestor's signature read in its own type variables, because at `T = string` a closed
// comparison cannot tell `A3<T>.M(T)` from `B3<T>.M(string)`.
//
// The refusal is checked rather than assumed. Removing it makes this file fail with "Signature
// comparison ... reached generic type parameter !0, but the declaring type's instantiation supplies
// only 0 argument(s)" -- measured -- because the override relation compares signatures under an empty
// substitution context, which is a detected misuse rather than a wrong answer. So the exclusion keeps
// these calls on the pre-existing walk, and the comparison itself is the backstop if it ever fails to.
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
