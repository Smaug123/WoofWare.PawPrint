using System;

// The dispatch counterpart of `ReflectionVirtualSlotsGenericDefinitionLayout.cs`. Vtable slots are laid
// out on the generic *definition*, so which slot a `callvirt` names -- and which method occupies it --
// is decided before any type argument is supplied. Two signatures that differ at the definition level
// and coincide once the arguments are substituted still belong to different slots.
//
//  * `A3<T>.M(T)` and `B3<T>.M(string)` are distinct declarations, so `C3<T>.M(T)` overrides A3's slot
//    and B3 keeps its own. At `T = string` all three read `M(string)`, yet `callvirt B3<string>::M(string)`
//    must still reach `B3.M`.
//  * `Kb<T> : Ka<string>` pins Ka's parameter, so Ka's `!0` and Kb's `!0` denote different things
//    though both are written `!0`. `Kc<T>.M(string)` overrides Ka's slot, and Kb keeps its own.
//  * `Hb<T>` shadows `Ha<T>.M(T)` with `new virtual`, so `Hc<T>.M(T)` overrides Hb's slot and Ha's is
//    untouched. Here the signatures are identical at every level and only the new slot separates them.
//
// Every shape is paired with the call through the slot the override *did* take, so a failure is
// attributable to the choice of slot rather than to override lookup in general.
public class Program
{
    public class A3<T>
    {
        public virtual int M (T x)
        {
            return 1;
        }
    }

    public class B3<T> : A3<T>
    {
        public virtual int M (string x)
        {
            return 2;
        }
    }

    public class C3<T> : B3<T>
    {
        public override int M (T x)
        {
            return 3;
        }
    }

    public class Ka<T>
    {
        public virtual int M (T x)
        {
            return 11;
        }
    }

    public class Kb<T> : Ka<string>
    {
        public virtual int M (T x)
        {
            return 12;
        }
    }

    public class Kc<T> : Kb<T>
    {
        public override int M (string x)
        {
            return 13;
        }
    }

    public class Ha<T>
    {
        public virtual int M (T x)
        {
            return 21;
        }
    }

    public class Hb<T> : Ha<T>
    {
        public new virtual int M (T x)
        {
            return 22;
        }
    }

    public class Hc<T> : Hb<T>
    {
        public override int M (T x)
        {
            return 23;
        }
    }

    public static int Main (string[] args)
    {
        C3<string> c = new C3<string> ();
        A3<string> viaA3 = c;
        B3<string> viaB3 = c;

        // C3.M overrode A3's slot.
        if (viaA3.M ("x") != 3)
        {
            return 1;
        }

        // B3's slot is its own, so it still holds B3.M.
        if (viaB3.M ("x") != 2)
        {
            return 2;
        }

        Kc<string> k = new Kc<string> ();
        Ka<string> viaKa = k;
        Kb<string> viaKb = k;

        if (viaKa.M ("x") != 13)
        {
            return 3;
        }

        if (viaKb.M ("x") != 12)
        {
            return 4;
        }

        Hc<int> h = new Hc<int> ();
        Ha<int> viaHa = h;
        Hb<int> viaHb = h;

        if (viaHa.M (5) != 21)
        {
            return 5;
        }

        if (viaHb.M (5) != 23)
        {
            return 6;
        }

        return 0;
    }
}
