using System;

// A `callvirt` names a *slot*, and the method that answers it is whichever method occupies that slot
// in the receiver's method table. `new virtual` is how C# asks for a slot of its own: `Nb.M` does not
// override `Na.M`, so `Nc.M`, which overrides `Nb.M`, occupies Nb's slot and leaves Na's alone. A call
// site spelled `Na::M` therefore still reaches `Na.M`, however derived the receiver is.
//
// No generics are involved, and the two signatures are identical, so nothing here turns on comparing
// them: the only thing that distinguishes Na's slot from Nb's is that Nb asked for a new one.
//
// Both parameter types are covered because a signature comparison that reads only the parameter types
// would treat them alike, and a failure on just one would point at the type rather than at slots.
public class Program
{
    public class Na
    {
        public virtual int M (string x)
        {
            return 31;
        }
    }

    public class Nb : Na
    {
        public new virtual int M (string x)
        {
            return 32;
        }
    }

    public class Nc : Nb
    {
        public override int M (string x)
        {
            return 33;
        }
    }

    public class Pa
    {
        public virtual int M (int x)
        {
            return 41;
        }
    }

    public class Pb : Pa
    {
        public new virtual int M (int x)
        {
            return 42;
        }
    }

    public class Pc : Pb
    {
        public override int M (int x)
        {
            return 43;
        }
    }

    public static int Main (string[] args)
    {
        Nc n = new Nc ();
        Na viaNa = n;
        Nb viaNb = n;

        // Na's slot was never overridden: Nb took a new one and Nc overrode that.
        if (viaNa.M ("x") != 31)
        {
            return 1;
        }

        // The control: dispatching through the slot Nc *did* override finds Nc's body, so a failure
        // above is about which slot the call site names rather than about override lookup as a whole.
        if (viaNb.M ("x") != 33)
        {
            return 2;
        }

        Pc p = new Pc ();
        Pa viaPa = p;
        Pb viaPb = p;

        if (viaPa.M (5) != 41)
        {
            return 3;
        }

        if (viaPb.M (5) != 43)
        {
            return 4;
        }

        return 0;
    }
}
