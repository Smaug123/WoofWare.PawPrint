using System;

// A covariant-return chain that narrows at *every* level. Each override is `newslot` plus a
// MethodImpl, and Roslyn names the *immediate parent* in each one -- `L1`'s declaration is `L0::F`
// and `L2`'s is `L1::F`, measured. So no MethodImpl anywhere names `L0::F` from `L2`, and yet
// `((L0)l2).F()` must reach `L2.F`.
//
// CoreCLR gets there by *unifying* slots rather than copying a body once. `SetupMethodTable2`
// (methodtablebuilder.cpp:11344-11381) iterates the whole vtable to a fixed point: any slot whose
// occupant's primary slot differs from the slot it sits in is overwritten with the primary slot's
// current occupant, repeatedly. Its comment records that MethodImpl on a class thereby came to mean
// "unify the slots of A and B" rather than "substitute the body of A with B", and that compilers
// rely on it. One pass is not enough, which is exactly this file: a single pass leaves `L0`'s slot
// holding `L1.F`.
//
// That is what PawPrint answers. `tryResolveVirtualImplementationForSlot` walks the receiver's chain
// looking for a MethodImpl whose declaration *is* the call site; at `L2` the only MethodImpl declares
// `L1::F`, which is not the call site, and `L2.F` itself is rejected as `newslot`, so the walk
// continues to `L1`, whose MethodImpl does declare `L0::F`. Measured from a debug trace: the call
// resolves to `L1::F`, so a plain `Mid` comes back and check 1 fails (measured). A silently wrong
// body, reachable from plain C# with no generics.
//
// Every body tags what it produced, because the return *types* alone cannot identify which body ran:
// `L0.F` and `L1.F` both yield a plain `Mid`, so an `is Leaf` test passes or fails identically for
// the two, and the mid-chain checks below would not notice a regression from `L1.F` to `L0.F`.
//
// Un-park when dispatch is keyed off the slot table and slot contents are unified to a fixed point.
// `CovariantReturnDispatchChain.cs` covers the single-level shapes that already work, so a
// regression there is distinguishable from this gap. Verified to exit 0 on real .NET.

public class Mid
{
    public int Tag;
}

public class Leaf : Mid
{
}

public class L0
{
    public virtual object F()
    {
        Mid m = new Mid();
        m.Tag = 10;
        return m;
    }
}

public class L1 : L0
{
    public override Mid F()
    {
        Mid m = new Mid();
        m.Tag = 11;
        return m;
    }
}

public class L2 : L1
{
    public override Leaf F()
    {
        Leaf l = new Leaf();
        l.Tag = 12;
        return l;
    }
}

public static class Program
{
    public static int Main()
    {
        L2 l2 = new L2();

        object viaL0 = ((L0)l2).F();
        if (!(viaL0 is Leaf)) return 1;
        if (((Mid)viaL0).Tag != 12) return 2;

        object viaL1 = ((L1)l2).F();
        if (!(viaL1 is Leaf)) return 3;
        if (((Mid)viaL1).Tag != 12) return 4;

        Leaf direct = l2.F();
        if (direct.Tag != 12) return 5;

        // A mid-chain receiver: dispatching on an `L1` must reach `L1.F` and not `L2.F` (which a rule
        // answering with the most-derived override would give) nor `L0.F` (which a rule that lost the
        // override entirely would give). The tag is what separates the last two.
        L1 l1 = new L1();
        object viaL0OnL1 = ((L0)l1).F();

        if (viaL0OnL1 is Leaf) return 6;
        if (((Mid)viaL0OnL1).Tag != 11) return 7;

        return 0;
    }
}
