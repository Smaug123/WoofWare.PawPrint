// An interface-map entry's slot may only be implemented by the type that owns the entry, or by
// that type's bases — never by a method a more-derived type happens to introduce with a matching
// signature.
//
// This matters as soon as variant dispatch retargets onto an entry the *base* class contributed:
// `tryResolveVirtualImplementation` resolves each retargeted entry with the entry's owner as the
// dispatch type, not the receiver. Resolving from the receiver instead would walk the derived
// type first and let its methods answer for a slot they have nothing to do with — PawPrint's
// class walk matches implicit interface implementations by name and signature, so it cannot tell
// the difference on its own.
//
// The complementary cases that PawPrint still gets wrong — where a derived type *does* need to
// take over an inherited slot — are in `InterfaceSlotHiddenByDerivedMethod.cs`.

using System;

interface IOwn<in T> { long Accept(T value); }
interface IOwnDim<in T> { long Accept(T value) => 100; }

// The base's IOwnDim<object> slot uses the default body; the derived type introduces an
// unrelated `Accept(object)` and declares only IOwnDim<ArgumentException>, whose slot also falls
// back to the default. Neither slot is implemented by `OwnDerived.Accept`, so the answer is the
// default body — resolving the IOwnDim<object> entry from the receiver would wrongly find it.
class OwnDimBase : IOwnDim<object>
{
}

sealed class OwnDimDerived : OwnDimBase, IOwnDim<ArgumentException>
{
    public long Accept(object value) => 7;
}

// The base implements IOwn<object>; the derived type merely *hides* that implementation with a
// `new` method and does not declare the interface. The slot stays with the base, so dispatch
// through IOwn<ArgumentException> — which retargets onto the base's IOwn<object> entry — must
// reach the base's body.
class OwnHideBase : IOwn<object>
{
    public long Accept(object value) => 1;
}

sealed class OwnHideDerived : OwnHideBase
{
    public new long Accept(object value) => 2;
}

class Program
{
    static long CallDim(IOwnDim<ArgumentException> sink, ArgumentException value) => sink.Accept(value);
    static long CallOwn(IOwn<ArgumentException> sink, ArgumentException value) => sink.Accept(value);

    static int Main(string[] args)
    {
        ArgumentException e = new ArgumentException("boom");

        if (CallDim(new OwnDimDerived(), e) != 100) return 1;

        OwnHideDerived hidden = new OwnHideDerived();
        if (CallOwn(hidden, e) != 1) return 2;

        // The hidden method is of course still reachable by a direct call.
        if (hidden.Accept(e) != 2) return 3;

        return 0;
    }
}
