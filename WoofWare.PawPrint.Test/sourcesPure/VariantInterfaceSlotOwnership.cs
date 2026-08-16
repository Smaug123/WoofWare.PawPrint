// Variant dispatch onto an interface-map entry a *base class* contributed asks two questions
// with different answers, and `tryResolveVirtualImplementation` has to keep them apart.
//
// Which method implements the slot is settled at the entry's owner: only the owner's own methods
// and its bases' are eligible, never a method a more-derived type happens to introduce with a
// matching signature. (PawPrint's class walk matches implicit interface implementations by name
// and signature, so it cannot tell the difference on its own — hence resolving with the owner as
// the dispatch type rather than the receiver.)
//
// Which body that method lands on is then ordinary virtual dispatch from the receiver: an
// implementing method may be `virtual` or `abstract` and overridden further down. Answering the
// first question alone would run the base's body — or, for an `abstract` implementation, reach a
// method with no body at all.
//
// The complementary cases that PawPrint still gets wrong — where a derived type *does* need to
// take over an inherited slot without overriding it — are in
// `InterfaceSlotHiddenByDerivedMethod.cs`.

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

// The base implements the interface with a `virtual` method and the derived type `override`s it.
// Unlike the `new` case above, the override *is* the slot's body.
class OwnVirtualBase : IOwn<object>
{
    public virtual long Accept(object value) => 3;
}

class OwnVirtualMiddle : OwnVirtualBase
{
    public override long Accept(object value) => 4;
}

sealed class OwnVirtualDerived : OwnVirtualMiddle
{
    public override long Accept(object value) => 5;
}

// The implementing method is `abstract`, so resolving at the owner alone reaches a method with no
// body — the interpreter's abstract-method guard, not a wrong answer, if the override is missed.
abstract class OwnAbstractBase : IOwn<object>
{
    public abstract long Accept(object value);
}

sealed class OwnAbstractDerived : OwnAbstractBase
{
    public override long Accept(object value) => 6;
}

// Implicit implementation of an interface slot requires a public instance method
// (ECMA-335 II.12.2). A private or static same-signature method is an ordinary member that
// happens to collide, so both slots below keep the default body.
//
// The static case is not merely a wrong answer if it slips through: dispatching a static method
// where an instance one is expected leaves the receiver on the evaluation stack, and the damage
// surfaces somewhere else entirely as "method returned with more than one evaluation stack
// value".
sealed class OwnPrivateShadow : IOwnDim<ArgumentException>, IOwnDim<object>
{
    private long Accept(object value) => 8;

    // Keep the private method reachable so the compiler cannot discard it.
    public long CallPrivate() => Accept(null);
}

sealed class OwnStaticShadow : IOwnDim<ArgumentException>, IOwnDim<object>
{
    public static long Accept(object value) => 9;
}

// `protected` and `internal` are the other two sides of the same rule: the guard tests the whole
// accessibility field, not just "is it private", so both must also leave the slot alone. (Not
// sealed, because C# rejects a new protected member on a sealed type.)
class OwnProtectedShadow : IOwnDim<ArgumentException>, IOwnDim<object>
{
    protected long Accept(object value) => 10;

    public long CallProtected() => Accept(null);
}

sealed class OwnInternalShadow : IOwnDim<ArgumentException>, IOwnDim<object>
{
    internal long Accept(object value) => 11;
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

        // The hidden method is still reachable by a direct call.
        if (hidden.Accept(e) != 2) return 3;

        // An override is followed, through as many levels as there are.
        if (CallOwn(new OwnVirtualBase(), e) != 3) return 4;
        if (CallOwn(new OwnVirtualMiddle(), e) != 4) return 5;
        if (CallOwn(new OwnVirtualDerived(), e) != 5) return 6;

        if (CallOwn(new OwnAbstractDerived(), e) != 6) return 7;

        // Neither a private nor a static same-signature method implements the slot.
        OwnPrivateShadow priv = new OwnPrivateShadow();
        if (CallDim(priv, e) != 100) return 8;
        if (priv.CallPrivate() != 8) return 9;

        if (CallDim(new OwnStaticShadow(), e) != 100) return 10;
        if (OwnStaticShadow.Accept(null) != 9) return 11;

        OwnProtectedShadow prot = new OwnProtectedShadow();
        if (CallDim(prot, e) != 100) return 12;
        if (prot.CallProtected() != 10) return 13;

        OwnInternalShadow intl = new OwnInternalShadow();
        if (CallDim(intl, e) != 100) return 14;
        if (intl.Accept(null) != 11) return 15;

        return 0;
    }
}
