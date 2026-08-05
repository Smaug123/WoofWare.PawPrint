// When a receiver declares several instantiations of a variant interface that are *all*
// variance-compatible with the call site, dispatch is not ambiguous: the runtime picks the
// first entry in the receiver's interface map, and swapping the declaration order swaps the
// answer. There is no AmbiguousImplementationException here — these are ordinary programs.
//
// This pins the tie-break rule that `tryRetargetToVariantInterfaceMapEntry` implements, and it
// pins the interface-map *order* itself, which is what makes that rule well-defined:
//
//   * a type's own declared interfaces come before its base class's, in metadata order;
//   * an interface is expanded through its own parents where it is declared.
//
// Every expectation below is checked against the real runtime as well as PawPrint, because
// `sourcesPure` cases are differential.

using System;

interface ISink<in T> { long Accept(T value); }

// --- Two directly-declared instantiations, in each order. -------------------------------

sealed class BothObjFirst : ISink<object>, ISink<Exception>
{
    public long Accept(object value) => 1;
    public long Accept(Exception value) => 2;
}

sealed class BothExcFirst : ISink<Exception>, ISink<object>
{
    public long Accept(object value) => 3;
    public long Accept(Exception value) => 4;
}

// --- One entry from the base class, one from the derived class, in each order. -----------

class BaseObj : ISink<object>
{
    public long Accept(object value) => 5;
}

sealed class DerivedExc : BaseObj, ISink<Exception>
{
    public long Accept(Exception value) => 6;
}

class BaseExc : ISink<Exception>
{
    public long Accept(Exception value) => 7;
}

sealed class DerivedObj : BaseExc, ISink<object>
{
    public long Accept(object value) => 8;
}

// --- One entry arriving transitively through a parent interface, in each order. -----------

interface IChild<in T> : ISink<T> { }

sealed class ChildFirst : IChild<Exception>, ISink<object>
{
    public long Accept(Exception value) => 9;
    public long Accept(object value) => 10;
}

sealed class ChildSecond : ISink<object>, IChild<Exception>
{
    public long Accept(Exception value) => 11;
    public long Accept(object value) => 12;
}

// --- An exact entry alongside a variance-compatible one: exact must win. ------------------

sealed class ExactAndWider : ISink<object>, ISink<ArgumentException>
{
    public long Accept(object value) => 13;
    public long Accept(ArgumentException value) => 14;
}

class Program
{
    // Kept out of line so the call really is a virtual dispatch through ISink<ArgumentException>
    // rather than something the JIT can fold at the call site.
    static long CallIt(ISink<ArgumentException> sink, ArgumentException value) => sink.Accept(value);

    static int Main(string[] args)
    {
        ArgumentException e = new ArgumentException("boom");

        // First-declared wins, so the two orders disagree.
        if (CallIt(new BothObjFirst(), e) != 1) return 1;
        if (CallIt(new BothExcFirst(), e) != 4) return 2;

        // The derived type's own interfaces precede the base class's, in both orders.
        if (CallIt(new DerivedExc(), e) != 6) return 3;
        if (CallIt(new DerivedObj(), e) != 8) return 4;

        // A parent interface is expanded where its child is declared, so IChild<Exception>
        // contributes ISink<Exception> at IChild's position in the list.
        if (CallIt(new ChildFirst(), e) != 9) return 5;
        if (CallIt(new ChildSecond(), e) != 12) return 6;

        // An exact instantiation is not subject to the tie-break at all: it is found by
        // ordinary dispatch, even though it is declared second and the first entry is also
        // variance-compatible.
        if (CallIt(new ExactAndWider(), e) != 14) return 7;

        return 0;
    }
}
