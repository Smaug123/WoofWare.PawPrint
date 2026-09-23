// Interface dispatch reads a per-type dispatch map, built the way CoreCLR's MethodTableBuilder
// builds it, and each case below is a rule of that construction which a name-and-signature walk
// from the receiver gets wrong. `TestInterfaceDispatchMap` checks the same rules against
// `Type.GetInterfaceMap` over a large corpus; this file checks that a real call lands where the
// map says.

using System;

interface ISink<in T> { long Accept(T value); }
interface IDefault { long M() => 100; }

// A variance-compatible entry on a derived type beats an exact entry on its base: each level of the
// chain is asked for an exact entry and then a variant one before the next level is consulted.
class ExactBase : ISink<ArgumentException> { public long Accept(ArgumentException value) => 1; }
sealed class VariantDerived : ExactBase, ISink<Exception> { public long Accept(Exception value) => 2; }

// A type that declares an interface may take the slot from a method its parent introduced, when
// neither it nor the parent implements the interface.
class Introducer { public virtual long M() => 3; }
class Declarer : Introducer, IDefault { }
sealed class Overrider : Declarer { public override long M() => 4; }

// Re-declaring an inherited interface re-implements it from the type's own methods only; a slot it
// has no method for keeps the parent's implementation.
class Implementer : IDefault { public long M() => 5; }
sealed class Redeclarer : Implementer, IDefault { }

// Under an abstract parent, a slot left to a default body is taken by a public virtual method the
// derived type declares, even though the derived type does not list the interface.
abstract class AbstractLeavesDefault : IDefault { }
class TakesDefaultSlot : AbstractLeavesDefault { public virtual long M() => 6; }
// ... but not under a concrete parent.
class ConcreteLeavesDefault : IDefault { }
class DoesNotTakeSlot : ConcreteLeavesDefault { public virtual long M() => 7; }

// Two entries that only coincide once closed: `Twice<int>` holds `ISink<int>` both as its parent's
// `ISink<T>` and as its own `ISink<int>`, and its own entry is found first.
class Overloads<T> : ISink<T> { public long Accept(int value) => 8; public long Accept(T value) => 9; }
sealed class Twice<T> : Overloads<T>, ISink<int> { }

class Program
{
    static long Sink(ISink<ArgumentException> sink) => sink.Accept(null);
    static long SinkInt(ISink<int> sink) => sink.Accept(0);
    static long Default(IDefault d) => d.M();

    static int Main(string[] args)
    {
        if (Sink(new VariantDerived()) != 2) return 1;
        if (Sink(new ExactBase()) != 1) return 2;

        if (Default(new Declarer()) != 3) return 3;
        if (Default(new Overrider()) != 4) return 4;

        if (Default(new Redeclarer()) != 5) return 5;

        if (Default(new TakesDefaultSlot()) != 6) return 6;
        if (Default(new DoesNotTakeSlot()) != 100) return 7;

        if (SinkInt(new Twice<int>()) != 8) return 8;
        if (SinkInt(new Overloads<int>()) != 9) return 9;

        return 0;
    }
}
