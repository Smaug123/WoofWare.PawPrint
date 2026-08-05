// Contravariant dispatch onto an implicit interface implementation, reached through
// `constrained.callvirt` on a value type rather than through an ordinary `callvirt` on a
// reference type.
//
// This is a distinct code path in the interpreter: `constrained.callvirt` on a value-type `T`
// first probes whether `T` itself supplies the method (ECMA-335 III.2.1 case 2), and that probe
// resolves with `walkBaseTypes = false`. Before the variant-interface-map retarget landed, the
// probe missed for exactly the same reason the reference-type case did, and the interpreter
// failed with
//
//   constrained.callvirt case 2: non-base method Accept had no direct value-type
//   implementation for type .StructSink
//
// The value type must also stay unboxed: the ECMA case-2 path calls the body with the managed
// pointer still serving as `this`, so mutations made by the callee are visible to the caller's
// local. `MutationsVisible` below fails if we fell through to the box-and-dispatch case instead.

using System;

interface ISink<in T> { long Accept(T value); }

struct StructSink : ISink<object>
{
    public long Count;
    public object Last;

    public long Accept(object value)
    {
        Count++;
        Last = value;
        return Count;
    }
}

class Program
{
    // The generic constraint is at ISink<ArgumentException>, while StructSink implements
    // ISink<object>; C# accepts this because contravariance makes the conversion an implicit
    // reference conversion. The call compiles to `constrained. !!T; callvirt
    // ISink`1<ArgumentException>::Accept`.
    static long CallConstrained<T>(ref T sink, ArgumentException value)
        where T : ISink<ArgumentException>
        => sink.Accept(value);

    static int Main(string[] args)
    {
        ArgumentException e = new ArgumentException("boom");

        StructSink sink = new StructSink();

        if (CallConstrained(ref sink, e) != 1) return 1;
        if (CallConstrained(ref sink, e) != 2) return 2;

        // Case 2 passes the managed pointer through unboxed, so the callee's writes landed in
        // our local. Had we boxed instead, Count would still be 0 here.
        if (sink.Count != 2) return 3;
        if (!ReferenceEquals(sink.Last, e)) return 4;

        return 0;
    }
}
