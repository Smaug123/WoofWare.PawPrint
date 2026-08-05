// Contravariant dispatch onto a STATIC ABSTRACT interface member: `IStatic<in T>` implemented
// at `object`, dispatched through `IStatic<ArgumentException>` by a `constrained.call` from a
// generic method whose constraint is the call-site instantiation.
//
// This one passes on both sides of the variant-interface-map retarget, and that is the point of
// having it. A static virtual slot has no name-based matching to fall back on, so implementing
// one requires an explicit MethodImpl row — which the C# compiler emits even for a member
// written as an ordinary `public static` method. Resolution therefore goes through the
// already-variance-aware MethodImpl path (`findMatchingMethodImplBodies`, "Found concrete
// implementation from MethodImpl" in the debug log) and never reaches
// `tryRetargetToVariantInterfaceMapEntry`, which explicitly declines static members.
//
// That is load-bearing rather than incidental: CoreCLR guards its own first-compatible-entry
// shortcut on `!pInterfaceMD->IsStatic()`, so a static member keeps scanning for a conflict and
// can throw AmbiguousResolutionException — the tie-break the retarget applies would be wrong
// here. This test fails if a future change routes static interface dispatch through the
// retarget instead.

using System;

interface IStatic<in T>
{
    static abstract long Handle(T value, int count);
}

// Implemented at `object`, so dispatch through `IStatic<ArgumentException>` must reach a body
// whose first parameter is `object`.
sealed class ObjectHandler : IStatic<object>
{
    public static long Handle(object value, int count) => value is ArgumentException ? 100 + count : count;
}

// A second, unrelated implementer, so the call really is resolved per type argument rather than
// there being only one candidate in the whole program.
sealed class ExceptionHandler : IStatic<Exception>
{
    public static long Handle(Exception value, int count) => 200 + count;
}

class Program
{
    // Compiles to `constrained. !!T; call IStatic`1<ArgumentException>::Handle`.
    static long CallStatic<T>(ArgumentException value, int count)
        where T : IStatic<ArgumentException>
        => T.Handle(value, count);

    static int Main(string[] args)
    {
        ArgumentException e = new ArgumentException("boom");

        if (CallStatic<ObjectHandler>(e, 7) != 107) return 1;
        if (CallStatic<ExceptionHandler>(e, 7) != 207) return 2;

        // A null still coerces to the body's wider parameter type.
        if (CallStatic<ObjectHandler>(null, 3) != 3) return 3;

        return 0;
    }
}
