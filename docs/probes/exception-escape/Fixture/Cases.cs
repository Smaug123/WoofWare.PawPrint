using System;

namespace Fixture;

/// Each method's name states the escaping set the analyser is expected to produce, so the
/// oracle is checkable by eye and by string comparison.
public static class Cases
{
    // Expect: System.InvalidOperationException
    public static void ThrowsDirectly()
    {
        throw new InvalidOperationException("boom");
    }

    // Expect: nothing (exact catch)
    public static void CaughtExactly()
    {
        try { ThrowsDirectly(); }
        catch (InvalidOperationException) { }
    }

    // Expect: nothing (base-class catch)
    public static void CaughtByBase()
    {
        try { ThrowsDirectly(); }
        catch (SystemException) { }
    }

    // Expect: System.InvalidOperationException (unrelated catch does not stop it)
    public static void UnrelatedCatch()
    {
        try { ThrowsDirectly(); }
        catch (ArgumentException) { }
    }

    // Expect: System.InvalidOperationException (finally does not stop propagation)
    public static void FinallyDoesNotCatch()
    {
        try { ThrowsDirectly(); }
        finally { GC.KeepAlive(null); }
    }

    // Expect: System.InvalidOperationException (transitive, one hop)
    public static void PropagatesOneHop()
    {
        UnrelatedCatch();
    }

    // Expect: System.InvalidOperationException, System.FormatException
    public static void TwoSources(bool b)
    {
        if (b) { ThrowsDirectly(); }
        else { throw new FormatException(); }
    }

    // Expect: nothing (the catch is outside, and covers both)
    public static void CatchesBoth(bool b)
    {
        try { TwoSources(b); }
        catch (Exception) { }
    }

    // Expect: nothing at all — a leaf that cannot raise.
    public static int Leaf(int a) => a;

    // Expect: System.InvalidOperationException only once round the recursion.
    public static void Recursive(int n)
    {
        if (n <= 0) { ThrowsDirectly(); }
        else { Recursive(n - 1); }
    }

    // Expect: UNKNOWN — a rethrow whose type this instrument does not track.
    public static void Rethrows()
    {
        try { ThrowsDirectly(); }
        catch (Exception) { throw; }
    }

    // Expect: Fixture.Derived — thrown, not caught by an unrelated local catch.
    public static void ThrowsLocalDerived()
    {
        throw new Derived();
    }

    // Expect: nothing — Derived : LocalBase, and the hierarchy is in this assembly, so a
    // single-assembly instrument can see the subtype relation.
    public static void CaughtByLocalBase()
    {
        try { ThrowsLocalDerived(); }
        catch (LocalBase) { }
    }
}

public class LocalBase : Exception { }

public class Derived : LocalBase { }

/// A type whose initializer throws. Calling `M` runs `.cctor` first (ECMA-335 I.8.9.5), so
/// `TypeInitializationException` can escape `CallsBoom` even though nothing in either body throws
/// it — and the `.cctor` is not the callee the call edge names, so an analyser following only the
/// named target sees nothing at all. That is the shape a `call` entry carrying
/// `TypeInitialization` exists for.
public static class Boom
{
    public static readonly int Value = int.Parse("not a number");

    public static int M() => Value;
}

public static class CctorCases
{
    public static int CallsBoom() => Boom.M();
}

public class Shadowed
{
    public int Field;
}

public static class ShadowCases
{
    /// A `catch` for a *locally declared* `System.NullReferenceException`, which is a different
    /// type from the one the runtime throws for a null dereference. The clause must not absorb the
    /// real fault. An analysis that canonicalised faults by name would say it does — and this is
    /// not a hypothetical shape when the analysis is pointed at a package somebody else wrote.
    public static int DereferencesNull(Shadowed s)
    {
        try { return s.Field; }
        catch (System.NullReferenceException) { return -1; }
    }
}
