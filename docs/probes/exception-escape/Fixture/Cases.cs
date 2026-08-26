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
