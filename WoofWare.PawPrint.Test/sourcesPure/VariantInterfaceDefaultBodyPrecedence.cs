// Interface-map order is the tie-break between *equally good* variance-compatible entries, not
// the whole precedence rule: a real implementation beats a default interface body regardless of
// which entry each came from.
//
// `DimSink` declares `IDim<object>` first, but supplies no body for its slot — that slot falls
// back to `IDim<in T>`'s default `Accept`. It declares `IDim<Exception>` second and *does*
// implement that slot with `Accept(Exception)`. A call through `IDim<ArgumentException>` is
// variance-compatible with both, and the real runtime runs the class method (200), not the
// default body (100), even though the default body's entry comes first.
//
// CoreCLR gets this ordering structurally: its dispatch map — which records class
// implementations — is consulted for every variance-compatible entry before
// `MethodTable::FindDefaultInterfaceImplementation` runs at all. PawPrint resolves per entry, so
// `tryResolveVirtualImplementation` has to resolve every compatible entry and compare afterwards.
// Selecting the first entry outright gets this case wrong.
//
// `VariantInterfaceMapOrder.cs` covers the complementary case, where all the compatible entries
// have real implementations and first-declared genuinely does win.

using System;

interface IDim<in T>
{
    long Accept(T value) => 100;
}

sealed class DimSink : IDim<object>, IDim<Exception>
{
    public long Accept(Exception value) => 200;
}

// The same shape with the declaration order reversed, so the answer cannot come from order.
sealed class DimSinkReversed : IDim<Exception>, IDim<object>
{
    public long Accept(Exception value) => 300;
}

// Both entries fall back to the default body: there is no real implementation to prefer, so the
// default body is the answer.
sealed class DimOnly : IDim<object>, IDim<Exception>
{
}

class Program
{
    static long CallIt(IDim<ArgumentException> sink, ArgumentException value) => sink.Accept(value);

    static int Main(string[] args)
    {
        ArgumentException e = new ArgumentException("boom");

        if (CallIt(new DimSink(), e) != 200) return 1;
        if (CallIt(new DimSinkReversed(), e) != 300) return 2;
        if (CallIt(new DimOnly(), e) != 100) return 3;

        return 0;
    }
}
