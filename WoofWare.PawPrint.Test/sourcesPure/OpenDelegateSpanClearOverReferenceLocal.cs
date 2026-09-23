using System;

// An open delegate over `Span<object>.Clear`, a member with no `[Intrinsic]` of its own on a type
// carrying a type-level one, applied to a span over a single reference-typed local.
//
// A delegate enters its target through the target's entry point, where only the target's own
// method-level `[Intrinsic]` would let the JIT or VM treat it specially; real .NET runs the IL.
// PawPrint serves `Span<T>.Clear` itself whichever way it is reached. That is equivalent to the
// IL, and it matters here: the IL clears through `SpanHelpers.ClearWithReferences`, whose
// pointer-sized store over a reference-typed local PawPrint does not model.

class Program
{
    delegate void ClearFn(ref Span<object> span);

    static int Main(string[] args)
    {
        object local = "occupied";
        Span<object> span = new Span<object>(ref local);

        ClearFn clear = typeof(Span<object>).GetMethod("Clear", Type.EmptyTypes).CreateDelegate<ClearFn>();
        clear(ref span);

        if (local != null)
        {
            return 1;
        }

        return 0;
    }
}
