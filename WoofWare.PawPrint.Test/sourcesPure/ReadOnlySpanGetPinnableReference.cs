using System;

namespace ReadOnlySpanGetPinnableReference
{
    // Exercises ReadOnlySpan<T>.GetPinnableReference() and Span<T>.GetPinnableReference().
    // Both methods are JIT-intrinsics on real .NET, but their IL bodies just allocate
    // a null managed pointer, branch on _length, optionally overwrite the pointer with
    // _reference, and return.  PawPrint already models all those primitives, so the
    // managed body is safe to run; this test pins the contract.
    public class Program
    {
        public static int Main(string[] args)
        {
            // Non-empty ReadOnlySpan: pinnable reference must resolve to element 0.
            int[] backing = new int[] { 42, 100 };
            ReadOnlySpan<int> ros = backing;
            ref readonly int rosRef = ref ros.GetPinnableReference();
            if (rosRef != 42) return 1;

            // Non-empty Span: pinnable reference must resolve to element 0.
            Span<int> span = backing;
            ref int spanRef = ref span.GetPinnableReference();
            if (spanRef != 42) return 2;

            // Mutating through the pinnable reference flows back into the backing array
            // (sanity-check: this is a real byref, not a copy).
            spanRef = 7;
            if (backing[0] != 7) return 3;

            // Empty ReadOnlySpan: GetPinnableReference returns a null ref. The caller is
            // expected not to dereference it, so we only check that the call itself
            // succeeds and produces something we can hand to Unsafe.IsNullRef.
            ReadOnlySpan<int> empty = default;
            ref readonly int emptyRef = ref empty.GetPinnableReference();
            if (!System.Runtime.CompilerServices.Unsafe.IsNullRef(in emptyRef)) return 4;

            return 0;
        }
    }
}
