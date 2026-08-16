using System;
using System.Runtime.InteropServices;

public class Program
{
    // Heap-rooted analogue of `SpanMemmoveFieldProjectedOverlap.cs`: the
    // overlapping span is built over a struct stored as a field of a
    // class. The byrefs share a `HeapObjectField` root (same heap address,
    // same struct field), so the overlap is genuine but undecidable until
    // `Field` projections are folded into a flat byte offset.
    //
    // The undecidable-overlap fail-loud covers heap roots too, so this
    // raises a clear host-level diagnostic rather than silently taking
    // the forward loop (memcpy-style corruption).
    [StructLayout(LayoutKind.Sequential)]
    struct S
    {
        public int A;
        public int B;
        public int C;
    }

    class Box
    {
        public S s;
    }

    public static int Main(string[] args)
    {
        Box box = new Box ();
        box.s = new S { A = 1, B = 2, C = 3 };
        Span<int> span = MemoryMarshal.CreateSpan(ref box.s.A, 3);
        span.Slice(0, 2).CopyTo(span.Slice(1));
        if (box.s.A != 1) return 10 + box.s.A;
        if (box.s.B != 1) return 20 + box.s.B;
        if (box.s.C != 2) return 30 + box.s.C;
        return 0;
    }
}
