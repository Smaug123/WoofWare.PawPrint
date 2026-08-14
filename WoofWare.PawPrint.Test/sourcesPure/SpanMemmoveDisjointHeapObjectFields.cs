using System;
using System.Runtime.InteropServices;

public class Program
{
    // Two byrefs into different fields of the same class instance —
    // `ref box.A` and `ref box.B`, i.e. `HeapObjectField (box, A)` /
    // `HeapObjectField (box, B)` — resolve into the *same* storage container
    // (the one heap object), at the fields' layout offsets. They are not
    // "disjoint by construction": under `LayoutKind.Explicit` two fields of
    // one object can overlap, so disjointness here must be, and is, proved
    // by offset arithmetic (offsets 0 and 4, four bytes each). This guest
    // pins that the proof goes through: if field roots ever stop resolving
    // to a flat byte coordinate, `Span<int>.CopyTo` over distinct heap
    // fields would trip the undecidable-overlap fail-loud.
    class Box
    {
        public int A;
        public int B;
    }

    public static int Main(string[] args)
    {
        Box box = new Box { A = 123, B = 456 };

        Span<int> src = MemoryMarshal.CreateSpan(ref box.A, 1);
        Span<int> dest = MemoryMarshal.CreateSpan(ref box.B, 1);
        src.CopyTo(dest);

        if (box.A != 123) return 10 + box.A;
        if (box.B != 123) return 20 + box.B;
        return 0;
    }
}
