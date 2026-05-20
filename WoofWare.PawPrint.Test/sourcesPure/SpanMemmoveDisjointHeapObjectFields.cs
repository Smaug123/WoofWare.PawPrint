using System;
using System.Runtime.InteropServices;

public class Program
{
    // Two byrefs into different fields of the same class instance are
    // disjoint by construction: `ref box.A` and `ref box.B` are
    // `HeapObjectField (box, A)` / `HeapObjectField (box, B)` and cannot
    // alias. The shared-storage discriminator must distinguish them rather
    // than collapsing to a single per-instance bucket, or `Span<int>.CopyTo`
    // over distinct heap fields would trip the undecidable-overlap fail-loud.
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
