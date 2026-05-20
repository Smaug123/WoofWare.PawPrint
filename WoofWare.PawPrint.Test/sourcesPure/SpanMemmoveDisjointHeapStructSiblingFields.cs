using System;
using System.Runtime.InteropServices;

public class Program
{
    // Two byrefs that share a `HeapObjectField (box, s)` root but project
    // into distinct interior fields of the contained struct `S`. Before
    // `CellAwareCopy`'s overlap analyser folded `Field` projections into
    // a flat byte offset, both byrefs collapsed onto the same coarse
    // `SharedStorageKey.HeapObjectField (box, s)` bucket and the
    // undecidable-overlap fail-loud rejected this disjoint sibling-field
    // copy. After folding, `byteLocation` resolves precise byte offsets
    // (`offsetof(A within S)` vs `offsetof(B within S)`) under the
    // `ByteStorageIdentity.HeapObjectField (box, s)` storage, so overlap
    // is determined by offset arithmetic and the forward copy proceeds.
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
        box.s = new S { A = 11, B = 22, C = 33 };

        Span<int> src = MemoryMarshal.CreateSpan(ref box.s.A, 1);
        Span<int> dest = MemoryMarshal.CreateSpan(ref box.s.B, 1);
        src.CopyTo(dest);

        if (box.s.A != 11) return 10 + box.s.A;
        if (box.s.B != 11) return 20 + box.s.B;
        if (box.s.C != 33) return 30 + box.s.C;
        return 0;
    }
}
