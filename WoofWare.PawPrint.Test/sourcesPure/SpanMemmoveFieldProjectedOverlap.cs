using System;
using System.Runtime.InteropServices;

public class Program
{
    // `MemoryMarshal.CreateSpan(ref s.A, 3)` produces a `Span<int>` whose
    // backing byref carries a `Field A` projection on the struct local `s`.
    // Slicing into two overlapping sub-spans yields two byrefs that share
    // the same root storage but whose `byteLocation` is undecidable under
    // PawPrint today, because `projectionByteOffset` (`CellAwareCopy.fs`)
    // doesn't fold `Field` projections into a flat byte offset.
    //
    // Before the overlap-undecidable fail-loud landed, the `Memmove` policy
    // silently took the forward loop and produced memcpy-style corruption
    // for overlapping field-projected copies. After the fix, the host
    // raises a clear diagnostic. Folding `Field` projections into flat byte
    // offsets is a separate enhancement.
    [StructLayout(LayoutKind.Sequential)]
    struct S
    {
        public int A;
        public int B;
        public int C;
    }

    public static int Main(string[] args)
    {
        S s = new S { A = 1, B = 2, C = 3 };
        Span<int> span = MemoryMarshal.CreateSpan(ref s.A, 3);
        span.Slice(0, 2).CopyTo(span.Slice(1));
        if (s.A != 1) return 10 + s.A;
        if (s.B != 1) return 20 + s.B;
        if (s.C != 2) return 30 + s.C;
        return 0;
    }
}
