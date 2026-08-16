using System;
using System.Runtime.InteropServices;

public class Program
{
    // `MemoryMarshal.CreateSpan(ref s.A, 3)` produces a `Span<int>` whose
    // backing byref carries a `Field A` projection on the struct local `s`.
    // Slicing into two overlapping sub-spans yields two byrefs sharing the
    // same root storage; `StorageLocation.byteLocation` folds the `Field`
    // projection into a flat byte offset, so the overlap is decided by
    // arithmetic and `Memmove` takes the backward loop rather than
    // corrupting memcpy-style. The value assertions below are what a
    // forward loop would get wrong.
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
