using System.Runtime.CompilerServices;

// `Unsafe.ByteOffset` between two byrefs whose byte displacement does not fit in an `int`.
//
// A byref's displacement from its root is native-int arithmetic in the real runtime, but PawPrint
// stores each `ByrefProjection.ByteOffset` step as an `int` -- so a chain carrying two large steps
// has a total no single step could hold. The walk that folds such a chain to a byte coordinate
// must therefore accumulate in 64 bits; folding in `int` wraps, and `Unsafe.ByteOffset` hands that
// wrapped number straight to the guest.
//
// **Why this is impure rather than a differential `sourcesPure` case.** The answers below are real
// .NET's -- measured directly, out of band -- but they cannot be *asserted* against real .NET,
// because the program is not deterministic there. Displacing a byref 2^31 bytes past a stack local
// is undefined behaviour: the byref is GC-reportable, and a collection that scans it while it
// points into unmapped memory faults. Measured on macOS arm64, this guest exited 0 nine times in
// ten and died with an `AccessViolationException` (SIGABRT, exit 134) the tenth. It never returned
// a *different* answer -- the arithmetic is forced, since `origin` sits far enough from either end
// of the address space that no address wraps -- so the expectation is sound; it is the oracle that
// is flaky, not the value. Under PawPrint the whole thing is deterministic, which is the point.
//
// Do not promote this to `sourcesPure`: the differential harness would fail roughly one run in ten
// for a reason that has nothing to do with the code under test.
//
// The cases are chosen for what an `int` fold does to each. The first totals 2^32 - 1, which wraps
// to -1: the sign flips, so the guest sees the two byrefs in the wrong order. The second totals
// exactly 2^32, which wraps to 0: the guest sees two byrefs at *the same address*, which is the
// shape that would let a wrapped coordinate answer an identity question the wrong way.
//
// Distances are compared as `long` rather than against an `nint` constant, because a
// `(nint)4294967295L` literal is a compile-time-checked narrowing that Roslyn warns on.
public class TestUnsafeByteOffsetInt32Overflow
{
    public struct Three
    {
        public byte A;
        public byte B;
        public byte C;
    }

    public static int Main(string[] argv)
    {
        Three s = default;
        ref byte origin = ref Unsafe.As<Three, byte>(ref s);

        // Chain: [ReinterpretAs Three; ByteOffset int.MaxValue; Field B; ReinterpretAs byte;
        //         ByteOffset int.MaxValue], totalling int.MaxValue + 1 + int.MaxValue = 2^32 - 1.
        ref Three viewB = ref Unsafe.As<byte, Three>(ref Unsafe.AddByteOffset(ref origin, (nint)int.MaxValue));
        ref byte farB = ref Unsafe.AddByteOffset(ref viewB.B, (nint)int.MaxValue);

        if ((long)Unsafe.ByteOffset(ref origin, ref farB) != 4294967295L)
            return 1;

        // The same shape through `C` instead, so the total is int.MaxValue + 2 + int.MaxValue = 2^32,
        // which an `int` fold collapses to zero distance.
        ref Three viewC = ref Unsafe.As<byte, Three>(ref Unsafe.AddByteOffset(ref origin, (nint)int.MaxValue));
        ref byte farC = ref Unsafe.AddByteOffset(ref viewC.C, (nint)int.MaxValue);

        if ((long)Unsafe.ByteOffset(ref origin, ref farC) != 4294967296L)
            return 2;

        // Reversing the arguments negates the answer. Asserted because the subtraction happens
        // after both chains are folded, so a fold that wrapped would be wrong in both directions
        // but not necessarily symmetrically.
        if ((long)Unsafe.ByteOffset(ref farB, ref origin) != -4294967295L)
            return 3;

        return 0;
    }
}
