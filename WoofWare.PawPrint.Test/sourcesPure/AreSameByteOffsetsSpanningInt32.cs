using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace AreSameByteOffsetsSpanningInt32Test
{
    [StructLayout(LayoutKind.Explicit)]
    struct S
    {
        [FieldOffset(0)]
        public byte A;

        [FieldOffset(1)]
        public byte B;
    }

    // Byref equality decided from byte coordinates that span more than an int32.
    //
    // This is the case issue #993 named. `ref s.B` displaced by `int.MaxValue` is at byte
    // 2^31, and `ref s.A` displaced by `int.MinValue` is at byte -2^31: two addresses 2^32
    // apart, and real .NET's `Unsafe.AreSame` says so. Folded into a wrapping int32 both
    // become `int.MinValue`, and the comparison answers `true` — the same address for two
    // that are as far apart as the type can express.
    //
    // Two things have to hold for this guest to be right, and it fails if either regresses:
    // the projection walk must accumulate in int64 (#1014), and byref comparison must be
    // willing to *use* the resulting coordinate rather than refusing. Neither alone is
    // enough, which is why this lives here rather than beside #1014's own tests: while
    // comparison refused every chain that needed field offsets, no guest could observe the
    // width of the accumulator through `AreSame` at all.
    //
    // The offsets are applied in a single `AddByteOffset` each, deliberately. Chaining two
    // would not reach the walk: `ManagedPointerSource.appendProjection` coalesces adjacent
    // `ByteOffset` steps and that addition is checked, so it throws instead of wrapping. The
    // wrap is only reachable where a `Field` offset and a `ByteOffset` first meet, which is
    // inside the walk.
    class Program
    {
        static int Main(string[] args)
        {
            S s = default;

            ref byte farAboveB = ref Unsafe.AddByteOffset(ref s.B, (nint)int.MaxValue);
            ref byte farBelowA = ref Unsafe.AddByteOffset(ref s.A, (nint)int.MinValue);

            if (Unsafe.AreSame(ref farAboveB, ref farBelowA))
            {
                return 1;
            }

            // Control, so that the check above cannot pass merely because everything here
            // answers `false`: `A` advanced one byte is `B`, by the declared layout, and this
            // pair mixes a chain that carries a byte cursor with one that does not.
            ref byte aPlusOne = ref Unsafe.AddByteOffset(ref s.A, (nint)1);

            if (!Unsafe.AreSame(ref aPlusOne, ref s.B))
            {
                return 2;
            }

            // A second control at the boundary itself: displacing *both* fields by the same
            // large amount preserves the one-byte gap between them, so these stay distinct.
            // Under a wrapping accumulator the two would be 2147483647 and -2147483648, which
            // is also unequal — so this control does not catch the bug, and is here to pin
            // that the widening did not collapse distinct large coordinates instead.
            ref byte farAboveA = ref Unsafe.AddByteOffset(ref s.A, (nint)int.MaxValue);

            if (Unsafe.AreSame(ref farAboveA, ref farAboveB))
            {
                return 3;
            }

            return 0;
        }
    }
}
