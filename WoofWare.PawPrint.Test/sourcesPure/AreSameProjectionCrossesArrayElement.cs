using System;
using System.Runtime.CompilerServices;

namespace AreSameProjectionCrossesArrayElementTest
{
    struct Pair
    {
        public int X;
        public int Y;
    }

    // The guest that shows root disjointness is not sufficient on its own.
    //
    // `a[0].Y` advanced by 4 bytes IS `a[1]`, and real .NET's `Unsafe.AreSame` says so. But
    // the two byrefs keep *different* `ByrefRoot.ArrayElement` roots: the intervening `Field`
    // stops the trailing cursor folding into the element index the way it would on a bare
    // element byref. Distinct array elements are disjoint, so comparison used to conclude the
    // addresses differ and answer `false` — measured, returning 1.
    //
    // That conclusion does not follow. Elements being disjoint says nothing once a projection
    // can walk out of the element it started from, so PawPrint now refuses instead.
    //
    // This is what the sibling `AreSame*` guests do not cover: both of those compare byrefs
    // that stay within one root, whereas this one is about displacement crossing between
    // roots. Un-park all of them together — they want the same thing, which is field-offset
    // layout available at the comparison.
    class Program
    {
        static int Main(string[] args)
        {
            Pair[] a = new Pair[2];

            ref byte fromElement0 = ref Unsafe.AddByteOffset(ref Unsafe.As<int, byte>(ref a[0].Y), (nint)4);
            ref byte fromElement1 = ref Unsafe.As<Pair, byte>(ref a[1]);

            if (!Unsafe.AreSame(ref fromElement0, ref fromElement1))
            {
                return 1;
            }

            // The aliasing is real, not merely nominal: writing through one is visible
            // through the other.
            fromElement1 = 42;

            if (fromElement0 != 42)
            {
                return 2;
            }

            return 0;
        }
    }
}
