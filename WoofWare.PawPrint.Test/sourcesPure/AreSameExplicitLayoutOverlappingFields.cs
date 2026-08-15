using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace AreSameExplicitLayoutOverlappingFieldsTest
{
    [StructLayout(LayoutKind.Explicit)]
    struct U
    {
        [FieldOffset(0)]
        public int A;

        [FieldOffset(0)]
        public int B;
    }

    // Two *distinct* fields deliberately laid out on one address. Real .NET's
    // `Unsafe.AreSame(ref u.A, ref u.B)` is therefore `true`.
    //
    // The byrefs are `[Field A]` and `[Field B]` over one root, and whether those alias is
    // decided entirely by the declaring type's field-offset table. Structural comparison does
    // not carry that table, so it cannot answer: `false` is wrong here, and `true` would be
    // wrong for the sequential struct that produces an identical pair of chains. It therefore
    // defers, and `StorageLocation.resolveCeq` resolves both sides to byte coordinates in one
    // container and compares them — 0 and 0.
    //
    // This is the counterexample to the obvious shortcut, and it is why the structural
    // comparison declines *every* field divergence rather than just the prefix ones:
    // "distinct fields occupy disjoint extents" is true for sequential and auto layout but
    // not for explicit. Measured rather than assumed — before the deferral existed this
    // returned 1, and the shape reaches `Field` projections rather than collapsing to a byte
    // range, contrary to what the byte-backed explicit-layout tests might suggest.
    //
    // Exercises the *projection* arm. `AreSameHeapFieldsOverlappingExplicitLayout.cs` is the
    // same aliasing through the *root* arm.
    class Program
    {
        static int Main(string[] args)
        {
            U u = default;
            u.A = 7;

            if (!Unsafe.AreSame(ref u.A, ref u.B))
            {
                return 1;
            }

            // The aliasing is real, not merely nominal: writing through one field is visible
            // through the other.
            u.B = 9;

            if (u.A != 9)
            {
                return 2;
            }

            return 0;
        }
    }
}
