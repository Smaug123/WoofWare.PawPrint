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
    // PawPrint refuses the comparison. Its byrefs are `[Field A]` and `[Field B]` over the
    // same root, and whether those alias is decided entirely by the declaring type's
    // field-offset table, which byref comparison does not carry. It cannot answer `false`
    // — that is wrong here — and it cannot answer `true`, which would be wrong for the
    // sequential struct that produces an identical pair of chains.
    //
    // This is the counterexample to the obvious shortcut, and it is why `ceqNormalised`
    // refuses *every* field divergence rather than just the prefix ones: "distinct fields
    // occupy disjoint extents" is true for sequential and auto layout but not for explicit.
    // Measured rather than assumed — before the refusal existed this returned 1, and the
    // shape reaches `Field` projections rather than collapsing to a byte range, contrary to
    // what the byte-backed explicit-layout tests might suggest.
    //
    // Same root cause as `AreSameFirstFieldVersusReinterpretedWhole.cs`: deciding needs
    // field offsets. Un-park both together.
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
