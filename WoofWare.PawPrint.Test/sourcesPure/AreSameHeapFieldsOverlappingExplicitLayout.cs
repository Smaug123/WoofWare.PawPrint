using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace AreSameHeapFieldsOverlappingExplicitLayoutTest
{
    [StructLayout(LayoutKind.Explicit)]
    class C
    {
        [FieldOffset(0)]
        public int A;

        [FieldOffset(0)]
        public int B;
    }

    // The heap-object counterpart of `AreSameExplicitLayoutOverlappingFields.cs`: explicit
    // layout on a *class* rather than a struct, so `ldflda` produces two byrefs with
    // different `ByrefRoot.HeapObjectField` roots rather than two `Field` projections over
    // one root. Real .NET's `Unsafe.AreSame(ref c.A, ref c.B)` is `true`.
    //
    // Distinct roots are ordinarily distinct storage, and comparison used to answer `false`
    // on that basis — measured, returning 1 before the refusal existed. But "different root"
    // is a fact about how each byref was built, not about where it points, and two fields of
    // one object can share an address. So this pair is refused too.
    //
    // Kept separate from the struct sibling because it exercises a different arm: this one is
    // about *roots* being wrongly treated as disjoint storage, that one about *projections*.
    // A fix for one does not automatically cover the other.
    //
    // Un-park together with the other two `AreSame*` guests: all three want field-offset
    // layout at the comparison.
    class Program
    {
        static int Main(string[] args)
        {
            C c = new C ();
            c.A = 7;

            if (!Unsafe.AreSame(ref c.A, ref c.B))
            {
                return 1;
            }

            // The aliasing is real, not merely nominal.
            c.B = 9;

            if (c.A != 9)
            {
                return 2;
            }

            return 0;
        }
    }
}
