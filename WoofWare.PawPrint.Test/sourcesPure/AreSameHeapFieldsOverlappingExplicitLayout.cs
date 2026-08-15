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
    // on that basis — measured, returning 1 before the deferral existed. But "different root"
    // is a fact about how each byref was built, not about where it points, and two fields of
    // one object can share an address. So structural comparison declines this pair, and
    // `StorageLocation.resolveCeq` decides it: a `HeapObjectField` root resolves as the whole
    // object with a leading `Field` projection, so both sides land in one
    // `ByteStorageIdentity.HeapObject` at the same byte coordinate. That re-rooting is what
    // makes this reachable — while a field root was its own container, the two identities were
    // unequal and `resolveCeq` would have declined rather than decided.
    //
    // Kept separate from the struct sibling because it exercises a different arm: this one is
    // about *roots* being wrongly treated as disjoint storage, that one about *projections*.
    // A fix for one does not automatically cover the other.
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
