using System;
using System.Runtime.InteropServices;

// Explicit layout may put two *reference* fields at the same offset: the GC sees one pointer slot,
// so the type loads. `ValueType_GetHashCodeStrategy` then walks to the first declared field, finds
// the aliased reference non-null, and reports `ReferenceField` — so the hash tracks whichever
// object the slot holds.
public class Program
{
    private sealed class Ref
    {
        private readonly int hash;

        public Ref(int hash)
        {
            this.hash = hash;
        }

        public override int GetHashCode() => hash;

        public override bool Equals(object obj) => obj is Ref other && other.hash == hash;
    }

    [StructLayout(LayoutKind.Explicit)]
    private struct OverlappingRefs
    {
        [FieldOffset(0)]
        public object A;

        [FieldOffset(0)]
        public Ref B;
    }

    public static int Main(string[] args)
    {
        OverlappingRefs seven1 = default;
        seven1.B = new Ref(7);
        OverlappingRefs seven2 = default;
        seven2.B = new Ref(7);
        if (seven1.GetHashCode() != seven2.GetHashCode())
        {
            return 1;
        }

        OverlappingRefs eight = default;
        eight.B = new Ref(8);
        if (seven1.GetHashCode() == eight.GetHashCode())
        {
            return 2;
        }

        // Both aliases null: no field contributes, which is a different hash again.
        OverlappingRefs empty = default;
        if (empty.GetHashCode() == seven1.GetHashCode())
        {
            return 3;
        }

        // Writing through the other alias is visible through this one: they are one slot.
        OverlappingRefs viaA = default;
        viaA.A = new Ref(7);
        if (viaA.GetHashCode() != seven1.GetHashCode())
        {
            return 4;
        }

        return 0;
    }
}
