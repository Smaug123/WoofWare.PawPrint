using System.Runtime.InteropServices;

// A byref names a byte address; the width of an access comes from the access itself — the value
// stored, or the type loaded — never from the storage the byref is rooted at. A whole-slot
// `stobj Narrow` through a pointer to a `Wide` slot would both lose `wide.B` and restamp the
// slot's declared type from `Wide` to `Narrow`, making the next `wide.B` fail in
// `CliValueType.FindFieldById` rather than return a wrong number.
//
// Every store below is byte-addressable, which is the half of the rule a C# guest can reach: a
// value with no byte image (a struct of managed pointers) cannot be named in C# source at all, and
// is pinned by `TestNarrowByrefAccess.fs` instead. `ReflectionInvokeMethodMultipleArguments.cs`
// covers the imageless half end to end, since `MethodBaseInvoker` builds exactly such a struct.
public class NarrowStructStoreThroughWideSlot
{
    [StructLayout(LayoutKind.Sequential)]
    struct Wide
    {
        public int A;
        public int B;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct Narrow
    {
        public int A;
    }

    // A heap-object-field root: `fixed` over a field of a class gives a byref rooted at the field
    // rather than at a stack slot, which is a different arm of the write router.
    class Holder
    {
        public Wide Field;
    }

    private static Wide StaticWide;

    public static unsafe int Main(string[] argv)
    {
        // `stobj Narrow` through a pointer to a `Wide` slot must write only the first four
        // bytes. There is deliberately no pointer arithmetic here: the IL is `ldloca wide;
        // ... ; stobj Narrow`, so this is about the write path alone.
        Wide wide = default;
        wide.A = 1;
        wide.B = 2;

        Narrow* p = (Narrow*)&wide;
        *p = new Narrow
        {
            A = 3,
        };

        if (wide.A != 3)
            return 1;

        if (wide.B != 2)
            return 2;

        // The read direction of the same rule: `ldobj Narrow` through the same pointer must read
        // only the first four bytes rather than handing back the whole `Wide`.
        Narrow readBack = *p;

        if (readBack.A != 3)
            return 3;

        // The same store reached by a dynamically computed zero offset, which must behave
        // identically: `p + 0` is `p`.
        Wide viaOffset = default;
        viaOffset.A = 4;
        viaOffset.B = 5;

        Narrow* q = (Narrow*)&viaOffset;

        for (int i = 0; i < 1; i++)
        {
            *(q + i) = new Narrow
            {
                A = 6,
            };
        }

        if (viaOffset.A != 6)
            return 4;

        if (viaOffset.B != 5)
            return 5;

        // `initobj Narrow` through the same pointer: zeroing a narrower type must zero only the
        // bytes it covers. This is a distinct opcode from `stobj` reaching the same writer.
        *q = default;

        if (viaOffset.A != 0)
            return 6;

        if (viaOffset.B != 5)
            return 7;

        // A heap-object-field root rather than a stack slot.
        Holder holder = new Holder();
        holder.Field.A = 7;
        holder.Field.B = 8;

        fixed (Wide* h = &holder.Field)
        {
            *(Narrow*)h = new Narrow
            {
                A = 9,
            };
        }

        if (holder.Field.A != 9)
            return 8;

        if (holder.Field.B != 8)
            return 9;

        // A static-field root. `writeRootValue` reaches statics through a different accessor again,
        // and a static slot is the one root whose extent has to be asked for rather than read
        // (an uninitialised one has no value at all), so it is worth its own case.
        StaticWide.A = 15;
        StaticWide.B = 16;

        fixed (Wide* s = &StaticWide)
        {
            *(Narrow*)s = new Narrow
            {
                A = 17,
            };
        }

        if (StaticWide.A != 17)
            return 14;

        if (StaticWide.B != 16)
            return 15;

        // An array-element root.
        Wide[] array = new Wide[2];
        array[0].A = 10;
        array[0].B = 11;
        array[1].A = 12;
        array[1].B = 13;

        fixed (Wide* a = array)
        {
            *(Narrow*)a = new Narrow
            {
                A = 14,
            };
        }

        if (array[0].A != 14)
            return 10;

        if (array[0].B != 11)
            return 11;

        // The element after the one written must be untouched: a whole-slot write through an
        // array-element byref would have replaced element 0 alone, but a *byte* write that got
        // the extent wrong could run past it.
        if (array[1].A != 12)
            return 16;

        if (array[1].B != 13)
            return 17;

        return 0;
    }
}
