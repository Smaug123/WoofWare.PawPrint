using System;
using System.Runtime.CompilerServices;

// Every access shape through a byref whose `ReinterpretAs` is followed by more than one `Field`:
// `buffer[k].M.I` is `[ReinterpretAs Elem; ByteOffset k*sizeof(Elem); Field M; Field I]`, two
// fields past the reinterpret, over an `[InlineArray]` whose element holds a reference.
//
// `ReinterpretReadNestedFieldThroughIndex.cs` covers `ldfld` off such a chain. The instructions
// here reach the byref layer through different entry points, and each one has to serve the chain
// from where it lands rather than from the reinterpret target:
//
//  * `stfld` of a byte leaf, whose value has a byte image, and of a reference leaf and a
//    reference-holding struct leaf, whose values do not: the two kinds go to different writers;
//  * `ldind.ref` / `stind.ref` through a `ref Box` local, and `ldobj` / `stobj` through a
//    `ref Inner` local, which read and write the byref itself rather than a field of it.
//
// Slot 1 carries a `ByteOffset`; slot 0 does not, and is kept as the control. Every write is
// followed by checks that its neighbours survived it, because a write that lands on the right cell
// by replacing its whole parent reads back correctly at the cell and wrongly next to it.
public class TestReinterpretWriteNestedFieldThroughIndex
{
    private sealed class Box { public int V; }

    private struct Inner { public byte Q; public Box P; }

    private struct Mid { public byte Pad; public Inner I; }

    private struct Elem { public Mid M; public byte Tag; }

    [InlineArray(2)]
    private struct Buffer
    {
        private Elem _item;
    }

    private static Buffer Make()
    {
        Buffer b = default;
        b[0] = new Elem { M = new Mid { Pad = 1, I = new Inner { Q = 3, P = new Box { V = 30 } } }, Tag = 5 };
        b[1] = new Elem { M = new Mid { Pad = 2, I = new Inner { Q = 4, P = new Box { V = 40 } } }, Tag = 6 };
        return b;
    }

    // The fields of slot `k` that a write to `M.I` must not disturb, and all of the other slot.
    private static bool OthersIntact(ref Buffer b, int k)
    {
        int other = 1 - k;
        if (b[k].Tag != 5 + k || b[k].M.Pad != 1 + k) return false;
        if (b[other].Tag != 5 + other || b[other].M.Pad != 1 + other) return false;
        if (b[other].M.I.Q != 3 + other || b[other].M.I.P.V != 30 + 10 * other) return false;
        return true;
    }

    public static int Main(string[] argv)
    {
        for (int k = 0; k < 2; k++)
        {
            int fail = 10 * (k + 1);

            // Reads through a ref local: ldind.ref and ldobj.
            {
                Buffer b = Make();
                ref Box rp = ref b[k].M.I.P;
                Box read = rp;
                if (read.V != 30 + 10 * k) return fail + 1;
                ref Inner ri = ref b[k].M.I;
                Inner copy = ri;
                if (copy.Q != 3 + k || copy.P.V != 30 + 10 * k) return fail + 2;
            }

            // stfld of a byte leaf.
            {
                Buffer b = Make();
                b[k].M.I.Q = 9;
                if (b[k].M.I.Q != 9 || b[k].M.I.P.V != 30 + 10 * k) return fail + 3;
                if (!OthersIntact(ref b, k)) return fail + 4;
            }

            // stfld of a reference leaf.
            {
                Buffer b = Make();
                b[k].M.I.P = new Box { V = 77 };
                if (b[k].M.I.P.V != 77 || b[k].M.I.Q != 3 + k) return fail + 5;
                if (!OthersIntact(ref b, k)) return fail + 6;
            }

            // stfld of a reference-holding struct leaf.
            {
                Buffer b = Make();
                b[k].M.I = new Inner { Q = 8, P = new Box { V = 80 } };
                if (b[k].M.I.Q != 8 || b[k].M.I.P.V != 80) return fail + 7;
                if (!OthersIntact(ref b, k)) return fail + 8;
            }

            // stind.ref through a ref local.
            {
                Buffer b = Make();
                ref Box rp = ref b[k].M.I.P;
                rp = new Box { V = 88 };
                if (b[k].M.I.P.V != 88 || b[k].M.I.Q != 3 + k) return fail + 9;
                if (!OthersIntact(ref b, k)) return fail + 10;
            }

            // stobj through a ref local.
            {
                Buffer b = Make();
                ref Inner ri = ref b[k].M.I;
                ri = new Inner { Q = 7, P = new Box { V = 70 } };
                if (b[k].M.I.Q != 7 || b[k].M.I.P.V != 70) return fail + 11;
                if (!OthersIntact(ref b, k)) return fail + 12;
            }
        }

        return 0;
    }
}
