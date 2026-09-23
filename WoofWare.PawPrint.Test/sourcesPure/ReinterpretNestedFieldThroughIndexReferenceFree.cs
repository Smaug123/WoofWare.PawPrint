using System;
using System.Runtime.CompilerServices;

// `buffer[k].M.I.R` over an `[InlineArray]` whose element holds no reference, so the storage has a
// byte image and every access is served bytewise rather than by naming a cell.
//
// The chain `[ReinterpretAs Elem; ByteOffset k*sizeof(Elem); Field M; Field I]` is the one
// `ReinterpretReadNestedFieldThroughIndex.cs` reads over reference-holding storage; what makes it
// hard does not depend on the element's contents, because the dispatch that serves it runs before
// anything looks at the storage. Covered: `ldfld` of a primitive and of a struct leaf, `stfld` of
// both, and `ldind.i4` / `stind.i4` through a `ref int` local.
public class TestReinterpretNestedFieldThroughIndexReferenceFree
{
    private struct Inner { public byte Q; public int R; }

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
        b[0] = new Elem { M = new Mid { Pad = 1, I = new Inner { Q = 3, R = 30 } }, Tag = 5 };
        b[1] = new Elem { M = new Mid { Pad = 2, I = new Inner { Q = 4, R = 40 } }, Tag = 6 };
        return b;
    }

    public static int Main(string[] argv)
    {
        Buffer b = Make();

        if (b[1].M.I.Q != 4 || b[1].M.I.R != 40) return 1;
        if (b[0].M.I.Q != 3 || b[0].M.I.R != 30) return 2;

        Inner copy = b[1].M.I;
        if (copy.Q != 4 || copy.R != 40) return 3;

        b[1].M.I.R = 99;
        b[1].M.I.Q = 7;
        if (b[1].M.I.R != 99 || b[1].M.I.Q != 7) return 4;
        if (b[1].M.Pad != 2 || b[1].Tag != 6 || b[0].M.I.R != 30) return 5;

        b[1].M.I = new Inner { Q = 1, R = 2 };
        if (b[1].M.I.Q != 1 || b[1].M.I.R != 2 || b[1].M.Pad != 2) return 6;

        ref int r = ref b[1].M.I.R;
        r = 55;
        int readBack = r;
        if (readBack != 55 || b[1].M.I.R != 55 || b[1].M.I.Q != 1) return 7;
        if (b[0].M.I.R != 30 || b[0].M.I.Q != 3) return 8;

        return 0;
    }
}
