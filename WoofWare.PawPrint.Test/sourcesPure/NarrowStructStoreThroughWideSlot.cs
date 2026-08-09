using System.Runtime.InteropServices;

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
            return 3;

        if (viaOffset.B != 5)
            return 4;

        return 0;
    }
}
