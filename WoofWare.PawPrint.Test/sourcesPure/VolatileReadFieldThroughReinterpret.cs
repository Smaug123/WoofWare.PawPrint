using System.Runtime.CompilerServices;
using System.Threading;

public class TestVolatileReadFieldThroughReinterpret
{
    // A plain sequential struct of int fields, used to project a slice of an
    // `int[]` as a struct view.
    private struct ThreeInts
    {
        public int A; // offset 0
        public int B; // offset 4
        public int C; // offset 8
    }

    // Reading a field of an `Unsafe.As`-projected struct view through
    // `Volatile.Read` produces the byref projection chain
    //     [ReinterpretAs ThreeInts, Field <name>, ReinterpretAs VolatileInt32]
    // because the BCL lowers `Volatile.Read(ref int)` to
    //     Unsafe.As<int, VolatileInt32>(ref location).Value
    // The interpreter has to iteratively peel both trailing byte-view
    // segments — the bare `ReinterpretAs VolatileInt32` and then the
    // `[ReinterpretAs ThreeInts, Field <name>]` pair — into a byte offset
    // before dispatching to the array-element byte-view reader. This is the
    // same chain shape the CoreCLR CastCache walk produces via
    // `Volatile.Read(ref pEntry._version)`.
    public static int Main(string[] argv)
    {
        int[] arr = new int[6];
        arr[0] = 100;
        arr[1] = 200;
        arr[2] = 300;
        arr[3] = 400;
        arr[4] = 500;
        arr[5] = 600;

        ref ThreeInts view = ref Unsafe.As<int, ThreeInts>(ref arr[1]);

        int aRead = Volatile.Read(ref view.A);
        if (aRead != 200) return 1;

        int bRead = Volatile.Read(ref view.B);
        if (bRead != 300) return 2;

        int cRead = Volatile.Read(ref view.C);
        if (cRead != 400) return 3;

        // Same chain shape, but the struct view starts at a different
        // array index so the per-cell byte offsets differ. The peel must
        // not bake the array index into the offset — only the in-struct
        // field offset.
        ref ThreeInts view2 = ref Unsafe.As<int, ThreeInts>(ref arr[3]);

        int dRead = Volatile.Read(ref view2.A);
        if (dRead != 400) return 4;

        int eRead = Volatile.Read(ref view2.B);
        if (eRead != 500) return 5;

        int fRead = Volatile.Read(ref view2.C);
        if (fRead != 600) return 6;

        return 0;
    }
}
