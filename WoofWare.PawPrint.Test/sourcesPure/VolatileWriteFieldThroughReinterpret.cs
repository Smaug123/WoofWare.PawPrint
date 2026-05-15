using System.Runtime.CompilerServices;
using System.Threading;

public class TestVolatileWriteFieldThroughReinterpret
{
    // Mirror of VolatileReadFieldThroughReinterpret: a plain sequential struct
    // of int fields, projected over an `int[]` via `Unsafe.As`.
    private struct ThreeInts
    {
        public int A; // offset 0
        public int B; // offset 4
        public int C; // offset 8
    }

    // Writing a field of an `Unsafe.As`-projected struct view through
    // `Volatile.Write` produces the byref projection chain
    //     [ReinterpretAs ThreeInts, Field <name>, ReinterpretAs VolatileInt32]
    // because the BCL lowers `Volatile.Write(ref int, int)` to
    //     Unsafe.As<int, VolatileInt32>(ref location).Value = value
    // The write-side fold has to iteratively peel both trailing byte-view
    // segments — the bare `ReinterpretAs VolatileInt32` and the
    // `[ReinterpretAs ThreeInts, Field <name>]` pair — into a byte offset
    // before dispatching to the array-element byte-view writer. Without the
    // iterative peel, the residual `[ReinterpretAs ThreeInts, Field <name>]`
    // is left unhandled and the write fails.
    public static int Main(string[] argv)
    {
        int[] arr = new int[6];

        ref ThreeInts view = ref Unsafe.As<int, ThreeInts>(ref arr[1]);

        Volatile.Write(ref view.A, 200);
        Volatile.Write(ref view.B, 300);
        Volatile.Write(ref view.C, 400);

        if (arr[0] != 0) return 1;
        if (arr[1] != 200) return 2;
        if (arr[2] != 300) return 3;
        if (arr[3] != 400) return 4;
        if (arr[4] != 0) return 5;
        if (arr[5] != 0) return 6;

        // Same chain shape, but the struct view starts at a different
        // array index so the per-cell byte offsets differ. The peel must
        // not bake the array index into the offset — only the in-struct
        // field offset.
        ref ThreeInts view2 = ref Unsafe.As<int, ThreeInts>(ref arr[3]);

        Volatile.Write(ref view2.A, 500);
        Volatile.Write(ref view2.B, 600);
        Volatile.Write(ref view2.C, 700);

        if (arr[0] != 0) return 7;
        if (arr[1] != 200) return 8;
        if (arr[2] != 300) return 9;
        if (arr[3] != 500) return 10;
        if (arr[4] != 600) return 11;
        if (arr[5] != 700) return 12;

        return 0;
    }
}
