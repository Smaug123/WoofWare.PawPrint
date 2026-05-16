using System;

public class TestMultiDimensionalArrayAddress
{
    private static void IncrementByRef(ref int x) => x++;

    private static int ReadByRef(ref int x) => x;

    public static int TestAddress()
    {
        int[,] arr = new int[2, 3];

        // Initialise via Set so the layout is well-defined before we take addresses.
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                arr[i, j] = i * 10 + j;

        // `ref arr[1, 2]` forces the C# compiler to emit `call instance int32&
        // int32[0...,0...]::Address(int32, int32)`. Mutating via the byref must be
        // visible through subsequent Get on the same indices.
        IncrementByRef(ref arr[1, 2]);
        if (arr[1, 2] != 13) return 1;

        // Address must round-trip every (i, j) — wrong row-major flattening would
        // surface here as a value mismatch on an interior cell.
        for (int i = 0; i < 2; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                int viaByref = ReadByRef(ref arr[i, j]);
                int expected = i * 10 + j + (i == 1 && j == 2 ? 1 : 0);
                if (viaByref != expected) return 100 + i * 10 + j;
            }
        }

        // Write through Address, read through Get to confirm the byref aliases
        // the canonical backing store rather than producing a stale copy.
        ref int slot = ref arr[0, 1];
        slot = 999;
        if (arr[0, 1] != 999) return 2;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result = TestAddress();
        if (result != 0) return 8500 + result;

        return 0;
    }
}
