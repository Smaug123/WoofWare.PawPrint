using System;
using System.Runtime.CompilerServices;

// An array is a legal accessor target. Modern CoreCLR gives arrays MethodTables rather than
// TypeDescs, so `ValidateTargetType` lets them through, and the constructor binds -- for a
// multi-dimensional array and for a single-dimensional one alike.
//
// Only the constructor binds: an array's other runtime-provided members are not candidates, so an
// accessor naming `Get` reports it missing. Measured on real .NET 10, which is also where the
// target type's rendered name comes from.
public class TestUnsafeAccessorArrayConstructor
{
    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[,] NewMultiDim(int n, int m);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[] NewSzArray(int n);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Get")]
    private static extern int ArrayGet(int[,] a, int i, int j);

    private static int Run()
    {
        int[,] rect = NewMultiDim(2, 3);
        if (rect == null) return 1;
        if (rect.GetLength(0) != 2) return 2;
        if (rect.GetLength(1) != 3) return 3;
        if (rect[1, 2] != 0) return 4;

        rect[1, 2] = 5;
        if (rect[1, 2] != 5) return 5;

        int[] flat = NewSzArray(4);
        if (flat == null) return 6;
        if (flat.Length != 4) return 7;

        // The runtime-provided members other than the constructor are not candidates.
        try
        {
            ArrayGet(rect, 1, 2);
            return 8;
        }
        catch (MissingMethodException e)
        {
            if (!e.Message.Contains("System.Int32[,].Get")) return 9;
        }

        return 0;
    }

    public static int Main() => Run();
}
