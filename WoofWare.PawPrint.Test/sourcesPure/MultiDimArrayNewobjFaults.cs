using System;
using System.Runtime.CompilerServices;

// `new int[a, b]` is `newobj int32[,]::.ctor(int32, int32)`, which hands its arguments to
// CoreCLR's allocator with nothing managed in front of it, unlike `Array.CreateInstance`: a
// negative length is an `OverflowException`, and an over-long one or an overflowing element count
// an `OutOfMemoryException`, raised from the frame holding the `newobj`.
//
// An `[UnsafeAccessor]` bound to the same constructor reaches the same allocator, and must agree
// with `new` for every pair of lengths. The pairs that would allocate a large array on real .NET
// are left out.
public class TestMultiDimArrayNewobjFaults
{
    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern int[,] Construct(int a, int b);

    private static string Outcome(Func<int[,]> make)
    {
        try
        {
            int[,] made = make();
            return "made " + made.GetLength(0) + " by " + made.GetLength(1);
        }
        catch (Exception e)
        {
            return e.GetType().FullName + ": " + e.Message;
        }
    }

    // Opaque to the compiler, which refuses a constant negative length.
    private static int Negative() => -1;

    private static int Run()
    {
        try
        {
            int[,] unused = new int[Negative(), 2];
            return 1;
        }
        catch (OverflowException)
        {
        }

        try
        {
            int[,] unused = new int[0x7FFFFFC8, 0];
            return 2;
        }
        catch (OutOfMemoryException e)
        {
            if (e.Message != "Array dimensions exceeded supported range.") return 3;
        }

        int[] lengths = { -1, 0, 1, 3, 65536, 0x7FFFFFC7, 0x7FFFFFC8, int.MaxValue, int.MinValue };
        int row = 10;

        foreach (int a in lengths)
        {
            foreach (int b in lengths)
            {
                row++;
                long product = (long)a * b;
                bool allocatesMuch = a >= 0 && b >= 0 && a <= 0x7FFFFFC7 && b <= 0x7FFFFFC7
                    && product > (1 << 20) && product <= uint.MaxValue;
                if (allocatesMuch) continue;

                if (Outcome(() => new int[a, b]) != Outcome(() => Construct(a, b))) return row;
            }
        }

        return 0;
    }

    public static int Main() => Run();
}
