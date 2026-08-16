// `Array.GetLength` with an out-of-range dimension raises `IndexOutOfRangeException`.
// `ArrayShapeQueries.cs` covers the caught case (including the message); this covers the
// uncaught one, where the exception escapes `Main` and the guest process dies.
//
// An `Array.GetLength` intrinsic that dispatched the exception itself would have nowhere to
// report "no handler was found", so an uncaught one would take the interpreter down rather
// than being reported as a dead guest.

using System;

public class Program
{
    // Keep the dimension opaque so nothing can fold the call away.
    private static int Dimension(int d)
    {
        return d;
    }

    public static int Main(string[] args)
    {
        int[,] grid = new int[2, 3];
        return grid.GetLength(Dimension(5));
    }
}
