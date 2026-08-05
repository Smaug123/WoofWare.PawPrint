// `Array.GetLength` with an out-of-range dimension raises `IndexOutOfRangeException`.
// `ArrayShapeQueries.cs` covers the caught case (including the message); this covers the
// uncaught one, where the exception escapes `Main` and the guest process dies.
//
// The `Array.GetLength` intrinsic used to dispatch the exception itself and had nowhere to
// report "no handler was found", so an uncaught one took the interpreter down rather than
// being reported as a dead guest.

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
