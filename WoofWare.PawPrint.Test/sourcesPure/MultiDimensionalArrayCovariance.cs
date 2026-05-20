// ECMA-335 II.14.2 / III.4.x: the runtime-synthesized `Set` operation on a
// multi-dimensional array is variance-checked the same way as `stelem`. A
// covariant view (`object[,]` aliasing `string[,]`) must accept stores whose
// value is assignment-compatible with the array's stored element type, and
// must raise ArrayTypeMismatchException otherwise. Null is always storable
// into a reference-typed element.

using System;

public class TestMultiDimensionalArrayCovariance
{
    public static int Main(string[] argv)
    {
        string[,] backing = new string[2, 2];
        object[,] view = backing;

        // Compatible store: string into string[,]-aliased-as-object[,].
        view[0, 0] = "ok";
        if (!ReferenceEquals(backing[0, 0], "ok")) return 1;

        // Null is storable.
        view[0, 1] = null;
        if (backing[0, 1] != null) return 2;

        // Incompatible store: boxed Int32 is not assignable to string.
        try
        {
            view[1, 0] = (object)42;
            return 3;
        }
        catch (ArrayTypeMismatchException)
        {
            // expected
        }

        // The failed store must have left the cell untouched.
        if (backing[1, 0] != null) return 4;

        // A second compatible store afterwards still works.
        view[1, 1] = "still ok";
        if (!ReferenceEquals(backing[1, 1], "still ok")) return 5;

        return 0;
    }
}
