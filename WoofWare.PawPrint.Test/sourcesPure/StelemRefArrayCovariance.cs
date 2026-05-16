// ECMA-335 III.4.x: `stelem` / `stelem.ref` into a reference-typed array must
// raise ArrayTypeMismatchException when the value's runtime type is not
// assignment-compatible with the array's stored element type. Covariant
// reads through a base-typed view are legal; covariant writes are gated on
// runtime assignability.
//
// `object[] view = new string[1]` aliases a string[]. Writing a string is
// fine; writing a boxed Int32 must trap. Null is always storable.

using System;

public class TestStelemRefArrayCovariance
{
    public static int Main(string[] argv)
    {
        string[] backing = new string[3];
        object[] view = backing;

        // Compatible store: string-into-string[]-aliased-as-object[] is fine.
        view[0] = "ok";
        if (!ReferenceEquals(backing[0], "ok")) return 1;

        // Null is always storable into a reference array.
        view[1] = null;
        if (backing[1] != null) return 2;

        // Incompatible store: Int32 (boxed to satisfy the object element type
        // statically) is not assignable to string at runtime.
        try
        {
            view[2] = (object)42;
            return 3;
        }
        catch (ArrayTypeMismatchException)
        {
            // expected
        }

        // The failed store must have left the array unchanged.
        if (backing[2] != null) return 4;

        return 0;
    }
}
