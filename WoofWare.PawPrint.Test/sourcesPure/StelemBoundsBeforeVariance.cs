// Regression: ECMA-335 III.4.x and CoreCLR check the array index bounds
// BEFORE the array-store variance check. A `stelem` against an out-of-range
// index on a covariant array must raise IndexOutOfRangeException, not
// ArrayTypeMismatchException, even when the value's runtime type is also
// incompatible with the array's element type.

using System;

public class TestStelemBoundsBeforeVariance
{
    private static void Store<T>(T[] arr, int index, T value)
    {
        // Emits `stelem !!0` with a TypeSpec metadata token, hitting
        // UnaryMetadataArrayOps.executeStelem.
        arr[index] = value;
    }

    public static int Main(string[] argv)
    {
        // `string[]` aliased as `object[]`: storing a boxed Int32 would
        // normally raise ArrayTypeMismatchException, but the index here is
        // out of range so IndexOutOfRangeException must fire first.
        object[] arr = new string[1];

        try
        {
            Store<object>(arr, 5, 42);
            return 1; // expected an exception
        }
        catch (IndexOutOfRangeException)
        {
            // good
        }
        catch (ArrayTypeMismatchException)
        {
            return 2; // wrong precedence
        }
        catch
        {
            return 3; // unexpected exception type
        }

        return 0;
    }
}
