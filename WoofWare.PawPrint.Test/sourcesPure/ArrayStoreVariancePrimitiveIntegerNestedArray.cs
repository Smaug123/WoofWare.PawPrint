// Regression: ECMA-335 III.8.7 / CoreCLR GetNormalizedIntegralArrayElementType
// permits signed/unsigned primitive integer arrays to interchange as element
// types of an outer array — `object[] view = new int[1][]; view[0] = new uint[0];`
// is a valid covariant store. The runtime variance gate must therefore consider
// `int[]` and `uint[]` to be assignable when checking the inner element type.

public class TestArrayStoreVariancePrimitiveIntegerNestedArray
{
    public static int Main(string[] argv)
    {
        // `object[]` aliasing `int[][]`. Storing a `uint[]` into it must succeed
        // by primitive-integer equivalence.
        object[] view = new int[1][];
        uint[] payload = new uint[] { 7u };
        view[0] = payload;

        // Round-trip identity check to confirm the store landed.
        if (!ReferenceEquals(view[0], payload))
            return 1;

        return 0;
    }
}
