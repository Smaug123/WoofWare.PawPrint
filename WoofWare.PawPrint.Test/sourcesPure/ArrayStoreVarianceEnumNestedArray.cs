// Regression: ECMA-335 / CoreCLR permit enums with an equivalent underlying
// integer type to interchange with the underlying primitive at array element
// boundaries. A `MyEnum : int` array element-stored into a slot whose array's
// stored element type is `int[]` is a valid covariant store. The variance gate
// must not raise ArrayTypeMismatchException for this case even though the
// assignability walk doesn't yet model enum-underlying-type equivalence: when
// either side of the inner element comparison is an enum, the walk reports
// "unknown" (`None`) and the gate degrades to permit.

public enum NestedEnumKind : int
{
    First = 0,
    Second = 1,
}

public class TestArrayStoreVarianceEnumNestedArray
{
    public static int Main(string[] argv)
    {
        object[] view = new int[1][];
        NestedEnumKind[] payload = new NestedEnumKind[] { NestedEnumKind.Second };
        view[0] = payload;

        if (!ReferenceEquals(view[0], payload))
            return 1;

        return 0;
    }
}
