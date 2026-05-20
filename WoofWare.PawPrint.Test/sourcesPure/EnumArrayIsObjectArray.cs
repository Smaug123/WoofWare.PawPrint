// Regression: when the assignability check meets an array source with a
// value-typed element type (here `MyKind : int`) and a target array with a
// reference-typed element (`object[]`), the answer must be a definitive
// "false" — the covariance + enum-equivalence rules don't apply across
// reference/value boundaries. Callers like `isinst` rely on a managed-false
// answer; an interpreter-level TODO failwith would be a regression.

public enum MyKind : int
{
    A,
    B,
}

public class TestEnumArrayIsObjectArray
{
    public static int Main(string[] argv)
    {
        object o = new MyKind[1];

        if (o is object[])
            return 1;

        // Sanity: the value really is non-null and lives in the object slot.
        if (o == null)
            return 2;

        return 0;
    }
}
