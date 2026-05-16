// Regression: an enum-array is never assignable to an array of a non-integer
// primitive (`float[]`, `double[]`, `bool[]`, `char[]`) or any non-integer
// struct. Enum-underlying equivalence — the rule that lets a `MyEnum : int`
// array interchange with `int[]` at the array-store / array-cast level — can
// only succeed when the partner element is itself an enum or a normalized
// primitive integer. For any other value-typed partner the answer is a
// definitive "false", not "unknown": `isinst`/`castclass` must observe a
// managed `false` rather than the interpreter host-failing.

public enum MyOtherKind : int
{
    Zero,
    One,
}

public class TestEnumArrayIsNonIntegerPrimitiveArray
{
    public static int Main(string[] argv)
    {
        object o = new MyOtherKind[1];

        if (o is float[])
            return 1;

        if (o is double[])
            return 2;

        if (o is bool[])
            return 3;

        if (o is char[])
            return 4;

        // Sanity: the value really is non-null and lives in the object slot.
        if (o == null)
            return 5;

        return 0;
    }
}
