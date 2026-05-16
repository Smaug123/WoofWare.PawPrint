// ECMA-335 / CoreCLR: an enum-typed SZ array and an SZ array of the enum's
// underlying integer are array-element-compatible. The `castclass` /
// `isinst` paths in the runtime answer "yes" for both directions and for
// signed/unsigned partners with the same normalized width (e.g.
// `MyEnum : int` ↔ `int[]`, ↔ `uint[]`).
//
// Today the assignability walk reports "unknown" for the enum case, which
// is fine for the variance gate (which degrades to permit) but a host
// failure for `castclass`/`isinst`: those callers must observe a managed
// boolean. This test exercises the cast paths directly, not the array-
// store gate.

public enum SignedKind : int
{
    Zero,
    One,
}

public class TestEnumArrayCastToUnderlying
{
    public static int Main(string[] argv)
    {
        object payload = new SignedKind[] { SignedKind.One };

        // Enum array → underlying-signed array.
        int[] asSigned = (int[]) payload;
        if (asSigned.Length != 1)
            return 1;
        if (asSigned[0] != 1)
            return 2;

        // Enum array → underlying-unsigned partner (same normalized width).
        uint[] asUnsigned = (uint[]) payload;
        if (asUnsigned.Length != 1)
            return 3;
        if (asUnsigned[0] != 1u)
            return 4;

        // Symmetric: underlying-signed array → enum array.
        object asInt = new int[] { 42 };
        SignedKind[] back = (SignedKind[]) asInt;
        if (back.Length != 1)
            return 5;
        if ((int) back[0] != 42)
            return 6;

        return 0;
    }
}
