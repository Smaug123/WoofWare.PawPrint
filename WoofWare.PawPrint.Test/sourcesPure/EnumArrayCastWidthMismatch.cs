// ECMA-335 / CoreCLR: array-element compatibility for enum elements is
// determined by their normalized underlying integer width. An array of an
// enum whose underlying type is `byte` is NOT element-compatible with
// `int[]` even though both are integer-typed: the widths differ.
//
// Today the assignability walk reports "unknown" for any enum-vs-integer
// element pair, which would let an invalid cast through if `castclass`
// degraded to permit. This test verifies the runtime answers a definitive
// `false` and raises `InvalidCastException`.

public enum ByteKind : byte
{
    Zero,
    One,
}

public class TestEnumArrayCastWidthMismatch
{
    public static int Main(string[] argv)
    {
        object payload = new ByteKind[] { ByteKind.One };

        bool threw = false;
        try
        {
            int[] _ = (int[]) payload;
        }
        catch (System.InvalidCastException)
        {
            threw = true;
        }

        if (!threw)
            return 1;

        // `isinst` is the non-throwing partner and must agree.
        if (payload is int[])
            return 2;

        // The same payload remains its own type — sanity check.
        if (!(payload is ByteKind[]))
            return 3;

        return 0;
    }
}
