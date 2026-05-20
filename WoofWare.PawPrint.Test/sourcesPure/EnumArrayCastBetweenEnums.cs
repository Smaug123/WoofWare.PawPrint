// ECMA-335 / CoreCLR: two enum-typed SZ arrays are element-compatible iff
// their enums share a normalized underlying integer width. Two enums on
// `int` interchange (and with their `uint` partner). Two enums on
// different underlying widths do not — the `castclass` must raise
// `InvalidCastException`, `isinst` must answer false.

public enum AKind : int
{
    A,
}

public enum BKind : int
{
    B,
}

public enum CKind : long
{
    C,
}

public class TestEnumArrayCastBetweenEnums
{
    public static int Main(string[] argv)
    {
        object aPayload = new AKind[] { AKind.A };

        // Same underlying int → succeeds.
        BKind[] asB = (BKind[]) aPayload;
        if (asB.Length != 1)
            return 1;

        // Symmetric (B → A) succeeds.
        object bPayload = new BKind[] { BKind.B };
        AKind[] asA = (AKind[]) bPayload;
        if (asA.Length != 1)
            return 2;

        // Different underlying width (int → long) is rejected.
        bool threw = false;
        try
        {
            CKind[] _ = (CKind[]) aPayload;
        }
        catch (System.InvalidCastException)
        {
            threw = true;
        }

        if (!threw)
            return 3;

        if (aPayload is CKind[])
            return 4;

        return 0;
    }
}
