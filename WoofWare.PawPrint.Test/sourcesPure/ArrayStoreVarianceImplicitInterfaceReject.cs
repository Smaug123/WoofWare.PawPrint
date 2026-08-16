// The variance gate must reject element-incompatible stores into
// arrays of generic implicit-interface element type:
// `isConcreteTypeAssignableTo` knows the SZ-array → implicit-generic-
// interface rule and answers definitively (not "unknown"/None) on this pair,
// so the gate must answer false and raise `ArrayTypeMismatchException`.
// A gate that degraded to "permit" on `None` would let a clearly-invalid
// store like `IEnumerable<string>[0] = new object[0]` land.
//
// Companion test `ArrayStoreVarianceGenericInterface.cs` covers the
// positive case (`IEnumerable<string>[0] = new string[0]`).

using System;
using System.Collections.Generic;

public class TestArrayStoreVarianceImplicitInterfaceReject
{
    public static int Main(string[] argv)
    {
        IEnumerable<string>[] view = new IEnumerable<string>[1];

        // Stored element type IEnumerable<string>; value runtime type
        // object[]. The element rule asks "is object covariantly-assignable
        // to string?" — no — so the store must raise ATME.
        bool threw = false;
        try
        {
            view[0] = (IEnumerable<string>) (object) new object[1];
        }
        catch (InvalidCastException)
        {
            // The cast itself fails before the store is attempted; this
            // is acceptable. The store gate's correctness is asserted by
            // the next block.
            threw = true;
        }

        if (!threw) return 1;

        // Construct the assignment via a `stelem.ref` whose RHS dodges the
        // C# cast (mismatch is at the IL boundary).
        object[] viewAsObj = (object[]) (object) view;
        threw = false;
        try
        {
            viewAsObj[0] = new object[1];
        }
        catch (ArrayTypeMismatchException)
        {
            threw = true;
        }

        if (!threw) return 2;

        // Positive sanity: the same view accepts a string[] payload (the
        // existing covariance case).
        viewAsObj[0] = new string[1];
        if (view[0] == null) return 3;

        return 0;
    }
}
