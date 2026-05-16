// Regression: the array-store variance gate previously caught `failwith
// "TODO:"` from `isConcreteTypeAssignableTo`'s generic-variance walk and
// degraded to "permit". With the variance walk now implemented, the gate
// gives a definitive answer for both positive and negative cases.
//
// Setup: `Func<DelegBase>[]` — element type is a delegate with declared
// covariance. Storing a `Func<DelegDerived>` should succeed (covariant);
// storing a `Func<int>` should raise ATME (value-type generic arg disables
// variance).

using System;

public class DelegBase
{
}

public class DelegDerived : DelegBase
{
}

public class TestArrayStoreVarianceDelegate
{
    public static int Main(string[] argv)
    {
        Func<DelegBase>[] arr = new Func<DelegBase>[2];

        // Positive: Func<DelegDerived> is covariantly assignable to Func<DelegBase>.
        Func<DelegDerived> makeDerived = () => new DelegDerived();
        object[] view = (object[]) (object) arr;
        view[0] = makeDerived;
        if (arr[0] == null) return 1;

        // Sanity: roundtrip the stored delegate.
        DelegBase produced = arr[0]();
        if (produced == null) return 2;

        // Negative: Func<int> ⊄ Func<DelegBase> (int is value-type; variance
        // walk rejects). The stelem.ref gate must raise ATME.
        Func<int> makeInt = () => 42;
        bool threw = false;
        try
        {
            view[1] = makeInt;
        }
        catch (ArrayTypeMismatchException)
        {
            threw = true;
        }
        if (!threw) return 3;

        // Slot 1 was rejected; should still be null.
        if (arr[1] != null) return 4;

        return 0;
    }
}
