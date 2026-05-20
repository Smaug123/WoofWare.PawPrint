// Regression: a covariant store from `string[]` into an `IEnumerable<string>[]`
// element is legitimate (arrays implement `IEnumerable<T>`). The runtime
// array-store variance gate must not turn this into a host failure even though
// the assignability walk in `isConcreteTypeAssignableTo` has not yet been
// taught the array-to-generic-interface rule. The gate should degrade to
// "permit" when assignability cannot be definitively decided.

using System;
using System.Collections.Generic;

public class TestArrayStoreVarianceGenericInterface
{
    public static int Main(string[] argv)
    {
        IEnumerable<string>[] arr = new IEnumerable<string>[1];

        // The compiler emits `stelem.ref` here; the runtime variance gate sees
        // value runtime type = string[], stored element type = IEnumerable<string>.
        string[] payload = new string[] { "x" };
        arr[0] = payload;

        // Round-trip through `IEnumerable<T>.GetEnumerator()` is a separate
        // codepath; restrict ourselves to confirming the store survived and
        // the element identity matches.
        if (!ReferenceEquals(arr[0], payload)) return 1;

        return 0;
    }
}
