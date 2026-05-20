// ECMA-335 / CoreCLR: a single-dimensional zero-bound array `T[]` (SZ-array)
// implicitly implements the five generic interfaces `IList<T>`,
// `IEnumerable<T>`, `ICollection<T>`, `IReadOnlyList<T>`, and
// `IReadOnlyCollection<T>` for any `T`, *plus* every instantiation that the
// SZ-array element-compatibility rule permits — even when the interface
// itself is invariant (CoreCLR's `IsCovariantArrayInterface` carve-out).
//
// For reference-typed elements, "element-compatible" reduces to recursive
// assignability (`string` ⊑ `object`, `string` ⊑ `IComparable`, …). This
// test pins down the cast/isinst behaviour for all five implicit interfaces,
// including the invariance-bypass on `IList<T>` and `ICollection<T>`.

using System;
using System.Collections.Generic;

public class TestSzArrayImplicitInterfaceCastRef
{
    public static int Main(string[] argv)
    {
        object payload = new string[] { "x" };

        // Variant carve-out for the three covariant interfaces:
        // string[] → IEnumerable<object>, IReadOnlyList<object>, IReadOnlyCollection<object>.
        IEnumerable<object> asEnumerable = (IEnumerable<object>) payload;
        if (asEnumerable == null) return 1;

        IReadOnlyList<object> asReadOnlyList = (IReadOnlyList<object>) payload;
        if (asReadOnlyList == null) return 2;

        IReadOnlyCollection<object> asReadOnlyCollection = (IReadOnlyCollection<object>) payload;
        if (asReadOnlyCollection == null) return 3;

        // Invariance bypass: `IList<T>` and `ICollection<T>` are declared
        // invariant, but the SZ-array implicit-interface rule still applies
        // CoreCLR's `CanCastParam` to the element, so string[] → IList<object>
        // and ICollection<object> succeed.
        IList<object> asList = (IList<object>) payload;
        if (asList == null) return 4;

        ICollection<object> asCollection = (ICollection<object>) payload;
        if (asCollection == null) return 5;

        // isinst partner must agree with castclass.
        if (!(payload is IEnumerable<object>)) return 6;
        if (!(payload is IList<object>)) return 7;
        if (!(payload is ICollection<object>)) return 8;
        if (!(payload is IReadOnlyList<object>)) return 9;
        if (!(payload is IReadOnlyCollection<object>)) return 10;

        // Recursive ref-covariance: string ⊑ IComparable, so string[] → IEnumerable<IComparable>.
        if (!(payload is IEnumerable<IComparable>)) return 11;

        // Negative: object[] is NOT IList<string> — element rule asks
        // "is object covariantly-assignable to string?" → false.
        object objArr = new object[] { "x" };
        if (objArr is IList<string>) return 12;

        bool threw = false;
        try
        {
            IList<string> _ = (IList<string>) objArr;
        }
        catch (InvalidCastException)
        {
            threw = true;
        }

        if (!threw) return 13;

        return 0;
    }
}
