// ECMA-335 / CoreCLR: the SZ-array → implicit-generic-interface carve-out
// (`IList<T>` / `IEnumerable<T>` / …) applies only to single-dimensional
// zero-bound arrays. Multi-dimensional arrays do NOT implement those
// interfaces. `MethodTable::ArraySupportsBizarreInterface` short-circuits
// `IsMultiDimArray()` to false; `isinst` answers false and `castclass`
// raises `InvalidCastException`.

using System.Collections.Generic;

public class TestMdArrayImplicitInterfaceRejected
{
    public static int Main(string[] argv)
    {
        object mdarray = new int[1, 1];

        if (mdarray is IList<int>) return 1;
        if (mdarray is IEnumerable<int>) return 2;
        if (mdarray is ICollection<int>) return 3;
        if (mdarray is IReadOnlyList<int>) return 4;
        if (mdarray is IReadOnlyCollection<int>) return 5;

        bool threw = false;
        try
        {
            IList<int> _ = (IList<int>) mdarray;
        }
        catch (System.InvalidCastException)
        {
            threw = true;
        }

        if (!threw) return 6;

        // The mdarray still implements the *non-generic* `System.Collections.IList`
        // (inherited from `System.Array`); this is a separate, non-generic path.
        if (!(mdarray is System.Collections.IList)) return 7;

        return 0;
    }
}
