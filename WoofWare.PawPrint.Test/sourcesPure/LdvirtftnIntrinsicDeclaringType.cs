using System;

// A delegate over a virtual method whose *resolved* body is declared on a type carrying a
// type-level `[Intrinsic]` (`Int128`). The direct `callvirt` and the delegate must agree,
// and on real .NET they do.
//
// The direct call is the control: it is the line that would fail first if PawPrint could not
// interpret `Int128::GetHashCode` at all. It passes, so what this file isolates is purely the
// delegate route. A wrong-but-consistent implementation would still pass — one that computed
// the same incorrect hash on both routes — but that is not the failure mode parked here.

class Program
{
    static int Main(string[] args)
    {
        object boxed = default(Int128);

        int direct = boxed.GetHashCode();

        Func<int> viaDelegate = boxed.GetHashCode;
        if (viaDelegate() != direct)
        {
            return 1;
        }

        return 0;
    }
}
