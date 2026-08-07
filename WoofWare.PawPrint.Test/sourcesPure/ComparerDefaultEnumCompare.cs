using System.Collections.Generic;

// `Comparer<TEnum>.Default` selection already works and is asserted by the sibling
// `ComparerDefault.cs`; what is parked here is *using* the resulting comparer.
// `EnumComparer<T>.Compare` delegates to `RuntimeHelpers.EnumCompareTo<T>`, a separate
// [Intrinsic] that PawPrint has not reviewed for the safe-intrinsic allowlist. Its IL body is
// `ldarga.s 0; ldarg.1; box T; constrained. T; callvirt Enum::CompareTo(object); ret`, so
// un-parking this needs `Enum.CompareTo(object)` to be reachable, not just an allowlist entry.

namespace ComparerDefaultEnumCompareTest
{
    enum Colour
    {
        Red = 1,
        Green = 2,
    }

    class Program
    {
        static int Main(string[] args)
        {
            if (Comparer<Colour>.Default.Compare(Colour.Red, Colour.Green) >= 0) return 1;
            if (Comparer<Colour>.Default.Compare(Colour.Green, Colour.Red) <= 0) return 2;
            if (Comparer<Colour>.Default.Compare(Colour.Red, Colour.Red) != 0) return 3;
            return 0;
        }
    }
}
