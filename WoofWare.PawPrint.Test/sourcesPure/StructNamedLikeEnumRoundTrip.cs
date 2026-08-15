using System;
using System.Runtime.InteropServices;

// `value__` is the CLR-reserved name for an enum's integer slot, but it is also a legal C#
// identifier, so an ordinary struct may use it. PawPrint used to decide enum-ness *structurally* —
// one instance field, named `value__`, at offset 0, of integral type — and so classified the
// structs below as enums and flattened them onto the eval stack as bare integers (issue #996).
//
// The sibling `AutoLayoutStructNamedLikeEnum.cs` pins the layout half of that misclassification.
// This file pins the eval-stack half, which is the more dramatic one: `Padded` below has declared
// `Size` padding that no field covers, so the rewrap on pop refused it outright
// ("CliValueType.OfFieldsLike: refusing to drop preserved bytes for non-tightly-packed value
// type") and an ordinary assignment killed the guest. Enum-ness is now decided nominally — the
// immediate base type is `System.Enum` — so none of these is an enum and none is flattened.
public class TestStructNamedLikeEnumRoundTrip
{
    // The shape that used to abort: declared bigger than its fields cover, so the flattened value
    // could not be rebuilt.
    [StructLayout(LayoutKind.Sequential, Size = 8)]
    private struct Padded { public int value__; }

    // Tightly packed, so the old rewrap round-tripped it; kept as the control which shows the
    // failure above is about the padding rather than about the name.
    private struct Tight { public int value__; }

    // A real enum, to pin that the fix did not stop enums flattening. If this regressed, enum
    // arithmetic would break far more visibly than this test — that is the point of the control.
    private enum Real { Zero = 0, Seven = 7 }

    private static int RoundTrip<T>(T value) => 0;

    public static int Main(string[] argv)
    {
        Padded p = default;
        p.value__ = 7;
        Padded p2 = p;
        if (p2.value__ != 7) return 1;

        Tight t = default;
        t.value__ = 9;
        Tight t2 = t;
        if (t2.value__ != 9) return 2;

        Real r = Real.Seven;
        Real r2 = r;
        if (r2 != Real.Seven) return 3;
        // An enum really does behave as its underlying integer for arithmetic and comparison.
        if ((int) r2 + 1 != 8) return 4;

        // Boxing takes the reconstruct-from-flattened path in `executeBox`, so it is a second,
        // independent route to the classification; assert the declared type survives it.
        object boxedPadded = p2;
        if (boxedPadded.GetType() != typeof(Padded)) return 5;
        object boxedReal = r2;
        if (boxedReal.GetType() != typeof(Real)) return 6;
        if ((Real) boxedReal != Real.Seven) return 7;

        return 0;
    }
}
