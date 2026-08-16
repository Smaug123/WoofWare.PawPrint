using System;
using System.Runtime.InteropServices;

// `value__` is the CLR-reserved name for an enum's integer slot, but it is also a legal C#
// identifier, so an ordinary struct may use it. Enum-ness must be decided nominally — the
// immediate base type is `System.Enum` — so none of the structs below is an enum and none is
// flattened onto the eval stack as a bare integer. A structural test (one instance field, named
// `value__`, at offset 0, of integral type) misclassifies them all (issue #996).
//
// The sibling `AutoLayoutStructNamedLikeEnum.cs` pins the layout half of that misclassification.
// This file pins the eval-stack half: `Padded` below has declared `Size` padding that no field
// covers, so a misclassifying rewrap on pop refuses it outright ("CliValueType.OfFieldsLike:
// refusing to drop preserved bytes for non-tightly-packed value type") and an ordinary
// assignment kills the guest.
public class TestStructNamedLikeEnumRoundTrip
{
    // Declared bigger than its fields cover, so a flattened value could not be rebuilt.
    [StructLayout(LayoutKind.Sequential, Size = 8)]
    private struct Padded { public int value__; }

    // Tightly packed, so even a misclassifying rewrap round-trips it: the control showing the
    // failure above is about the padding rather than the name.
    private struct Tight { public int value__; }

    // A real enum, as the control that enums still flatten.
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
