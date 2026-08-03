// The composition property that box and unbox.any must jointly satisfy: for every `T?` value x,
// `(T?)(object)x` equals x — for both the null and the non-null case, and across payload types.
//
// This is the property that would catch box and unbox.any disagreeing about the representation:
// `box` of a `Nullable<T>` erases the Nullable (yielding null or a boxed `T`), so `unbox.any`
// has to rebuild it from strictly less information than it started with.

using System;

public struct Pair
{
    public int A;
    public int B;
}

public enum Flavour
{
    Sweet = 3,
}

public class TestUnboxAnyNullableRoundTrip
{
    private static bool RoundTripsInt(int? x)
    {
        int? back = (int?) (object) x;

        if (back.HasValue != x.HasValue) return false;
        if (!x.HasValue) return true;

        return back.Value == x.Value;
    }

    private static bool RoundTripsPair(Pair? x)
    {
        Pair? back = (Pair?) (object) x;

        if (back.HasValue != x.HasValue) return false;
        if (!x.HasValue) return true;

        return back.Value.A == x.Value.A && back.Value.B == x.Value.B;
    }

    private static bool RoundTripsFlavour(Flavour? x)
    {
        Flavour? back = (Flavour?) (object) x;

        if (back.HasValue != x.HasValue) return false;
        if (!x.HasValue) return true;

        return back.Value == x.Value;
    }

    public static int Main(string[] argv)
    {
        if (!RoundTripsInt(null)) return 1;
        if (!RoundTripsInt(0)) return 2;
        if (!RoundTripsInt(-1)) return 3;
        if (!RoundTripsInt(int.MinValue)) return 4;
        if (!RoundTripsInt(int.MaxValue)) return 5;

        if (!RoundTripsPair(null)) return 6;

        Pair p = new Pair();
        p.A = 11;
        p.B = -22;
        if (!RoundTripsPair(p)) return 7;
        if (!RoundTripsPair(new Pair())) return 8;

        if (!RoundTripsFlavour(null)) return 9;
        if (!RoundTripsFlavour(Flavour.Sweet)) return 10;

        // A null Nullable boxes to a genuine null reference, which is what makes the null
        // round trip work at all.
        int? none = null;
        if ((object) none != null) return 11;

        // A non-null Nullable boxes to a boxed T, not to a boxed Nullable<T>.
        int? some = 5;
        object boxedSome = (object) some;
        if (boxedSome == null) return 12;
        if (!(boxedSome is int)) return 13;

        return 0;
    }
}
