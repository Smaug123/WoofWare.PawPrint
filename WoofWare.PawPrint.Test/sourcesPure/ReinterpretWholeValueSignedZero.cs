using System;
using System.Runtime.CompilerServices;

// Elided writes must not use structural equality to decide "nothing changed".
//
// Replacing a whole reference-containing cell through a transparent wrapper compares the
// replacement against what is there to skip no-op writes. `-0.0f` and `+0.0f` compare *equal* but
// are distinguishable: `1.0f / x` is negative infinity for one and positive infinity for the
// other. A write that only flips that sign therefore looks like a no-op and gets dropped, which
// the CLR does not do.
//
// The reference field is what forces the elided route: it makes the storage byte-unaddressable, so
// naming the cell is the only way to serve the write, and the comparison sits on that path.
public class TestReinterpretWholeValueSignedZero
{
    private sealed class Box { public int V; }

    private struct Elem { public Box Payload; public float F; }

    private struct Wrapper { public Elem Value; }

    private static bool IsNegativeZero(float f)
    {
        return f == 0.0f && 1.0f / f < 0.0f;
    }

    public static bool IsPositiveZero(float f)
    {
        return f == 0.0f && 1.0f / f > 0.0f;
    }

    public static int Main(string[] argv)
    {
        Box shared = new Box { V = 1 };
        Elem e = default;

        Unsafe.As<Elem, Wrapper>(ref e).Value = new Elem { Payload = shared, F = -0.0f };
        if (!IsNegativeZero(e.F)) return 1;

        // Differs from what is stored only in the sign of a zero, and only in a field that is
        // not the reference — so the replacement compares structurally equal to the current cell.
        Unsafe.As<Elem, Wrapper>(ref e).Value = new Elem { Payload = shared, F = 0.0f };
        if (!IsPositiveZero(e.F)) return 2;
        if (!ReferenceEquals(e.Payload, shared)) return 3;

        // And back the other way.
        Unsafe.As<Elem, Wrapper>(ref e).Value = new Elem { Payload = shared, F = -0.0f };
        if (!IsNegativeZero(e.F)) return 4;

        return 0;
    }
}
