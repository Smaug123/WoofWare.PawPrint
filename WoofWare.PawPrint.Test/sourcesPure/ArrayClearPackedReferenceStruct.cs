using System;
using System.Runtime.InteropServices;

// A `Pack = 1` struct that contains an object reference. CoreCLR ignores the packing request
// for GC-containing types: references must stay pointer-aligned, so the element is laid out as
// 16 bytes and `Array.Clear` hands `SpanHelpers.ClearWithReferences` two pointer-sized slots.
//
// PawPrint's layout does not apply that GC-alignment rule, so it computes a smaller element and
// the derived slot count no longer lines up with the fields. The clear then asks to zero a
// range that cuts across the reference, which fails loudly rather than silently leaving a field
// set -- the right failure mode, but still a divergence from the real runtime.
//
// This is a struct-layout gap, not a clearing gap: the fix is for PawPrint to round a
// GC-containing value type up to pointer alignment the way CoreCLR does.
public class TestArrayClearPackedReferenceStruct
{
    private sealed class Box
    {
        public int Value;
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    private struct Packed
    {
        public Box O;
        public byte B;
    }

    public static int Main(string[] argv)
    {
        // The single-element case first, and deliberately: it is the dangerous one. With one
        // element the truncated slot count still covers the reference exactly, so nothing
        // straddles and nothing would object -- the trailing byte would just quietly survive.
        // With two or more elements a later store lands across the next element's reference and
        // is refused, which masks the underlying problem behind a different symptom.
        Packed[] one = new Packed[1];
        one[0].O = new Box { Value = 1 };
        one[0].B = 7;

        Array.Clear(one, 0, 1);

        if (one[0].O != null) return 1;
        if (one[0].B != 0) return 2;

        Packed[] a = new Packed[2];
        a[0].O = new Box { Value = 1 };
        a[0].B = 7;
        a[1].O = new Box { Value = 2 };
        a[1].B = 9;

        Array.Clear(a, 0, 2);

        if (a[0].O != null || a[0].B != 0) return 3;
        if (a[1].O != null || a[1].B != 0) return 4;

        return 0;
    }
}
