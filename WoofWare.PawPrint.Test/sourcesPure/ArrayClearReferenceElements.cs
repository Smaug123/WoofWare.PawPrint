using System;

// `Array.Clear` on an array whose elements contain GC pointers takes the other half of
// `Array.Clear`'s branch: `pMT->ContainsGCPointers` is true, so it calls
// `SpanHelpers.ClearWithReferences(ref IntPtr, nuint)` rather than
// `SpanHelpers.ClearWithoutReferences`. That helper is not `[Intrinsic]` -- it is ordinary
// managed IL -- so it is a distinct boundary from the one `ArrayClear.cs` covers.
public class TestArrayClearReferenceElements
{
    private sealed class Box
    {
        public int Value;
    }

    private struct HasReference
    {
        public int N;
        public Box B;
    }

    // Deliberately fresh `Box` instances rather than constructed strings: `new string(char,
    // int)` is a separate unimplemented InternalCall, and reaching it would make this file
    // fail for a reason that has nothing to do with clearing reference elements.
    //
    // Targeted rather than exhaustive, unlike the sibling ArrayClear.cs sweep. Each clear here
    // interprets the whole of `ClearWithReferences`' managed IL rather than dispatching one
    // native intrinsic, so a full cross-product is far too slow for the suite. `Array.Clear`
    // passes `pointerSizeLength = byteLength / sizeof(IntPtr)`, which for a reference array is
    // just `length`, and the helper branches on it at 8 (the backward block loop), then 4..7,
    // 2..3, 1 and 0. Small lengths are still swept exhaustively because that is where the
    // index arithmetic is easiest to get wrong; the larger lengths are chosen to enter the
    // block loop once, more than once, and with a remainder.
    private static int CheckClear(int len, int index, int length)
    {
        Box[] a = new Box[len];
        Box[] seed = new Box[len];
        for (int i = 0; i < len; i++)
        {
            seed[i] = new Box { Value = i + 1 };
            a[i] = seed[i];
        }

        Array.Clear(a, index, length);

        for (int i = 0; i < len; i++)
        {
            bool cleared = i >= index && i < index + length;
            if (cleared)
            {
                if (a[i] != null) return 1;
            }
            else if (!ReferenceEquals(a[i], seed[i]))
            {
                return 2;
            }
        }

        return 0;
    }

    private static int SweepReferences()
    {
        // Exhaustive at small sizes: every (index, length) pair for len 0..8.
        for (int len = 0; len <= 8; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    int r = CheckClear(len, index, length);
                    if (r != 0) return (((len * 10) + index) * 10 + length) * 10 + r;
                }
            }
        }

        // Chosen larger cases: exactly one block-loop iteration (8), one plus a remainder
        // (9, 11), two iterations (16), two plus a remainder (19), and each at an offset so
        // the walk does not start at element 0.
        int[] lens = { 8, 9, 11, 16, 19, 20 };
        int[] starts = { 0, 1, 3 };

        for (int li = 0; li < lens.Length; li++)
        {
            for (int si = 0; si < starts.Length; si++)
            {
                int len = lens[li];
                int index = starts[si];
                if (index > len) continue;

                int r = CheckClear(len, index, len - index);
                if (r != 0) return 100000 + (((len * 10) + index) * 10) + r;
            }
        }

        return 0;
    }

    // A struct element that mixes a primitive with an object reference: the whole element is
    // still cleared, so the reference slot must become null and the primitive slot zero.
    private static int TestStructContainingReference()
    {
        HasReference[] a = new HasReference[4];
        for (int i = 0; i < 4; i++)
        {
            a[i].N = i + 1;
            a[i].B = new Box { Value = i + 1 };
        }

        Box survivor = a[3].B;

        Array.Clear(a, 1, 2);

        if (a[0].N != 1 || a[0].B == null || a[0].B.Value != 1) return 1;
        if (a[1].N != 0 || a[1].B != null) return 2;
        if (a[2].N != 0 || a[2].B != null) return 3;
        if (a[3].N != 4 || !ReferenceEquals(a[3].B, survivor)) return 4;

        return 0;
    }

    private static int TestWholeArrayOverload()
    {
        object[] a = new object[3];
        a[0] = new Box ();
        a[1] = "hello";
        a[2] = new Box ();

        Array.Clear(a);

        if (a[0] != null) return 1;
        if (a[1] != null) return 2;
        if (a[2] != null) return 3;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = SweepReferences();
        if (result != 0) return 1000000 + result;

        result = TestStructContainingReference();
        if (result != 0) return 2000000 + result;

        result = TestWholeArrayOverload();
        if (result != 0) return 3000000 + result;

        return 0;
    }
}
