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

    private static int SweepStrings()
    {
        for (int len = 0; len <= 8; len++)
        {
            for (int index = 0; index <= len; index++)
            {
                for (int length = 0; length <= len - index; length++)
                {
                    string[] a = new string[len];
                    string[] seed = new string[len];
                    for (int i = 0; i < len; i++)
                    {
                        seed[i] = new string('x', i + 1);
                        a[i] = seed[i];
                    }

                    Array.Clear(a, index, length);

                    for (int i = 0; i < len; i++)
                    {
                        bool cleared = i >= index && i < index + length;
                        if (cleared)
                        {
                            if (a[i] != null) return (((len * 10) + index) * 10 + length) * 10 + 1;
                        }
                        else
                        {
                            if (!ReferenceEquals(a[i], seed[i]))
                            {
                                return (((len * 10) + index) * 10 + length) * 10 + 2;
                            }
                        }
                    }
                }
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

        result = SweepStrings();
        if (result != 0) return 1000000 + result;

        result = TestStructContainingReference();
        if (result != 0) return 2000000 + result;

        result = TestWholeArrayOverload();
        if (result != 0) return 3000000 + result;

        return 0;
    }
}
