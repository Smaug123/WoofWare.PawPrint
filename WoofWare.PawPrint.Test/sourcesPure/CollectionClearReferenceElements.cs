using System;
using System.Collections.Generic;

// The real callers of `Array.Clear` over reference-containing element types. `List<T>.Clear()`
// clears a `T[]` of plain references; `Dictionary<K,V>.Clear()` clears an `Entry[]`, where
// `Entry` is a struct mixing `int` fields with the key and value. That struct is the reason a
// pointer-slot walk over the element data has slots containing no reference at all, so this
// exercises the mixed case through the BCL rather than through a hand-written struct.
public class TestCollectionClearReferenceElements
{
    private sealed class Box
    {
        public int Value;
    }

    private static int TestListOfReferences()
    {
        List<Box> l = new List<Box> ();
        Box first = new Box { Value = 1 };
        l.Add (first);
        l.Add (new Box { Value = 2 });
        l.Add (new Box { Value = 3 });

        if (l.Count != 3) return 1;

        l.Clear ();

        if (l.Count != 0) return 2;

        // The list is still usable afterwards, and the referent we held on to is untouched.
        l.Add (first);
        if (l.Count != 1) return 3;
        if (!ReferenceEquals (l[0], first)) return 4;
        if (first.Value != 1) return 5;

        return 0;
    }

    private static int TestDictionaryWithReferenceValues()
    {
        Dictionary<int, Box> d = new Dictionary<int, Box> ();
        Box kept = new Box { Value = 42 };
        d[1] = kept;
        d[2] = new Box { Value = 2 };
        d[3] = new Box { Value = 3 };

        if (d.Count != 3) return 10;

        d.Clear ();

        if (d.Count != 0) return 11;
        if (d.ContainsKey (1)) return 12;

        // Still usable, and the value we kept a reference to survived being cleared out.
        d[7] = kept;
        if (d.Count != 1) return 13;
        if (!ReferenceEquals (d[7], kept)) return 14;
        if (kept.Value != 42) return 15;

        return 0;
    }

    private static int TestDictionaryWithReferenceKeys()
    {
        Dictionary<string, int> d = new Dictionary<string, int> ();
        d["alpha"] = 1;
        d["beta"] = 2;

        if (d.Count != 2) return 20;

        d.Clear ();

        if (d.Count != 0) return 21;
        if (d.ContainsKey ("alpha")) return 22;

        d["gamma"] = 3;
        if (d.Count != 1) return 23;
        if (d["gamma"] != 3) return 24;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = TestListOfReferences();
        if (result != 0) return 1000 + result;

        result = TestDictionaryWithReferenceValues();
        if (result != 0) return 2000 + result;

        result = TestDictionaryWithReferenceKeys();
        if (result != 0) return 3000 + result;

        return 0;
    }
}
