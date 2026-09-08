using System;
using System.Reflection;

// Two properties on one type sharing a name — which for C# means overloaded indexers, both called
// `Item` — make `RuntimeType.PopulateProperties` compare their signatures to decide whether the
// second is a duplicate, via `RuntimePropertyInfo.EqualsSig` and so `Signature.AreEqual`.
//
// This file reaches only the *unequal* answer, and cannot tell a real comparison from one that
// never returns true: for two genuinely distinct indexers, "not equal" is the correct answer, so an
// implementation stuck there produces exactly the property list asserted below, accessor identities
// included. `ReflectionPropertyHiding.cs` is what pins the other direction — `new` hiding is the
// shape that makes the comparison answer "equal" — and `ReflectionPropertyHidingCrossModule.cs`
// pins it across assemblies, where the byte-equality fast path cannot fire.
//
// What this file does add over those: two properties on *one* type, so the comparison runs with
// both blobs in the same module and at the same declaring type, and index parameters rather than a
// property type carry the difference.

public class Sample
{
    public int this[int i] { get { return i * 2; } }
    public int this[string s] { get { return s.Length; } }
}

public class Program
{
    public static int Main()
    {
        PropertyInfo[] ps = typeof(Sample).GetProperties();
        if (ps.Length != 2) return 1;

        Sample s = new Sample();
        int viaInt = 0;
        int viaString = 0;
        int seen = 0;

        foreach (PropertyInfo p in ps)
        {
            if (p.Name != "Item") return 2;
            ParameterInfo[] ix = p.GetIndexParameters();
            if (ix.Length != 1) return 3;

            if (ix[0].ParameterType == typeof(int))
            {
                viaInt = (int)p.GetValue(s, new object[] { 21 });
                seen |= 1;
            }
            else if (ix[0].ParameterType == typeof(string))
            {
                viaString = (int)p.GetValue(s, new object[] { "abcd" });
                seen |= 2;
            }
            else
            {
                return 4;
            }
        }

        if (seen != 3) return 5;
        if (viaInt != 42) return 6;
        if (viaString != 4) return 7;

        return 0;
    }
}
