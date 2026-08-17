using System;
using System.Reflection;

// Parked. Two properties on one type sharing a name — which for C# means overloaded indexers, both
// called `Item` — make `RuntimeType.PopulateProperties` compare their signatures to decide whether
// the second is a duplicate, via `RuntimePropertyInfo.EqualsSig` and so `Signature.AreEqual`.
//
// Measured: this dies in
//   Unimplemented native method (PInvokeImpl QCall!Signature_AreEqual)
// which is a *different* QCall from `Signature_Init`, with its own semantics
// (`MetaSig::CompareMethodSigs`, comparing two blobs under two type contexts).
//
// This became reachable only once PropertySig decoding landed; before that, any property reflection
// died earlier in `Signature_Init`. A hidden inherited property of the same name reaches the same
// comparison, so this file stands in for that shape too.
//
// Not vacuous: an implementation that always answered "not equal" would report two properties here
// and still pass a test that only counted them, so the accessor identities are checked as well —
// naming which overload each `GetValue` reached.

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
