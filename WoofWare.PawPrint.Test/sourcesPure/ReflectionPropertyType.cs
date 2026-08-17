using System;
using System.Reflection;

// `PropertyInfo`'s signature-derived surface, which all funnels into `RuntimePropertyInfo.Signature`
// and so into `Signature_Init` with a raw PropertySig blob (ECMA-335 II.23.2.5).
//
// `GetIndexParameters` is deliberately here even though it does *not* read `Signature.Arguments` —
// it reads the getter's parameters, falling back to the setter minus one. It is a control: it keeps
// working whatever this slice does to the signature, so a failure there means something else broke.
//
// `ToString()` is the opposite: it is the only member that reads `Signature.Arguments`, so the
// indexer's `ToString` is the single check that an empty or misfilled argument array would fail.

public class Holder<T>
{
    public T Value { get; set; }
    public T[] Values { get; set; }
}

public class Sample
{
    public int Number { get; set; }
    public string Text { get; set; }
    public int[] Array { get; set; }
    public Holder<int> Nested { get; set; }
    public static double Stat { get; set; }

    private string setOnly;
    public string SetOnly { set { setOnly = value; } }
    public string ReadSetOnly() => setOnly;

    public string this[int i, long j] { get { return i.ToString() + j.ToString(); } }

    // `ref readonly` puts a modreq(InAttribute) on the property type in the PropertySig.
    // Real .NET reports the *stripped* type, `System.Int32&`.
    private int refBacking = 77;
    public ref readonly int RefReadonly => ref refBacking;
}

public class Program
{
    public static int Main()
    {
        Type sample = typeof(Sample);

        PropertyInfo number = sample.GetProperty("Number");
        if (number.PropertyType != typeof(int)) return 1;
        if (number.GetIndexParameters().Length != 0) return 2;

        Sample s = new Sample();
        number.SetValue(s, 42);
        if ((int)number.GetValue(s) != 42) return 3;

        if (sample.GetProperty("Text").PropertyType != typeof(string)) return 4;
        if (sample.GetProperty("Array").PropertyType != typeof(int[])) return 5;

        // A generic instantiation as a property type.
        if (sample.GetProperty("Nested").PropertyType != typeof(Holder<int>)) return 6;

        // Static property: the PropertySig carries no HASTHIS, which is what the calling
        // convention must reflect.
        PropertyInfo stat = sample.GetProperty("Stat");
        if (stat.PropertyType != typeof(double)) return 7;
        stat.SetValue(null, 1.5);
        if ((double)stat.GetValue(null) != 1.5) return 8;

        // Setter-only: `GetIndexParameters` must take the setter path (its parameter count minus
        // one), and `CanRead` must be false.
        PropertyInfo setOnly = sample.GetProperty("SetOnly");
        if (setOnly.CanRead) return 9;
        if (!setOnly.CanWrite) return 10;
        if (setOnly.PropertyType != typeof(string)) return 11;
        if (setOnly.GetIndexParameters().Length != 0) return 12;
        setOnly.SetValue(s, "written");
        if (s.ReadSetOnly() != "written") return 13;

        // Indexer: two index parameters, of different types, so an implementation that reported
        // the property type where an index parameter belongs would be visible.
        PropertyInfo indexer = sample.GetProperty("Item");
        if (indexer == null) return 14;
        if (indexer.PropertyType != typeof(string)) return 15;
        ParameterInfo[] ix = indexer.GetIndexParameters();
        if (ix.Length != 2) return 16;
        if (ix[0].ParameterType != typeof(int)) return 17;
        if (ix[1].ParameterType != typeof(long)) return 18;
        if ((string)indexer.GetValue(s, new object[] { 3, 4L }) != "34") return 19;

        // The only member that reads `Signature.Arguments`, and the only place their *order* is
        // observable. Two distinct index parameter types are what make a swap visible.
        if (number.ToString() != "Int32 Number") return 20;
        if (indexer.ToString() != "System.String Item [Int32, Int64]") return 21;

        // Custom modifiers are stripped from the reported type, as `GetRetTypeHandleThrowing`
        // does; the byref is not.
        // Spelled without `MakeByRefType()`, which is a separate unimplemented QCall: this must
        // fail only if the property signature is decoded wrongly.
        PropertyInfo refReadonly = sample.GetProperty("RefReadonly");
        if (!refReadonly.PropertyType.IsByRef) return 25;
        if (refReadonly.PropertyType.GetElementType() != typeof(int)) return 26;

        // Properties declared *on* a generic type are covered by
        // `ReflectionPropertyTypeGenericDeclaringType.cs`, which is parked: reflecting over them at
        // all needs `ModuleHandle.ResolveMethod` for an open generic definition, which this slice
        // does not touch. `Nested` above still pins a generic *instantiation* as a property type,
        // which is the part that is reachable.

        return 0;
    }
}
