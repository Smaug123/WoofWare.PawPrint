using System;

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true)]
sealed class MarkerAttribute : Attribute
{
    public MarkerAttribute(int ctorArg)
    {
        CtorArg = ctorArg;
    }

    public int CtorArg { get; }

    // Exercised through the property route: GetProperty + InvokePropertySetter.
    public string Label { get; set; }

    public bool Flag { get; set; }

    // Exercised through the field route: GetField + FieldInfo.SetValue.
    public int Count;

    public string NullLabel { get; set; }
}

// A parameterless-ctor attribute reaches the same QCall by a different route: the managed caller
// computes the blob cursor itself (`blobStart + 4`) rather than receiving it from
// CreateCustomAttributeInstance, so this pins the second cursor provenance.
[AttributeUsage(AttributeTargets.Class)]
sealed class PlainMarkerAttribute : Attribute
{
    public int Amount;
}

[Marker(7, Label = "hi", Flag = true, Count = 42, NullLabel = null)]
[Marker(7, Label = "", Flag = false, Count = -1)]
sealed class Decorated
{
}

[PlainMarker(Amount = 99)]
sealed class PlainDecorated
{
}

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    static int Main()
    {
        object[] attrs = typeof(Decorated).GetCustomAttributes(typeof(MarkerAttribute), false);
        Check(attrs.Length == 2);

        if (attrs.Length != 2)
        {
            return firstFailure;
        }

        // Attribute order within a single decorated type is not something this test should pin, so
        // pick out the two applications by a value only one of them carries.
        MarkerAttribute withHi = null;
        MarkerAttribute withEmpty = null;

        for (int i = 0; i < attrs.Length; i++)
        {
            MarkerAttribute m = (MarkerAttribute)attrs[i];
            if (m.Label == "hi")
            {
                withHi = m;
            }
            else if (m.Label == "")
            {
                withEmpty = m;
            }
        }

        Check(withHi != null);
        Check(withEmpty != null);

        if (withHi == null || withEmpty == null)
        {
            return firstFailure;
        }

        // The fixed arg still decodes correctly alongside named args.
        Check(withHi.CtorArg == 7);
        Check(withEmpty.CtorArg == 7);

        // Property route, string value.
        Check(withHi.Label == "hi");
        Check(withEmpty.Label == "");

        // Property route, boxed bool: the box's type must be System.Boolean, not System.Int32, or
        // the setter's argument coercion rejects it.
        Check(withHi.Flag);
        Check(!withEmpty.Flag);

        // Field route, boxed int.
        Check(withHi.Count == 42);
        Check(withEmpty.Count == -1);

        // A named arg omitted from the second application must leave the default, not leak the
        // first application's value.
        Check(withHi.NullLabel == null);
        Check(withEmpty.NullLabel == null);

        // The parameterless-ctor cursor provenance.
        object[] plain = typeof(PlainDecorated).GetCustomAttributes(typeof(PlainMarkerAttribute), false);
        Check(plain.Length == 1);

        if (plain.Length == 1)
        {
            Check(((PlainMarkerAttribute)plain[0]).Amount == 99);
        }

        return firstFailure;
    }
}
