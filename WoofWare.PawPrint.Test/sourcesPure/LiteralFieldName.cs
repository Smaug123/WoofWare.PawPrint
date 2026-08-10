using System;
using System.Reflection;

// Reading a literal field's *name* reaches the `MetadataImport.GetName` InternalCall, by two
// different routes that fail differently:
//
//   - `MdFieldInfo.Name` calls it directly and returns whatever it produced;
//   - `RuntimeType.PopulateLiteralFields` calls it to evaluate a *name filter*, and a wrong answer
//     there shows up as `GetField(name)` returning null (or the wrong field) rather than as a wrong
//     string.
//
// Both are exercised below. Every subject must be `const`: a literal has no FieldDesc, so it is
// reflected over by `MdFieldInfo`, and a plain field's name comes from `RuntimeFieldHandle.GetName`
// instead and never reaches this handler.
//
// `Größe` is here because its UTF-8 encoding is longer than its character count, so an ASCII or
// UTF-16 encode of the name is visible end to end rather than only in the unit tests.
public class NamedConstants
{
    public const int Alpha = 1;
    public const int Beta = 2;
    public const int Größe = 3;
}

public class LiteralFieldName
{
    // Deliberately order-independent: neither runtime promises an order for `GetFields`, and this
    // test is compared against real .NET by exit code, so an order assumption would be testing the
    // BCL's iteration order rather than the name lookup.
    private static bool HasField(FieldInfo[] fields, string name)
    {
        foreach (FieldInfo f in fields)
        {
            if (f.Name == name)
            {
                return true;
            }
        }

        return false;
    }

    public static int Main(string[] argv)
    {
        Type t = typeof(NamedConstants);

        // Route 1: MdFieldInfo.Name.
        FieldInfo[] fields = t.GetFields();
        if (fields.Length != 3) return 1;
        if (!fields[0].IsLiteral) return 2;
        if (!HasField(fields, "Alpha")) return 3;
        if (!HasField(fields, "Beta")) return 4;
        if (!HasField(fields, "Größe")) return 5;

        // Route 2: the name filter inside PopulateLiteralFields. Asking for the *second* constant
        // means a handler that always answered with the first field's name would fail here, and
        // checking the value proves the filter picked the field it was asked for.
        FieldInfo beta = t.GetField("Beta");
        if (beta == null) return 6;
        if (!beta.IsLiteral) return 7;
        if (!((int)beta.GetRawConstantValue() == 2)) return 8;

        FieldInfo größe = t.GetField("Größe");
        if (größe == null) return 9;
        if (!((int)größe.GetRawConstantValue() == 3)) return 10;

        // A filter that matched everything — the failure mode of a handler that returned a constant
        // or empty name — would find a field here.
        if (t.GetField("Gamma") != null) return 11;

        return 0;
    }
}
