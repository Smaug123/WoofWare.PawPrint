using System;
using System.Reflection;

// A literal field has no FieldDesc, so CoreCLR reflects over it with `MdFieldInfo` rather than
// `RtFieldInfo`, and `MdFieldInfo.FieldType` is the only managed caller of
// `MetadataImport.GetSigOfFieldDef`: it reads the raw signature blob and then builds a `Signature`
// from the blob pointer alone (`new Signature(void*, int, RuntimeType)`), which is the handle-less
// path of the `Signature_Init` QCall. A plain static or instance field takes `RtFieldInfo`'s
// field-handle path instead and would exercise none of that, so `NotALiteral` below is the contrast
// case rather than the subject.
//
// Each literal gets its own holder type, and fields are reached through `GetFields()` rather than
// `GetField(name)`, deliberately: a name filter makes `PopulateLiteralFields` call
// `MetadataImport.GetName` (and `MdFieldInfo.Name` does too), which PawPrint does not implement.
// Identifying the field positionally keeps this case about the signature blob alone.
public class IntLiteralHolder
{
    public const int Value = 42;
}

public class StringLiteralHolder
{
    public const string Value = "hi";
}

public class BoolLiteralHolder
{
    public const bool Value = true;
}

public class NonLiteralHolder
{
    public static int NotALiteral = 7;
}

public class LiteralFieldType
{
    private static FieldInfo TheOnlyField(Type t)
    {
        FieldInfo[] fields = t.GetFields();
        if (fields.Length != 1)
        {
            return null;
        }

        return fields[0];
    }

    public static int Main(string[] argv)
    {
        FieldInfo intField = TheOnlyField(typeof(IntLiteralHolder));
        if (intField == null) return 1;
        // The premise of the test: were this false, everything below would be exercising the
        // field-handle path and would pass without touching the code under test.
        if (!intField.IsLiteral) return 2;
        if (intField.FieldType != typeof(int)) return 3;

        FieldInfo stringField = TheOnlyField(typeof(StringLiteralHolder));
        if (stringField == null) return 4;
        if (!stringField.IsLiteral) return 5;
        if (stringField.FieldType != typeof(string)) return 6;

        FieldInfo boolField = TheOnlyField(typeof(BoolLiteralHolder));
        if (boolField == null) return 7;
        if (!boolField.IsLiteral) return 8;
        if (boolField.FieldType != typeof(bool)) return 9;

        // Reading FieldType twice must give the same answer: MdFieldInfo caches it in m_fieldType,
        // so the second read takes a different route through the managed code than the first.
        if (intField.FieldType != typeof(int)) return 10;

        FieldInfo plain = TheOnlyField(typeof(NonLiteralHolder));
        if (plain == null) return 11;
        if (plain.IsLiteral) return 12;
        if (plain.FieldType != typeof(int)) return 13;

        return 0;
    }
}
