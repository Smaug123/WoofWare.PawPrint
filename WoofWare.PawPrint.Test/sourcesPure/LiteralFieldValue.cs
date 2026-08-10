using System;
using System.Reflection;

// `FieldInfo.GetRawConstantValue` on a literal reaches `MdConstant.GetValue`, which calls the
// `MetadataImport.GetDefaultValue` InternalCall to read the field's Constant-table row.
//
// Two constraints, both load-bearing, and both the same as in the sibling LiteralFieldType.cs.
// Every subject must be `const`: a literal has no FieldDesc, so it is reflected over by
// `MdFieldInfo`, and only that class reaches `MdConstant`. And fields are reached through
// `GetFields()` rather than `GetField(name)`, so that this case depends on nothing but the
// Constant-table read: a name filter would drag in `MetadataImport.GetName` as well (see the
// sibling LiteralFieldName.cs, which covers that deliberately). Hence one holder type per literal.
public class IntConst { public const int Value = 42; }
public class NegativeSByteConst { public const sbyte Value = -1; }
public class NegativeLongConst { public const long Value = -1234567890123L; }
public class ULongConst { public const ulong Value = 18446744073709551615UL; }
public class BoolConst { public const bool Value = true; }
public class CharConst { public const char Value = 'q'; }
public class DoubleConst { public const double Value = 0.5; }
public class FloatConst { public const float Value = 0.25f; }
public class StringConst { public const string Value = "hello"; }
public class EmptyStringConst { public const string Value = ""; }
public class NullStringConst { public const string Value = null; }

public class LiteralFieldValue
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

    private static bool Check(Type t, object expected)
    {
        FieldInfo f = TheOnlyField(t);
        if (f == null || !f.IsLiteral)
        {
            return false;
        }

        object actual = f.GetRawConstantValue();
        if (expected == null)
        {
            return actual == null;
        }

        return expected.Equals(actual);
    }

    public static int Main(string[] argv)
    {
        if (!Check(typeof(IntConst), 42)) return 1;
        // -1 as an I1 is the byte 0xFF; the managed side recovers the sign by reinterpreting the
        // low byte, so a handler that widened it wrongly shows up here.
        if (!Check(typeof(NegativeSByteConst), (sbyte)(-1))) return 2;
        if (!Check(typeof(NegativeLongConst), -1234567890123L)) return 3;
        // All eight bytes set: distinguishes a buffer packed as unsigned from one that lost a bit.
        if (!Check(typeof(ULongConst), 18446744073709551615UL)) return 4;
        if (!Check(typeof(BoolConst), true)) return 5;
        if (!Check(typeof(CharConst), 'q')) return 6;
        // Floating point is reinterpreted from the buffer's bits, not converted.
        if (!Check(typeof(DoubleConst), 0.5)) return 7;
        if (!Check(typeof(FloatConst), 0.25f)) return 8;
        // The string cases are the ones that exercise the `char*` out-parameter.
        if (!Check(typeof(StringConst), "hello")) return 9;
        // An empty string constant has a zero-length blob, which the runtime reports as a *null*
        // pointer with length 0; `string.Empty` is recovered by the managed wrapper, not by us.
        if (!Check(typeof(EmptyStringConst), "")) return 10;
        // A null string constant is ELEMENT_TYPE_CLASS, which is a different code from "no Constant
        // row at all" and must not be confused with it.
        if (!Check(typeof(NullStringConst), null)) return 11;

        return 0;
    }
}
