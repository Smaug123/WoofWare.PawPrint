using System;
using System.Reflection;
using System.Runtime.InteropServices;

// `ParameterInfo.HasDefaultValue`, `DefaultValue` and `RawDefaultValue`, which all funnel into
// `RuntimeParameterInfo.TryGetDefaultValueInternal` and so into `MetadataImport.GetDefaultValue`
// with a ParamDef token.
//
// Two of these parameters deliberately do *not* exercise that primitive, and are here as controls:
// `Dec` has no Constant row at all (C# encodes a decimal default as a `[DecimalConstant]` custom
// attribute), and `NoDefault` has a Param row with no Constant row, so both take the
// custom-attribute fallback instead. If a change to the primitive broke that fallback, only these
// two would notice.
//
// `Enumerated` is the only parameter whose `DefaultValue` and `RawDefaultValue` disagree —
// `MdConstant.GetValue` reports an enum-typed default as the enum itself unless `raw` is set — so
// it is the single check that tells the two paths apart.

public enum Colour
{
    Red = 0,
    Green = 5,
}

public class Sample
{
    public void Defaults(
        int mandatory,
        int i = 7,
        string s = "hi",
        double d = 2.5,
        bool b = true,
        char c = 'x',
        long l = -9000000000L,
        byte by = 200,
        string nul = null,
        string empty = "",
        // Five UTF-16 code units in ten bytes, across four code points: the trailing paw print is
        // astral, so it is a surrogate pair. The FCall reports a string constant's length in code
        // units while reporting every other type's in bytes, so a handler measuring bytes gets 10;
        // one counting code *points* gets 4, and only a surrogate pair separates that from 5.
        string nonAscii = "mäß\U0001F43E",
        Colour enumerated = Colour.Green,
        decimal dec = 1.5m,
        int? nullableInt = 3)
    {
    }

    public void NoDefault([Optional] int noDefault)
    {
    }
}

public class Program
{
    public static int Main()
    {
        ParameterInfo[] ps = typeof(Sample).GetMethod("Defaults").GetParameters();
        if (ps.Length != 14) return 1;

        // No Constant row, and not optional: DBNull from the primitive, and no fallback rescues it.
        if (ps[0].HasDefaultValue) return 2;
        if (!(ps[0].DefaultValue is DBNull)) return 3;
        if (!(ps[0].RawDefaultValue is DBNull)) return 4;

        if (!ps[1].HasDefaultValue) return 5;
        if (!(ps[1].DefaultValue is int)) return 6;
        if ((int)ps[1].DefaultValue != 7) return 7;
        if ((int)ps[1].RawDefaultValue != 7) return 8;

        if (!(ps[2].DefaultValue is string)) return 9;
        if ((string)ps[2].DefaultValue != "hi") return 10;

        if (!(ps[3].DefaultValue is double)) return 11;
        if ((double)ps[3].DefaultValue != 2.5) return 12;

        if (!(ps[4].DefaultValue is bool)) return 13;
        if (!(bool)ps[4].DefaultValue) return 14;

        if (!(ps[5].DefaultValue is char)) return 15;
        if ((char)ps[5].DefaultValue != 'x') return 16;

        if (!(ps[6].DefaultValue is long)) return 17;
        if ((long)ps[6].DefaultValue != -9000000000L) return 18;

        if (!(ps[7].DefaultValue is byte)) return 19;
        if ((byte)ps[7].DefaultValue != 200) return 20;

        // ELEMENT_TYPE_CLASS: ECMA-335 II.22.9 permits it only as a null reference.
        if (!ps[8].HasDefaultValue) return 21;
        if (ps[8].DefaultValue != null) return 22;

        // An empty string constant has a zero-length blob, which the FCall reports as a null
        // pointer; `string.Empty` is recovered by the managed wrapper, not by the primitive.
        if (!(ps[9].DefaultValue is string)) return 23;
        if ((string)ps[9].DefaultValue != "") return 24;

        if (!(ps[10].DefaultValue is string)) return 25;
        if ((string)ps[10].DefaultValue != "mäß\U0001F43E") return 26;
        if (((string)ps[10].DefaultValue).Length != 5) return 27;

        // The one place raw and non-raw differ.
        if (!(ps[11].DefaultValue is Colour)) return 28;
        if ((Colour)ps[11].DefaultValue != Colour.Green) return 29;
        if (!(ps[11].RawDefaultValue is int)) return 30;
        if ((int)ps[11].RawDefaultValue != 5) return 31;

        // Control: no Constant row, so this is the `[DecimalConstant]` fallback.
        if (!(ps[12].DefaultValue is decimal)) return 32;
        if ((decimal)ps[12].DefaultValue != 1.5m) return 33;

        // A `Nullable<int>` default is reported as the underlying value, not as a Nullable.
        if (!(ps[13].DefaultValue is int)) return 34;
        if ((int)ps[13].DefaultValue != 3) return 35;

        // Control: a Param row with no Constant row, on an optional parameter. The primitive says
        // "no default"; `Type.Missing` comes from `GetDefaultValue`'s `IsOptional` fallback.
        ParameterInfo opt = typeof(Sample).GetMethod("NoDefault").GetParameters()[0];
        if (opt.HasDefaultValue) return 36;
        if (!opt.IsOptional) return 37;
        if (opt.DefaultValue != Type.Missing) return 38;

        return 0;
    }
}
