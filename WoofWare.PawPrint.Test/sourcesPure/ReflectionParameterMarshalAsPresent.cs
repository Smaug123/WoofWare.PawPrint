using System;
using System.Reflection;
using System.Runtime.InteropServices;

// The present half of `ReflectionParameterMarshalAsAbsent.cs`: a parameter that really does carry a
// `[MarshalAs]`, so `MetadataImport.GetFieldMarshal` hands back a non-empty MarshalSpec blob and
// `GetMarshalAsCustomAttribute` goes on to parse it with `MetadataImport.GetMarshalAs`.
//
// Two of the three shapes are deliberate. `LPWStr` is the minimal one-byte blob. `LPArray` is the
// shape with trailing ECMA-335 II.23.2 compressed integers, which must be decoded in the right
// order (element type then size-param index, not the reverse), so an implementation that read them
// the other way round would report `ArraySubType = 1, SizeParamIndex = 7` and fail here rather than
// pass. `ByValArray` would be the richer shape, but C# rejects it on a parameter (CS7055: it is
// valid only for fields), and the fixture in `TestNativeMetadataImport.fs` covers it on one.
//
// `MarshalType`/`MarshalCookie` are absent here: they are the strings only
// `UnmanagedType.CustomMarshaler` carries, and `ReflectionParameterMarshalAsCustomMarshaler.cs` is
// the file about them.

public class Subject
{
    public static void Takes(
        [MarshalAs(UnmanagedType.LPWStr)] string text,
        [MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.I4, SizeParamIndex = 1)] int[] array,
        int plain)
    {
    }
}

public class Program
{
    public static int Main()
    {
        ParameterInfo[] parameters = typeof(Subject).GetMethod("Takes").GetParameters();

        object[] textAttrs = parameters[0].GetCustomAttributes(typeof(MarshalAsAttribute), false);
        if (textAttrs.Length != 1) return 1;
        MarshalAsAttribute text = (MarshalAsAttribute)textAttrs[0];
        if (text.Value != UnmanagedType.LPWStr) return 2;

        object[] arrayAttrs = parameters[1].GetCustomAttributes(typeof(MarshalAsAttribute), false);
        if (arrayAttrs.Length != 1) return 3;
        MarshalAsAttribute array = (MarshalAsAttribute)arrayAttrs[0];
        if (array.Value != UnmanagedType.LPArray) return 4;
        if (array.ArraySubType != UnmanagedType.I4) return 5;
        if (array.SizeParamIndex != 1) return 6;

        // The control: a neighbouring parameter with no row, so a handler that answered "the last
        // blob I saw" would fail here.
        if (parameters[2].GetCustomAttributes(typeof(MarshalAsAttribute), false).Length != 0) return 7;

        if (!parameters[0].IsDefined(typeof(MarshalAsAttribute), false)) return 8;
        if (parameters[2].IsDefined(typeof(MarshalAsAttribute), false)) return 9;

        return 0;
    }
}
