using System;
using System.Reflection;
using System.Runtime.InteropServices;

// A `[MarshalAs(UnmanagedType.CustomMarshaler)]` parameter, the one MarshalSpec shape carrying
// strings. `MetadataImport.GetMarshalAs` hands each string back as a pointer to its first byte
// inside the blob together with its length prefix, and the managed wrapper decodes exactly that
// many bytes. So `MarshalType` reads back as the name, `MarshalTypeRef` resolves from it, and the
// absent cookie is the empty string rather than null: the blob still spells it, with a zero length
// prefix, and the pointer to it is not null.

public class Marshaller
{
}

public class Subject
{
    public static void Takes(
        [MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(Marshaller))] object custom,
        int plain)
    {
    }
}

public class Program
{
    public static int Main()
    {
        ParameterInfo[] parameters = typeof(Subject).GetMethod("Takes").GetParameters();

        object[] attrs = parameters[0].GetCustomAttributes(typeof(MarshalAsAttribute), false);
        if (attrs.Length != 1) return 1;
        MarshalAsAttribute custom = (MarshalAsAttribute)attrs[0];
        if (custom.Value != UnmanagedType.CustomMarshaler) return 2;
        if (custom.MarshalType != "Marshaller") return 3;
        if (custom.MarshalTypeRef != typeof(Marshaller)) return 4;
        if (custom.MarshalCookie != "") return 5;

        // The numeric properties a CustomMarshaler spec does not set stay at the QCall's zeroes.
        if (custom.SizeConst != 0) return 6;
        if (custom.SizeParamIndex != 0) return 7;
        if (custom.ArraySubType != 0) return 8;

        if (parameters[1].GetCustomAttributes(typeof(MarshalAsAttribute), false).Length != 0) return 9;

        return 0;
    }
}
