using System;
using System.Reflection;
using System.Runtime.InteropServices;

// A `[MarshalAs(UnmanagedType.CustomMarshaler)]` parameter, the one MarshalSpec shape carrying
// strings. `MetadataImport.GetMarshalAs` hands each string back as a pointer to its first byte
// inside the blob, and the managed wrapper reads it with `CreateReadOnlySpanFromNullTerminated`.
// MarshalSpec strings are length-prefixed rather than NUL-terminated, so where the scan stops
// depends on the bytes that follow:
//
// * With no cookie, the marshaler type name is followed by the cookie's own zero length prefix,
//   so `MarshalType` reads back exactly and `MarshalTypeRef` resolves from it.
// * The cookie is the last thing in the blob, so its scan always runs on into whatever `#Blob`
//   bytes follow. Real .NET reports those bytes; this file asserts only that the scan finished
//   and produced a string, because *which* bytes follow is a fact about Roslyn's heap layout.
//   `TestNativeMetadataImport.fs` compares those bytes against the host runtime's.

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
        if (custom.MarshalCookie == null) return 5;

        // The numeric properties a CustomMarshaler spec does not set stay at the FCall's zeroes.
        if (custom.SizeConst != 0) return 6;
        if (custom.SizeParamIndex != 0) return 7;
        if (custom.ArraySubType != 0) return 8;

        if (parameters[1].GetCustomAttributes(typeof(MarshalAsAttribute), false).Length != 0) return 9;

        return 0;
    }
}
