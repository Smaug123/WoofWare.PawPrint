using System;
using System.Reflection;
using System.Runtime.InteropServices;

// A `[MarshalAs(UnmanagedType.CustomMarshaler)]` whose marshaler type name names no type. The
// managed wrapper over `MetadataImport_GetMarshalAs` resolves that name with
// `TypeNameResolver.GetTypeReferencedByCustomAttribute`, which throws TypeLoadException, and
// catches it: the attribute still reports the name as written, with no `MarshalTypeRef`. The
// cookie is spelled out here, so it reads back as written too.

public class Subject
{
    public static void Takes(
        [MarshalAs(UnmanagedType.CustomMarshaler, MarshalType = "No.Such.Marshaller", MarshalCookie = "cookie")] object custom)
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
        if (custom.MarshalType != "No.Such.Marshaller") return 3;
        if (custom.MarshalTypeRef != null) return 4;
        if (custom.MarshalCookie != "cookie") return 5;

        return 0;
    }
}
