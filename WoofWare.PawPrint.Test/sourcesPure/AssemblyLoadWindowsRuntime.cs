using System;
using System.IO;
using System.Reflection;

// `ContentType=WindowsRuntime` in a display name. On .NET 10, nothing the default load context
// holds has that content type, so the request is not found whether or not the simple name is
// known, and the `FileName` reported carries the content-type segment.
//
// .NET 11's binder ignores a requested content type (`BINDER_SPACE::AssemblyName::Init` no longer
// copies it, and the display name no longer prints it), so there the first request is not found
// under a `FileName` without the segment, and this exits 11 (measured on 11.0.0-rc.1).
public class Program
{
    static int ExpectNotFound (int code, string name, string expectedFileName)
    {
        try
        {
            Assembly.Load (name);
            return code;
        }
        catch (FileNotFoundException e)
        {
            return e.FileName == expectedFileName ? 0 : code + 1;
        }
    }

    public static int Main (string[] args)
    {
        int r = ExpectNotFound (10, "No.Such, ContentType=WindowsRuntime", "No.Such, Culture=neutral, PublicKeyToken=null, ContentType=WindowsRuntime");
        if (r != 0) return r;

        // Known by name, and still not found.
        r = ExpectNotFound (20, "System.Security.Claims, ContentType=WindowsRuntime", "System.Security.Claims, Culture=neutral, PublicKeyToken=null, ContentType=WindowsRuntime");
        if (r != 0) return r;

        return 0;
    }
}
