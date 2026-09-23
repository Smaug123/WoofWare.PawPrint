using System;
using System.IO;
using System.Reflection;

// `ContentType=WindowsRuntime` in a display name. Nothing the default load context holds has
// that content type, so the request is not found whether or not the simple name is known, and
// the `FileName` reported carries the content-type segment.
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
