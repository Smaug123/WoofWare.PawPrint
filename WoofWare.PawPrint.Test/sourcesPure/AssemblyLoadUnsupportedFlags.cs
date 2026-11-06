using System;
using System.IO;
using System.Reflection;

// Requests whose `AssemblyNameFlags` carry more than the public-key bit. `processorArchitecture=MSIL`
// binds a framework assembly as if it named no architecture, and each flag adds its own segment to
// the `FileName` a failed load reports, after the token.
public class Program
{
    static int Expect (int code, string name, string expectedFileName)
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
        Assembly claims = Assembly.Load ("System.Security.Claims");
        if (!ReferenceEquals (claims, Assembly.Load ("System.Security.Claims, processorArchitecture=MSIL")))
            return 1;

        int r;

        r = Expect (10,
            "No.Such, processorArchitecture=MSIL",
            "No.Such, Culture=neutral, PublicKeyToken=null, processorArchitecture=MSIL");
        if (r != 0) return r;

        r = Expect (20,
            "No.Such, Version=1.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a, Retargetable=Yes",
            "No.Such, Version=1.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a, Retargetable=Yes");
        if (r != 0) return r;

        r = Expect (30,
            "No.Such, processorArchitecture=x86",
            "No.Such, Culture=neutral, PublicKeyToken=null, processorArchitecture=x86");
        if (r != 0) return r;

        return 0;
    }
}
