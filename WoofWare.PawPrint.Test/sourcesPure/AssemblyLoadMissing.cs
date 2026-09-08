using System;
using System.IO;
using System.Reflection;

// What `AssemblyNative_InternalLoad` reports when nothing answers to a name: a
// `FileNotFoundException` whose `FileName` is the *requested* display name -- the version only
// when a major was given, an unspecified lesser component as 65535, the culture, the public key
// token or `null`, and any flag segments -- and whose message wraps that name. A known assembly at
// too high a version is reported the same way as an unknown one. `Type.GetType` swallows the exception unless asked to throw.
public class Program
{
    static int Expect (int code, Func<object> load, string expectedFileName)
    {
        try
        {
            load ();
            return code;
        }
        catch (FileNotFoundException e)
        {
            if (e.FileName != expectedFileName)
                return code + 1;
            // The trailing newline is the PAL's `FormatMessage` rendering of ERROR_FILE_NOT_FOUND,
            // which `FileLoadException.FormatFileLoadExceptionMessage` pastes in verbatim.
            if (e.Message != "Could not load file or assembly '" + expectedFileName + "'. The system cannot find the file specified.\n")
                return code + 2;
            if (e.HResult != unchecked ((int) 0x80070002))
                return code + 3;
            return 0;
        }
    }

    public static int Main (string[] args)
    {
        int r;

        r = Expect (10, () => Assembly.Load ("No.Such.Assembly"), "No.Such.Assembly, Culture=neutral, PublicKeyToken=null");
        if (r != 0) return r;

        r = Expect (20,
            () => Assembly.Load ("No.Such.Assembly, Version=1.2.3.4, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"),
            "No.Such.Assembly, Version=1.2.3.4, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a");
        if (r != 0) return r;

        r = Expect (30, () => Assembly.Load ("No Such"), "No Such, Culture=neutral, PublicKeyToken=null");
        if (r != 0) return r;

        r = Expect (40,
            () => Assembly.Load (new AssemblyName ("No.Such") { Version = new Version (1, 2) }),
            "No.Such, Version=1.2.65535.65535, Culture=neutral, PublicKeyToken=null");
        if (r != 0) return r;

        r = Expect (50,
            () => Assembly.Load ("No.Such, Version=1.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a, Retargetable=Yes"),
            "No.Such, Version=1.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a, Retargetable=Yes");
        if (r != 0) return r;

        r = Expect (60,
            () => Assembly.Load ("No.Such, processorArchitecture=x86"),
            "No.Such, Culture=neutral, PublicKeyToken=null, processorArchitecture=x86");
        if (r != 0) return r;

        // Known, but not at that version.
        r = Expect (70,
            () => Assembly.Load ("System.Security.Claims, Version=99.0.0.0"),
            "System.Security.Claims, Version=99.0.0.0, Culture=neutral, PublicKeyToken=null");
        if (r != 0) return r;

        r = Expect (80,
            () => Assembly.Load ("System.Security.Claims, Version=99.1"),
            "System.Security.Claims, Version=99.1.65535.65535, Culture=neutral, PublicKeyToken=null");
        if (r != 0) return r;

        // A request for a culture with no satellite (`Culture=fr`) is reported the same way, but
        // is not a fact this guest can state: building the `AssemblyName` needs culture data the
        // emulated runtime does not carry, and raises `CultureNotFoundException` before the bind.
        // `TestAssemblyBinding` covers the satellite probe on the host side.

        if (Type.GetType ("Foo, No.Such.Assembly") != null)
            return 100;

        r = Expect (110,
            () => Type.GetType ("Foo, No.Such.Assembly", throwOnError: true),
            "No.Such.Assembly, Culture=neutral, PublicKeyToken=null");
        if (r != 0) return r;

        // The assembly binds and the type is what is missing, which is a different exception
        // from a different layer.
        if (Type.GetType ("System.Security.Principal.NoSuch, System.Security.Claims") != null)
            return 120;

        try
        {
            Type.GetType ("System.Security.Principal.NoSuch, System.Security.Claims", throwOnError: true);
            return 130;
        }
        catch (TypeLoadException e)
        {
            string claims = Assembly.Load ("System.Security.Claims").FullName;
            if (e.Message != "Could not resolve type 'System.Security.Principal.NoSuch' in assembly '" + claims + "'.")
                return 131;
        }

        return 0;
    }
}
