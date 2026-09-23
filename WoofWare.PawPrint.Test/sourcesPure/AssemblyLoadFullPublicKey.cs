using System;
using System.IO;
using System.Reflection;

// A display name carrying a full `PublicKey=` rather than a token. The managed parser accepts
// any even-length hex string; `AssemblyNative_InternalLoad` then derives the token from the key
// before binding, and the token plays no part in the bind -- so the framework's assembly binds
// under its own key, and a missing name is reported under the derived token. The sixteen-byte
// ECMA pseudo-key derives the same token as its well-known `PublicKeyToken=`. An odd-length hex
// string never reaches the runtime: the parser refuses it as an invalid name.
public class Program
{
    const string MicrosoftKey =
        "002400000480000094000000060200000024000052534131000400000100010007d1fa57c4aed9f0a32e84aa0faefd0de9e8fd6aec8f87fb03766c834c99921eb23be79ad9d5dcc1dd9ad236132102900b723cf980957fc4e177108fc607774f29e8320e92ea05ece4e821c0a5efe8f1645c4c0c93c1ab99285d622caa652c1dfad63d745d6f2de5f17e5eaf0fc4963d261c8a12436518206dc093344d5ad293";

    const string EcmaKey = "00000000000000000400000000000000";

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
        Assembly claims = Assembly.Load ("System.Security.Claims");
        if (!ReferenceEquals (claims, Assembly.Load ("System.Security.Claims, PublicKey=" + MicrosoftKey)))
            return 1;

        int r = ExpectNotFound (10, "No.Such, PublicKey=" + MicrosoftKey, "No.Such, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a");
        if (r != 0) return r;

        r = ExpectNotFound (20, "No.Such, PublicKey=" + EcmaKey, "No.Such, Culture=neutral, PublicKeyToken=b77a5c561934e089");
        if (r != 0) return r;

        r = ExpectNotFound (30, "No.Such, PublicKeyToken=b77a5c561934e089", "No.Such, Culture=neutral, PublicKeyToken=b77a5c561934e089");
        if (r != 0) return r;

        try
        {
            Assembly.Load ("No.Such, PublicKey=001");
            return 40;
        }
        catch (FileLoadException e)
        {
            if (e.Message != "The given assembly name was invalid.")
                return 41;
            if (e.FileName != "No.Such, PublicKey=001")
                return 42;
        }

        return 0;
    }
}
