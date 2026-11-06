using System;
using System.Reflection;
using System.Security;

// A full `PublicKey=` the managed parser accepts (even-length hex) but that is not a public key
// blob: too short to carry the twelve-byte header and a key. `AssemblySpec::Init` runs
// `StrongNameIsValidPublicKey` over it before deriving a token and raises `SecurityException`
// "Invalid assembly public key." -- before any probe, so the name need not exist.
public class Program
{
    public static int Main (string[] args)
    {
        try
        {
            Assembly.Load ("No.Such, PublicKey=0011");
            return 1;
        }
        catch (SecurityException e)
        {
            if (!e.Message.StartsWith ("Invalid assembly public key."))
                return 2;
        }
        return 0;
    }
}
