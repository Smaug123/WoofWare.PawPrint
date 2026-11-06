using System;
using System.Reflection;

// `Assembly.Load(string)` and an assembly-qualified `Type.GetType` both reach the
// `AssemblyNative_InternalLoad` QCall, which binds a display name the way CoreCLR's default load
// context does: by simple name ignoring case, with the culture, and with the requested version no
// higher than the found one. The public key token is not consulted. Nothing here holds a
// compile-time reference to System.Security.Claims, so the string is the only way to reach it.
public class Program
{
    public static int Main (string[] args)
    {
        Assembly claims = Assembly.Load ("System.Security.Claims");
        if (claims == null)
            return 1;
        if (!claims.FullName.StartsWith ("System.Security.Claims, Version="))
            return 2;

        // One assembly per simple name: every spelling that binds gives back the same object.
        if (!ReferenceEquals (claims, Assembly.Load ("System.Security.Claims")))
            return 3;
        if (!ReferenceEquals (claims, Assembly.Load ("system.security.claims")))
            return 4;
        if (!ReferenceEquals (claims, Assembly.Load ("System.Security.Claims, Version=4.0.0.0")))
            return 5;
        if (!ReferenceEquals (claims, Assembly.Load ("System.Security.Claims, Version=10.0")))
            return 6;
        if (!ReferenceEquals (claims, Assembly.Load ("System.Security.Claims, PublicKeyToken=0000000000000000")))
            return 7;
        if (!ReferenceEquals (claims, Assembly.Load ("System.Security.Claims, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a")))
            return 8;
        if (!ReferenceEquals (claims, Assembly.Load (new AssemblyName ("System.Security.Claims"))))
            return 9;

        Type principal = Type.GetType ("System.Security.Principal.GenericPrincipal, System.Security.Claims");
        if (principal == null)
            return 10;
        if (principal.FullName != "System.Security.Principal.GenericPrincipal")
            return 11;
        if (!ReferenceEquals (principal.Assembly, claims))
            return 12;

        if (!ReferenceEquals (Assembly.Load ("System.Private.CoreLib"), typeof (object).Assembly))
            return 13;
        if (!Assembly.Load ("System.Runtime").FullName.StartsWith ("System.Runtime, Version="))
            return 14;

        return 0;
    }
}
