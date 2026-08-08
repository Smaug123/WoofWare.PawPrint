using System;
using System.Collections.Generic;
using System.Reflection;

class Program
{
    // Every method here has its whole signature parsed by the Signature_Init QCall, not just the
    // part this program can read back: the QCall fills `_arguments` eagerly (CoreCLR allocates the
    // array even for a nullary method). So the byref and generic-instantiation parameters below are
    // exercised even though `GetParameters()` is not reachable yet — reading `ReturnType` is enough
    // to make the QCall concretize every parameter type, and an unsupported parameter shape would
    // fail the run rather than go unnoticed.

    static int Twice (int x, string s) => x * 2;

    static string Describe (List<int> xs, out bool ok)
    {
        ok = true;
        return "";
    }

    static void Bump (ref long n) => n++;

    static void Nothing ()
    {
    }

    int[] Instance (double d) => null;

    static MethodInfo Get (string name)
    {
        MethodInfo m = typeof (Program).GetMethod (
            name,
            BindingFlags.Static | BindingFlags.Instance | BindingFlags.NonPublic);

        if (m == null)
            throw new Exception ("could not find " + name);

        return m;
    }

    static int Main (string[] args)
    {
        // MethodBase.ReturnType is Signature.ReturnType, i.e. `_returnTypeORfieldType` as the
        // Signature_Init method arm fills it.
        if (Get ("Twice").ReturnType != typeof (int))
            return 1;

        if (Get ("Describe").ReturnType != typeof (string))
            return 2;

        if (Get ("Instance").ReturnType != typeof (int[]))
            return 3;

        // CoreCLR takes the return type from `msig.GetRetTypeHandleThrowing()`, which for a void
        // return is System.Void's TypeHandle rather than a null one.
        if (Get ("Nothing").ReturnType != typeof (void))
            return 4;

        if (Get ("Bump").ReturnType != typeof (void))
            return 5;

        // MethodBase.CallingConvention is Signature.CallingConvention, the low byte of
        // `_managedCallingConventionAndArgIteratorFlags`. That field holds the *translated*
        // CallingConventions bits SignatureNative::SetCallingConvention derives, not the raw ECMA
        // calling-convention byte: IMAGE_CEE_CS_CALLCONV_DEFAULT is 0x0 while
        // CallingConventions.Standard is 0x1, so a handler that stored the raw byte would report 0
        // here for every one of these methods.
        if (Get ("Twice").CallingConvention != CallingConventions.Standard)
            return 6;

        if (Get ("Nothing").CallingConvention != CallingConventions.Standard)
            return 7;

        if (Get ("Instance").CallingConvention != (CallingConventions.Standard | CallingConventions.HasThis))
            return 8;

        // None of these is a VarArgs method, so the VarArgs bit must never appear; a handler that
        // inverted the VarArgs/Standard choice would pass every check above but fail this one.
        if ((Get ("Twice").CallingConvention & CallingConventions.VarArgs) != 0)
            return 9;

        if ((Get ("Instance").CallingConvention & CallingConventions.VarArgs) != 0)
            return 10;

        // ExplicitThis never appears on a metadata method definition; only a calli site's signature
        // can carry it.
        if ((Get ("Instance").CallingConvention & CallingConventions.ExplicitThis) != 0)
            return 11;

        return 0;
    }
}
