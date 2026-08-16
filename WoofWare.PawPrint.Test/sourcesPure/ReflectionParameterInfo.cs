using System;
using System.Reflection;

// `MethodBase.GetParameters`, which reaches `RuntimeParameterInfo.GetParameters`
// (`RuntimeParameterInfo.cs:29`). That drives three `MetadataImport` primitives: the `Enum` QCall
// over `mdtParamDef` for the method's Param run, `GetParamDefProps` for each row's sequence and
// flags, and `GetName` when a `ParameterInfo` is asked for its name.
//
// The checks that can detect a *wrong row* are the `Name` ones: a Param run off by one rid, or in
// the wrong order, still has the right length and the right sequence numbers, but names its
// parameters wrongly. Keep them if this file is ever trimmed.
//
// Every lookup is `GetMethod(string)` rather than a binder-taking overload such as
// `GetMethod(string, Type[])`: the default binder calls `GetParameters()` on every candidate, so a
// failure here would surface as an AmbiguousMatchException far from its cause. Generic methods are
// absent because `Signature_Init` on a generic method *definition* is a separate unimplemented
// primitive, unrelated to parameters.

public class Sample
{
    public Sample (int ctorArg)
    {
    }

    public int TwoParams (int x, string y)
    {
        return x;
    }

    public void NoParams ()
    {
    }

    // A plain `ref` sets neither the In nor the Out flag on its Param row, so this separates "read
    // the row's flags" from "read the signature's byref-ness".
    public void Modifiers (out int o, ref string r, in double i)
    {
        o = 0;
        r = null;
    }

    public void Defaulted (int a, int b = 7)
    {
    }

    public void Variadic (params int[] xs)
    {
    }

    public int Returns (int q)
    {
        return q;
    }
}

public class Program
{
    /// The table byte: ECMA-335 II.22 metadata tokens are `(table << 24) | rid`.
    private static int Table (int token)
    {
        return (int) ((uint) token >> 24);
    }

    private static int Rid (int token)
    {
        return token & 0x00FFFFFF;
    }

    public static int Main (string[] args)
    {
        Type t = typeof (Sample);

        // --- ordinary method: count, order, names, types ------------------------------------------
        ParameterInfo[] ps = t.GetMethod ("TwoParams").GetParameters ();

        if (ps.Length != 2)
            return 1;

        if (ps[0].Position != 0 || ps[1].Position != 1)
            return 2;

        if (ps[0].Name != "x" || ps[1].Name != "y")
            return 3;

        if (ps[0].ParameterType != typeof (int))
            return 4;

        if (ps[1].ParameterType != typeof (string))
            return 5;

        if (ps[0].Member != t.GetMethod ("TwoParams"))
            return 6;

        if (t.GetMethod ("NoParams").GetParameters ().Length != 0)
            return 7;

        // --- a constructor is an ordinary MethodDef row with its own Param run ---------------------
        ConstructorInfo[] ctors = t.GetConstructors ();

        if (ctors.Length != 1)
            return 8;

        ParameterInfo[] cs = ctors[0].GetParameters ();

        if (cs.Length != 1)
            return 9;

        if (cs[0].Name != "ctorArg")
            return 10;

        if (cs[0].Position != 0)
            return 11;

        // --- Param.Flags, which the signature cannot supply ----------------------------------------
        ParameterInfo[] ms = t.GetMethod ("Modifiers").GetParameters ();

        if (ms.Length != 3)
            return 12;

        if (!ms[0].IsOut)
            return 13;

        if (ms[1].IsOut || ms[1].IsIn)
            return 14;

        if (!ms[2].IsIn)
            return 15;

        if (!ms[0].ParameterType.IsByRef)
            return 16;

        if (ms[0].Name != "o" || ms[1].Name != "r" || ms[2].Name != "i")
            return 17;

        ParameterInfo[] os = t.GetMethod ("Defaulted").GetParameters ();

        if (os.Length != 2)
            return 18;

        if (os[0].IsOptional)
            return 19;

        if (!os[1].IsOptional)
            return 20;

        ParameterInfo[] vs = t.GetMethod ("Variadic").GetParameters ();

        if (vs.Length != 1)
            return 21;

        if (vs[0].Name != "xs")
            return 22;

        if (vs[0].ParameterType != typeof (int[]))
            return 23;

        // --- the return parameter is position -1, i.e. the sequence-0 row -------------------------
        // Roslyn emits no Param row for it here, so this is the "fill in the ones without tokens"
        // path rather than a row read back out of the table.
        MethodInfo returns = t.GetMethod ("Returns");

        if (returns.ReturnParameter.Position != -1)
            return 24;

        if (returns.ReturnParameter.ParameterType != typeof (int))
            return 25;

        // --- shape: a ParamDef token with a non-nil rid --------------------------------------------
        ParameterInfo p0 = t.GetMethod ("TwoParams").GetParameters ()[0];

        if (Table (p0.MetadataToken) != 0x08)
            return 26;

        if (Rid (p0.MetadataToken) == 0)
            return 27;

        // --- deterministic, and injective across a method's parameters -----------------------------
        if (p0.MetadataToken == t.GetMethod ("TwoParams").GetParameters ()[1].MetadataToken)
            return 28;

        if (p0.MetadataToken != t.GetMethod ("TwoParams").GetParameters ()[0].MetadataToken)
            return 29;

        return 0;
    }
}
