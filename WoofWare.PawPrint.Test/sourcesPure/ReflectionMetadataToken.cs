using System;
using System.Reflection;

// `MethodBase.MetadataToken`, which is `RuntimeMethodHandle.GetMethodDef` verbatim
// (`RuntimeMethodInfo.CoreCLR.cs:231`, `RuntimeConstructorInfo.CoreCLR.cs:170`). CoreCLR's FCall
// (`runtimehandles.cpp:1577`) returns `pMethod->GetMemberDef()`, a value *stored* on the MethodDesc
// when it was built rather than looked up, so the answer is a function of the MethodDef row alone.
//
// The checks that can detect a wrong *row* are the round trips through `Module.ResolveMethod`,
// rather than the bit-pattern ones. `ResolveMethod` is the independent token->row direction (a
// different QCall, `ModuleHandle_ResolveMethod`), so a token off by one rid resolves to a
// different method and the name comparison fails; a "table byte is 0x06" check cannot see that.
// Keep them if this file is ever trimmed.
//
// Everything is asserted through `Name` / `DeclaringType` / `MetadataToken`, never `GetParameters()`
// and never a binder overload such as `GetMethod(string, Type[])` or `GetConstructor(Type[])`: the
// binder calls `GetParameters()`, which goes on to `MetadataImport.Enum` for `mdtParamDef`, an
// unimplemented token type. That is the next blocker on this path, not this one.

public class Sample
{
    public Sample ()
    {
    }

    public int A ()
    {
        return 1;
    }

    public int B ()
    {
        return 2;
    }

    public static int S ()
    {
        return 4;
    }
}

public class Gen<T>
{
    public T Id (T x)
    {
        return x;
    }

    public U Map<U> (T x, U y)
    {
        return y;
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
        MethodInfo a = typeof (Sample).GetMethod ("A");
        MethodInfo b = typeof (Sample).GetMethod ("B");
        MethodInfo s = typeof (Sample).GetMethod ("S");

        ConstructorInfo[] ctors = typeof (Sample).GetConstructors ();

        if (ctors.Length != 1)
            return 1;

        ConstructorInfo ctor = ctors[0];

        // --- shape: a MethodDef token with a non-nil rid ------------------------------------------
        // `MdToken.IsNullToken` (MdImport.cs:149) is `(token & 0x00FFFFFF) == 0`, so the nil MethodDef
        // token is 0x06000000 rather than 0 -- CoreCLR's `MergeToken` (method.hpp:148) ORs the table
        // tag back in. A real method must not report it.
        if (Table (a.MetadataToken) != 0x06)
            return 2;

        if (Rid (a.MetadataToken) == 0)
            return 3;

        if (Table (s.MetadataToken) != 0x06)
            return 4;

        if (Rid (s.MetadataToken) == 0)
            return 5;

        // A constructor is an ordinary MethodDef row; `RuntimeConstructorInfo.MetadataToken` reaches
        // the same FCall.
        if (Table (ctor.MetadataToken) != 0x06)
            return 6;

        if (Rid (ctor.MetadataToken) == 0)
            return 7;

        // --- deterministic, and injective across a type's methods ---------------------------------
        if (typeof (Sample).GetMethod ("A").MetadataToken != a.MetadataToken)
            return 8;

        if (a.MetadataToken == b.MetadataToken)
            return 9;

        if (a.MetadataToken == ctor.MetadataToken)
            return 10;

        if (a.MetadataToken == s.MetadataToken)
            return 11;

        // --- round trip: the token names the row it came from -------------------------------------
        Module guestModule = typeof (Sample).Module;

        MethodBase backA = guestModule.ResolveMethod (a.MetadataToken);

        if (backA.Name != "A")
            return 12;

        if (backA.DeclaringType != typeof (Sample))
            return 13;

        MethodBase backS = guestModule.ResolveMethod (s.MetadataToken);

        if (backS.Name != "S")
            return 14;

        MethodBase backCtor = guestModule.ResolveMethod (ctor.MetadataToken);

        if (backCtor.Name != ".ctor")
            return 15;

        if (backCtor.DeclaringType != typeof (Sample))
            return 16;

        // --- instantiation independence -----------------------------------------------------------
        // `MethodDesc::GetMemberDef` reads a stored token, and `InstantiatedMethodDesc::CreateMethodDesc`
        // (genmeth.cpp:85,134) copies the *generic definition's* token onto every instantiation. So
        // every instantiation of a method reports one and the same token.
        MethodInfo idInt = typeof (Gen<int>).GetMethod ("Id");
        MethodInfo idStr = typeof (Gen<string>).GetMethod ("Id");

        if (idInt.MetadataToken != idStr.MetadataToken)
            return 17;

        // No `ResolveMethod` round trip for these two, unlike the non-generic methods above:
        // `ModuleHandle.ResolveMethod` refuses a MethodDef declared on a generic type in PawPrint
        // today ("the MethodHandle registry only supports fully concretised methods"), which is a
        // limitation of that QCall and nothing to do with the token. Measured by writing the check
        // and watching it abort there.

        // A method-level instantiation is the same story.
        MethodInfo map = typeof (Gen<int>).GetMethod ("Map");
        MethodInfo mapOfString = map.MakeGenericMethod (typeof (string));

        if (mapOfString.MetadataToken != map.MetadataToken)
            return 18;

        // --- cross-assembly -----------------------------------------------------------------------
        // A corelib method's token belongs to corelib's tables, so it must round-trip through
        // corelib's module. Tokens from different assemblies may legitimately collide numerically;
        // nothing here may assume global uniqueness.
        MethodInfo getType = typeof (object).GetMethod ("GetType");

        if (Table (getType.MetadataToken) != 0x06)
            return 19;

        if (typeof (object).Module.ResolveMethod (getType.MetadataToken).Name != "GetType")
            return 20;

        // Reflecting the inherited method on a guest type reports corelib's row, not a guest one.
        MethodInfo inherited = typeof (Sample).GetMethod ("GetType");

        if (inherited.MetadataToken != getType.MetadataToken)
            return 21;

        // --- a method token is not a type token ---------------------------------------------------
        if (Table (typeof (Sample).MetadataToken) != 0x02)
            return 22;

        return 0;
    }
}
