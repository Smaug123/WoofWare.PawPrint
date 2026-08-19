using System;
using System.Reflection;
using System.Runtime.InteropServices;

// `MarshalAsAttribute` is a *pseudo*-custom attribute: it is stored as a FieldMarshal row
// (ECMA-335 II.22.17) rather than as a CustomAttribute row, so reflecting for it goes through
// `PseudoCustomAttribute.GetCustomAttributes`, which asks `MetadataImport.GetFieldMarshal` for the
// row's NativeType blob and reports the attribute only when that blob is non-empty.
//
// Every check here is on the *absent* side, because that is the whole of what is reachable: a
// present `[MarshalAs]` makes the managed side go on to `MetadataImport.GetMarshalAs` to parse the
// blob, which is a separate unimplemented primitive. `ReflectionParameterMarshalAsPresent.cs` is
// parked on exactly that.
//
// The return parameter of `Nothing` is the interesting one and the reason this file exists: a
// method whose return value carries no attributes gets no Param row at all, so
// `RuntimeParameterInfo` manufactures the *nil* ParamDef token `0x08000000`
// (RuntimeParameterInfo.cs:175, 190, 208) and hands that to `GetFieldMarshal`. A handler that
// validated the row number before noticing the nil would abort here.
//
// The field route (`FieldInfo.GetCustomAttributes`) is not covered: it stops earlier still, at the
// unimplemented `RuntimeFieldHandle.GetToken`.

public class Subject
{
    public static void Nothing(int plain, string alsoPlain)
    {
    }

    public static int Returns(long only)
    {
        return (int)only;
    }
}

public class Program
{
    public static int Main()
    {
        MethodInfo nothing = typeof(Subject).GetMethod("Nothing");
        ParameterInfo[] parameters = nothing.GetParameters();
        if (parameters.Length != 2) return 1;

        // A parameter that does have a Param row, but no FieldMarshal row hanging off it.
        if (parameters[0].GetCustomAttributes(typeof(MarshalAsAttribute), false).Length != 0) return 2;
        if (parameters[1].GetCustomAttributes(typeof(MarshalAsAttribute), false).Length != 0) return 3;

        // `IsDefined` takes a different branch of the same pseudo-attribute code
        // (`PseudoCustomAttribute.IsDefined`), so it is not a restatement of the two checks above.
        if (parameters[0].IsDefined(typeof(MarshalAsAttribute), false)) return 4;
        if (parameters[1].IsDefined(typeof(MarshalAsAttribute), false)) return 5;

        // The nil-ParamDef shape: `void` return, so no sequence-0 Param row exists.
        ParameterInfo voidReturn = nothing.ReturnParameter;
        if (voidReturn.ParameterType != typeof(void)) return 6;
        if (voidReturn.GetCustomAttributes(typeof(MarshalAsAttribute), false).Length != 0) return 7;
        if (voidReturn.IsDefined(typeof(MarshalAsAttribute), false)) return 8;

        // A non-void return with no attributes on it: also no Param row, so also the nil token.
        // Here because a `void` return is special-cased in enough places that it is worth having a
        // second shape reach the same token.
        MethodInfo returns = typeof(Subject).GetMethod("Returns");
        ParameterInfo intReturn = returns.ReturnParameter;
        if (intReturn.ParameterType != typeof(int)) return 9;
        if (intReturn.GetCustomAttributes(typeof(MarshalAsAttribute), false).Length != 0) return 10;
        if (returns.GetParameters()[0].GetCustomAttributes(typeof(MarshalAsAttribute), false).Length != 0) return 11;

        return 0;
    }
}
