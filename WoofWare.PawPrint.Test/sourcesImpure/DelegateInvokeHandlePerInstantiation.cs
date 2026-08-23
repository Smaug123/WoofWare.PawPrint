using System;
using System.Reflection;

// PawPrint's side of a recorded divergence: the handle `Delegate::GetInvokeMethod` answers is
// per-*instantiation*, where CoreCLR's is per-canonical-form. `COMDelegate::GetInvokeMethod`
// (comdelegate.cpp:2156) reads a field of the `DelegateEEClass`, and an `EEClass` is shared by
// every instantiation whose arguments are all reference types, so real .NET hands one
// `MethodDesc*` to `Func<string, int>` and `Func<object, int>` alike. PawPrint mints a registry
// id per exact instantiation and so hands out two.
//
// Impure because that is exactly what the differential oracle would disagree about: measured on
// .NET 10, check 3 below returns 3 there. See docs/divergences.md, "A generic delegate type's
// `Invoke` handle is per-instantiation, not per-canonical-form", and the value/reference pair in
// `sourcesPure/DelegateDynamicInvoke.cs` that both runtimes do agree on.
//
// Returns 0 on success, or the number of the first check that failed.

public delegate int Taking<T> (T t);

public class DelegateInvokeHandlePerInstantiation
{
    private static MethodInfo _getInvokeMethod;

    private static IntPtr Handle (Delegate d)
    {
        return (IntPtr) _getInvokeMethod.Invoke (d, null);
    }

    private static int OfString (string s)
    {
        return s.Length;
    }

    private static int OfObject (object o)
    {
        return 1;
    }

    private static int OfInt (int i)
    {
        return i;
    }

    public static int Main (string[] argv)
    {
        _getInvokeMethod = typeof (Delegate).GetMethod (
            "GetInvokeMethod",
            BindingFlags.NonPublic | BindingFlags.Instance,
            null,
            Type.EmptyTypes,
            null);
        if (_getInvokeMethod == null) return 1;

        Func<string, int> str = OfString;
        Func<object, int> obj = OfObject;
        Func<int, int> num = OfInt;

        // The control, and the half that agrees with real .NET: a value-type instantiation shares
        // no `EEClass` with a reference-type one, so both runtimes answer two handles.
        if (Handle (str).Equals (Handle (num))) return 2;

        // The divergence. Real .NET returns 3 here.
        if (Handle (str).Equals (Handle (obj))) return 3;

        // Not a `Func<>` peculiarity: a user-declared generic delegate behaves the same way.
        Taking<string> tstr = OfString;
        Taking<object> tobj = OfObject;
        if (Handle (tstr).Equals (Handle (tobj))) return 4;

        // And the divergence is confined to identity. Both delegates still invoke correctly, which
        // is what makes this representational rather than semantic: `RuntimeType.GetMethodBase`
        // reaches the same `MethodInfo` from PawPrint's exact handle that CoreCLR reaches by
        // remapping its shared one onto the exact reflected type.
        if (!str.DynamicInvoke ("abcd").Equals (4)) return 5;
        if (!obj.DynamicInvoke (new object ()).Equals (1)) return 6;
        if (!num.DynamicInvoke (7).Equals (7)) return 7;

        // Asking twice still gives one answer, so what differs between the two instantiations is
        // the identity of the delegate type and not a fresh id per call.
        if (!Handle (str).Equals (Handle (str))) return 8;
        if (!Handle (obj).Equals (Handle (obj))) return 9;

        return 0;
    }
}
