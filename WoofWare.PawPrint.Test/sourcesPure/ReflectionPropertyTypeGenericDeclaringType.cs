using System;
using System.Reflection;

// Parked. Properties declared *on* a generic type, which is the shape that makes
// `Signature_Init` resolve a PropertySig against the declaring type's instantiation
// (`SigTypeContext::InitTypeContext(declType, ...)`).
//
// Blocked before it gets there: `Type.GetProperty` on a generic instantiation populates the
// property list, which resolves the accessors' MethodDef tokens, and that dies in
//   TODO: ModuleHandle.ResolveMethod: MethodDef token ... declared on generic type Holder`1;
//   CoreCLR returns the open metadata definition without consuming the caller's
//   typeInstantiation, but the MethodHandle registry only supports fully concretised methods.
// So this is blocked on the MethodHandle registry, not on signature decoding.
//
// `ReflectionPropertyType.cs` covers a generic instantiation appearing as a property *type*, which
// is reachable; this file covers a class type parameter appearing *in* the property's signature,
// which is not.

public class Holder<T>
{
    public T Value { get; set; }
    public T[] Values { get; set; }
}

public class Program
{
    public static int Main()
    {
        // The class type parameter as the property type, substituted from the instantiation.
        if (typeof(Holder<int>).GetProperty("Value").PropertyType != typeof(int)) return 1;
        if (typeof(Holder<string>).GetProperty("Value").PropertyType != typeof(string)) return 2;

        // Under a structural type, so the substitution happens below the top level.
        if (typeof(Holder<string>).GetProperty("Values").PropertyType != typeof(string[])) return 3;

        Holder<int> h = new Holder<int>();
        PropertyInfo value = typeof(Holder<int>).GetProperty("Value");
        value.SetValue(h, 9);
        if ((int)value.GetValue(h) != 9) return 4;

        return 0;
    }
}
