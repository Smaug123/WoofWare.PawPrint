using System;
using System.Reflection;

// Properties declared *on* a generic type, which is the shape that makes `Signature_Init` resolve
// a PropertySig against the declaring type's instantiation
// (`SigTypeContext::InitTypeContext(declType, ...)`).
//
// `Type.GetProperty` on a generic instantiation populates the property list, which resolves the
// accessors' MethodDef tokens through `ModuleHandle.ResolveMethod`. That answers with the *typical*
// definition -- `Holder<T>.get_Value`, declaring type the open definition -- and
// `RuntimeType.GetMethodBase` binds it back onto `Holder<int>`, so the two halves have to agree
// about which type is doing the substituting.
//
// `ReflectionPropertyType.cs` covers a generic instantiation appearing as a property *type*; this
// file covers a class type parameter appearing *in* the property's signature.

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
