using System;

class Element { }

class SubElement : Element { }

public class Program
{
    public static int Main(string[] args)
    {
        // Array.SetValue on a reference-typed array reaches CastHelpers.IsInstanceOfAny to
        // decide whether the value may be stored (Array.CoreCLR.cs:227), and on a value-typed
        // array to decide whether it may be unboxed into the slot (Array.CoreCLR.cs:237).
        // Both are the non-throwing arm of the IsInstanceOf_NoCacheLookup QCall: managed code
        // turns a false into InvalidCastException(SR.InvalidCast_StoreArrayElement) itself.

        Element[] refs = new Element[2];
        refs.SetValue(new SubElement(), 0);
        if (refs[0] is not SubElement) return 1;

        // Storing an unrelated reference type is refused.
        try
        {
            refs.SetValue("not an Element", 1);
            return 2;
        }
        catch (InvalidCastException)
        {
            if (refs[1] != null) return 3;
        }

        // Everything stores into an object[]; the element MethodTable is System.Object, which
        // managed code short-circuits before the QCall.
        object[] objects = new object[1];
        objects.SetValue("anything", 0);
        if ((string) objects[0] != "anything") return 4;

        // The value-typed arm of Array.SetValue (Array.CoreCLR.cs:237) also consults the
        // QCall, and an `int?[]` element would additionally exercise its Nullable branch --
        // but managed code then completes the store via CastHelpers.Unbox_Nullable, which
        // needs the MethodTable::NullableValueAddrOffset projection PawPrint does not have.
        // The Nullable branch itself is covered from `Type.IsInstanceOfType` in
        // IsInstanceOfTypeQCall.cs, which needs no unboxing.

        return 0;
    }
}
