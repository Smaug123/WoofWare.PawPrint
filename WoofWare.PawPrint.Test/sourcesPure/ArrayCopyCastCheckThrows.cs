using System;
using System.Collections.Generic;

class Base { }

class Derived : Base { }

namespace Outer
{
    class Nested { }

    class OtherNested { }
}

class Enclosing
{
    public class Inner { }

    public class OtherInner { }
}

public class Program
{
    public static int Main(string[] args)
    {
        // Array.Copy between two reference-typed arrays whose element types are not
        // assignment-compatible takes CopyImplCastCheckEachElement, which calls
        // CastHelpers.ChkCastAny per element (Array.cs:619). That is the *throwing* arm of the
        // IsInstanceOf_NoCacheLookup QCall: throwCastException is true, so a failing element
        // makes the QCall itself raise InvalidCastException rather than return false.

        // Compatible elements copy fine.
        object[] source = new object[] { new Derived(), new Derived() };
        Base[] dest = new Base[2];
        Array.Copy(source, dest, 2);
        if (dest[0] == null || dest[1] == null) return 1;

        // An element that is not a Base fails on the element that is wrong, and only then:
        // the first element must already have been copied.
        object[] mixed = new object[] { new Derived(), "not a Base" };
        Base[] dest2 = new Base[2];

        try
        {
            Array.Copy(mixed, dest2, 2);
            return 2;
        }
        catch (InvalidCastException e)
        {
            if (dest2[0] == null) return 3;
            if (dest2[1] != null) return 4;

            // The EE formats this message itself (IDS_EE_CANNOTCAST in mscorrc.rc, thrown from
            // COMPlusThrowInvalidCastException), so it is part of the QCall's contract rather
            // than of any managed throw helper.
            if (e.Message != "Unable to cast object of type 'System.String' to type 'Base'.")
            {
                Console.WriteLine(e.Message);
                return 5;
            }
        }

        // The EE renders those names with TypeHandle::GetName, which reads the TypeDef row's
        // own namespace and name and does *not* walk the nesting chain -- so a nested type
        // appears under its bare name, with neither its enclosing type nor a namespace. This
        // differs from the reflection renderer behind Type.FullName, so it is worth pinning
        // separately from the top-level case above.
        object[] nestedMixed = new object[] { new Enclosing.Inner() };
        Enclosing.OtherInner[] nestedDest = new Enclosing.OtherInner[1];

        try
        {
            Array.Copy(nestedMixed, nestedDest, 1);
            return 6;
        }
        catch (InvalidCastException e)
        {
            if (e.Message != "Unable to cast object of type 'Inner' to type 'OtherInner'.")
            {
                Console.WriteLine(e.Message);
                return 7;
            }
        }

        // Generic *arguments*, by contrast, are rendered by TypeString::AppendInst, which is
        // the ordinary reflection renderer -- so a nested argument keeps its full chain, and
        // that chain is '+'-joined.
        object[] genericMixed = new object[] { new List<Enclosing.Inner>() };
        List<Enclosing.OtherInner>[] genericDest = new List<Enclosing.OtherInner>[1];

        try
        {
            Array.Copy(genericMixed, genericDest, 1);
            return 10;
        }
        catch (InvalidCastException e)
        {
            if (e.Message
                != "Unable to cast object of type 'System.Collections.Generic.List`1[Enclosing+Inner]' to type 'System.Collections.Generic.List`1[Enclosing+OtherInner]'.")
            {
                Console.WriteLine(e.Message);
                return 11;
            }
        }

        // A namespace, by contrast, does appear: it is on the TypeDef row itself.
        object[] namespacedMixed = new object[] { new Outer.Nested() };
        Outer.OtherNested[] namespacedDest = new Outer.OtherNested[1];

        try
        {
            Array.Copy(namespacedMixed, namespacedDest, 1);
            return 8;
        }
        catch (InvalidCastException e)
        {
            if (e.Message != "Unable to cast object of type 'Outer.Nested' to type 'Outer.OtherNested'.")
            {
                Console.WriteLine(e.Message);
                return 9;
            }
        }

        return 0;
    }
}
