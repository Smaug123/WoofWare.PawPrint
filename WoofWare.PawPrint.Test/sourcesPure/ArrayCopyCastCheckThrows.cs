using System;

class Base { }

class Derived : Base { }

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

        return 0;
    }
}
