// `RuntimeHelpers.InitializeArray` validates its two arguments in order, and the two failures
// are *different* exceptions:
//
//     if (array is null) ThrowHelper.ThrowArgumentNullException(ExceptionArgument.array);
//     if (fldHandle.IsNullHandle()) throw new ArgumentException(SR.Argument_InvalidHandle);
//
// so a null array is an `ArgumentNullException`, not the `NullReferenceException` you might
// expect from the name. The JIT only expands this intrinsic when it recognises a `newarr`
// plus a constant `ldtoken`, which neither call below matches, so the managed body — and
// hence these checks — is what really runs.
//
// `ArgumentNullException` derives from `ArgumentException`, so the second test has to
// discriminate on the exact type rather than relying on catch ordering alone.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    private static Array NullArray()
    {
        return null;
    }

    public static int Main(string[] args)
    {
        // Null array: ArgumentNullException. Checked first, so the null handle here is not
        // what is being reported.
        //
        // The message is deliberately not asserted. The CLR's is "Value cannot be null.
        // (Parameter 'array')", where the suffix is synthesised by `ArgumentException.Message`
        // from `_paramName`; PawPrint constructs this exception through its parameterless ctor
        // and has no way to set `_paramName`, so it reports "Value cannot be null.". Asserting
        // the CLR's string here would be asserting a divergence we know about.
        try
        {
            RuntimeHelpers.InitializeArray(NullArray(), default);
            return 1;
        }
        catch (ArgumentNullException)
        {
        }

        // Null field handle with a real array: plain ArgumentException, whose message is a
        // constant (`SR.Argument_InvalidHandle`) with nothing interpolated, so it can be — and
        // is — asserted exactly.
        try
        {
            RuntimeHelpers.InitializeArray(new int[4], default);
            return 2;
        }
        catch (Exception e) when (e.GetType() == typeof(ArgumentException))
        {
            if (e.Message != "The handle is invalid.")
            {
                return 3;
            }
        }

        return 0;
    }
}
