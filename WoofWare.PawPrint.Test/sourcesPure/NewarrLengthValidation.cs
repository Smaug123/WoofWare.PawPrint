using System;

public struct NewarrFourByteStruct
{
    public int A;
}

// The two length checks `newarr` inherits from CoreCLR's `AllocateSzArray`
// (gchelpers.cpp:637-641), and the exceptions they raise.
//
// Both are checked before any allocation is attempted, so the over-long cases below cost
// nothing on either runtime; nothing here allocates anything but tiny arrays.
public class TestNewarrLengthValidation
{
    // `Array.MaxLength`, which upstream keeps equal to the native `MaxArrayLength()`.
    private const int MaxLength = 0x7FFFFFC7;

    // Roslyn rejects a *constant* negative array size outright (CS0248), so the length has to
    // arrive through a call.
    private static int Neg() => -1;

    private static int NegBig() => -1000;

    // Somewhere for the allocations to go, so that none of them is dead code.
    private static object sink;

    public static int Main(string[] argv)
    {
        // ---- negative length: OverflowException, whatever the element type ----
        //
        // Only the exception *type* is asserted here, deliberately. CoreCLR's message depends
        // on which allocation helper the JIT picked for the element type: elements of exactly
        // pointer size get CORINFO_HELP_NEWARR_1_PTR (jitinterface.cpp:5776-5781), whose slow
        // path takes `numElements` as an *unsigned* word and so rejects a negative length via
        // `numElements > INT_MAX` with IDS_EE_ARRAY_DIMENSIONS_EXCEEDED (gchelpers.cpp:90-97),
        // while everything else reaches `AllocateSzArray`'s bare `COMPlusThrow(kOverflowException)`.
        // `getNewArrHelperStatic` also falls back to the slow helper when ETW allocation
        // tracking is on, so the message is not even stable for a fixed element type on a
        // fixed target. The type is. PawPrint always reports the `AllocateSzArray` message;
        // see docs/divergences.md and sourcesImpure/NewarrNegativeLengthMessage.cs.
        //
        // The element types below straddle the pointer-size split on a 64-bit target: int,
        // byte and the 4-byte struct take the fast helper, long/string/object the pointer one.
        try
        {
            sink = new int[Neg()];
            return 1;
        }
        catch (OverflowException)
        {
        }
        catch (Exception)
        {
            return 2;
        }

        try
        {
            sink = new byte[Neg()];
            return 3;
        }
        catch (OverflowException)
        {
        }
        catch (Exception)
        {
            return 4;
        }

        try
        {
            sink = new long[Neg()];
            return 5;
        }
        catch (OverflowException)
        {
        }
        catch (Exception)
        {
            return 6;
        }

        try
        {
            sink = new string[Neg()];
            return 7;
        }
        catch (OverflowException)
        {
        }
        catch (Exception)
        {
            return 8;
        }

        try
        {
            sink = new object[Neg()];
            return 9;
        }
        catch (OverflowException)
        {
        }
        catch (Exception)
        {
            return 10;
        }

        try
        {
            sink = new NewarrFourByteStruct[Neg()];
            return 11;
        }
        catch (OverflowException)
        {
        }
        catch (Exception)
        {
            return 12;
        }

        // Not just -1: any negative length, so that an implementation which only special-cased
        // the sentinel would still fail.
        try
        {
            sink = new int[NegBig()];
            return 13;
        }
        catch (OverflowException)
        {
        }
        catch (Exception)
        {
            return 14;
        }

        // ---- past MaxArrayLength(): OutOfMemoryException carrying the native resource
        // string IDS_EE_ARRAY_DIMENSIONS_EXCEEDED rather than the parameterless ctor's
        // default. Uniform across element types, because it comes from `AllocateSzArray`
        // itself on every helper path. The literal is safe to assert across runtimes:
        // CoreCLR's native (mscorrc) strings ship English-only in .NET Core, and both
        // runtimes run this same guest text.
        try
        {
            sink = new byte[MaxLength + 1];
            return 20;
        }
        catch (OutOfMemoryException e)
        {
            if (e.Message != "Array dimensions exceeded supported range.") return 21;

            // And it is genuinely not the default message, which is what makes the check
            // above more than a tautology on either runtime.
            if (e.Message == new OutOfMemoryException().Message) return 22;
        }
        catch (Exception)
        {
            return 23;
        }

        try
        {
            sink = new int[int.MaxValue];
            return 24;
        }
        catch (OutOfMemoryException e)
        {
            if (e.Message != "Array dimensions exceeded supported range.") return 25;
        }
        catch (Exception)
        {
            return 26;
        }

        try
        {
            sink = new string[MaxLength + 1];
            return 27;
        }
        catch (OutOfMemoryException e)
        {
            if (e.Message != "Array dimensions exceeded supported range.") return 28;
        }
        catch (Exception)
        {
            return 29;
        }

        // `MaxLength` itself is deliberately not tested from below: CoreCLR's comparison is
        // `> MaxArrayLength()`, so the boundary is exclusive, but demonstrating that takes a
        // genuine multi-gigabyte allocation on the oracle side and an array of two billion
        // element cells on PawPrint's.

        // ---- a rejected allocation must leave the allocator usable ----
        int[] empty = new int[0];
        if (empty == null) return 40;
        if (empty.Length != 0) return 41;

        int[] ok = new int[3];
        if (ok == null) return 42;
        if (ok.Length != 3) return 43;
        if (ok[0] != 0 || ok[1] != 0 || ok[2] != 0) return 44;
        ok[2] = 42;
        if (ok[2] != 42) return 45;

        string[] strs = new string[2];
        if (strs.Length != 2) return 46;
        if (strs[0] != null || strs[1] != null) return 47;

        return 0;
    }
}
