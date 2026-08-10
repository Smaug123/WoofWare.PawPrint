using System;

// PawPrint's chosen message for a negative-length `newarr`, which is a PawPrint-only fact and
// so cannot live in sourcesPure.
//
// CoreCLR has two answers, selected by which allocation helper the JIT emitted for the element
// type (`CEEInfo::getNewArrHelperStatic`, jitinterface.cpp:5752-5806):
//
//   * elements of exactly pointer size -> CORINFO_HELP_NEWARR_1_PTR, whose slow path takes
//     `numElements` as an unsigned word, so a negative length is caught by
//     `numElements > INT_MAX` and carries IDS_EE_ARRAY_DIMENSIONS_EXCEEDED
//     (gchelpers.cpp:90-97);
//   * everything else -> `AllocateSzArray`'s bare `COMPlusThrow(kOverflowException)`
//     (gchelpers.cpp:637-638), i.e. the parameterless ctor's own default message.
//
// PawPrint has no JIT and no notion of a selected allocation helper, so it always gives the
// second answer. This file pins that: `string[]` is the case where a real 64-bit CoreCLR gives
// the *first*. Recorded in docs/divergences.md.
//
// The exception type is identical on both runtimes either way, and that much is asserted
// differentially in sourcesPure/NewarrLengthValidation.cs.
public class TestNewarrNegativeLengthMessage
{
    private static int Neg() => -1;

    private static object sink;

    public static int Main(string[] argv)
    {
        try
        {
            sink = new string[Neg()];
            return 1;
        }
        catch (OverflowException e)
        {
            // Compared against a freshly constructed exception rather than against the literal,
            // so the assertion cannot drift out of step with CoreLib's own resource string.
            if (e.Message != new OverflowException().Message) return 2;

            // ...and that default really is distinguishable from the message CoreCLR would
            // have used here, so this test is not vacuous.
            if (e.Message == "Array dimensions exceeded supported range.") return 3;
        }
        catch (Exception)
        {
            return 4;
        }

        return 0;
    }
}
