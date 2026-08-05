using System;

namespace SpanBulkWritePrimitiveLikeTest
{
    enum SmallEnum : short
    {
        Zero = 0,
        First = 1,
        Third = -3,
    }

    // Records a pre-existing bug in the span bulk-write paths, found while adding
    // Span<T>.Fill. Writing an element through a span byref stores the value in its
    // eval-stack representation rather than the destination cell's storage form, so
    // the write succeeds and nothing fails until an ordinary read of that element:
    //
    //     bool   -> expected one-byte integer in Ldelem.u1, got: Bool 0uy
    //     nint   -> expected native int in Ldelem.i
    //     enum   -> expected two-byte integer in Ldelem.i2, got: ValueType ... EnumLike
    //
    // It is exactly the element types whose CliType is "primitive-like" but distinct
    // from the raw storage form. `char`, `byte`, `int`, `long`, `double`, multi-field
    // structs and reference types are all unaffected and are covered by the passing
    // SpanFill.cs.
    //
    // This is not about executing the BCL's IL. `Span<T>.Clear` is implemented
    // natively in Intrinsics.fs and corrupts these arrays identically, which is why
    // each case below is driven through Clear first and Fill second: whichever way the
    // store is reached, it needs to normalise to the destination representation.
    // Plain stelem/ldelem round-trips are unaffected.
    class Program
    {
        static int Main(string[] args)
        {
            bool[] boolClear = new bool[2];
            boolClear[0] = true;
            ((Span<bool>)boolClear).Clear();

            if (boolClear[0])
            {
                return 1;
            }

            bool[] boolFill = new bool[2];
            ((Span<bool>)boolFill).Fill(true);

            if (!boolFill[0])
            {
                return 2;
            }

            nint[] nintClear = new nint[2];
            nintClear[0] = 5;
            ((Span<nint>)nintClear).Clear();

            if (nintClear[0] != 0)
            {
                return 3;
            }

            nint[] nintFill = new nint[2];
            ((Span<nint>)nintFill).Fill(5);

            if (nintFill[0] != 5)
            {
                return 4;
            }

            SmallEnum[] enumClear = new SmallEnum[2];
            enumClear[0] = SmallEnum.First;
            ((Span<SmallEnum>)enumClear).Clear();

            if (enumClear[0] != SmallEnum.Zero)
            {
                return 5;
            }

            SmallEnum[] enumFill = new SmallEnum[2];
            ((Span<SmallEnum>)enumFill).Fill(SmallEnum.Third);

            if (enumFill[0] != SmallEnum.Third)
            {
                return 6;
            }

            return 0;
        }
    }
}
