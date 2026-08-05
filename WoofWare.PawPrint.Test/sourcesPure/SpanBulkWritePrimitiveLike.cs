using System;

namespace SpanBulkWritePrimitiveLikeTest
{
    enum SmallEnum : short
    {
        Zero = 0,
        First = 1,
        Third = -3,
    }

    // Bulk writes through a span put an array cell back into its *declared* storage form
    // (`CliType.Bool` for bool[], a primitive-like value type for nint[]/enum arrays), which
    // is the form `newarr` zero-fills with. Both `Span<T>.Clear` (implemented natively in
    // Intrinsics.fs) and `Span<T>.Fill` (which runs the BCL's IL) reach that state, so each
    // case below is driven through Clear first and Fill second.
    //
    // This used to fail on the subsequent ordinary read, because the concrete-width
    // `ldelem.*` arms matched strictly on `CliType.Numeric` and so rejected the declared
    // form outright:
    //
    //     bool   -> expected one-byte integer in Ldelem.u1, got: Bool 0uy
    //     nint   -> expected native int in Ldelem.i
    //     enum   -> expected two-byte integer in Ldelem.i2, got: ValueType ... EnumLike
    //
    // The bug was never in the span write. A fresh `new bool[2]` read with no span anywhere
    // in the program failed identically; `stelem.*` merely masked it by stamping the
    // opcode's raw primitive over the cell, so only a read of a still-declared-form cell
    // tripped it. ArrayPrimitiveLikeElementRead.cs covers that span-free half directly.
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
