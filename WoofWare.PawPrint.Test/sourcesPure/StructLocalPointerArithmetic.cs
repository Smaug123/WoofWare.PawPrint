using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class StructLocalPointerArithmetic
{
    [StructLayout(LayoutKind.Sequential)]
    struct Pair
    {
        public int A;
        public int B;
    }

    [InlineArray(4)]
    struct Quad
    {
        private long _e0;
    }

    // Four one-byte fields starting four bytes in, so an `int`-wide read at offset 4 has to
    // span all of them. An interpreter that resolved `p + 1` to "whichever field begins at
    // byte 4" would answer with just `A`.
    [StructLayout(LayoutKind.Explicit, Size = 8)]
    struct FourBytes
    {
        [FieldOffset(4)] public byte A;
        [FieldOffset(5)] public byte B;
        [FieldOffset(6)] public byte C;
        [FieldOffset(7)] public byte D;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct Narrow
    {
        public int A;
    }

    static Pair staticPair;

    // A by-value struct parameter, whose address is an argument slot rather than a local
    // one. Only a by-value parameter gives that: a `ref` parameter's address is the
    // caller's variable.
    static unsafe int SumViaArgumentPointer(Pair pair)
    {
        int* p = (int*)&pair;
        int sum = 0;

        for (int i = 0; i < 2; i++)
        {
            sum += *(p + i);
        }

        // Measuring a cursor into an argument slot, not just making one: subtraction refuses
        // argument-rooted pointers unless both sides reach into the same slot.
        int* cursor = p;

        for (int i = 1; i < 2; i++)
        {
            cursor += i;
        }

        if (cursor - p != 1)
            return -1;

        if (p - cursor != -1)
            return -2;

        for (int i = 1; i < 2; i++)
        {
            *(p + i) = 40;
        }

        return sum + pair.B;
    }

    // Offsets come from a loop variable rather than a literal so Roslyn cannot fold
    // `p + 0` away at compile time: the point of the test is that the interpreter sees
    // an `add` of a byref-to-a-whole-local and a computed offset.
    public static unsafe int Main(string[] argv)
    {
        Pair pair = default;
        int* pairPtr = (int*)&pair;

        // `p + 0` must be the same pointer, not merely one that happens to read back
        // the same value: byref equality is what `Unsafe.AreSame` and `ceq` report.
        for (int i = 0; i < 1; i++)
        {
            if (pairPtr + i != pairPtr)
                return 1;
        }

        // A store through the whole-struct pointer must write only the first field's
        // worth of bytes; the sibling field must survive.
        for (int i = 0; i < 1; i++)
        {
            *(pairPtr + i) = 7;
        }

        if (pair.A != 7)
            return 2;

        if (pair.B != 0)
            return 3;

        for (int i = 1; i < 2; i++)
        {
            *(pairPtr + i) = 9;
        }

        if (pair.A != 7)
            return 4;

        if (pair.B != 9)
            return 5;

        // Reads through the same pointer.
        for (int i = 0; i < 2; i++)
        {
            int expected = i == 0 ? 7 : 9;

            if (*(pairPtr + i) != expected)
                return 6;
        }

        // The same walk over an inline array, whose storage repeats are addressed by
        // index rather than by distinct field names.
        Quad quad = default;
        long* quadPtr = (long*)&quad;

        for (int i = 0; i < 4; i++)
        {
            *(quadPtr + i) = 100 + i;
        }

        for (int i = 0; i < 4; i++)
        {
            if (quad[i] != 100 + i)
                return 7;
        }

        for (int i = 0; i < 4; i++)
        {
            if (*(quadPtr + i) != 100 + i)
                return 8;
        }

        // Walking back down again must land on the same locations.
        for (int i = 3; i >= 0; i--)
        {
            if (*(quadPtr + i) != 100 + i)
                return 9;
        }

        // Advancing a pointer and coming back must return to where it started. Offsetting an
        // address does not choose a type view, so a round trip cannot land on a different
        // one: `q` here must be the same byref as `p`, not a byref to the field at offset 0.
        Pair roundTrip = default;
        int* start = (int*)&roundTrip;
        int* cursor = start;

        for (int i = 1; i < 2; i++)
        {
            cursor += i;
        }

        if (cursor == start)
            return 10;

        // Measuring the advance must work wherever making it does: the slot's own address is
        // the zero point, so the difference is the cursor's offset.
        if (cursor - start != 1)
            return 11;

        if (start - cursor != -1)
            return 12;

        if (start - start != 0)
            return 13;

        for (int i = 1; i < 2; i++)
        {
            cursor -= i;
        }

        if (cursor != start)
            return 14;

        if (cursor - start != 0)
            return 15;

        // Accumulating two offsets that do not cancel, where testing cancellation by negating
        // the first would overflow. Nothing is dereferenced: only the arithmetic is under test,
        // and `int.MinValue + 1` is perfectly representable.
        Pair wide = default;
        byte* wideStart = (byte*)&wide;
        byte* wideCursor = wideStart;

        for (int i = 1; i < 2; i++)
        {
            wideCursor += int.MinValue;
        }

        for (int i = 1; i < 2; i++)
        {
            wideCursor += i;
        }

        if (wideCursor - wideStart != -2147483647)
            return 16;

        // Cancelling this particular cursor back to zero is *not* tested here: `q -= int.MinValue`
        // needs an offset of +2147483648, which the int32 symbolic-offset model cannot express in
        // one step even though the endpoint is representable. That fails loudly in
        // `narrowSymbolicOffset` and is a separate limitation from anything this file covers;
        // cancellation itself is pinned by the round trip above and by `TestBinaryArithmetic`.

        // An `int`-wide access at an offset where a narrower field begins must span the
        // neighbouring fields, exactly as it does on the CLR: the access width comes from the
        // dereference, not from whatever field happens to start at that address.
        FourBytes fourBytes = default;
        fourBytes.A = 1;
        fourBytes.B = 2;
        fourBytes.C = 3;
        fourBytes.D = 4;

        int* fourBytesPtr = (int*)&fourBytes;

        for (int i = 1; i < 2; i++)
        {
            if (*(fourBytesPtr + i) != 0x04030201)
                return 17;
        }

        for (int i = 1; i < 2; i++)
        {
            *(fourBytesPtr + i) = 0x08070605;
        }

        if (fourBytes.A != 5 || fourBytes.B != 6 || fourBytes.C != 7 || fourBytes.D != 8)
            return 18;

        // A narrower `stobj` through a cursor that has round-tripped back to offset zero must
        // write only the bytes it covers. Worth pinning because the bare whole-slot pointer does
        // *not* currently manage this — see `sourcesPure/NarrowStructStoreThroughWideSlot.cs`,
        // parked on that pre-existing write-path gap — so anything that canonicalises a
        // zero-offset cursor back to the bare form would have to fix the write path first, or it
        // would regress this.
        Pair narrowTarget = default;
        narrowTarget.A = 1;
        narrowTarget.B = 2;

        Narrow* narrowPtr = (Narrow*)&narrowTarget;
        Narrow* narrowCursor = narrowPtr;

        for (int i = 1; i < 2; i++)
        {
            narrowCursor += i;
        }

        for (int i = 1; i < 2; i++)
        {
            narrowCursor -= i;
        }

        *narrowCursor = new Narrow
        {
            A = 3,
        };

        if (narrowTarget.A != 3)
            return 19;

        if (narrowTarget.B != 2)
            return 20;

        // An argument slot rather than a local slot: 3 + 4 read back, then B overwritten
        // with 40 through the same pointer.
        Pair arg = default;
        arg.A = 3;
        arg.B = 4;

        if (SumViaArgumentPointer(arg) != 47)
            return 21;

        // The callee wrote through a pointer into its own argument slot, so the caller's
        // copy must be untouched.
        if (arg.B != 4)
            return 22;

        // A static field's slot.
        staticPair.A = 5;
        staticPair.B = 6;

        fixed (Pair* staticPtr = &staticPair)
        {
            int* staticInts = (int*)staticPtr;

            for (int i = 0; i < 2; i++)
            {
                if (*(staticInts + i) != 5 + i)
                    return 23;
            }

            for (int i = 1; i < 2; i++)
            {
                *(staticInts + i) = 60;
            }
        }

        if (staticPair.A != 5)
            return 24;

        if (staticPair.B != 60)
            return 25;

        return 0;
    }
}
