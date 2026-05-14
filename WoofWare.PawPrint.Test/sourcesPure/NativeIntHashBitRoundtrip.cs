using System;

class Program
{
    // Exercises the NativeIntSource `OpaqueHashBits` round-trip: widen a
    // pointer-shaped nuint to ulong (`conv.u8`, producing
    // `Int64Source.OpaqueHashBits`), bit-mix in the int64 domain, then narrow
    // back to nuint (`conv.u`, which lands in
    // `NativeIntSource.OpaqueHashBits`), then re-widen to ulong (`conv.u8`,
    // routing through `Int64Source.widenedNativeInt`'s normalisation back to
    // `Int64Source.OpaqueHashBits`) and finally narrow to int32 (`conv.i4`,
    // the cast-cache "bucket index" form).
    //
    // This is the operation the BCL's
    // `BitOperations.RotateLeft(nuint, int)` inlines as
    // `(nuint)RotateLeft((ulong)value, offset)` — the final `(nuint)` cast
    // is exactly the round-trip this test covers in isolation, without
    // requiring `BitOperations.RotateLeft` or anything downstream.
    static int Main(string[] args)
    {
        IntPtr h = typeof(int).TypeHandle.Value;
        ulong widened = (ulong)h;
        ulong rotated = (widened << 16) | (widened >> 48);
        nuint narrowed = (nuint)rotated;
        ulong rewidened = (ulong)narrowed;
        int bucket = (int)(rewidened & 0xFFFFUL);
        if (bucket < 0 || bucket > 0xFFFF)
        {
            return 1;
        }

        return 0;
    }
}
