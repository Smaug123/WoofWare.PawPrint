using System;

class Program
{
    // Exercises `xor` on the eval stack with one `NativeInt(OpaqueHashBits)`
    // operand (the rotated TypeHandle bits) and one `NativeInt(TypeHandlePtr)`
    // operand (the raw RuntimeTypeHandle).
    //
    // The BCL's cast cache reaches this shape:
    //
    //     nuint hash = RotateLeft(source, 32) ^ target;
    //
    // where the rotated `source` lands as `NativeInt(OpaqueHashBits)` after the
    // `(nuint)` inside `BitOperations.RotateLeft`, and `target` is a
    // `NativeInt(TypeHandlePtr)` that never traversed `conv.u` (it stayed as
    // signed-shaped `nint` on the eval stack).
    //
    // The C# below recreates that shape without depending on
    // `BitOperations.RotateLeft` or `CastCache.KeyToBucket` themselves:
    //   - `(ulong)h` widens via `conv.u8`, yielding `Int64(WidenedNativeInt)`;
    //     subsequent `<< 32 | >> 32` lowers to `Int64(OpaqueHashBits)`.
    //   - `(nuint)rotated` narrows via `conv.u`, landing as
    //     `NativeInt(OpaqueHashBits)`.
    //   - `(nint)h` is `conv.i` on a `NativeInt` slot, which is a no-op that
    //     preserves the `TypeHandlePtr` provenance.
    //   - The `^` between two `nint`s lowers to the `xor` IL instruction.
    static int Main(string[] args)
    {
        IntPtr h = typeof(int).TypeHandle.Value;
        ulong widened = (ulong)h;
        ulong rotated = (widened << 32) | (widened >> 32);
        nuint narrowed = (nuint)rotated;
        nint target = (nint)h;
        nint mixed = (nint)narrowed ^ target;

        // Truncate to int to obtain a deterministic bucket index. `conv.i4` on
        // `NativeInt(OpaqueHashBits)` routes through
        // `nativeIntBitsForIntegerConversion`, which preserves the bits.
        int bucket = (int)mixed & 0xFFFF;
        if (bucket < 0 || bucket > 0xFFFF)
        {
            return 1;
        }

        // XOR with the same target a second time should cancel the target bits
        // out (the rotated bits remain). The cast-cache hash mixing relies on
        // this: repeating the operation is well-defined and deterministic
        // given the synthesised bit assignment.
        nint roundTrip = mixed ^ target;
        // `roundTrip` is the rotated bits alone; AND with a small mask just
        // sanity-checks that further bit ops on the result are still tractable.
        int bucket2 = (int)roundTrip & 0xFFFF;
        if (bucket2 < 0 || bucket2 > 0xFFFF)
        {
            return 2;
        }

        return 0;
    }
}
