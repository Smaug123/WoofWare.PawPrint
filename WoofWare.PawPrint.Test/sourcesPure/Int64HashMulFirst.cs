using System;

class Program
{
    // Exercises the Int64 `OpaqueHashBits` pipeline when arithmetic — not a
    // bit-mixing shift/xor/and — is the first op applied to a widened native
    // int. This is the shape used by CastCache.KeyToBucket's multiply-by-
    // golden-ratio step: `hash * 11400714819323198485ul` arrives at the
    // dispatcher as `WidenedNativeInt × Verbatim`, not `OpaqueHashBits ×
    // Verbatim`, so it must materialise the pointer bits in
    // `BinaryArithmetic` rather than relying on the bit-mixing helpers.
    static int Main(string[] args)
    {
        IntPtr h = typeof(int).TypeHandle.Value;
        ulong widened = (ulong)h;
        ulong mixed = widened * 11400714819323198485UL;
        mixed &= 0xFFFFUL;
        int bucket = (int)mixed;
        if (bucket < 0 || bucket > 0xFFFF)
        {
            return 1;
        }

        return 0;
    }
}
