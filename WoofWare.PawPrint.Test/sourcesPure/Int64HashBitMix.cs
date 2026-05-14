using System;

class Program
{
    // Exercises the Int64 `OpaqueHashBits` pipeline: widen a pointer-shaped
    // nuint to ulong via `conv.u8`, bit-mix via shl/shr.un/and/or/xor, then
    // extract the low 32 bits via `conv.i4`. This stays entirely in the int64
    // domain — it does not narrow back to nuint mid-stream — so it should
    // succeed with the OpaqueHashBits machinery alone, without requiring a
    // parallel `NativeIntSource.OpaqueHashBits` variant.
    static int Main(string[] args)
    {
        IntPtr h = typeof(int).TypeHandle.Value;
        ulong widened = (ulong)h;
        ulong mixed = ((widened << 16) ^ (widened >> 13)) | 0x1234UL;
        mixed &= 0xFFFFUL;
        int bucket = (int)mixed;
        if (bucket < 0 || bucket > 0xFFFF)
        {
            return 1;
        }

        return 0;
    }
}
