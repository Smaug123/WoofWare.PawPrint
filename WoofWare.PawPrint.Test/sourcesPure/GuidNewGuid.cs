using System;

// `Guid.NewGuid()` on Unix is a thin shell over
// `SystemNative_GetCryptographicallySecureRandomBytes`: Guid.Unix.cs draws
// `sizeof(Guid)` bytes straight over the struct's storage, then forces the
// RFC 4122 version-4 and variant-10xx bits. This test asserts the parts of
// that contract that are true of every correct implementation, so it can run
// against the real CLR and PawPrint alike.
class Program
{
    static int Main(string[] args)
    {
        Guid a = Guid.NewGuid();
        Guid b = Guid.NewGuid();

        if (a == Guid.Empty) return 1;
        if (a == b) return 2;

        byte[] bytes = a.ToByteArray();
        if (bytes.Length != 16) return 3;

        // `ToByteArray` emits the first three fields little-endian, so
        // `time_hi_and_version` (the 16-bit `_c` field) occupies indices 6
        // and 7 with its most significant byte at index 7. RFC 4122 puts the
        // version in that byte's high nibble; Guid.NewGuid forces it to 4.
        if ((bytes[7] & 0xF0) != 0x40) return 4;

        // `clock_seq_hi_and_reserved` is the `_d` byte, at index 8. The
        // variant field is its top two bits, forced to 0b10.
        if ((bytes[8] & 0xC0) != 0x80) return 5;

        return 0;
    }
}
