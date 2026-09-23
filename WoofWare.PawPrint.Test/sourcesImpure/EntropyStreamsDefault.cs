using System;
using System.Runtime.InteropServices;

// Writes to stdout what a default run hands the guest from its two random
// streams, so that the F# registration can pin the exact bytes: PawPrint's
// replay contract says every `Guid.NewGuid` and every `new Random()` sequence
// is a fixed function of the configuration, and nothing else pins that.
//
// Both streams are read through CoreLib's own consumers and through the raw
// entry points, interleaved so that a stream which drew from the other's
// state would shift every later row. The raw draws are 24 bytes: not a
// multiple of eight, so a generator that unpacked its 64-bit outputs
// differently would show in the last block.
//
// PawPrint-only: on a real runtime every byte here is fresh entropy.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetNonCryptographicallySecureRandomBytes")]
    static extern unsafe void GetNonCryptographicallySecureRandomBytes(byte* buffer, int length);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetCryptographicallySecureRandomBytes")]
    static extern unsafe int GetCryptographicallySecureRandomBytes(byte* buffer, int length);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int RawDraw = 24;

    static unsafe int Emit(byte[] bytes)
    {
        fixed (byte* p = bytes)
        {
            return Write((IntPtr)1, p, bytes.Length) == bytes.Length ? 0 : 1;
        }
    }

    static int EmitInt32(int value)
    {
        return Emit(new byte[]
        {
            (byte)(value & 0xFF),
            (byte)((value >> 8) & 0xFF),
            (byte)((value >> 16) & 0xFF),
            (byte)((value >> 24) & 0xFF),
        });
    }

    static unsafe int Main(string[] args)
    {
        if (Emit(Guid.NewGuid().ToByteArray()) != 0) return 1;

        var random = new Random();
        if (EmitInt32(random.Next()) != 0) return 2;
        if (EmitInt32(random.Next()) != 0) return 3;

        byte[] crypto = new byte[RawDraw];
        fixed (byte* p = crypto)
        {
            if (GetCryptographicallySecureRandomBytes(p, RawDraw) != 0) return 4;
        }
        if (Emit(crypto) != 0) return 5;

        byte[] nonCrypto = new byte[RawDraw];
        fixed (byte* p = nonCrypto)
        {
            GetNonCryptographicallySecureRandomBytes(p, RawDraw);
        }
        if (Emit(nonCrypto) != 0) return 6;

        if (Emit(Guid.NewGuid().ToByteArray()) != 0) return 7;

        return 0;
    }
}
