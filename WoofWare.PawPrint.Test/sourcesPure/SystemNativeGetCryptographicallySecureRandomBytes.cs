using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_GetCryptographicallySecureRandomBytes PawPrint
// handler directly via a P/Invoke stub, mirroring the declaration CoreLib
// itself uses (`Interop.Sys.GetCryptographicallySecureRandomBytes(byte*, int)
// -> int`, where 0 means success and CoreLib's wrapper throws
// CryptographicException on anything else).
//
// This test runs against both the real CLR and PawPrint, so it can only
// assert properties that hold of *any* correct implementation: the return
// code, that the callee writes within exactly the requested window, and that
// two successive draws differ. It deliberately does not pin exact bytes —
// PawPrint's substitute stream is a seeded splitmix64 (deterministic by
// design), while the host draws from real OS entropy.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetCryptographicallySecureRandomBytes")]
    static unsafe extern int GetCryptographicallySecureRandomBytes(byte* buffer, int length);

    const int Capacity = 64;
    const int Draw = 32;
    const byte Sentinel = 0xAB;

    static unsafe void Fill(byte* buffer)
    {
        for (int i = 0; i < Capacity; i++)
        {
            buffer[i] = Sentinel;
        }
    }

    static unsafe bool TailIsIntact(byte* buffer)
    {
        for (int i = Draw; i < Capacity; i++)
        {
            if (buffer[i] != Sentinel) return false;
        }

        return true;
    }

    static unsafe int Main(string[] args)
    {
        byte* first = stackalloc byte[Capacity];
        byte* second = stackalloc byte[Capacity];

        Fill(first);
        Fill(second);

        // A zero-length draw succeeds and touches nothing. CoreLib reaches
        // this path whenever it hands over an empty span.
        if (GetCryptographicallySecureRandomBytes(first, 0) != 0) return 1;

        for (int i = 0; i < Capacity; i++)
        {
            if (first[i] != Sentinel) return 2;
        }

        if (GetCryptographicallySecureRandomBytes(first, Draw) != 0) return 3;
        if (!TailIsIntact(first)) return 4;

        if (GetCryptographicallySecureRandomBytes(second, Draw) != 0) return 5;
        if (!TailIsIntact(second)) return 6;

        // Two successive 32-byte draws must differ. A stuck implementation
        // (one that never writes, or writes a constant) fails here; a
        // correct one collides with probability 2^-256, which is not a
        // flakiness source anybody will ever observe.
        bool identical = true;

        for (int i = 0; i < Draw; i++)
        {
            if (first[i] != second[i])
            {
                identical = false;
                break;
            }
        }

        if (identical) return 7;

        return 0;
    }
}
