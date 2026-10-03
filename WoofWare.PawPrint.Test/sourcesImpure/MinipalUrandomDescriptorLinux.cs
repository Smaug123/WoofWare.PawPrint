using System;
using System.Runtime.InteropServices;
using System.Security.Cryptography;

// System.Native's minipal on Linux keeps a /dev/urandom descriptor from its
// first secure call, and a guest can see it: the next open lands one higher,
// and closing it makes the next secure call fail with EBADF, which
// `Guid.NewGuid` turns into a CryptographicException. Measured on real .NET
// 10.0.11 on Linux 6.18.5 (minipal-random-descriptors.cs).
//
// PawPrint-only: on a real runtime CoreCLR's own copy of minipal holds a
// descriptor too, and so do the runtime's other start-up files, so the numbers
// are not these.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetCryptographicallySecureRandomBytes")]
    static extern unsafe int GetCryptographicallySecureRandomBytes(byte* buffer, int length);

    static unsafe IntPtr OpenDevNull()
    {
        byte[] path = System.Text.Encoding.ASCII.GetBytes("/dev/null\0");
        fixed (byte* p = path) return Open(p, 0, 0);
    }

    static unsafe int Secure(byte[] buffer)
    {
        fixed (byte* p = buffer) return GetCryptographicallySecureRandomBytes(p, buffer.Length);
    }

    static int Main(string[] args)
    {
        IntPtr lowest = OpenDevNull();
        if (lowest == (IntPtr)(-1)) return 1;
        if (Close(lowest) != 0) return 2;

        byte[] buffer = new byte[16];
        if (Secure(buffer) != 0) return 3;

        // minipal took the lowest free number.
        IntPtr next = OpenDevNull();
        if (next != lowest + 1) return 4;
        if (Close(next) != 0) return 5;

        if (Close(lowest) != 0) return 6;

        Marshal.SetLastSystemError(0);
        if (Secure(buffer) != -1) return 7;
        // EBADF.
        if (Marshal.GetLastSystemError() != 9) return 8;

        try
        {
            Guid.NewGuid();
            return 9;
        }
        catch (CryptographicException)
        {
        }

        return 0;
    }
}
