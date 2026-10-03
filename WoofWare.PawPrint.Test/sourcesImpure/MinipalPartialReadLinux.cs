using System;
using System.Runtime.InteropServices;

// System.Native's non-secure random bytes on Linux are minipal's secure read
// of its cached /dev/urandom descriptor, then glibc's lrand48 XORed over the
// buffer as it then stands. When that descriptor number has since been reused
// by a nonblocking pipe holding 4 bytes, the read writes those 4 bytes into the
// buffer, then fails with EAGAIN, and the XOR runs over them and over the
// buffer's own last 4 bytes, which the guest initialised. The secure entry
// point, over the same descriptor, leaves the read's prefix and the guest's
// own bytes, and answers -1 with EAGAIN.
//
// PawPrint-only: on a real runtime CoreCLR's own copy of minipal and the
// runtime's start-up files hold descriptors too, so the numbers are not these.
// The mask is lrand48's first two outputs after srand48(0), the run's clock
// starting at the epoch: 366850414 and 1610402240 (lrand48.c).
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking", SetLastError = true)]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetCryptographicallySecureRandomBytes")]
    static extern unsafe int GetCryptographicallySecureRandomBytes(byte* buffer, int length);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetNonCryptographicallySecureRandomBytes")]
    static extern unsafe void GetNonCryptographicallySecureRandomBytes(byte* buffer, int length);

    static unsafe IntPtr OpenDevNull()
    {
        byte[] path = System.Text.Encoding.ASCII.GetBytes("/dev/null\0");
        fixed (byte* p = path) return Open(p, 0, 0);
    }

    static unsafe int Main(string[] args)
    {
        IntPtr lowest = OpenDevNull();
        if (lowest == (IntPtr)(-1)) return 1;
        if (Close(lowest) != 0) return 2;

        // minipal opens its descriptor, which takes the lowest free number.
        byte* scratch = stackalloc byte[16];
        if (GetCryptographicallySecureRandomBytes(scratch, 16) != 0) return 3;

        // Hand that number to a pipe's read end.
        if (Close(lowest) != 0) return 4;
        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 5;
        if ((IntPtr)fds[0] != lowest) return 6;

        byte* payload = stackalloc byte[4];
        payload[0] = 0x11; payload[1] = 0x22; payload[2] = 0x33; payload[3] = 0x44;
        if (Write((IntPtr)fds[1], payload, 4) != 4) return 7;
        if (SetIsNonBlocking((IntPtr)fds[0], 1) != 0) return 8;

        byte* buffer = (byte*)NativeMemory.Alloc(8);
        buffer[4] = 0x55; buffer[5] = 0x66; buffer[6] = 0x77; buffer[7] = 0x88;

        GetNonCryptographicallySecureRandomBytes(buffer, 8);

        uint first = 366850414;
        uint second = 1610402240;
        byte[] expected =
        {
            (byte)(0x11 ^ first), (byte)(0x22 ^ (first >> 8)), (byte)(0x33 ^ (first >> 16)), (byte)(0x44 ^ (first >> 24)),
            (byte)(0x55 ^ second), (byte)(0x66 ^ (second >> 8)), (byte)(0x77 ^ (second >> 16)), (byte)(0x88 ^ (second >> 24)),
        };

        for (int i = 0; i < 8; i++)
        {
            if (buffer[i] != expected[i]) return 10 + i;
        }

        // The secure entry point over the same descriptor: the read writes
        // its prefix and fails, the call returns -1 with EAGAIN, and the rest
        // of the buffer keeps what the guest put there.
        if (Write((IntPtr)fds[1], payload, 4) != 4) return 20;
        buffer[4] = 0x55; buffer[5] = 0x66; buffer[6] = 0x77; buffer[7] = 0x88;
        Marshal.SetLastSystemError(0);
        if (GetCryptographicallySecureRandomBytes(buffer, 8) != -1) return 21;
        if (Marshal.GetLastSystemError() != 11) return 22;

        byte[] untouched = { 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88 };

        for (int i = 0; i < 8; i++)
        {
            if (buffer[i] != untouched[i]) return 30 + i;
        }

        NativeMemory.Free(buffer);
        return 0;
    }
}
