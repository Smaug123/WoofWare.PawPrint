using System;
using System.Runtime.InteropServices;

// The parts of SystemNative_ReadLink's contract that the differential oracle
// cannot be asked about. Three unrelated-looking things, together because each
// fails the cross-runtime test for its own reason.
//
//  1. **A multi-byte target, truncated inside a character.** readlink deals in
//     bytes; a handler that truncated a .NET string by *characters* instead
//     would agree with this one on every ASCII target -- which is every target
//     the pure sibling can use, because RealRuntime.validateSeedForOracle
//     restricts a seeded symlink target to an alphabet whose case folding is
//     unambiguous (a stock macOS filesystem aliases "ss" with the sharp s). So
//     the one seed that distinguishes the two implementations is the one seed
//     the oracle may not be given.
//  2. **The two calls that really do provoke a fault.** A null path with room
//     to write, and a good link into an unmapped buffer. Both are EFAULT, and
//     both are kept away from the oracle for the reason
//     GetCwdNoDereferenceErrors.cs gives: PawPrint's simulated address space
//     contains the mistake, and a real kernel need not be asked to. Their
//     assert-safe siblings -- the ones where neither kernel ever attempts the
//     write -- are in the pure file instead.
//  3. **errno untouched by a successful call.** POSIX forbids a function from
//     setting errno to zero, but permits a successful one to change it, so
//     "the previous errno survives" is a claim about PawPrint's kernel rather
//     than a portable fact.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f (a regular file), lf -> f, and mb, whose target
// is U+00DF followed by 'x' -- three bytes, C3 9F 78, so that a truncation at
// one or two bytes lands inside the first character.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ReadLink", SetLastError = true)]
    static extern unsafe int ReadLink(byte* path, byte* buffer, int bufferSize);

    const int ENOENT = 2;
    const int EFAULT = 14;

    const int BufSize = 64;

    // The UTF-8 encoding of the mb target, written as bytes so that this file's
    // own source stays ASCII and cannot itself be the thing that decides the
    // encoding.
    const byte Lead = 0xC3;
    const byte Trail = 0x9F;
    const byte Ex = 0x78;

    static unsafe byte* Bogus => (byte*)8;

    static unsafe void Ascii(string s, byte* dest)
    {
        for (int i = 0; i < s.Length; i++)
        {
            dest[i] = (byte)s[i];
        }

        dest[s.Length] = 0;
    }

    static unsafe void Poison(byte* buffer)
    {
        for (int i = 0; i < BufSize; i++)
        {
            buffer[i] = (byte)'#';
        }
    }

    static unsafe int Main(string[] args)
    {
        byte* path = stackalloc byte[BufSize];
        byte* buf = stackalloc byte[BufSize];
        int check = 0;

        // Room for all of it. The return value is 3 -- the byte count -- where
        // a character count would be 2.
        check = 1;
        Ascii("mb", path);
        Poison(buf);
        if (ReadLink(path, buf, 16) != 3) return check;
        check = 2;
        if (buf[0] != Lead || buf[1] != Trail || buf[2] != Ex) return check;
        check = 3;
        if (buf[3] != (byte)'#') return check;

        // One byte: the lead byte alone, which is not a character at all. A
        // character-wise handler would write both bytes of it and overrun the
        // caller's buffer by one.
        check = 4;
        Poison(buf);
        if (ReadLink(path, buf, 1) != 1) return check;
        check = 5;
        if (buf[0] != Lead) return check;
        check = 6;
        if (buf[1] != (byte)'#') return check;

        // Two bytes: exactly the first character, and the 'x' must not appear.
        check = 7;
        Poison(buf);
        if (ReadLink(path, buf, 2) != 2) return check;
        check = 8;
        if (buf[0] != Lead || buf[1] != Trail) return check;
        check = 9;
        if (buf[2] != (byte)'#') return check;

        // A path that addresses nothing, with room to write: the kernel copies
        // the path in before it does anything else, so this is EFAULT.
        check = 10;
        Marshal.SetLastSystemError(0);
        if (ReadLink(null, buf, 16) != -1) return check;
        check = 11;
        if (Marshal.GetLastSystemError() != EFAULT) return check;

        // A good link into a buffer that addresses nothing: everything
        // succeeds until the copy out, which faults.
        check = 12;
        Ascii("lf", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, Bogus, 16) != -1) return check;
        check = 13;
        if (Marshal.GetLastSystemError() != EFAULT) return check;

        // A successful call reports no error, and -- Unix convention -- does
        // not clear the previous one either. Provoke a real ENOENT first, so
        // that a handler zeroing errno on success is distinguishable from one
        // leaving it alone; starting from 0 would make the two agree.
        check = 14;
        Ascii("nx", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, buf, 16) != -1) return check;
        check = 15;
        if (Marshal.GetLastSystemError() != ENOENT) return check;

        check = 16;
        Ascii("lf", path);
        Poison(buf);
        if (ReadLink(path, buf, 16) != 1) return check;
        check = 17;
        if (Marshal.GetLastSystemError() != ENOENT) return check;

        return 0;
    }
}
