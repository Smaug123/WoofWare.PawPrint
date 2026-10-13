using System;
using System.Runtime.InteropServices;

// A descriptor argument with bits set above its low 32. System.Native declares
// every descriptor argument `intptr_t` and converts it with `ToFileDescriptor`
// (src/native/libs/Common/pal_utilities.h), which asserts the value is a
// plausible descriptor and then returns `(int)fd`. The shipped shim is a
// release build, where the assert is compiled out, so only the low 32 bits
// reach the kernel: 0x1_0000_0003 is descriptor 3.
//
// Facts pinned:
//
//   * a value whose low half names an open descriptor reaches that descriptor,
//     whether the bits above are a positive high half or the sign bit;
//   * that holds for a call that changes the descriptor (F_SETFD, close) as
//     well as for one that reads it;
//   * a value whose low half names no open descriptor answers -1.
//
// The answers are the shim's truncation and FD_CLOEXEC, which are the same on
// both kernels, so this is compared with real .NET on every host.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetFD")]
    static extern int GetFD(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetFD")]
    static extern int SetFD(IntPtr fd, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldFd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    static IntPtr WithHigh(IntPtr fd, long high) => (IntPtr)(high | (long)fd);

    const long AboveLowHalf = 1L << 32;
    const long SignBit = unchecked((long)0x8000_0000_0000_0000UL);
    const long AllHigh = unchecked((long)0xFFFF_FFFF_0000_0000UL);

    static int Main(string[] args)
    {
        // `Dup` is F_DUPFD_CLOEXEC, so the copy has FD_CLOEXEC.
        IntPtr copy = Dup((IntPtr)0);
        if ((long)copy < 0) return 1;
        if (GetFD(copy) != 1) return 2;

        if (GetFD(WithHigh(copy, AboveLowHalf)) != 1) return 3;
        if (GetFD(WithHigh(copy, SignBit)) != 1) return 4;
        if (GetFD(WithHigh(copy, AllHigh)) != 1) return 5;

        // A copy made through the alias is a copy of the descriptor it names.
        IntPtr second = Dup(WithHigh(copy, AboveLowHalf));
        if ((long)second < 0) return 6;
        if (GetFD(second) != 1) return 7;
        if (Close(second) != 0) return 8;

        // Changing the descriptor through the alias changes the descriptor.
        if (SetFD(WithHigh(copy, AboveLowHalf), 0) != 0) return 9;
        if (GetFD(copy) != 0) return 10;

        // Closing through the alias closes it.
        if (Close(WithHigh(copy, SignBit)) != 0) return 11;
        if (GetFD(copy) != -1) return 12;

        // The low half is all that is read: neither of these is open.
        if (GetFD(WithHigh(copy, AboveLowHalf)) != -1) return 13;
        if (GetFD((IntPtr)(AboveLowHalf | 0x7FFF_FFFFL)) != -1) return 14;

        return 0;
    }
}
