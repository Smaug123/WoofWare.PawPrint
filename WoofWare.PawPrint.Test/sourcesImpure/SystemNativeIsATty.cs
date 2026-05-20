using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_IsATty PawPrint handler directly via a P/Invoke
// stub, without depending on `SafeFileHandle` marshalling. PawPrint intercepts
// the call and routes it through FileDescriptorRegistry.
//
// This is an *impure* test: it runs only inside PawPrint, never against the
// real CLR. PawPrint models a headless simulated process where no fd ever
// refers to a terminal, so every IsATty call returns 0. On a real CLR host
// the answer depends on whether stdin/stdout/stderr happen to be attached to
// a TTY at the moment the test runs — under NUnit on a developer's terminal
// that would flake the std-stream assertions. PawPrint's behaviour is stable
// by construction.
//
// The assertions:
//   * IsATty(-1) returns 0 (bad fd, errno = EBADF in PawPrint)
//   * IsATty(0)/(1)/(2) return 0 (live but never a tty in PawPrint)
//   * IsATty(duped) returns 0 (a dup of stdin is not a tty either)
//   * IsATty(closedFd) returns 0 (bad fd after close)
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_IsATty")]
    static extern int SystemNative_IsATty(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr SystemNative_Dup(IntPtr oldfd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int SystemNative_Close(IntPtr fd);

    static int Main(string[] args)
    {
        // -1 is never a live fd; IsATty returns 0.
        if (SystemNative_IsATty((IntPtr)(-1)) != 0) return 1;

        // Standard streams are live but PawPrint never treats them as a tty.
        if (SystemNative_IsATty((IntPtr)0) != 0) return 2;
        if (SystemNative_IsATty((IntPtr)1) != 0) return 3;
        if (SystemNative_IsATty((IntPtr)2) != 0) return 4;

        // dup stdin to a fresh fd; the duplicate is also not a tty.
        IntPtr duped = SystemNative_Dup((IntPtr)0);
        if ((long)duped < 3L) return 5;
        if (SystemNative_IsATty(duped) != 0) return 6;

        // After closing the duped fd, IsATty on the now-empty slot still
        // returns 0 (PawPrint maps it to the EBADF path).
        if (SystemNative_Close(duped) != 0) return 7;
        if (SystemNative_IsATty(duped) != 0) return 8;

        return 0;
    }
}
