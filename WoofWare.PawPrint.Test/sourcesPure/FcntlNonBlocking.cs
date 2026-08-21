using System;
using System.Runtime.InteropServices;

// The `O_NONBLOCK` status flag, through the two entry points that carry it:
// `SystemNative_FcntlSetIsNonBlocking` (pal_io.c:655, `fcntl(F_GETFL)` /
// toggle / `fcntl(F_SETFL)`) and `SystemNative_FcntlGetIsNonBlocking`
// (pal_io.c:677). Differential: every row here answers identically on Linux
// and macOS, because the flag round-trips through `fcntl` the same way on
// both and the shim's own screens are platform-free.
//
// Three facts about the C are pinned deliberately:
//
//   * any nonzero `isNonBlocking` sets the flag -- the C tests `== 0`, so 2
//     is "set", not an error;
//   * a NULL out-pointer answers `Error_EFAULT` -- the PAL *enum* value,
//     returned from a function whose other answers are 0 or -1-and-errno;
//   * on failure the getter stores 0 through the pointer before returning -1,
//     so a seeded local is overwritten.
//
// The flag lives on the open file description, so a `dup(2)` pair shares it
// in both directions; `SystemNative_Dup` is the third entry point here for
// exactly that row.
//
// Return codes only: no `SetLastError`, no errno reads. A raw `DllImport`'s
// errno capture is not modelled under PawPrint, and the raw numbers differ
// between the platforms anyway.
//
// The exit code names the first check that failed (each check has its own
// code, though not in file order); 0 means all passed. Kept below 128, since
// an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldFd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    // The same entry point through the other shape a hand-rolled P/Invoke
    // plausibly declares: a `bool` marshals as a four-byte BOOL by default,
    // so the C sees the same int32_t either way.
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")]
    static extern int SetIsNonBlockingBool(IntPtr fd, bool isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetIsNonBlocking")]
    static extern unsafe int GetIsNonBlocking(IntPtr fd, int* isNonBlocking);

    const int PAL_SUCCESS = 0;
    const int PAL_EFAULT = 0x10015;

    // PAL AddressFamily / SocketType / ProtocolType.
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;

    static unsafe int Main()
    {
        IntPtr fd;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &fd) != PAL_SUCCESS) return 1;
        if (fd == (IntPtr)(-1)) return 2;

        // A fresh socket is blocking: neither kernel is handed SOCK_NONBLOCK.
        int flag = 7;
        if (GetIsNonBlocking(fd, &flag) != 0) return 3;
        if (flag != 0) return 4;

        if (SetIsNonBlocking(fd, 1) != 0) return 5;
        if (GetIsNonBlocking(fd, &flag) != 0) return 6;
        if (flag != 1) return 7;

        // Any nonzero value sets; 2 is not an error and not stored verbatim.
        if (SetIsNonBlocking(fd, 2) != 0) return 8;
        if (GetIsNonBlocking(fd, &flag) != 0) return 9;
        if (flag != 1) return 10;

        // The flag is on the open file description: a dup pair shares it in
        // both directions.
        IntPtr duplicated = Dup(fd);
        if (duplicated == (IntPtr)(-1)) return 11;
        if (GetIsNonBlocking(duplicated, &flag) != 0) return 12;
        if (flag != 1) return 13;

        if (SetIsNonBlocking(duplicated, 0) != 0) return 14;
        if (GetIsNonBlocking(fd, &flag) != 0) return 15;
        if (flag != 0) return 16;

        // The `bool` declaration round-trips like the `int` one.
        if (SetIsNonBlockingBool(fd, true) != 0) return 26;
        if (GetIsNonBlocking(fd, &flag) != 0) return 27;
        if (flag != 1) return 28;
        if (SetIsNonBlockingBool(fd, false) != 0) return 29;
        if (GetIsNonBlocking(fd, &flag) != 0) return 30;
        if (flag != 0) return 31;

        // NULL out-pointer: the shim's own screen, ahead of any fcntl, and it
        // answers with the PAL enum value rather than -1.
        if (GetIsNonBlocking(fd, null) != PAL_EFAULT) return 17;

        // The NULL screen also precedes any look at the descriptor: a pointer
        // masquerading as the fd never reaches an fcntl.
        if (GetIsNonBlocking((IntPtr)(&flag), null) != PAL_EFAULT) return 33;

        if (Close(duplicated) != 0) return 18;

        // Closing one half of the pair leaves the description, and its flag,
        // reachable through the survivor.
        if (SetIsNonBlocking(fd, 1) != 0) return 19;
        if (GetIsNonBlocking(fd, &flag) != 0) return 20;
        if (flag != 1) return 21;

        if (Close(fd) != 0) return 22;

        // A dead descriptor: -1 from both entry points, and the getter stores
        // 0 through the pointer on the way out.
        if (SetIsNonBlocking(fd, 1) != -1) return 23;
        flag = 7;
        if (GetIsNonBlocking(fd, &flag) != -1) return 24;
        if (flag != 0) return 25;

        // On a dead fd the NULL screen and the descriptor lookup disagree
        // (EFAULT against -1), and the screen's precedence says EFAULT.
        if (GetIsNonBlocking(fd, null) != PAL_EFAULT) return 32;

        return 0;
    }
}
