using System;
using System.Runtime.InteropServices;

// `SystemNative_CreateSocketEventBuffer` under the kqueue backend, whose element
// stride is 32 bytes: `sizeof(struct kevent)`, with no `max` against
// `sizeof(SocketEvent)` to flatten it.
//
// The Darwin half of `SocketEventBufferLinux.cs`; that file's header explains why
// neither is differential and why this boundary is the only guest observer of the
// stride there is. The rows below are the same rows at 32 bytes an element, and
// the last one is the one that disagrees with its Linux twin.
//
// The exit code is the index of the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventBuffer")]
    static extern unsafe int CreateSocketEventBuffer(int count, byte** buffer);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FreeSocketEventBuffer")]
    static extern unsafe int FreeSocketEventBuffer(byte* buffer);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetErrNo")]
    static extern void SetErrNo(int platformErrno);

    const int PAL_SUCCESS = 0;
    const int PAL_EFAULT = 0x10015;
    const int PAL_ENOMEM = 0x10031;

    // Raw `<errno.h>` numbers, not PAL codes: these are what `Marshal.GetLastSystemError`
    // reports. ENOMEM is 12 under both numberings, so this row needs no flavour split.
    const int RAW_ENOMEM = 12;

    // Any value no row here produces, so "errno moved" and "errno was already this"
    // cannot be confused.
    const int RAW_SENTINEL = 111;

    // `Int32.MaxValue / 32`: 67108863 * 32 is 2147483616, and one more element is
    // 2147483648.
    const int LargestRepresentable = 67108863;

    static unsafe int Main()
    {
        int check;

        byte sentinelStorage;
        byte* sentinel = &sentinelStorage;

        byte* big = null;
        check = 1;
        if (CreateSocketEventBuffer(LargestRepresentable, &big) != PAL_SUCCESS) return check;
        check = 2;
        if (big == null) return check;
        check = 3;
        if (FreeSocketEventBuffer(big) != PAL_SUCCESS) return check;

        byte* tooBig = sentinel;
        check = 4;
        if (CreateSocketEventBuffer(LargestRepresentable + 1, &tooBig) != PAL_ENOMEM) return check;
        check = 5;
        if (tooBig != null) return check;

        // The row that separates the two strides. 100000000 elements is 3.2e9 bytes
        // under kqueue, which leaves `int32`; the same count is representable under
        // epoll's 16, where `SocketEventBufferLinux.cs` asserts SUCCESS.
        byte* strideProbe = sentinel;
        check = 6;
        if (CreateSocketEventBuffer(100000000, &strideProbe) != PAL_ENOMEM) return check;
        check = 7;
        if (strideProbe != null) return check;

        // ---- errno. A failed `malloc` sets it; the wrapper's own EFAULT screen does not,
        // because no libc call happens on that path at all. Both directions are asserted
        // from a known prior value, and neither is differential: the errno half follows
        // from the divergence above, which no real libc reproduces.
        SetErrNo(RAW_SENTINEL);
        check = 8;
        if (CreateSocketEventBuffer(-1, &tooBig) != PAL_EFAULT) return check;
        check = 9;
        if (Marshal.GetLastSystemError() != RAW_SENTINEL) return check;

        check = 10;
        if (CreateSocketEventBuffer(LargestRepresentable + 1, &tooBig) != PAL_ENOMEM) return check;
        check = 11;
        if (Marshal.GetLastSystemError() != RAW_ENOMEM) return check;

        return 0;
    }
}
