using System;
using System.Runtime.InteropServices;

// `SystemNative_CreateSocketEventBuffer` under the epoll backend, whose element
// stride is 16 bytes: `max(sizeof(struct epoll_event), sizeof(SocketEvent))`, and
// that `max` is 16 whichever way `struct epoll_event` is packed.
//
// Not differential, for two reasons that pull in the same direction. The stride is
// the flavour's, so a differential guest would have to agree with whichever kernel
// the test host is; and the boundary these rows sit on is PawPrint's own -- a
// native block is addressed by an `int32` byte offset, so a request whose byte
// extent leaves `int32` cannot be represented and is reported as the allocation
// failure the C's own `malloc` arm reports. A real 64-bit libc succeeds at both
// counts below, by overcommit, so no oracle agrees with this file.
//
// That boundary is also the *only* thing a guest can use to see the stride at all:
// where the exact block width is invisible (reading past the end faults rather
// than returning a code), the count at which the request stops being
// representable is `Int32.MaxValue / stride`, which differs between the flavours.
// `SocketEventBufferDarwin.cs` is the same file at 32.
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

    // `Int32.MaxValue / 16`: the largest count whose byte extent PawPrint can
    // address. 134217727 * 16 is 2147483632, and one more element is 2147483648.
    const int LargestRepresentable = 134217727;

    static unsafe int Main()
    {
        int check;

        byte sentinelStorage;
        byte* sentinel = &sentinelStorage;

        // Nothing dereferences this block: the point is only that a request this
        // large is answerable. PawPrint's blocks are sparse, so the two-gigabyte
        // extent costs nothing until something writes to it.
        byte* big = null;
        check = 1;
        if (CreateSocketEventBuffer(LargestRepresentable, &big) != PAL_SUCCESS) return check;
        check = 2;
        if (big == null) return check;
        check = 3;
        if (FreeSocketEventBuffer(big) != PAL_SUCCESS) return check;

        // One element further, and the extent leaves `int32`.
        byte* tooBig = sentinel;
        check = 4;
        if (CreateSocketEventBuffer(LargestRepresentable + 1, &tooBig) != PAL_ENOMEM) return check;

        // Nulled, not left alone. The C reaches ENOMEM by two routes: its
        // `multiply_s` overflow check, which short-circuits before the store and so
        // leaves the caller's value in place, and a `malloc` that answers NULL, where
        // the store has already run. PawPrint's failure is the second of those --
        // the product is representable, it is the block that is not -- so the
        // out-parameter is null on return.
        check = 5;
        if (tooBig != null) return check;

        // The stride, seen through that boundary: 16 bytes an element means a count
        // of 100000000 is 1.6e9 bytes and representable. The same count under
        // kqueue's 32-byte stride is 3.2e9 and is not, which is what
        // `SocketEventBufferDarwin.cs` asserts of the identical row.
        byte* strideProbe = null;
        check = 6;
        if (CreateSocketEventBuffer(100000000, &strideProbe) != PAL_SUCCESS) return check;
        check = 7;
        if (FreeSocketEventBuffer(strideProbe) != PAL_SUCCESS) return check;

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
