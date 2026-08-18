using System;
using System.Runtime.InteropServices;

// `SystemNative_CreateSocketEventBuffer` and `SystemNative_FreeSocketEventBuffer`:
// the rows whose answer does not depend on which kernel is underneath.
//
// The whole of both entry points is flavour-independent except one number, the
// stride of the block that gets allocated (16 under epoll, 32 under kqueue). That
// number has no guest observer at all -- reading past the end of the block is a
// fault rather than a return code, and 16 is a prefix of 32, so a guest can only
// ever establish a lower bound. So this file writes and reads back one 16-byte
// element, which is in range on either kernel, and the exact stride is pinned by
// `TestSocketEventBuffer.fs` reaching into the machine state instead.
//
// The two EFAULT conditions -- `buffer == NULL` and `count < 0` -- produce the
// same answer, so no input can order them and there is no ordering row to write.
// Each is still reached with the *other* conjunct valid, so that the row means
// what it says.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventBuffer")]
    static extern unsafe int CreateSocketEventBuffer(int count, byte** buffer);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FreeSocketEventBuffer")]
    static extern unsafe int FreeSocketEventBuffer(byte* buffer);

    // Interop.Error values, not raw errnos: both entry points return the PAL enum
    // directly rather than -1-and-errno, so neither touches errno on any row here.
    const int PAL_SUCCESS = 0;
    const int PAL_EFAULT = 0x10015;

    static unsafe int Main()
    {
        int check;

        // A non-null value to leave in the out-parameter before a row that must not
        // write to it. Without a sentinel, "left untouched" and "nulled" are the same
        // observation -- and they are different rows of the C, which stores NULL
        // through `buffer` on one of its two ENOMEM paths.
        byte sentinelStorage;
        byte* sentinel = &sentinelStorage;

        byte* buffer = sentinel;

        // A null out-parameter, with a count that would otherwise be fine.
        check = 1;
        if (CreateSocketEventBuffer(1, null) != PAL_EFAULT) return check;

        // A negative count, with an out-parameter that would otherwise be fine.
        check = 2;
        if (CreateSocketEventBuffer(-1, &buffer) != PAL_EFAULT) return check;
        check = 3;
        if (buffer != sentinel) return check;

        // A negative count alongside a `buffer` that is not null but is no address
        // either. The C's screen short-circuits, so it answers EFAULT having never
        // dereferenced `buffer` -- and a handler that resolved the out-parameter to
        // storage before screening the count would instead meet a pointer it cannot
        // resolve. That is the shape of the ordering bug review caught in the sibling
        // wait handler, so it is worth a row of its own here.
        check = 4;
        if (CreateSocketEventBuffer(-1, (byte**)123) != PAL_EFAULT) return check;

        // The one-element allocation the real caller performs 1024 of.
        check = 5;
        if (CreateSocketEventBuffer(1, &buffer) != PAL_SUCCESS) return check;
        check = 6;
        if (buffer == null) return check;

        // One 16-byte element is in range under either backend, so the whole of it
        // is writable and reads back what was written.
        for (int i = 0; i < 16; i++)
        {
            buffer[i] = (byte)(i + 1);
        }

        for (int i = 0; i < 16; i++)
        {
            check = 7;
            if (buffer[i] != (byte)(i + 1)) return check;
        }

        // `count == 0` is a zero-byte request. C permits `malloc(0)` to answer NULL,
        // but both libcs a differential run can land on -- glibc and Darwin's
        // libmalloc -- hand back a distinct non-null pointer instead, which is
        // measured rather than recalled. Nothing dereferences it: the block really is
        // zero bytes wide.
        byte* empty = null;
        check = 8;
        if (CreateSocketEventBuffer(0, &empty) != PAL_SUCCESS) return check;
        check = 9;
        if (empty == null) return check;
        check = 10;
        if (empty == buffer) return check;

        // `free(NULL)` is a documented no-op, and this entry point has no screen at
        // all: it returns SUCCESS unconditionally.
        check = 11;
        if (FreeSocketEventBuffer(null) != PAL_SUCCESS) return check;

        check = 12;
        if (FreeSocketEventBuffer(buffer) != PAL_SUCCESS) return check;
        check = 13;
        if (FreeSocketEventBuffer(empty) != PAL_SUCCESS) return check;

        // Allocating again after the frees, which is what `SocketAsyncEngine` does
        // when its constructor fails and a later one retries.
        byte* again = null;
        check = 14;
        if (CreateSocketEventBuffer(2, &again) != PAL_SUCCESS) return check;
        check = 15;
        if (again == null) return check;
        check = 16;
        if (FreeSocketEventBuffer(again) != PAL_SUCCESS) return check;

        return 0;
    }
}
