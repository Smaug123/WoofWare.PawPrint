using System;
using System.Runtime.InteropServices;

// Exercises the fact that `dup(2)` shares the *open file description* rather
// than copying it: a descriptor produced by dup behaves for `write(2)` exactly
// as its source does, because the access mode lives on the description both
// descriptors name.
//
// This is the end-to-end counterpart to TestFileDescriptorRegistry's sharing
// property. Without it, a wiring regression that dropped the role on the dup
// path (routing every duped descriptor to stdout, say) would be invisible to
// every registry-level test AND to SystemNativeDup.cs, which only checks fd
// arithmetic.
//
// Like SystemNativeWrite.cs, this test must pass on the real runtime as well
// as PawPrint, so it asserts only invariants that hold on every Unix kernel
// and emits no bytes at all — every write here has either a zero byte count
// or a descriptor that refuses the write. The byte-emitting counterpart, which
// asserts that the bytes land under the stdout role, is necessarily
// PawPrint-only and lives in sourcesImpure/SystemNativeDupWriteRole.cs.
//
// The two assertions are chosen to *distinguish* the two roles rather than
// merely to fail on error, since both a wrong-role dup and a broken dup would
// otherwise return -1 alike:
//   * a dup of stdout accepts a zero-byte write (returns 0)
//   * a dup of stdin refuses a one-byte write (returns -1, EBADF)
// An implementation that routed duped descriptors to a single fixed role would
// fail one of these whichever role it picked.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr SystemNative_Dup(IntPtr oldfd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int SystemNative_Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int SystemNative_Close(IntPtr fd);

    static unsafe int Main(string[] args)
    {
        byte b = 0;

        // A dup of stdout is writable, exactly as stdout is. A zero-byte
        // write is the strongest assertion available that does not actually
        // emit anything: write(2) performs no I/O and no dereference, and
        // returns 0. SystemNativeWrite.cs already establishes that this
        // idiom behaves identically on both runtimes for fd 1 itself.
        IntPtr dupedOut = SystemNative_Dup((IntPtr)1);
        if ((long)dupedOut < 3L) return 1;
        if (SystemNative_Write(dupedOut, (byte*)0, 0) != 0) return 2;

        // A dup of stdin is *not* writable: the process was launched with
        // stdin redirected (a pipe opened O_RDONLY), and the access mode is
        // a property of the open file description, so the dup inherits it.
        // write(2) therefore fails with EBADF.
        IntPtr dupedIn = SystemNative_Dup((IntPtr)0);
        if ((long)dupedIn < 3L) return 3;
        if ((long)dupedIn == (long)dupedOut) return 4;
        if (SystemNative_Write(dupedIn, &b, 1) != -1) return 5;

        // Closing one descriptor does not disturb the other, even though the
        // stdout pair shares a description: fd 1 still names it.
        if (SystemNative_Close(dupedIn) != 0) return 6;
        if (SystemNative_Write(dupedOut, (byte*)0, 0) != 0) return 7;
        if (SystemNative_Close(dupedOut) != 0) return 8;
        if (SystemNative_Write((IntPtr)1, (byte*)0, 0) != 0) return 9;

        return 0;
    }
}
