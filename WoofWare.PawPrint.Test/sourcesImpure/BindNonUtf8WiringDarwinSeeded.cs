using System;
using System.Runtime.InteropServices;

// Binding a name that is not valid UTF-8, on a **Darwin**-configured kernel:
// `mkdir`, a creating `open` and `rename` onto it are each EILSEQ, as APFS
// answers, while looking the same name up is plain ENOENT.
//
// PawPrint-only, because a differential run would compare the configured
// kernel against whichever kernel ran the oracle. The rows themselves are
// measured (docs/plans/2026-09-20-unix-path-bytes/darwin-eilseq-is-last.c) and
// pinned in WoofWare.PosixKernel.Test; this guest and its Linux twin exist to
// see the flavour travel from `KernelConfig` to the syscall boundary, and
// EILSEQ come back out through both numberings the guest can read: the raw
// errno (92 on Darwin, 84 on Linux) and the PAL's `Interop.Error`.
//
// Raw P/Invoke with byte paths, because a `string` cannot carry 0xFF: CoreLib
// would encode it as UTF-8 before the call.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): d/ holding f, and ro/ (0o555) — run as uid 1000,
// so ro/ really is unwritable.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Rename", SetLastError = true)]
    static extern unsafe int Rename(byte* oldPath, byte* newPath);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // `Interop.Sys.OpenFlags`, the PAL's portable numbering.
    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;
    const int O_CREAT = 0x0020;

    const int PAL_EACCES = 0x10002;
    const int PAL_EILSEQ = 0x10019;
    const int PAL_ENOENT = 0x1002D;

    // Darwin's <errno.h>. Linux numbers EILSEQ 84, which Darwin uses for EOVERFLOW.
    const int DARWIN_EILSEQ = 92;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    // "d/" followed by one 0xFF byte, and so on: ASCII text, then raw bytes, then the NUL.
    static byte[] PathOf(string prefix, params byte[] tail)
    {
        byte[] bytes = new byte[prefix.Length + tail.Length + 1];
        for (int i = 0; i < prefix.Length; i++) bytes[i] = (byte)prefix[i];
        Array.Copy(tail, 0, bytes, prefix.Length, tail.Length);
        return bytes;
    }

    static unsafe int MkDirPath(byte[] path)
    {
        fixed (byte* p = path) return MkDir(p, 0x1FF /* 0o777 */);
    }

    static unsafe IntPtr OpenPath(byte[] path, int flags)
    {
        fixed (byte* p = path) return Open(p, flags, 0x1B6 /* 0o666 */);
    }

    static unsafe int RenamePath(byte[] source, byte[] destination)
    {
        fixed (byte* s = source)
        fixed (byte* d = destination)
            return Rename(s, d);
    }

    static int Main()
    {
        int check = 0;
        byte[] undecodable = PathOf("d/", 0xFF);

        // ---- binding is refused, and the refusal is EILSEQ in both numberings.
        check++;
        if (MkDirPath(undecodable) != -1) return check;
        check++;
        if (Marshal.GetLastSystemError() != DARWIN_EILSEQ) return check;
        check++;
        if (LastPalError() != PAL_EILSEQ) return check;

        check++;
        if (OpenPath(undecodable, O_WRONLY | O_CREAT) != (IntPtr)(-1)) return check;
        check++;
        if (LastPalError() != PAL_EILSEQ) return check;

        check++;
        if (RenamePath(PathOf("d/f"), undecodable) != -1) return check;
        check++;
        if (LastPalError() != PAL_EILSEQ) return check;

        // ---- an unwritable parent is EACCES first: the encoding is checked last.
        check++;
        if (MkDirPath(PathOf("ro/", 0xFF)) != -1) return check;
        check++;
        if (LastPalError() != PAL_EACCES) return check;

        // ---- looking the name up is not binding it: it is simply absent.
        check++;
        if (OpenPath(undecodable, O_RDONLY) != (IntPtr)(-1)) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // ---- the control: a name that is UTF-8 ("é") binds.
        check++;
        if (MkDirPath(PathOf("d/", 0xC3, 0xA9)) != 0) return check;

        return 0;
    }
}
