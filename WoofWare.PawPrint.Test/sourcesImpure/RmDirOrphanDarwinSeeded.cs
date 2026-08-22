using System;
using System.Runtime.InteropServices;

// What becomes reachable once `rmdir(2)` can remove a directory something still
// holds: an *orphaned* current directory, on a **Darwin**-configured kernel.
//
// PawPrint-only, and it has to be. On the real runtime the guest's working
// directory holds the guest image itself, so it can never be emptied — and
// PawPrint has no `chdir`, so the only route to standing in an orphan is a
// `KernelConfig.CurrentDirectory` the guest then removes by absolute path.
//
// Every row here is measured on both flavours except the last, which is why
// this file has a Linux twin. Probed with the current directory removed out
// from under the process:
//
//   creating anything inside it      ENOENT, at 0o755 as at 0o555
//   looking a name up inside it      ENOENT (it is necessarily empty)
//   "." and ".." from inside it      still resolve, to the old inodes
//   getcwd with an ample buffer      ENOENT
//   getcwd with a zero-length one    EINVAL, the shim's own guard
//   getcwd with a buffer too small
//     for the path that used to be
//     there, but at least 2 bytes    ENOENT on both
//   getcwd with a one-byte buffer    ENOENT on Linux, ERANGE on Darwin
//
// The last of those is `SimulatedUnixPlatform.getCwdOrphanAnswer`, and it is the
// only thing this file asserts that its twin does not. The row above it is what
// makes that answer a *minimum size* rather than a comparison against the stale
// path: measured by sweeping the size from 1 past the old path's length, Darwin
// is ERANGE only at 1.
//
// What no guest can see — that the orphan's *ancestors* stay alive, and that
// they are collected together — is asserted on the terminal state; see
// `TestImpureCases.assertRmDirOrphanChainSurvives`.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): work/inner/ (empty), and the current directory is
// "/work/inner".
class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct FileStatus
    {
        public int Flags;
        public int Mode;
        public uint Uid;
        public uint Gid;
        public long Size;
        public long ATime;
        public long ATimeNsec;
        public long MTime;
        public long MTimeNsec;
        public long CTime;
        public long CTimeNsec;
        public long BirthTime;
        public long BirthTimeNsec;
        public long Dev;
        public long RDev;
        public long Ino;
        public uint UserFlags;
    }

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_RmDir", SetLastError = true)]
    static extern unsafe int RmDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetCwd", SetLastError = true)]
    static extern unsafe byte* GetCwd(byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Error, the PAL error enum.
    const int PAL_EINVAL = 0x1001C;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ERANGE = 0x10047;

    // Interop.Sys.OpenFlags.
    const int O_RDWR = 0x0002;
    const int O_CREAT = 0x0020;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe void Ascii(string s, byte* dest)
    {
        for (int i = 0; i < s.Length; i++)
        {
            dest[i] = (byte)s[i];
        }

        dest[s.Length] = 0;
    }

    static unsafe int RmDirPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        Marshal.SetLastSystemError(0);
        return RmDir(buf);
    }

    static unsafe int MkDirPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        Marshal.SetLastSystemError(0);
        return MkDir(buf, 0x1ED /* 0o755 */);
    }

    static unsafe IntPtr CreatePath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        Marshal.SetLastSystemError(0);
        return Open(buf, O_RDWR | O_CREAT, 0x1A4 /* 0o644 */);
    }

    static unsafe int LStatPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        FileStatus status;
        Marshal.SetLastSystemError(0);
        return LStat(buf, &status);
    }

    static unsafe long StatIno(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        FileStatus status;
        Marshal.SetLastSystemError(0);
        if (Stat(buf, &status) != 0) return -1;
        return status.Ino;
    }

    /// The PAL error `getcwd` failed with, or 0 if it unexpectedly succeeded.
    static unsafe int GetCwdError(int bufferSize)
    {
        byte* buf = stackalloc byte[256];
        Marshal.SetLastSystemError(0);
        return GetCwd(buf, bufferSize) == null ? LastPalError() : 0;
    }

    static unsafe int Main()
    {
        int check = 0;

        // Before anything: the ordinary answers, so that the rows below are
        // about the orphaning rather than about these calls never working.
        long innerBefore = StatIno(".");
        check++;
        if (innerBefore < 0) return check;

        long workBefore = StatIno("..");
        check++;
        if (workBefore < 0) return check;

        check++;
        if (GetCwdError(256) != 0) return check;

        // ---- remove the directory the process is standing in ----

        // By absolute path: "." is EINVAL on both kernels, which is the point of
        // that arm.
        check++;
        if (RmDirPath(".") != -1) return check;

        check++;
        if (RmDirPath("/work/inner") != 0) return check;

        check++;
        if (LStatPath("/work/inner") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // ---- "." and ".." still resolve, to the same inodes ----

        // Measured on both: the orphan keeps its "..", and keeps it even after
        // the parent is itself removed. This is why the ancestors must not be
        // freed while anything holds the orphan.
        check++;
        if (StatIno(".") != innerBefore) return check;
        check++;
        if (StatIno("..") != workBefore) return check;

        // ---- but nothing can be created inside it ----

        check++;
        if (MkDirPath("sub") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        check++;
        if (CreatePath("x") != new IntPtr(-1)) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // Creation through ".." is refused for the same reason: `/work` is about
        // to be an orphan too, and until then it is an ordinary directory.
        // Looking a name up is ENOENT the ordinary way — an orphan is always
        // empty, because `rmdir` refuses a populated directory and the rule
        // above stops one ever gaining an entry.
        check++;
        if (LStatPath("nx") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // ---- getcwd has no answer to give ----

        check++;
        if (GetCwdError(256) != PAL_ENOENT) return check;

        // The shim's own guard, before `getcwd` is reached at all: measured
        // EINVAL on both even with the directory removed.
        check++;
        if (GetCwdError(0) != PAL_EINVAL) return check;

        // ---- and the parent can go too, while the orphan still holds it ----

        check++;
        if (RmDirPath("/work") != 0) return check;
        check++;
        if (LStatPath("/work") != -1) return check;

        // Still climbing, into a chain nothing names any more.
        check++;
        if (StatIno("..") != workBefore) return check;

        // Creation elsewhere is unaffected, so the ENOENT rows above are about
        // the orphan rather than about creation being broken.
        check++;
        if (MkDirPath("/newtop") != 0) return check;
        check++;
        if (LStatPath("/newtop") != 0) return check;

        // ---- the one flavour-dependent row ----

        // Smaller than the path that used to be here ("/work/inner" is eleven
        // bytes) but big enough for "/" and a terminator: ENOENT on both, so
        // neither kernel is comparing the buffer against a path that no longer
        // exists. This is the row that makes the ERANGE below a *minimum size*.
        check++;
        if (GetCwdError(5) != PAL_ENOENT) return check;

        // And the one flavour-dependent row: a buffer that cannot hold even "/".
        // Darwin's libc `getcwd` writes the root first and so needs two bytes
        // before it can start; Linux's `sys_getcwd` fails ENOENT before any
        // length is considered. The twin asserts the other value.
        check++;
        if (GetCwdError(1) != PAL_ERANGE) return check;

        check++;
        if (GetCwdError(2) != PAL_ENOENT) return check;

        return 0;
    }
}
