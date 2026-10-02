using System;
using System.Runtime.InteropServices;

// Every path-taking shim entry point, handed a pathname the kernel cannot copy
// in: NULL is EFAULT, a pathname with no NUL within PATH_MAX bytes is
// ENAMETOOLONG (5000 bytes is past both Linux's 4096 and Darwin's 1024), and
// the empty path is ENOENT. Measured on both kernels by
// docs/plans/2026-08-23-posix-kernel-extraction/path-copyin-order.c, which is
// why this can be a pure test: every row below is one Linux and macOS agree on.
//
// Left out, because the two disagree: `OpenDir(NULL)`, which glibc's
// opendir(3) dies on with SIGSEGV where Darwin's answers EFAULT; and `Rename`
// with a readable source that does not exist and an unreadable destination,
// which Darwin answers ENOENT (it resolves the source first) and Linux EFAULT.
//
// Errnos are compared as PAL values, as in UnlinkSeeded.cs. Every call here
// fails, so nothing is created, removed or changed, and no seed is needed.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    // Must match `Interop.Sys.FileStatus`: 17 sequential fields, 120 bytes.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_OpenDir", SetLastError = true)]
    static extern unsafe IntPtr OpenDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Unlink", SetLastError = true)]
    static extern unsafe int Unlink(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_RmDir", SetLastError = true)]
    static extern unsafe int RmDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ChDir", SetLastError = true)]
    static extern unsafe int ChDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ChMod", SetLastError = true)]
    static extern unsafe int ChMod(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ReadLink", SetLastError = true)]
    static extern unsafe int ReadLink(byte* path, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Access", SetLastError = true)]
    static extern unsafe int Access(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Rename", SetLastError = true)]
    static extern unsafe int Rename(byte* oldPath, byte* newPath);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Error, the PAL error enum.
    const int PAL_ENOENT = 0x1002D;
    const int PAL_EFAULT = 0x10015;
    const int PAL_ENAMETOOLONG = 0x10025;

    static int check = 0;

    static bool Is(bool condition)
    {
        check++;
        return condition;
    }

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    // Call number `which` on `path`, reporting its return value.
    static unsafe long Call(int which, byte* path, byte* buffer, FileStatus* status)
    {
        Marshal.SetLastSystemError(0);
        switch (which)
        {
            case 0: return Stat(path, status);
            case 1: return LStat(path, status);
            case 2: return (long)Open(path, 0, 438);
            case 3: return MkDir(path, 0x1ff);
            case 4: return Unlink(path);
            case 5: return RmDir(path);
            case 6: return ChDir(path);
            case 7: return ChMod(path, 0x1a4);
            case 8: return ReadLink(path, buffer, 64);
            case 9: return Access(path, 0);
            case 10: return Rename(path, path);
            // `opendir(3)` reports failure as NULL rather than -1.
            case 11: return OpenDir(path) == IntPtr.Zero ? -1 : 0;
            default: throw new ArgumentOutOfRangeException(nameof(which));
        }
    }

    const int Calls = 11;
    const int OpenDirCall = 11;

    static unsafe int Main()
    {
        byte* tooLong = stackalloc byte[5001];
        for (int i = 0; i < 5000; i++)
        {
            tooLong[i] = (byte)(i % 2 == 0 ? 'a' : '/');
        }
        tooLong[4999] = (byte)'a';
        tooLong[5000] = 0;

        byte* empty = stackalloc byte[1];
        empty[0] = 0;

        byte* buffer = stackalloc byte[64];
        FileStatus status;

        for (int which = 0; which < Calls; which++)
        {
            if (!Is(Call(which, null, buffer, &status) == -1 && LastPalError() == PAL_EFAULT)) return check;
            if (!Is(Call(which, tooLong, buffer, &status) == -1 && LastPalError() == PAL_ENAMETOOLONG)) return check;
            if (!Is(Call(which, empty, buffer, &status) == -1 && LastPalError() == PAL_ENOENT)) return check;
        }

        // `OpenDir` without its NULL row; see the header.
        if (!Is(Call(OpenDirCall, tooLong, buffer, &status) == -1 && LastPalError() == PAL_ENAMETOOLONG)) return check;
        if (!Is(Call(OpenDirCall, empty, buffer, &status) == -1 && LastPalError() == PAL_ENOENT)) return check;

        return 0;
    }
}
