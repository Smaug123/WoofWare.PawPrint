using System;
using System.Runtime.InteropServices;

// `rmdir(2)` through the raw shim, in the rows Linux and macOS answer
// identically. This is a *pure* test, so it runs on the real CLR as well as
// under PawPrint, and every fact below is one both must agree on.
//
// The rows they do *not* agree on are the root-navigation arms (Linux gives "/"
// EBUSY and "/." EINVAL, Darwin gives them EISDIR and EBUSY) and whether a
// trailing separator reaches past a final symlink — which is the divergence
// that *destroys different objects*, since Darwin's `rmdir("ld/")` removes the
// link's target. Those live in sourcesImpure/RmDirWiring{Linux,Darwin}Seeded.cs
// and in TestRmDirRules.
//
// **Errnos are compared as PAL values, not raw numbers.** ENOTEMPTY is the
// sharpest case in this file: raw 39 on Linux against raw 66 on Darwin, and each
// of those numbers names a different error on the other kernel.
//
// Everything is hand-rolled `DllImport`, for the reason
// sourcesPure/UnlinkSeeded.cs gives: `Directory.Delete` builds its failures
// through `SystemNative_StrErrorR`, which PawPrint does not implement.
//
// No permission row here, and deliberately: a run as root would answer
// differently on the two sides, since PawPrint's uid is `KernelConfig`'s
// whatever the host's is. Those rows are in the unit tier, on both flavours.
//
// This guest *deletes* things. On the oracle side that is the per-run scratch
// directory, so every path below must be a relative name the seed put there —
// never an absolute path, never "..", and never the guest image.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): empty/ (an empty directory), full/x,
// f (a file), ld -> full, dang -> nx, nav/kid/ (two levels), held/ (an empty
// directory this guest opens before removing). "nx" deliberately does not exist.
class Program
{
    // Must match `Interop.Sys.FileStatus` exactly: 17 sequential fields, 120
    // bytes. See sourcesPure/SystemNativeOpen.cs, which declares the same shape.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FStat", SetLastError = true)]
    static extern unsafe int FStat(IntPtr fd, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Error, the PAL error enum.
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;
    const int PAL_ENOTEMPTY = 0x1003A;
    const int PAL_EINVAL = 0x1001C;

    // Interop.Sys.OpenFlags, and the mode CoreLib's own OpenReadOnly passes.
    const int O_RDONLY = 0x0000;
    const int DefaultCreateMode = 438;

    // The S_IFMT band and the directory bit, as Interop.Sys declares them.
    const int S_IFMT = 0xF000;
    const int S_IFDIR = 0x4000;

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

    static unsafe int StatPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        FileStatus status;
        Marshal.SetLastSystemError(0);
        return Stat(buf, &status);
    }

    static unsafe int LStatPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        FileStatus status;
        Marshal.SetLastSystemError(0);
        return LStat(buf, &status);
    }

    static unsafe IntPtr OpenPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        Marshal.SetLastSystemError(0);
        return Open(buf, O_RDONLY, DefaultCreateMode);
    }

    static int check = 0;

    /// Expect `path` to be refused with exactly `pal`, and to still be there.
    static int Refused(string path, int pal, string survivor)
    {
        check++;
        if (RmDirPath(path) != -1) return check;
        check++;
        if (LastPalError() != pal) return check;
        check++;
        if (LStatPath(survivor) != 0) return check;
        return 0;
    }

    static unsafe int Main()
    {
        int bad;

        // ---- removing an empty directory ----

        check++;
        if (RmDirPath("empty") != 0) return check;
        check++;
        if (StatPath("empty") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // Removing it again is an ordinary missing-name failure.
        check++;
        if (RmDirPath("empty") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        check++;
        if (RmDirPath("nx") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // A trailing separator on a free name changes nothing.
        check++;
        if (RmDirPath("nx/") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // ---- a directory that still holds an entry ----

        // The errno this whole slice added: raw 39 on Linux and 66 on Darwin,
        // one PAL value.
        bad = Refused("full", PAL_ENOTEMPTY, "full");
        if (bad != 0) return bad;
        bad = Refused("full/", PAL_ENOTEMPTY, "full/x");
        if (bad != 0) return bad;

        // ---- anything that is not a directory ----

        bad = Refused("f", PAL_ENOTDIR, "f");
        if (bad != 0) return bad;
        bad = Refused("f/", PAL_ENOTDIR, "f");
        if (bad != 0) return bad;

        // `NoFollowFinal`: named without a separator, a symlink resolves to the
        // link itself, which is not a directory whatever it points at. An
        // implementation that followed it would destroy `full`.
        bad = Refused("ld", PAL_ENOTDIR, "ld");
        if (bad != 0) return bad;
        check++;
        if (LStatPath("full") != 0) return check;

        bad = Refused("dang", PAL_ENOTDIR, "dang");
        if (bad != 0) return bad;

        // ---- the navigation arms, below the root ----

        // "." is EINVAL and ".." is ENOTEMPTY on both kernels once the
        // directory reached is not the root — which is what `nav/kid` is two
        // levels deep for. (The root itself splits by flavour, so it is not
        // asserted here.)
        bad = Refused("nav/kid/.", PAL_EINVAL, "nav/kid");
        if (bad != 0) return bad;
        bad = Refused("nav/kid/..", PAL_ENOTEMPTY, "nav/kid");
        if (bad != 0) return bad;

        // ...and the ordinary case still works, so the arms above are refusals
        // rather than the whole path being unreachable.
        check++;
        if (RmDirPath("nav/kid") != 0) return check;
        check++;
        if (StatPath("nav") != 0) return check;

        // ---- an open descriptor keeps the directory's inode alive ----

        // The reason removing a directory's only name cannot free the inode.
        // Measured on both kernels: after `rmdir`, the name is gone while
        // `fstat` on the descriptor still reports the same inode.
        IntPtr fd = OpenPath("held");
        check++;
        if (fd == new IntPtr(-1)) return check;

        FileStatus before;
        Marshal.SetLastSystemError(0);
        check++;
        if (FStat(fd, &before) != 0) return check;

        check++;
        if (RmDirPath("held") != 0) return check;
        check++;
        if (StatPath("held") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        FileStatus after;
        Marshal.SetLastSystemError(0);
        check++;
        if (FStat(fd, &after) != 0) return check;
        check++;
        if (after.Ino != before.Ino) return check;

        // Still a directory as far as the descriptor is concerned. Its `ctime`
        // is deliberately *not* compared: Linux moves it and Darwin does not,
        // which is the one thing about this row the two kernels disagree on.
        check++;
        if ((after.Mode & S_IFMT) != S_IFDIR) return check;

        check++;
        if (Close(fd) != 0) return check;

        return 0;
    }
}
