using System;
using System.Runtime.InteropServices;

// `rmdir(2)`'s flavour-dependent facts on a **Darwin**-configured kernel: which
// errno each navigation carries, and whether a trailing separator reaches past
// a final symlink — which here it does, destructively.
//
// PawPrint-only, because a differential run would compare PawPrint's configured
// kernel against whichever kernel happened to run the oracle.
// sourcesPure/RmDirSeeded.cs carries everything the two platforms agree about.
//
// This file and its Linux twin exist as a **pair**, and neither alone is
// enough: `TestRmDirRules` hands the flavour in explicitly and the host oracle
// compares against whichever kernel it is running on, so a handler that ignored
// `SimulatedUnixPlatform.rmDirRules` and hardcoded either answer would satisfy
// every one of them plus one of these two guests.
//
// The uid is 1000 — not `KernelConfig`'s default — so a handler that assumed
// privilege instead of reading `Kernel.UserId` fails the `nowrite` rows here and
// nowhere else.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): d/ (empty), f (a file), ld -> d, dang -> nx,
// cyc -> cyc, lroot -> "/", stamped/ (empty), nowrite/ (0o555, holding kdir/
// and kid). The
// current directory is "/".
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
    const int PAL_EACCES = 0x10002;
    const int PAL_EBUSY = 0x1000A;
    const int PAL_EISDIR = 0x1001F;
    const int PAL_ELOOP = 0x10020;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;

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

    static unsafe int LStatPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        FileStatus status;
        Marshal.SetLastSystemError(0);
        return LStat(buf, &status);
    }

    // Interop.Sys.OpenFlags, and the mode CoreLib's own OpenReadOnly passes.
    const int O_RDONLY = 0x0000;
    const int DefaultCreateMode = 438;

    static unsafe IntPtr OpenPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        Marshal.SetLastSystemError(0);
        return Open(buf, O_RDONLY, DefaultCreateMode);
    }

    static int check = 0;

    /// Expect `path` to be refused with exactly `pal`. Returns 0 to carry on.
    static int Refused(string path, int pal)
    {
        check++;
        if (RmDirPath(path) != -1) return check;
        check++;
        if (LastPalError() != pal) return check;
        return 0;
    }

    static unsafe int Main()
    {
        int bad;

        // ---- Darwin specialises the root inode, not the path ----

        // A path that consumed no component at all is EISDIR, where Linux says
        // EBUSY. Reached directly and — because Darwin's walk resolves a final
        // symlink under a trailing separator — through `lroot -> "/"`, which
        // Linux answers ENOTDIR.
        foreach (string p in new[] { "/", "lroot/" })
        {
            bad = Refused(p, PAL_EISDIR);
            if (bad != 0) return bad;
        }

        // Every way of reaching the root by a navigation is EBUSY: XNU refuses a
        // mount's root vnode before it looks at which navigation got there, and
        // PawPrint mounts one filesystem. Linux gives these EINVAL and
        // ENOTEMPTY. The current directory here is "/", so "." and ".." reach
        // the root too.
        foreach (string p in new[] { ".", "./", "..", "/.", "/..", "lroot/.", "lroot/.." })
        {
            bad = Refused(p, PAL_EBUSY);
            if (bad != 0) return bad;
        }

        // ---- a trailing separator resolves the final symlink ----

        // `TrailingSeparatorPolicy.Demand`, so each of these lands on what the
        // link *named* rather than on the link. Linux answers every one ENOTDIR.
        bad = Refused("f/", PAL_ENOTDIR);
        if (bad != 0) return bad;

        bad = Refused("dang/", PAL_ENOENT);
        if (bad != 0) return bad;

        bad = Refused("cyc/", PAL_ELOOP);
        if (bad != 0) return bad;

        // ---- the uid, and the order the checks are made in ----

        // `nowrite/kid` is a *file* inside a directory this caller cannot write.
        // Darwin reports the type; Linux reports the permission.
        bad = Refused("nowrite/kdir", PAL_EACCES);
        if (bad != 0) return bad;

        bad = Refused("nowrite/kid", PAL_ENOTDIR);
        if (bad != 0) return bad;

        // A free name in that same unwritable directory is ENOENT, so the
        // missing-name check beats the permission one.
        bad = Refused("nowrite/nx", PAL_ENOENT);
        if (bad != 0) return bad;


        // ---- the removed directory's own ctime ----

        // Watched through a descriptor held across the call, which is the only
        // way to see an inode whose last name has just gone. Linux drops its
        // `st_nlink` from 2 to 0 and moves its `ctime`; Darwin leaves both.
        // `FileStatus` has no `st_nlink`, so the stamp is the only half a guest
        // can read — and it is `RmDirRules.RemovedDirectoryEffect`.
        IntPtr fd = OpenPath("stamped");
        check++;
        if (fd == new IntPtr(-1)) return check;

        FileStatus before;
        Marshal.SetLastSystemError(0);
        check++;
        if (FStat(fd, &before) != 0) return check;

        check++;
        if (RmDirPath("stamped") != 0) return check;

        FileStatus after;
        Marshal.SetLastSystemError(0);
        check++;
        if (FStat(fd, &after) != 0) return check;

        check++;
        if (after.Ino != before.Ino) return check;

        // Unmoved on both, so the row below is about `ctime` alone.
        check++;
        if (after.MTime != before.MTime || after.MTimeNsec != before.MTimeNsec) return check;

        check++;
        if (after.CTime != before.CTime || after.CTimeNsec != before.CTimeNsec) return check;

        check++;
        if (Close(fd) != 0) return check;

        // ---- nothing so far has removed anything ----

        foreach (string p in new[] { "d", "f", "ld", "dang", "cyc", "lroot", "nowrite/kdir", "nowrite/kid" })
        {
            check++;
            if (LStatPath(p) != 0) return check;
        }

        // ---- and then the row that destroys the wrong object ----

        // Last, because it is the only check here that succeeds. `ld -> d`, and
        // Darwin's walk follows the final link before imposing the directory
        // demand — so this removes `d`, the link's *target*, while `ld` itself
        // stays. Linux cannot reach past the link at all and answers ENOTDIR
        // (asserted in the twin). A handler that hardcoded either flavour would
        // delete the wrong thing on the other.
        check++;
        if (RmDirPath("ld/") != 0) return check;

        check++;
        if (LStatPath("d") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        check++;
        if (LStatPath("ld") != 0) return check;

        return 0;
    }
}
