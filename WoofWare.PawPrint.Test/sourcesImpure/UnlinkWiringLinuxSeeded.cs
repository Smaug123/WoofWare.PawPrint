using System;
using System.Runtime.InteropServices;

// `unlink(2)`'s flavour-dependent facts on a **Linux**-configured kernel: which
// errno each refusal carries, and whether a trailing separator reaches past a
// final symlink.
//
// PawPrint-only, because a differential run would compare PawPrint's configured
// kernel against whichever kernel happened to run the oracle.
// sourcesPure/UnlinkSeeded.cs carries everything the two platforms agree about.
//
// This file and its Darwin twin exist as a **pair**, and neither alone is
// enough: `TestUnlinkRules` hands the flavour in explicitly and the host
// oracle compares against whichever kernel it is running on, so a handler that
// ignored `SimulatedUnixPlatform.unlinkRules` and hardcoded either answer would
// satisfy every one of them plus one of these two guests.
//
// The uid is 1000 — not `KernelConfig`'s default of privileged anything — so a
// handler that assumed privilege instead of reading `Kernel.UserId` fails the
// `nowrite` rows here and nowhere else.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): d/ (empty), f (a file), ld -> d, dang -> nx,
// cyc -> cyc, lroot -> "/", nowrite/ (0o555, holding kdir/ and kid).
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Unlink", SetLastError = true)]
    static extern unsafe int Unlink(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Error, the PAL error enum.
    const int PAL_EACCES = 0x10002;
    const int PAL_EISDIR = 0x1001F;
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

    static unsafe int UnlinkPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        Marshal.SetLastSystemError(0);
        return Unlink(buf);
    }

    static unsafe int LStatPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        FileStatus status;
        Marshal.SetLastSystemError(0);
        return LStat(buf, &status);
    }

    static int check = 0;

    /// Expect `path` to be refused with exactly `pal`. Returns 0 to carry on.
    static int Refused(string path, int pal)
    {
        check++;
        if (UnlinkPath(path) != -1) return check;
        check++;
        if (LastPalError() != pal) return check;
        return 0;
    }

    static int Main()
    {
        int bad;

        // ---- every directory is EISDIR, however it was reached ----

        // Linux spends no errno distinguishing the navigations. The current
        // directory here is "/", so "." and ".." reach the root; Darwin answers
        // EBUSY for those and EPERM for an ordinary directory.
        foreach (string p in new[] { "d", "d/", ".", "..", "/", "/.", "/..", "lroot/.", "nowrite/kdir/" })
        {
            bad = Refused(p, PAL_EISDIR);
            if (bad != 0) return bad;
        }

        // ---- a trailing separator never traverses the final symlink ----

        // `TrailingSeparatorPolicy.Ignore`, so no ELOOP for the cycle and no
        // ENOENT for the dangling link. "lroot/" is the row that proves it:
        // Darwin follows it to the root and answers EISDIR.
        foreach (string p in new[] { "ld/", "dang/", "cyc/", "lroot/", "f/" })
        {
            bad = Refused(p, PAL_ENOTDIR);
            if (bad != 0) return bad;
        }

        // ---- the uid, and the order the checks are made in ----

        // The pair that pins the order, and the only thing telling the two
        // EISDIR arms apart: without the separator the unwritable parent wins,
        // with it the directory demand does. Darwin answers EPERM to both.
        bad = Refused("nowrite/kdir", PAL_EACCES);
        if (bad != 0) return bad;

        bad = Refused("nowrite/kid", PAL_EACCES);
        if (bad != 0) return bad;

        // A free name in that same unwritable directory is ENOENT, so the
        // missing-name check beats the permission one.
        bad = Refused("nowrite/nx", PAL_ENOENT);
        if (bad != 0) return bad;

        // ---- and none of the above removed anything ----

        foreach (string p in new[] { "d", "f", "ld", "dang", "cyc", "lroot", "nowrite/kdir", "nowrite/kid" })
        {
            check++;
            if (LStatPath(p) != 0) return check;
        }

        return 0;
    }
}
