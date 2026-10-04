using System;
using System.IO;
using System.Runtime.InteropServices;

// `link(2)`'s flavour-dependent rows, on a **Darwin**-configured kernel which has no protected_hardlinks:
// whether plain link follows a symbolic link source, what a trailing separator
// after a taken name costs, and which of another user's files the caller may
// link. The caller is uid and gid 1000, not root.
//
// PawPrint-only, because a differential run would compare PawPrint's configured
// kernel against whichever kernel happened to run the oracle.
// sourcesPure/LinkSeeded.cs carries everything the two platforms agree about,
// and this file's twin for the other flavour the other answers: a handler that
// hardcoded either flavour's rules would pass one of them.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f, g (the caller's), lf -> f, dang -> nx, and
// root's ro (0644), rw (0666) and su (04666). "nx" deliberately does not exist.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Link", SetLastError = true)]
    static extern unsafe int Link(byte* source, byte* linkTarget);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    const int PAL_EEXIST = 0x10014;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;
    const int PAL_EPERM = 0x10042;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static byte[] Bytes(string s)
    {
        byte[] bytes = new byte[s.Length + 1];
        for (int i = 0; i < s.Length; i++) bytes[i] = (byte)s[i];
        bytes[s.Length] = 0;
        return bytes;
    }

    static unsafe int LinkPath(string source, string linkTarget)
    {
        byte[] s = Bytes(source);
        byte[] l = Bytes(linkTarget);
        fixed (byte* sp = s)
        fixed (byte* lp = l)
            return Link(sp, lp);
    }

    static unsafe FileStatus LStatOf(string path)
    {
        byte[] p = Bytes(path);
        FileStatus status;
        fixed (byte* pp = p)
        {
            if (LStat(pp, &status) != 0) throw new IOException("lstat " + path);
        }
        return status;
    }

    static int Expect(int check, int result, int palError) =>
        result == -1 && LastPalError() == palError ? 0 : check;

    static int Main()
    {
        int check = 0;

        // Plain link follows a symbolic link source: the new name is a
        // second name for f, and a dangling link is ENOENT.
        check++;
        if (LinkPath("lf", "x") != 0) return check;
        check++;
        if ((LStatOf("x").Mode & 0xF000) != 0x8000) return check;
        check++;
        if (Expect(check, LinkPath("dang", "y"), PAL_ENOENT) != 0) return check;

        // A trailing separator after a taken file is looked up: ENOTDIR.
        check++;
        if (Expect(check, LinkPath("f", "g/"), PAL_ENOTDIR) != 0) return check;

        // Darwin has no protected_hardlinks: another user's file links
        // whatever its mode.
        check++;
        if (LinkPath("ro", "z") != 0) return check;
        check++;
        if (LinkPath("su", "z2") != 0) return check;

        return 0;
    }
}
