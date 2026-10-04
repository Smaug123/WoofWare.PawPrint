using System;
using System.IO;
using System.Runtime.InteropServices;

// `symlink(2)`'s flavour-dependent facts, on a **Darwin**-configured kernel: what a
// trailing separator after an existing name costs, what a new link's mode is
// under the process's umask, and which group it gets.
//
// PawPrint-only, because a differential run would compare PawPrint's configured
// kernel against whichever kernel happened to run the oracle.
// sourcesPure/SymLinkSeeded.cs carries everything the two platforms agree about.
//
// This file and its twin for the other flavour exist as a **pair**: a handler
// that hardcoded either flavour's answers would pass one of them. The kernel is
// registered with a umask of 0o027 and a uid and gid of 1000, none of them
// KernelConfig's default, so a handler that reached for a constant instead of
// the process's umask or credentials fails here.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f ("hello"), lf -> f, dang -> nx, grp/ (0o777,
// group 4242), sg/ (0o2777, group 4242). "nx" deliberately does not exist.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SymLink", SetLastError = true)]
    static extern unsafe int SymLink(byte* target, byte* linkPath);

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

    // A new link is 0o777 less the umask 0o027; a seeded one was made by
    // another process at umask 0o022.
    const int NewLinkMode = 0x1E8;    // 0o750
    const int SeededLinkMode = 0x1ED; // 0o755
    // The directory's group, in a plain directory and a set-group-ID one alike.
    const uint GroupInPlain = 4242;
    const uint GroupInSetGid = 4242;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static byte[] Bytes(string s)
    {
        byte[] bytes = new byte[s.Length + 1];
        for (int i = 0; i < s.Length; i++) bytes[i] = (byte)s[i];
        bytes[s.Length] = 0;
        return bytes;
    }

    static unsafe int Link(string target, string linkPath)
    {
        byte[] t = Bytes(target);
        byte[] l = Bytes(linkPath);
        fixed (byte* tp = t)
        fixed (byte* lp = l)
            return SymLink(tp, lp);
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

    static int Main()
    {
        int check = 0;

        // ---- a trailing separator after an existing name ----

        // Darwin resolves a trailing separator as a lookup would: an existing
        // file is ENOTDIR, and a dangling link is followed to its free target,
        // which a trailing separator never creates.
        check++;
        if (Link("t", "f/") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOTDIR) return check;
        check++;
        if (Link("t", "dang/") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;
        check++;
        if (File.Exists("nx") || Directory.Exists("nx")) return check;

        // ---- a new link's mode and group ----

        check++;
        if (Link("f", "plain") != 0) return check;
        check++;
        if ((LStatOf("plain").Mode & 0xFFF) != NewLinkMode) return check;
        check++;
        if ((LStatOf("lf").Mode & 0xFFF) != SeededLinkMode) return check;

        check++;
        if (Link("t", "grp/l") != 0) return check;
        check++;
        if (LStatOf("grp/l").Gid != GroupInPlain) return check;
        check++;
        if (Link("t", "sg/l") != 0) return check;
        check++;
        if (LStatOf("sg/l").Gid != GroupInSetGid) return check;

        // The owner is the caller.
        check++;
        if (LStatOf("plain").Uid != 1000) return check;

        return 0;
    }
}
