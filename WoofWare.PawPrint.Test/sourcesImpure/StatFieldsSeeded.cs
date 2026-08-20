using System;
using System.Runtime.InteropServices;
using System.Text;

// Reads every field SystemNative_LStat and SystemNative_Stat write, through a
// hand-rolled P/Invoke rather than through the BCL.
//
// PawPrint-only, and not merely by convention: a real file's uid is whoever ran
// the test, and its timestamps are "just now", neither of which a simulated
// filesystem can agree with. So there is no oracle here and the expectations
// are PawPrint's own contract.
//
// Going through the BCL instead is not an option today. FileInfo.Length reaches
// FileStatus.HasReadOnlyFlag, which calls SystemNative_GetEUid — unimplemented,
// so it crashes before returning a length. Declaring the struct here also
// exercises the deliberate looseness of the handler's parameter match: the real
// Interop.Sys.FileStatus is internal to CoreLib, so a guest can only reach this
// entry point with a layout-identical struct of its own.
//
// Errors are read with Marshal.GetLastSystemError rather than the
// GetLastPInvokeError that CoreLib itself uses (Interop.Errors.cs:163). The two
// hold the same number here -- the imports declare SetLastError, and the stub
// that copies one slot into the other is modelled -- but the system slot is the
// one the syscall itself writes, so it is what these rows are about. The CoreLib
// path is exercised by FileExistsSeeded.cs, where a failing LStat is what makes
// File.Exists answer false.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f = "hello" (5 bytes), d/ a directory,
// lf -> f, dang -> nx.
class Program
{
    // Must match Interop.Sys.FileStatus exactly: 17 sequential fields, 120
    // bytes. This is the layout PawPrint derives its writes from, so a
    // disagreement here would show up as garbage rather than as a type error.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    const int S_IFMT = 0xF000;
    const int S_IFREG = 0x8000;
    const int S_IFDIR = 0x4000;
    const int S_IFLNK = 0xA000;

    static int check = 0;

    static bool Is(bool condition)
    {
        check++;
        return condition;
    }

    static unsafe int LStatOf(string path, out FileStatus status)
    {
        byte[] bytes = Encoding.UTF8.GetBytes(path + "\0");
        fixed (byte* p = bytes)
        {
            fixed (FileStatus* s = &status) { return LStat(p, s); }
        }
    }

    static unsafe int StatOf(string path, out FileStatus status)
    {
        byte[] bytes = Encoding.UTF8.GetBytes(path + "\0");
        fixed (byte* p = bytes)
        {
            fixed (FileStatus* s = &status) { return Stat(p, s); }
        }
    }

    static int Main(string[] args)
    {
        FileStatus st;

        // ---- a regular file ----
        if (!Is(LStatOf("/f", out st) == 0)) return check;
        if (!Is((st.Mode & S_IFMT) == S_IFREG)) return check;
        // 0o644: what a umask-022 process creates.
        if (!Is((st.Mode & 0xFFF) == 0x1A4)) return check;
        if (!Is(st.Size == 5)) return check;
        // Configured as 1000/2000, deliberately unequal so that swapping the
        // two fields would be visible.
        if (!Is(st.Uid == 1000)) return check;
        if (!Is(st.Gid == 2000)) return check;
        if (!Is(st.Dev != 0)) return check;
        if (!Is(st.RDev == 0)) return check;
        if (!Is(st.Ino != 0)) return check;
        if (!Is(st.UserFlags == 0)) return check;

        // The whole filesystem is created at the instant the process boots,
        // which the test configures as 1_700_000_123 ms since the epoch — so
        // 1_700_000 seconds and 123_000_000 nanoseconds. A non-round value on
        // purpose: a zero here would be indistinguishable from a field nobody
        // wrote, and a whole number of seconds would let the nanosecond split
        // be skipped entirely.
        if (!Is(st.ATime == 1700000 && st.ATimeNsec == 123000000)) return check;
        if (!Is(st.MTime == 1700000 && st.MTimeNsec == 123000000)) return check;
        if (!Is(st.CTime == 1700000 && st.CTimeNsec == 123000000)) return check;

        // The default platform is Linux, whose stat reports no creation time:
        // pal_io.c zeroes BirthTime and leaves HAS_BIRTHTIME clear. Note the
        // inode *has* a birth time — the same instant as the rest — so a zero
        // here is the platform declining to report it, not an absent fact.
        if (!Is(st.Flags == 0)) return check;
        if (!Is(st.BirthTime == 0 && st.BirthTimeNsec == 0)) return check;

        long fileIno = st.Ino;
        long fileDev = st.Dev;

        // ---- a directory ----
        if (!Is(LStatOf("/d", out st) == 0)) return check;
        if (!Is((st.Mode & S_IFMT) == S_IFDIR)) return check;
        // 0o755.
        if (!Is((st.Mode & 0xFFF) == 0x1ED)) return check;
        if (!Is(st.Size == 4096)) return check;
        // One device for the whole tree, and a distinct inode per file.
        if (!Is(st.Dev == fileDev)) return check;
        if (!Is(st.Ino != fileIno)) return check;

        // ---- a symlink, not followed ----
        if (!Is(LStatOf("/lf", out st) == 0)) return check;
        if (!Is((st.Mode & S_IFMT) == S_IFLNK)) return check;
        // Linux reports 0o777 for every symlink, whatever the umask.
        if (!Is((st.Mode & 0xFFF) == 0x1FF)) return check;
        // A symlink's size is its target's byte length: "f" is one byte.
        if (!Is(st.Size == 1)) return check;

        // ---- the same symlink, followed ----
        if (!Is(StatOf("/lf", out st) == 0)) return check;
        if (!Is((st.Mode & S_IFMT) == S_IFREG)) return check;
        if (!Is(st.Size == 5)) return check;
        if (!Is(st.Ino == fileIno)) return check;

        // ---- errors ----
        // A dangling link: lstat sees the link, stat sees nothing.
        if (!Is(LStatOf("/dang", out st) == 0)) return check;
        if (!Is((st.Mode & S_IFMT) == S_IFLNK)) return check;
        if (!Is(StatOf("/dang", out st) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == 2)) return check; // ENOENT

        if (!Is(LStatOf("/nx", out st) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == 2)) return check;

        // A path cannot continue through a regular file.
        if (!Is(LStatOf("/f/x", out st) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == 20)) return check; // ENOTDIR

        // The empty path is ENOENT on every Unix.
        if (!Is(LStatOf("", out st) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == 2)) return check;

        return 0;
    }
}
