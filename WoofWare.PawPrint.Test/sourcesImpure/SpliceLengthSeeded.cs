using System;
using System.Runtime.InteropServices;
using System.Text;

// Drives SystemNative_Stat through symbolic links whose targets are long
// enough that expanding them overflows PATH_MAX, to pin end to end that the
// emulated kernel applies Darwin's splice re-check and reports ENAMETOOLONG.
//
// The kernel this runs against is configured as **macOS**, which is the whole
// point: Linux performs no such check at any length, so on the default kernel
// every path here would simply resolve. That also means the raw errno is the
// Darwin one (63), not the Linux one (36) that PathLengthLimitsSeeded.cs reads.
//
// PawPrint-only, and not a cross-runtime fact: it is a claim about the kernel
// PawPrint is configured to be. The equivalent claim about a *real* Darwin
// kernel is checked separately, by the host oracle in
// TestVirtualFileSystemAgainstHost.fs, which bisects the boundary on whatever
// kernel it is running on.
//
// Why this exists when unit tests already cover the rule: those call the
// resolver directly, so a `resolveGuestPath` that passed hardcoded limits
// instead of the configured platform's would satisfy every one of them. Only a
// guest can see that the limits reach the syscall boundary at all.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    // Darwin numbering, per the comment above.
    const int ENOENT = 2;
    const int ENAMETOOLONG_DARWIN = 63;

    static int check = 0;

    static bool Is(bool condition)
    {
        check++;
        return condition;
    }

    static unsafe int StatOf(string path)
    {
        FileStatus status;
        byte[] bytes = Encoding.UTF8.GetBytes(path + "\0");
        fixed (byte* p = bytes)
        {
            return Stat(p, &status);
        }
    }

    static int Main(string[] args)
    {
        // The seed holds "atMax" -> a 1021-byte dangling target and "overMax" ->
        // a 1022-byte one. Through the remainder "/a" the spliced buffer is the
        // target, one separator, one byte and the NUL: 1021 + 2 + 1 = 1024 fits
        // exactly, and 1022 + 2 + 1 = 1025 does not.

        // Just inside: the target does not exist, so this is an ordinary ENOENT
        // and *not* a length failure. Without this control, "long paths fail"
        // would pass against a kernel that refused everything.
        if (!Is(StatOf("/atMax/a") == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENOENT)) return check;

        // One byte over.
        if (!Is(StatOf("/overMax/a") == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENAMETOOLONG_DARWIN)) return check;

        // With nothing left to resolve the budget is PATH_MAX - 1, so the same
        // 1022-byte target that overflows above is comfortably fine here. This
        // is what distinguishes the rule from "a long target is refused".
        if (!Is(StatOf("/overMax") == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENOENT)) return check;

        // The remainder's length is what moved, so a longer one refuses a
        // target that a shorter one accepts: "/ab" is one byte more than "/a".
        if (!Is(StatOf("/atMax/ab") == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENAMETOOLONG_DARWIN)) return check;

        return 0;
    }
}
