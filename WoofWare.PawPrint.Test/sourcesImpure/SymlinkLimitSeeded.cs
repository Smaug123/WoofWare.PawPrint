using System;
using System.Runtime.InteropServices;
using System.Text;

// Resolves symlink chains of three lengths through a hand-rolled
// SystemNative_Stat, to pin that the emulated kernel's MAXSYMLINKS is the one
// its own SimulatedUnixPlatform states.
//
// PawPrint-only, and necessarily so: the whole subject is a chain of 33 links,
// which is exactly the length Linux resolves and macOS refuses. A differential
// test would therefore assert a fact that is *not* true across runtimes — it
// would pass on a Linux oracle and fail on a macOS one — which is the one thing
// a differential test may never do. The seed's shortest and longest chains are
// cross-runtime facts, but they resolved correctly before this behaviour
// existed, so they are controls rather than evidence.
//
// The BCL cannot see any of this, which is why the P/Invoke is hand-rolled:
// File.Exists answers true for a looping symlink exactly as it does for a
// dangling one, because FileSystem.Exists.Unix.cs returns true when the
// follow-up Stat fails. Only the raw return value distinguishes them.
//
// Errors are read with Marshal.GetLastSystemError, for the reason
// StatFieldsSeeded.cs sets out.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): a1 -> a2 -> ... -> a32 -> atarget (32 traversals,
// which every Unix permits); b1 -> ... -> b33 -> btarget (33, which Linux
// permits and macOS refuses); c1 -> ... -> c41 -> ctarget (41, which no Unix
// permits). The kernel under test is the default, LinuxX64.
class Program
{
    // Must match Interop.Sys.FileStatus exactly: 17 sequential fields, 120
    // bytes. Only the return value is read here, but the buffer must still be
    // big enough for the handler to write into.
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

    // ELOOP's raw number under the platform the kernel is configured to be.
    // Linux, because that is KernelConfig's default; on Darwin this would be 62,
    // and that difference is the entire reason UnixError refuses to answer
    // without being told which Unix it is.
    const int ELOOP_LINUX = 40;

    static int check = 0;

    static bool Is(bool condition)
    {
        check++;
        return condition;
    }

    static unsafe int StatOf(string path)
    {
        // A local, so it is already fixed; only the array needs pinning.
        FileStatus status;
        byte[] bytes = Encoding.UTF8.GetBytes(path + "\0");
        fixed (byte* p = bytes)
        {
            return Stat(p, &status);
        }
    }

    static int Main(string[] args)
    {
        // ---- 32 traversals: below every platform's limit ----
        if (!Is(StatOf("/a1") == 0)) return check;

        // ---- 33 traversals: the band the two platforms disagree about ----
        // Linux resolves this and macOS does not. Before the resolver was told
        // which Unix it was, this aborted the interpreter rather than answering.
        if (!Is(StatOf("/b1") == 0)) return check;

        // ---- 41 traversals: beyond every platform's limit ----
        if (!Is(StatOf("/c1") == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ELOOP_LINUX)) return check;

        // The chains' targets are ordinary files, so a failure above is about
        // the traversal count and not about what the chain ends at.
        if (!Is(StatOf("/atarget") == 0)) return check;
        if (!Is(StatOf("/btarget") == 0)) return check;
        if (!Is(StatOf("/ctarget") == 0)) return check;

        return 0;
    }
}
