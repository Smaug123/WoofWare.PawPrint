using System;
using System.Runtime.InteropServices;
using System.Text;

// Reads the effective user id through SystemNative_GetEUid and writes it to
// stdout as four little-endian bytes, so that the F# registration (which
// configured it in the first place) can assert the exact value. That keeps a
// single source of truth per case and lets one guest source cover every
// identity TestImpureCases registers it under.
//
// The value deliberately does *not* travel in the exit code, which is only
// eight bits: a uid is a uint32, and real systems use large ones (4294967294
// is `nobody` on Linux). A handler that truncated to 16 bits, or that confused
// signed and unsigned above 2^31, would be invisible to any test whose uids all
// fit in a byte. The exit code is left free to carry sentinels instead.
//
// PawPrint-only: on a real runtime the answer is whoever ran the suite, which
// is exactly the host dependence the emulated kernel exists to remove. There is
// no oracle here.
//
// Two things are checked in-guest before the write, each with its own sentinel:
//
//  1. Environment.IsPrivilegedProcess is literally GetEUid() == 0
//     (Environment.Unix.cs:29). This is why EmulatedKernel's default uid is
//     1000 rather than 0 — a guest booted as root silently takes the privileged
//     branch of every check it makes about itself — so the root registration is
//     what makes that comment load-bearing. Under PawPrint today both ends of
//     the comparison reach this same handler, so it is close to a tautology;
//     what it guards against is a future intrinsic arm answering
//     IsPrivilegedProcess without consulting the kernel at all.
//
//  2. stat("/f") succeeds at all, so that check 3 below is never satisfied by
//     an untouched output buffer.
//
//  3. stat reports this same identity as st_uid. That is not decoration: it is
//     the premise on which SystemNative_GetEGid and SystemNative_GetGroups are
//     deliberately *not* implemented. Within CoreLib, IsMemberOfGroup is
//     reachable only from FileStatus.IsModeReadOnlyCore, behind
//     `if (_fileCache.Uid == Interop.Sys.GetEUid())` (FileStatus.Unix.cs:106),
//     so one process-wide identity makes that branch dead by construction. If a
//     later slice gives an inode an owner of its own, this fails and says so,
//     rather than the group path quietly becoming reachable and aborting the
//     interpreter somewhere unrelated.
class Program
{
    // Must match Interop.Sys.FileStatus exactly: 17 sequential fields, 120
    // bytes, because the handler refuses a buffer of any other size. Only Uid
    // is read here; see StatFieldsSeeded.cs, which reads every field.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetEUid")]
    static extern uint GetEUid();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    static unsafe int LStatOf(string path, out FileStatus status)
    {
        byte[] bytes = Encoding.UTF8.GetBytes(path + "\0");
        fixed (byte* p = bytes)
        {
            fixed (FileStatus* s = &status) { return LStat(p, s); }
        }
    }

    static unsafe int Main(string[] args)
    {
        uint euid = GetEUid();

        if (Environment.IsPrivilegedProcess != (euid == 0)) return 1;

        FileStatus st;
        if (LStatOf("/f", out st) != 0) return 2;
        if (st.Uid != euid) return 3;

        // Shifted out by hand rather than via BitConverter, so that the byte
        // order is stated here and matched by an equally explicit expectation
        // on the F# side, instead of both sides silently agreeing to be
        // little-endian because the host happens to be.
        byte[] observed = new byte[]
        {
            (byte)(euid & 0xFF),
            (byte)((euid >> 8) & 0xFF),
            (byte)((euid >> 16) & 0xFF),
            (byte)((euid >> 24) & 0xFF),
        };

        fixed (byte* p = observed)
        {
            if (Write((IntPtr)1, p, observed.Length) != observed.Length) return 4;
        }

        return 0;
    }
}
