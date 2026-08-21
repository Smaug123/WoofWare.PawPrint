using System;
using System.IO;
using System.Runtime.InteropServices;

// The directory search bit, end to end, at an unprivileged uid.
//
// PawPrint-only, and it has to be: the rule is uid-dependent, and this suite
// does not choose the uid its differential oracle runs as, while PawPrint's is
// `KernelConfig.UserId`. Its twin `SearchPermissionRootSeeded.cs` runs the same
// checks at uid 0 and expects the opposite of every one of them, which is what
// makes the handler's privilege argument falsifiable in both directions: a
// hardcoded `Unprivileged` passes this file and fails that one, and a hardcoded
// `Privileged` does the reverse.
//
// The unit tests hand the walk its privilege directly, so only a guest can see
// that `resolveGuestPathFull` reads `Kernel.UserId` at all.
//
// Errnos come from the raw shim rather than from a caught exception: building
// the BCL's IOException needs `SystemNative_StrErrorR`, which does not exist, so
// a managed row that throws would abort the run rather than fail it.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): ns/ (0o666, holding kid/ and f), open/ (0o755,
// holding kid/), lns -> ns.
class Program
{
    // Must match `Interop.Sys.FileStatus` exactly: 17 sequential fields, 120
    // bytes. See sourcesPure/SystemNativeOpen.cs, which declares the same shape.
    // Never read here — only the return code matters — but the handler checks
    // the declared layout, as it must.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EACCES = 0x10002;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe int StatPath(string name)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        FileStatus status;
        fixed (byte* p = bytes) return Stat(p, &status);
    }

    static int Main()
    {
        int check = 0;

        // Every path *through* the unsearchable directory is refused, whatever
        // the component turns out to be — present, absent, or reached by
        // splicing a symlink target.
        foreach (string blocked in new[] { "ns/kid", "ns/f", "ns/nx", "ns/.", "ns/..", "lns/kid" })
        {
            check++;
            if (StatPath(blocked) != -1) return check;
            check++;
            if (LastPalError() != PAL_EACCES) return check;
        }

        // The directory itself resolves: nothing looks inside it. So does a
        // trailing separator, which is not the same as "/." — the row above
        // covers that one.
        check++;
        if (StatPath("ns") != 0) return check;
        check++;
        if (StatPath("ns/") != 0) return check;

        // A searchable directory is unaffected, which is what says the refusals
        // above are about the mode rather than about the handler being broken.
        check++;
        if (StatPath("open/kid") != 0) return check;

        return 0;
    }
}
