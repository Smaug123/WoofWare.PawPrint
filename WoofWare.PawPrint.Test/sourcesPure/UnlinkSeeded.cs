using System;
using System.Runtime.InteropServices;

// `unlink(2)` through the raw shim, in the rows Linux and macOS answer
// identically. This is a *pure* test, so it runs on the real CLR as well as
// under PawPrint, and every fact below is one both must agree on.
//
// The rows they do *not* agree on are all about *which* errno a refusal
// carries — a directory is EISDIR on Linux and EPERM on Darwin, a trailing
// separator over a symlink is ENOTDIR against whatever the link resolves to,
// and the root reached by "." is EISDIR against EBUSY. Those live in
// sourcesImpure/UnlinkWiring{Linux,Darwin}Seeded.cs, one per configured
// flavour, and in TestUnlinkRules. What survives here is the *shape* of the
// refusal: -1, and the object still there afterwards.
//
// **Errnos are compared as PAL values, not raw numbers**, exactly as
// SystemNativeOpen.cs does: a raw errno is portable only inside the band Linux
// and macOS number identically.
//
// Everything is hand-rolled `DllImport`. `File.Delete` swallows every failure
// it can (a missing file is not an error there), and the failures it does not
// swallow are built through `SystemNative_StrErrorR`, which PawPrint does not
// implement — so a managed row that threw would abort the run rather than fail
// it.
//
// No permission row here, and deliberately: a run as root would answer
// differently on the two sides, since PawPrint's uid is `KernelConfig`'s 1000
// whatever the host's is. Those rows are in the unit tier, on both flavours.
//
// This guest *deletes* things. On the oracle side that is the per-run scratch
// directory `RealRuntime.executeWithTimeoutAndSeed` creates, so every path
// below must be a relative name the seed put there — never an absolute path,
// never "..", and never the guest image or its runtimeconfig.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f (5 bytes), g (3 bytes),
// held ("payload"), f2 (a file), d/ (a directory holding g), lg -> g,
// dang -> nx. "nx" deliberately does not exist.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Unlink", SetLastError = true)]
    static extern unsafe int Unlink(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Error, the PAL error enum.
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;

    // Interop.Sys.OpenFlags, and the mode CoreLib's own OpenReadOnly passes.
    const int O_RDONLY = 0x0000;
    const int DefaultCreateMode = 438;

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

    static unsafe int Main()
    {
        int check = 0;

        // ---- removing a name ----

        check++;
        if (UnlinkPath("f") != 0) return check;
        check++;
        if (StatPath("f") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // Removing it again is an ordinary missing-name failure, not a
        // double-free of any kind.
        check++;
        if (UnlinkPath("f") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        check++;
        if (UnlinkPath("nx") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // ---- a symlink goes, and its target does not ----

        // The whole content of `SymlinkPolicy.NoFollowFinal` here: an
        // implementation that followed the link would leave a dangling `lg`
        // and destroy `g`.
        check++;
        if (UnlinkPath("lg") != 0) return check;
        check++;
        if (LStatPath("lg") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;
        check++;
        if (StatPath("g") != 0) return check;

        // A link whose target never existed goes just as readily; nothing
        // resolves it.
        check++;
        if (UnlinkPath("dang") != 0) return check;
        check++;
        if (LStatPath("dang") != -1) return check;

        // ---- a trailing separator over a file ----

        // ENOTDIR on both, by different routes: Darwin's walk enforces the
        // directory demand, while Linux's ignores it and the verdict enforces
        // it. What matters here is that neither removes anything.
        check++;
        if (UnlinkPath("f2/") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOTDIR) return check;
        check++;
        if (StatPath("f2") != 0) return check;

        // ---- a directory is refused, whatever the errno ----

        // EISDIR on Linux and EPERM on Darwin, so only the refusal and the
        // survival are asserted. `rmdir(2)` is what removes a directory, and
        // PawPrint does not implement it yet.
        check++;
        if (UnlinkPath("d") != -1) return check;
        check++;
        if (StatPath("d") != 0) return check;

        check++;
        if (UnlinkPath("d/") != -1) return check;
        check++;
        if (StatPath("d") != 0) return check;

        // The current directory reached by name is refused too, and again the
        // errno diverges (EISDIR against EPERM or EBUSY, depending on whether
        // the process sits on a mount root).
        check++;
        if (UnlinkPath(".") != -1) return check;

        // ---- a name inside a subdirectory ----

        check++;
        if (UnlinkPath("d/g") != 0) return check;
        check++;
        if (StatPath("d/g") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // The holding directory is still there: only the entry went.
        check++;
        if (StatPath("d") != 0) return check;

        // ---- an open descriptor keeps the inode alive ----

        // The reason removing the last name cannot free the inode. Measured on
        // both kernels: after `unlink`, the name is gone while the descriptor
        // still reads the bytes.
        byte* path = stackalloc byte[256];
        Ascii("held", path);
        IntPtr fd = Open(path, O_RDONLY, DefaultCreateMode);

        check++;
        if (fd == new IntPtr(-1)) return check;

        check++;
        if (UnlinkPath("held") != 0) return check;
        check++;
        if (StatPath("held") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        byte* buffer = stackalloc byte[16];
        int read = Read(fd, buffer, 16);

        check++;
        if (read != 7) return check;

        string expected = "payload";
        for (int i = 0; i < expected.Length; i++)
        {
            check++;
            if (buffer[i] != (byte)expected[i]) return check;
        }

        check++;
        if (Close(fd) != 0) return check;

        return 0;
    }
}
