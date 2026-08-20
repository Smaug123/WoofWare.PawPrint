using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_Open and SystemNative_FStat handlers directly via
// P/Invoke stubs, mirroring the shapes CoreLib's own [LibraryImport]s generate.
// The managed paths that reach them are covered by the sibling
// OpenMissingFile.cs (and, once the read path lands, by File.ReadAllBytes).
//
// This is a *pure* test, so it runs on the real CLR as well as under PawPrint,
// and every fact below is one both must agree on.
//
// **Errnos are compared as PAL values, not raw numbers.** A raw errno is only
// portable inside the 1-34 band that Linux and macOS number identically, and
// two of the interesting ones here are outside it: ELOOP is 40 on Linux and 62
// on macOS. SystemNative_ConvertErrorPlatformToPal is the shim's own
// normaliser, so routing through it makes the comparison portable -- and it is
// what CoreLib does with every errno it reads. (Marshal.GetLastSystemError
// rather than GetLastPInvokeError: the imports below declare SetLastError, so
// the two hold the same number, and the system slot is the one the syscall
// itself writes. sourcesPure/PInvokeSetLastError.cs pins that agreement.)
//
// **File descriptor numbers are deliberately not asserted.** PawPrint's first
// open is fd 3; the oracle's process already holds stdin/out/err plus whatever
// the runtime has open, so its number is unpredictable. Only ">= 0" and
// distinctness are cross-runtime; the lowest-free-descriptor rule is pinned
// PawPrint-side by sourcesImpure/OpenFdNumbering.cs.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f (5 bytes), d/ (a directory holding
// g), lf -> f, ld -> d. "nx" deliberately does not exist.
class Program
{
    // Must match Interop.Sys.FileStatus exactly: 17 sequential fields, 120
    // bytes. See StatFieldsSeeded.cs, which declares the same shape.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FStat", SetLastError = true)]
    static extern unsafe int FStat(IntPtr fd, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Sys.OpenFlags. A PAL enum, translated to the platform's own bits
    // by ConvertOpenFlags (pal_io.c:275), so these values are the same
    // everywhere and PawPrint has no platform question to answer here.
    const int O_RDONLY = 0x0000;
    const int O_CLOEXEC = 0x0010;
    const int O_NOFOLLOW = 0x0200;

    // Interop.Error, the PAL error enum.
    const int PAL_ENOENT = 0x1002D;
    const int PAL_EBADF = 0x10008;
    const int PAL_ELOOP = 0x10020;
    const int PAL_ENOTDIR = 0x10039;

    // Interop.Sys.FileTypes.
    const int S_IFMT = 0xF000;
    const int S_IFREG = 0x8000;
    const int S_IFDIR = 0x4000;

    // What CoreLib's own OpenReadOnly passes: DefaultCreateMode, 0666. It
    // passes this even for a read-only open of an existing file
    // (SafeFileHandle.Unix.cs:168), so a handler that refused a nonzero mode
    // without O_CREAT would break the BCL's own path. Passed here for exactly
    // that reason.
    const int DefaultCreateMode = 438;

    static unsafe void Ascii(string s, byte* dest)
    {
        for (int i = 0; i < s.Length; i++)
        {
            dest[i] = (byte)s[i];
        }

        dest[s.Length] = 0;
    }

    static unsafe IntPtr OpenPath(string path, int flags)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        Marshal.SetLastSystemError(0);
        return Open(buf, flags, DefaultCreateMode);
    }

    static int LastPalError() => ConvertErrorPlatformToPal (Marshal.GetLastSystemError ());

    static unsafe int Main(string[] args)
    {
        int check = 0;
        FileStatus st;

        // A plain read-only open of a regular file.
        check = 1;
        IntPtr f = OpenPath("f", O_RDONLY | O_CLOEXEC);
        if ((long)f < 0) return check;

        check = 2;
        if (FStat(f, &st) != 0) return check;
        check = 3;
        if ((st.Mode & S_IFMT) != S_IFREG) return check;
        check = 4;
        if (st.Size != 5) return check;

        // Two opens of the same path are two descriptors, and neither is a
        // standard stream.
        check = 5;
        IntPtr f2 = OpenPath("f", O_RDONLY);
        if ((long)f2 < 0) return check;
        check = 6;
        if (f2 == f) return check;
        check = 7;
        if ((long)f < 3 || (long)f2 < 3) return check;

        check = 8;
        if (Close(f2) != 0) return check;
        check = 9;
        if (Close(f) != 0) return check;

        // A closed descriptor is not a descriptor.
        check = 10;
        Marshal.SetLastSystemError(0);
        if (FStat(f, &st) != -1) return check;
        check = 11;
        if (LastPalError() != PAL_EBADF) return check;

        // ...and neither is one that was never live. Asserted separately
        // because the check above has a theoretical race on the real runtime:
        // a background thread could reclaim the descriptor we just freed.
        check = 12;
        Marshal.SetLastSystemError(0);
        if (FStat((IntPtr)12345, &st) != -1) return check;
        check = 13;
        if (LastPalError() != PAL_EBADF) return check;

        check = 14;
        Marshal.SetLastSystemError(0);
        if (Close((IntPtr)12345) != -1) return check;
        check = 15;
        if (LastPalError() != PAL_EBADF) return check;

        check = 16;
        if ((long)OpenPath("nx", O_RDONLY) >= 0) return check;
        check = 17;
        if (LastPalError() != PAL_ENOENT) return check;

        // A path that runs *through* a regular file.
        check = 18;
        if ((long)OpenPath("f/g", O_RDONLY) >= 0) return check;
        check = 19;
        if (LastPalError() != PAL_ENOTDIR) return check;

        // A directory opens successfully for reading. CoreLib depends on this:
        // SafeFileHandle.Init opens, then FStats, and raises
        // UnauthorizedAccessException on seeing S_IFDIR -- so refusing at open
        // would produce the wrong exception from File.ReadAllBytes("d").
        check = 20;
        IntPtr d = OpenPath("d", O_RDONLY);
        if ((long)d < 0) return check;
        check = 21;
        if (FStat(d, &st) != 0) return check;
        check = 22;
        if ((st.Mode & S_IFMT) != S_IFDIR) return check;
        check = 23;
        if (Close(d) != 0) return check;

        // Open follows a final symlink, unlike lstat: what arrives is the
        // target's inode, so the type is the target's too.
        check = 24;
        IntPtr lf = OpenPath("lf", O_RDONLY);
        if ((long)lf < 0) return check;
        check = 25;
        if (FStat(lf, &st) != 0) return check;
        check = 26;
        if ((st.Mode & S_IFMT) != S_IFREG || st.Size != 5) return check;
        check = 27;
        if (Close(lf) != 0) return check;

        // ...unless O_NOFOLLOW, which is what CoreLib's OpenNoFollowSymlink
        // uses to refuse a symlink without a race.
        check = 28;
        if ((long)OpenPath("lf", O_RDONLY | O_NOFOLLOW) >= 0) return check;
        check = 29;
        if (LastPalError() != PAL_ELOOP) return check;

        // A symlink to a directory still resolves to the directory.
        check = 30;
        IntPtr ld = OpenPath("ld", O_RDONLY);
        if ((long)ld < 0) return check;
        check = 31;
        if (FStat(ld, &st) != 0) return check;
        check = 32;
        if ((st.Mode & S_IFMT) != S_IFDIR) return check;
        check = 33;
        if (Close(ld) != 0) return check;

        // The same file through two paths is one inode: st_ino is what the BCL
        // compares to decide two paths name the same file, so a handler that
        // invented a per-descriptor identity would be caught here.
        check = 34;
        IntPtr viaName = OpenPath("f", O_RDONLY);
        IntPtr viaLink = OpenPath("lf", O_RDONLY);
        FileStatus st2;
        if (FStat(viaName, &st) != 0 || FStat(viaLink, &st2) != 0) return check;
        check = 35;
        if (st.Ino != st2.Ino) return check;
        check = 36;
        if (Close(viaName) != 0 || Close(viaLink) != 0) return check;

        return 0;
    }
}
