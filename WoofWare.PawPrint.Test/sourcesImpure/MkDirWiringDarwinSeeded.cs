using System;
using System.IO;
using System.Runtime.InteropServices;

// `mkdir(2)`'s three flavour-dependent facts, on a **Darwin**-configured kernel:
// what a trailing separator on the final component costs, which mode bits
// survive, and whether a new directory inherits set-group-ID from its parent.
//
// PawPrint-only, because a differential run would compare PawPrint's configured
// kernel against whichever kernel happened to run the oracle, and because the
// mode rows need a umask and a uid this suite chooses rather than inherits.
// sourcesPure/MkDirSeeded.cs carries everything the two platforms agree about.
//
// This file and its Linux twin exist as a **pair**, and neither alone is
// enough: the unit tests hand `MkDirRules` in explicitly and the host oracle
// compares against whichever kernel it is running on, so a handler that ignored
// `SimulatedUnixPlatform.mkDirRules` and hardcoded either answer would satisfy
// every one of them plus one of these two guests.
//
// The kernel is registered with a umask of 0o027 and a uid of 1000 — neither of
// them `KernelConfig`'s default — so a handler that reached for a constant
// instead of `Kernel.Umask`, or that assumed privilege, fails here and nowhere
// else.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f (5 bytes), d/ (a directory), lf -> f, ld -> d,
// dang -> nx, cyc -> cyc, sg/ (a directory at 0o2777), nowrite/ (0o555, holding
// kid/), nosearch/ (0o666, holding kid/).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EACCES = 0x10002;
    const int PAL_EEXIST = 0x10014;
    const int PAL_ELOOP = 0x10020;
    const int PAL_ENOTDIR = 0x10039;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe int MkDirPath(string name, int mode)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        fixed (byte* p = bytes) return MkDir(p, mode);
    }

    const int Mode777 = 0x1FF;   // 0o0777
    const int Mode7777 = 0xFFF;  // 0o7777

    // What a Darwin kernel leaves behind, under the umask 0o027 this guest's
    // kernel is configured with. XNU masks with ACCESSPERMS, so all three upper
    // bits go and there is nothing for a set-group-ID parent to add.
    const UnixFileMode Plain777 = UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
        | UnixFileMode.GroupRead | UnixFileMode.GroupExecute;                                     // 0o0750
    const UnixFileMode Plain7777 = Plain777;                                                      // 0o0750
    const UnixFileMode Inherited777 = Plain777;                                                   // 0o0750
    const UnixFileMode Inherited7777 = Plain777;                                                  // 0o0750

    static int Main()
    {
        int check = 0;

        // ---- a trailing separator on the final component ----

        // Darwin resolves the last component as any lookup would, so the
        // trailing separator reaches *past* the name and each row fails for its
        // own reason. Linux answers EEXIST to all five.
        check++;
        if (MkDirPath("f/", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOTDIR) return check;

        check++;
        if (MkDirPath("lf/", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOTDIR) return check;

        // The link is followed to the directory it names, which does exist.
        check++;
        if (MkDirPath("ld/", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_EEXIST) return check;

        check++;
        if (MkDirPath("cyc/", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_ELOOP) return check;

        // The destructive row: the dangling link is followed and its *target* is
        // created. "dang" is still a symbolic link afterwards, and "nx" — which
        // the path never mentions — is now a directory.
        check++;
        if (Directory.Exists("nx")) return check;
        check++;
        if (MkDirPath("dang/", Mode777) != 0) return check;
        check++;
        if (!Directory.Exists("nx")) return check;
        check++;
        if (File.GetUnixFileMode("nx") != Plain777) return check;

        // ---- the mode mask ----

        // 0o777 under umask 0o027. A handler reaching for the 0o022 default
        // instead of `Kernel.Umask` gets 0o755 here.
        check++;
        if (MkDirPath("m777", Mode777) != 0) return check;
        check++;
        if (File.GetUnixFileMode("m777") != Plain777) return check;

        // Darwin drops the sticky bit as well as both set-ID bits, so 0o7777
        // is indistinguishable from 0o777 here — which is exactly what the Linux
        // twin's 0o1750 says it is not.
        check++;
        if (MkDirPath("m7777", Mode7777) != 0) return check;
        check++;
        if (File.GetUnixFileMode("m7777") != Plain7777) return check;

        // ---- set-group-ID inheritance ----

        // The seeded parent carries S_ISGID and Darwin confers nothing: both
        // children come out exactly as they would anywhere else.
        check++;
        if (MkDirPath("sg/child777", Mode777) != 0) return check;
        check++;
        if (File.GetUnixFileMode("sg/child777") != Inherited777) return check;

        check++;
        if (MkDirPath("sg/child7777", Mode7777) != 0) return check;
        check++;
        if (File.GetUnixFileMode("sg/child7777") != Inherited7777) return check;

        // ...and a mode that asks for the bit outright does not get it either.
        check++;
        if (MkDirPath("plain2777", 0x5FF /* 0o2777 */) != 0) return check;
        check++;
        if (File.GetUnixFileMode("plain2777") != Plain777) return check;

        // ---- the uid ----

        // Binding a name needs write *and* search on the holding directory, and
        // this kernel's uid is 1000, so the 0o555 seed refuses. Unanimous with
        // Linux, and here because the uid is this suite's choice.
        check++;
        if (MkDirPath("nowrite/new", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_EACCES) return check;

        // The two bits are checked either side of EEXIST, which is the only
        // thing that tells them apart. An *existing* child of that same 0o555
        // directory is EEXIST rather than the EACCES its free sibling above
        // got...
        check++;
        if (MkDirPath("nowrite/kid", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_EEXIST) return check;

        // ...while an existing child of one that cannot be *searched* is EACCES,
        // because the lookup that would have found it never happens.
        check++;
        if (MkDirPath("nosearch/kid", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_EACCES) return check;

        return 0;
    }
}
