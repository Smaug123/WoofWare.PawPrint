using System;
using System.IO;
using System.Runtime.InteropServices;

// The one thing `mkdir`'s permission rule does that an unprivileged guest cannot
// show: root binds a name into a directory whose mode forbids it.
//
// PawPrint-only, and it has to be. The rule is uid-dependent, and this suite does
// not choose the uid its differential oracle runs as; PawPrint's is
// `KernelConfig.UserId`, set to 0 here. Every *other* guest sets it to 1000 and
// watches the same call fail, so a handler that passed a constant to
// `MkDirRules.verdict` fails one of the two whichever constant it picked.
//
// Measured on Linux as uid 0: `mkdir` into a 0o555 directory succeeds. The
// refusals that sit above the permission check do not care about privilege, so
// an existing name is still EEXIST here.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): nowrite/ (0o555, holding kid/), nosearch/ (0o666,
// holding kid/), d/ (a directory).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EEXIST = 0x10014;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe int MkDirPath(string name, int mode)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        fixed (byte* p = bytes) return MkDir(p, mode);
    }

    const int Mode777 = 0x1FF;

    static int Main()
    {
        int check = 0;

        // Root bypasses the write-and-search rule the other guests watch refuse.
        check++;
        if (MkDirPath("nowrite/new", Mode777) != 0) return check;
        check++;
        if (!Directory.Exists("nowrite/new")) return check;

        // Root searches a directory it could not otherwise look into.
        check++;
        if (MkDirPath("nosearch/fresh", Mode777) != 0) return check;
        check++;
        if (!Directory.Exists("nosearch/fresh")) return check;

        // ...but nothing above it. An existing name is EEXIST for root too.
        check++;
        if (MkDirPath("d", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_EEXIST) return check;

        check++;
        if (MkDirPath("nowrite/new", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_EEXIST) return check;

        return 0;
    }
}
