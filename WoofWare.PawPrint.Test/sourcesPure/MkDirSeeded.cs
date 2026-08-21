using System;
using System.IO;
using System.Runtime.InteropServices;

// `mkdir(2)` through both the BCL and the raw shim, in the rows Linux and macOS
// answer identically. This is a *pure* test, so it runs on the real CLR as well
// as under PawPrint, and every fact below is one both must agree on.
//
// The rows they do *not* agree on — everything with a trailing separator on the
// final component, the mode mask, and set-group-ID inheritance — are in
// sourcesImpure/MkDirWiring{Linux,Darwin}Seeded.cs, one per configured flavour.
// No mode is asserted here at all: PawPrint's default umask happens to equal the
// 0o022 the devshell pins for the oracle, so a mode row would pass by
// coincidence rather than by agreement.
//
// **Errnos are compared as PAL values, not raw numbers**, exactly as
// SystemNativeOpen.cs does: a raw errno is portable only inside the band Linux
// and macOS number identically, and ENAMETOOLONG (36 against 63) is outside it.
//
// The BCL rows are creations only. `Directory.CreateDirectory`'s failure paths
// build an exception through `SystemNative_StrErrorR`, which PawPrint does not
// implement, so a managed row that throws aborts the run rather than failing it.
// The raw shim reports the same errnos without constructing anything.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f (5 bytes), d/ (a directory holding g),
// lf -> f, ld -> d, dang -> nx, cyc -> cyc. "nx" deliberately does not exist.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EEXIST = 0x10014;
    const int PAL_ENAMETOOLONG = 0x10025;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe int MkDirPath(string name, int mode)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        fixed (byte* p = bytes) return MkDir(p, mode);
    }

    /// 0o777, which is what `Directory.CreateDirectory` passes.
    const int Mode777 = 0x1FF;

    static int Main()
    {
        int check = 0;

        // ---- the raw shim, in the rows both kernels agree on ----

        // A free name is created, and only that name: creating "one" must not
        // have made anything else appear.
        check++;
        if (MkDirPath("one", Mode777) != 0) return check;
        check++;
        if (!Directory.Exists("one")) return check;

        // A name that already exists is EEXIST, whatever it is — a directory, a
        // file, a link to either, a dangling link, or a cyclic one. `mkdir` never
        // dereferences the name it is about to bind, so none of these is ELOOP or
        // ENOTDIR.
        foreach (string taken in new[] { "d", "f", "lf", "ld", "dang", "cyc", "one" })
        {
            check++;
            if (MkDirPath(taken, Mode777) != -1) return check;
            check++;
            if (LastPalError() != PAL_EEXIST) return check;
        }

        // A path that consumed no component at all is EEXIST too, whichever
        // navigation got it there. `rmdir` owes these three different errnos;
        // `mkdir` does not distinguish them.
        foreach (string navigated in new[] { ".", "d/.", "d/..", "/" })
        {
            check++;
            if (MkDirPath(navigated, Mode777) != -1) return check;
            check++;
            if (LastPalError() != PAL_EEXIST) return check;
        }

        // Failures on the way in, before the last component is ever reached.
        check++;
        if (MkDirPath("nx/new", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        check++;
        if (MkDirPath("dang/new", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        check++;
        if (MkDirPath("f/new", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOTDIR) return check;

        // The empty path is the one every Unix rejects.
        check++;
        if (MkDirPath("", Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        // Over-long in ASCII, which is past both kernels' NAME_MAX however they
        // count it — APFS counts UTF-16 units and ext4 counts bytes, and 300
        // exceeds 255 either way.
        check++;
        if (MkDirPath(new string('a', 300), Mode777) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENAMETOOLONG) return check;

        // A *free* final name carrying a trailing separator creates on both,
        // which is where `mkdir` parts company with a creating `open`.
        check++;
        if (MkDirPath("two/", Mode777) != 0) return check;
        check++;
        if (!Directory.Exists("two")) return check;

        check++;
        if (MkDirPath("three//", Mode777) != 0) return check;
        check++;
        if (!Directory.Exists("three")) return check;

        // Non-final symlinks are followed by both, so this binds inside `d`.
        check++;
        if (MkDirPath("ld/inside", Mode777) != 0) return check;
        check++;
        if (!Directory.Exists("d/inside")) return check;

        // ---- and the managed path that reaches it ----

        check++;
        Directory.CreateDirectory("bcl");
        if (!Directory.Exists("bcl")) return check;

        // The recursive walk: `CreateDirectory` tries the leaf, gets ENOENT, and
        // creates the parents from the top down.
        check++;
        Directory.CreateDirectory("a/b/c");
        if (!Directory.Exists("a/b/c")) return check;
        check++;
        if (!Directory.Exists("a/b")) return check;
        check++;
        if (!Directory.Exists("a")) return check;

        // An existing directory is not an error: `CreateDirectory` swallows the
        // EEXIST after confirming what is there is a directory.
        check++;
        Directory.CreateDirectory("d");
        if (!Directory.Exists("d")) return check;

        // The BCL trims a trailing separator before calling `mkdir`, so this row
        // is a claim about CoreLib rather than about either kernel.
        check++;
        Directory.CreateDirectory("four/");
        if (!Directory.Exists("four")) return check;

        return 0;
    }
}
