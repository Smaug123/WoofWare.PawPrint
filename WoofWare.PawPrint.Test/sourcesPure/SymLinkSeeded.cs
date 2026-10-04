using System;
using System.IO;
using System.Runtime.InteropServices;

// `symlink(2)` through both the BCL (File.CreateSymbolicLink and
// Directory.CreateSymbolicLink) and the raw shim, in the rows Linux and macOS
// answer identically. This is a *pure* test, so it runs on the real CLR as well
// as under PawPrint, and every fact below is one both must agree on.
//
// The rows they do *not* agree on — a trailing separator after an existing
// name, the empty target, and the new link's mode and group — are in
// sourcesImpure/SymLinkWiring{Linux,Darwin}Seeded.cs, one per configured
// flavour.
//
// Paths are relative: PawPrint puts the seed at the root with "/" as the
// current directory, while the oracle runs the guest in a scratch directory.
//
// **Errnos are compared as PAL values, not raw numbers**, as MkDirSeeded.cs
// does: a raw errno is portable only inside the band both number identically.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f ("hello"), d/ (holding g, "nested"),
// lf -> f, ld -> d, dang -> nx. "nx" deliberately does not exist.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SymLink", SetLastError = true)]
    static extern unsafe int SymLink(byte* target, byte* linkPath);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EEXIST = 0x10014;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static byte[] Bytes(string s)
    {
        byte[] bytes = new byte[s.Length + 1];
        for (int i = 0; i < s.Length; i++) bytes[i] = (byte)s[i];
        bytes[s.Length] = 0;
        return bytes;
    }

    static unsafe int Link(string target, string linkPath)
    {
        byte[] t = Bytes(target);
        byte[] l = Bytes(linkPath);
        fixed (byte* tp = t)
        fixed (byte* lp = l)
            return SymLink(tp, lp);
    }

    static int Main()
    {
        int check = 0;

        // ---- the BCL ----

        // A link to a file reads through to the file, and reports its target.
        check++;
        File.CreateSymbolicLink("lnew", "f");
        check++;
        if (File.ReadAllText("lnew") != "hello") return check;
        check++;
        if (new FileInfo("lnew").LinkTarget != "f") return check;

        // A link to a directory reaches what the directory holds.
        check++;
        Directory.CreateSymbolicLink("dnew", "d");
        check++;
        if (File.ReadAllText("dnew/g") != "nested") return check;
        check++;
        if (new DirectoryInfo("dnew").LinkTarget != "d") return check;

        // The target is held byte for byte and never resolved: it need not
        // exist, and its doubled and trailing separators survive.
        check++;
        File.CreateSymbolicLink("odd", "a//b/");
        check++;
        if (new FileInfo("odd").LinkTarget != "a//b/") return check;

        // A name that is already taken is an IOException.
        check++;
        try
        {
            File.CreateSymbolicLink("f", "x");
            return check;
        }
        catch (IOException)
        {
        }

        // ---- the raw shim, in the rows both kernels agree on ----

        // A free name is created.
        check++;
        if (Link("t", "one") != 0) return check;
        check++;
        if (new FileInfo("one").LinkTarget != "t") return check;

        // A name that already exists is EEXIST, whatever it is: `symlink` never
        // follows the name it is about to bind, so a dangling link is EEXIST too.
        foreach (string taken in new[] { "f", "d", "lf", "dang", "one", ".", "d/" })
        {
            check++;
            if (Link("t", taken) != -1) return check;
            check++;
            if (LastPalError() != PAL_EEXIST) return check;
        }

        // A free name with a trailing separator is ENOENT: unlike mkdir,
        // symlink never creates "n/".
        check++;
        if (Link("t", "n/") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;
        check++;
        if (File.Exists("n") || Directory.Exists("n")) return check;

        // The walk to the name: a missing directory, a file in the way, and a
        // link to a directory, which is followed.
        check++;
        if (Link("t", "nxdir/n") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;
        check++;
        if (Link("t", "f/n") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOTDIR) return check;
        check++;
        if (Link("t", "ld/through") != 0) return check;
        check++;
        if (new FileInfo("d/through").LinkTarget != "t") return check;

        // The empty name.
        check++;
        if (Link("t", "") != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;

        return 0;
    }
}
