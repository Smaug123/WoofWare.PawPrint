using System;
using System.IO;
using System.Runtime.InteropServices;

// `link(2)` through the BCL (File.Replace with a backup, and File.Move onto a
// name that is taken, both of which reach SystemNative_Link) and the raw shim,
// in the rows Linux and macOS answer identically. This is a *pure* test, so it
// runs on the real CLR as well as under PawPrint.
//
// The rows they do *not* agree on (whether plain link follows a symbolic link
// source, a trailing separator after a taken name, and Linux's
// protected_hardlinks) are in sourcesImpure/LinkWiring{Linux,Darwin}Seeded.cs.
//
// Paths are relative, and errnos are compared as PAL values, as
// MkDirSeeded.cs does.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f ("hello"), g ("other"), d/,
// lf -> f, src ("new content"), dst ("old content"). "nx" deliberately does
// not exist.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Link", SetLastError = true)]
    static extern unsafe int Link(byte* source, byte* linkTarget);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EEXIST = 0x10014;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;
    const int PAL_EPERM = 0x10042;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static byte[] Bytes(string s)
    {
        byte[] bytes = new byte[s.Length + 1];
        for (int i = 0; i < s.Length; i++) bytes[i] = (byte)s[i];
        bytes[s.Length] = 0;
        return bytes;
    }

    static unsafe int LinkPath(string source, string linkTarget)
    {
        byte[] s = Bytes(source);
        byte[] l = Bytes(linkTarget);
        fixed (byte* sp = s)
        fixed (byte* lp = l)
            return Link(sp, lp);
    }

    static int Expect(int check, int result, int palError) =>
        result == -1 && LastPalError() == palError ? 0 : check;

    static int Main()
    {
        int check = 0;

        // ---- the BCL ----

        // File.Replace with a backup links the backup to the destination's
        // file before renaming the source over the destination.
        check++;
        File.Replace("src", "dst", "bak");
        check++;
        if (File.ReadAllText("dst") != "new content") return check;
        check++;
        if (File.ReadAllText("bak") != "old content") return check;
        check++;
        if (File.Exists("src")) return check;

        // File.Move onto a taken name, without overwrite, links rather than
        // renames, so that it cannot replace the destination: EEXIST, and both
        // files are left as they were.
        check++;
        try
        {
            File.Move("f", "g");
            return check;
        }
        catch (IOException)
        {
        }
        check++;
        if (File.ReadAllText("f") != "hello" || File.ReadAllText("g") != "other") return check;

        // ---- the raw shim, in the rows both kernels agree on ----

        // A new name for f is f: what is written through one is read through
        // the other.
        check++;
        if (LinkPath("f", "n") != 0) return check;
        check++;
        File.WriteAllText("n", "changed");
        if (File.ReadAllText("f") != "changed") return check;

        // A link to a file, followed or not, reads as the file.
        check++;
        if (LinkPath("lf", "viaLink") != 0) return check;
        check++;
        if (File.ReadAllText("viaLink") != "changed") return check;

        check++;
        if (Expect(check, LinkPath("f", "g"), PAL_EEXIST) != 0) return check;
        check++;
        if (Expect(check, LinkPath("nx", "n2"), PAL_ENOENT) != 0) return check;
        check++;
        if (Expect(check, LinkPath("d", "n3"), PAL_EPERM) != 0) return check;
        check++;
        if (Expect(check, LinkPath("f", "nxdir/n"), PAL_ENOENT) != 0) return check;
        check++;
        if (Expect(check, LinkPath("f/", "n4"), PAL_ENOTDIR) != 0) return check;
        check++;
        if (Expect(check, LinkPath("f", "n5/"), PAL_ENOENT) != 0) return check;
        check++;
        if (Expect(check, LinkPath("f", ""), PAL_ENOENT) != 0) return check;

        return 0;
    }
}
