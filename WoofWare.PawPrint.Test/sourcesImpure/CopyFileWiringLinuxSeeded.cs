using System;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;

// File.Copy on a Linux kernel, where the rows depend on things this suite
// chooses rather than on the host: a umask of 077 and a uid of 1000 that owns
// most of the tree, a file root owns that anyone may write, set-ID bits, and
// destinations that are symbolic links. Then `SystemNative_CopyFile` called
// directly, for the errno a successful copy leaves behind.
//
// Every row was checked against real .NET 10.0.7 on Linux 6.18.5, as uid 1000
// with umask 077 in a tree laid out as this seed is
// (docs/plans/2026-08-23-posix-kernel-extraction/copy-file-guests-linux.sh).
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f (0o640), suid (0o4755), theirs (0o666, owned
// by root), ro/ (0o555) holding w (0o666), dang -> nowhere, lt -> t, and src
// and empty for the direct calls.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CopyFile")]
    static extern int CopyFile(IntPtr source, IntPtr destination, long sourceLength);

    const int EBADF = 9;
    const int EOPNOTSUPP = 95;

    static bool Throws<T>(Action action) where T : Exception
    {
        try
        {
            action();
            return false;
        }
        catch (Exception e)
        {
            return e.GetType() == typeof(T);
        }
    }

    // A direct copy of `source` into a new file `destination`, answering what
    // it returned and the errno it left.
    static (int, int) Direct(string source, string destination, long length)
    {
        using SafeFileHandle input = File.OpenHandle(source, FileMode.Open, FileAccess.Read);
        using SafeFileHandle output = File.OpenHandle(destination, FileMode.CreateNew, FileAccess.ReadWrite);
        Marshal.SetLastSystemError(0);
        int result = CopyFile(input.DangerousGetHandle(), output.DangerousGetHandle(), length);
        int errno = Marshal.GetLastSystemError();
        return (result, errno);
    }

    static int Main(string[] args)
    {
        int check = 0;

        // First of all, before anything has asked System.Native whether it may
        // use copy_file_range: the first copy probes that, and leaves the
        // probe's EBADF behind it; later ones do not probe, and leave FICLONE's
        // EOPNOTSUPP; a length of 0 asks neither, and leaves nothing.
        check = 1;
        if (Direct("src", "direct1", 5) != (0, EBADF)) return check;
        check = 2;
        if (File.ReadAllText("direct1") != "hello") return check;
        check = 3;
        if (Direct("src", "direct2", 5) != (0, EOPNOTSUPP)) return check;
        check = 4;
        if (Direct("empty", "direct3", 0) != (0, 0)) return check;
        // A length of 0 for a source that holds bytes is a hint, not the copy:
        // the read/write loop still moves them.
        check = 17;
        if (Direct("src", "direct4", 0) != (0, 0)) return check;
        check = 18;
        if (File.ReadAllText("direct4") != "hello") return check;

        // The source's permission bits whatever the umask, and without its
        // set-ID bits.
        check = 5;
        File.Copy("f", "c1");
        if (File.GetUnixFileMode("c1") != (UnixFileMode)Convert.ToInt32("640", 8)) return check;
        check = 6;
        File.Copy("suid", "c2");
        if (File.GetUnixFileMode("c2") != (UnixFileMode)Convert.ToInt32("755", 8)) return check;

        // Onto root's file, which this user may write but not otherwise
        // change: the bytes go in, and its mode and times stay its own.
        check = 7;
        File.Copy("f", "theirs", true);
        if (File.ReadAllText("theirs") != "hello") return check;
        check = 8;
        if (File.GetUnixFileMode("theirs") != (UnixFileMode)Convert.ToInt32("666", 8)) return check;
        check = 9;
        if (File.GetLastWriteTimeUtc("theirs") == File.GetLastWriteTimeUtc("f")) return check;

        // Onto a dangling link: refused without overwrite, and with it the
        // link's target is created.
        check = 10;
        if (!Throws<IOException>(() => File.Copy("f", "dang"))) return check;
        check = 11;
        if (File.Exists("nowhere")) return check;
        check = 12;
        File.Copy("f", "dang", true);
        if (File.ReadAllText("nowhere") != "hello") return check;
        check = 13;
        if (new FileInfo("dang").LinkTarget != "nowhere") return check;

        // Onto a link to a file: written through, and the link stays.
        check = 14;
        File.Copy("f", "lt", true);
        if (File.ReadAllText("t") != "hello" || new FileInfo("lt").LinkTarget != "t") return check;

        // Over a writable file in a directory this user may not write.
        check = 15;
        File.Copy("f", "ro/w", true);
        if (File.ReadAllText("ro/w") != "hello") return check;
        check = 16;
        if (File.GetUnixFileMode("ro/w") != (UnixFileMode)Convert.ToInt32("640", 8)) return check;

        return 0;
    }
}
