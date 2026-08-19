using System;
using System.IO;
using System.Text;

// `O_CREAT` and `O_EXCL` in SystemNative_Open: the first thing in PawPrint that
// binds a *new* name in the emulated filesystem.
//
// Pure, and that is a measured claim rather than an assumption. Every row below
// was measured identically on macOS 26.6/APFS and Linux 6.x with C probes
// against real `open(2)`: creation itself, the mode of an existing file
// surviving an `O_CREAT` open, EEXIST for `O_EXCL` on anything that already
// exists (including a symlink, which `O_EXCL` does *not* follow), ENOENT under a
// missing parent, ENOTDIR under a regular file, and a dangling link resolving to
// its target.
//
// What is *not* portable stays out of this file. Two whole families of it:
//
//   * Everything a *creating* open does to a directory or to a trailing
//     separator. Linux answers EISDIR for `open(dir, O_CREAT)` and for any path
//     whose final component carries a trailing separator, where Darwin treats
//     `O_CREAT` as having no bearing on an object that already exists. Those
//     rows live in TestVirtualFileSystemAgainstHost.fs, which instantiates the
//     model at the *host's* flavour and so pins one column per machine.
//   * The mode a created file ends up with, which depends on the umask and uid
//     of whichever process ran, and this suite chooses neither for its oracle.
//     PawPrint's own are KernelConfig.Umask and KernelConfig.UserId.
//
// ELOOP is likewise absent even though both kernels agree it is the answer: its
// raw *number* is 40 on Linux and 62 on Darwin, and a `sourcesPure` case runs
// PawPrint's default Linux kernel against the host's, so any assertion that saw
// the number would flip between a Mac and CI.
//
// **Every EEXIST row is absent, and not by choice.** `O_EXCL` on an existing
// name is the headline behaviour of this slice, but observing it from managed
// code means catching the IOException the BCL builds for EEXIST, and that arm of
// `Interop.GetExceptionForIoErrno` goes through `GetIOException`, which needs
// `SystemNative_ConvertErrorPalToPlatform` and `SystemNative_StrErrorR`. Neither
// is implemented, so such a guest aborts *while constructing the exception*
// rather than failing an assertion -- the same wall `OpenMissingFile.cs` records
// for its own deliberately-absent EACCES row. The EEXIST rows therefore live in
// TestVirtualFileSystemAgainstHost.fs, where they are compared against real
// `open(2)` at the errno level and need no exception at all.
//
// FileShare.None throughout, deliberately: it takes LOCK_EX, where a shared lock
// taken with write access would first consult SystemNative_GetFileSystemType,
// which PawPrint does not implement.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f = "hello", d/ a directory, lf -> f,
// dang -> nx (dangling), cyc -> cyc. "made", "made2", "viadang" and "nx" do not
// exist.
class Program
{
    static byte[] Bytes(string s) => Encoding.UTF8.GetBytes(s);

    static int Main(string[] args)
    {
        int check;

        // The point of the slice: a name that did not exist does after this, and
        // the bytes written through the new descriptor read back.
        check = 1;
        using (FileStream fs = new FileStream("made", FileMode.CreateNew, FileAccess.Write, FileShare.None))
        {
            fs.Write(Bytes("written"), 0, 7);
        }
        if (!File.Exists("made")) return check;
        check = 2;
        if (Encoding.UTF8.GetString(File.ReadAllBytes("made")) != "written") return check;

        // A freshly created file is empty before anything is written to it --
        // which is also what makes the O_TRUNC that FileMode.Create would want
        // unnecessary for a name that was free.
        check = 3;
        using (new FileStream("made2", FileMode.CreateNew, FileAccess.Write, FileShare.None)) { }
        if (File.ReadAllBytes("made2").Length != 0) return check;

        // Without O_EXCL a dangling link *is* followed, and the file appears at
        // the link's target rather than replacing the link. This is the row that
        // fails if O_EXCL's NoFollowFinal policy leaks into the plain O_CREAT
        // path.
        check = 9;
        using (FileStream fs = new FileStream("viadang", FileMode.OpenOrCreate, FileAccess.Write, FileShare.None))
        {
            fs.Write(Bytes("target"), 0, 6);
        }
        check = 10;
        if (Encoding.UTF8.GetString(File.ReadAllBytes("viadangtarget")) != "target") return check;

        // OpenOrCreate on a file that already exists opens it and leaves its
        // contents alone -- it emits O_CREAT but no O_TRUNC. This is exactly the
        // case the handler used to over-refuse.
        check = 11;
        using (new FileStream("f", FileMode.OpenOrCreate, FileAccess.Read, FileShare.None)) { }
        check = 12;
        if (Encoding.UTF8.GetString(File.ReadAllBytes("f")) != "hello") return check;

        // Append is O_CREAT too, and creates when the name is free.
        check = 13;
        using (FileStream fs = new FileStream("appended", FileMode.Append, FileAccess.Write, FileShare.None))
        {
            fs.Write(Bytes("one"), 0, 3);
        }
        check = 14;
        if (Encoding.UTF8.GetString(File.ReadAllBytes("appended")) != "one") return check;

        // Creating cannot conjure the directory that would hold the name.
        check = 15;
        try
        {
            using (new FileStream("nodir/made", FileMode.CreateNew, FileAccess.Write, FileShare.None))
            {
                return check;
            }
        }
        catch (DirectoryNotFoundException) { }

        // ...nor bind a name inside a regular file.
        check = 16;
        try
        {
            using (new FileStream("f/made", FileMode.CreateNew, FileAccess.Write, FileShare.None))
            {
                return check;
            }
        }
        catch (DirectoryNotFoundException) { }

        // Creation inside a subdirectory, so the binding directory is something
        // other than the root -- a handler that created in the start directory
        // rather than in the one the walk resolved passes everything above.
        check = 17;
        using (FileStream fs = new FileStream("d/inner", FileMode.CreateNew, FileAccess.Write, FileShare.None))
        {
            fs.Write(Bytes("deep"), 0, 4);
        }
        check = 18;
        if (Encoding.UTF8.GetString(File.ReadAllBytes("d/inner")) != "deep") return check;
        check = 19;
        if (File.Exists("inner")) return check;

        return 0;
    }
}
