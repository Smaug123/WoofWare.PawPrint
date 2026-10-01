using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

// `clonefile(2)` on a Darwin kernel, called through the guest's own P/Invoke of
// libc, so that it runs the same whichever CoreLib PawPrint resolved (a Linux
// CoreLib's File.Copy never calls it). The umask is 077 and the uid 501, so a
// clone that applied the umask fails here.
//
// Every row was measured on Darwin 27.0 at uid 501
// (docs/plans/2026-08-23-posix-kernel-extraction/clonefile-rules.c); the raw
// errnos are Darwin's numbers.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f (0o640), suid (0o4755), g, ro/ (0o555),
// dang -> nowhere.
class Program
{
    [DllImport("libc", EntryPoint = "clonefile")]
    static extern unsafe int CloneFile(byte* source, byte* destination, int flags);

    const int CLONE_ACL = 0x4;
    const int ENOENT = 2;
    const int EACCES = 13;
    const int EFAULT = 14;
    const int EEXIST = 17;
    const int EINVAL = 22;
    const int EILSEQ = 92;

    static byte[] CString(byte[] bytes)
    {
        byte[] result = new byte[bytes.Length + 1];
        Array.Copy(bytes, result, bytes.Length);
        return result;
    }

    static unsafe (int, int) Clone(string source, byte[] destination, int flags)
    {
        byte[] s = CString(Encoding.UTF8.GetBytes(source));
        byte[] d = CString(destination);
        fixed (byte* sp = s)
        fixed (byte* dp = d)
        {
            Marshal.SetLastSystemError(0);
            int result = CloneFile(sp, dp, flags);
            return (result, result == 0 ? 0 : Marshal.GetLastSystemError());
        }
    }

    static unsafe (int, int) CloneFrom(byte* source, int flags)
    {
        Marshal.SetLastSystemError(0);
        int result = CloneFile(source, null, flags);
        return (result, result == 0 ? 0 : Marshal.GetLastSystemError());
    }

    static (int, int) Clone(string source, string destination, int flags) =>
        Clone(source, Encoding.UTF8.GetBytes(destination), flags);

    static int Main(string[] args)
    {
        int check = 0;

        // A clone keeps the source's bits whatever the umask, and its access,
        // modification and birth times.
        check = 1;
        if (Clone("f", "c1", CLONE_ACL) != (0, 0)) return check;
        check = 2;
        if (File.ReadAllText("c1") != "hello") return check;
        check = 3;
        if (File.GetUnixFileMode("c1") != (UnixFileMode)Convert.ToInt32("640", 8)) return check;
        check = 4;
        if (File.GetLastWriteTimeUtc("c1") != File.GetLastWriteTimeUtc("f")) return check;
        check = 5;
        if (File.GetCreationTimeUtc("c1") != File.GetCreationTimeUtc("f")) return check;

        // ...less both set-ID bits.
        check = 6;
        if (Clone("suid", "c2", 0) != (0, 0)) return check;
        check = 7;
        if (File.GetUnixFileMode("c2") != (UnixFileMode)Convert.ToInt32("755", 8)) return check;

        // A dangling link is replaced by a file at its target.
        check = 8;
        if (Clone("f", "dang", CLONE_ACL) != (0, 0)) return check;
        check = 9;
        if (File.ReadAllText("nowhere") != "hello" || new FileInfo("dang").LinkTarget != "nowhere") return check;

        // The refusals, each in Darwin's numbering.
        check = 10;
        if (Clone("f", "g", CLONE_ACL) != (-1, EEXIST)) return check;
        check = 11;
        if (File.ReadAllText("g") != "other") return check;
        check = 12;
        if (Clone("absent", "c3", CLONE_ACL) != (-1, ENOENT)) return check;
        check = 13;
        if (Clone("f", "ro/c4", CLONE_ACL) != (-1, EACCES)) return check;
        check = 14;
        if (Clone("absent", "g", 1 << 20) != (-1, EINVAL)) return check;
        check = 15;
        if (Clone("f", new byte[] { 0xff, 0xfe }, CLONE_ACL) != (-1, EILSEQ)) return check;

        // A pathname is read only when the kernel reaches it: bad flags are
        // EINVAL whatever the pointers are, a type handle included, and good
        // ones EFAULT for an unmapped source.
        check = 16;
        unsafe
        {
            if (CloneFrom((byte*)typeof(int).TypeHandle.Value, 1 << 20) != (-1, EINVAL)) return check;
            check = 17;
            if (CloneFrom((byte*)1, CLONE_ACL) != (-1, EFAULT)) return check;
        }

        return 0;
    }
}
