using System;
using System.Runtime.InteropServices;

// Binding a name that is not valid UTF-8, on a **Linux**-configured kernel:
// `mkdir`, a creating `open` and `rename` onto it all succeed, since ext4 binds
// any NUL-free bytes, and the name is found afterwards.
//
// The twin of BindNonUtf8WiringDarwinSeeded.cs, run over the same seed, and
// neither alone is enough: a handler that ignored the configured flavour would
// satisfy one of the two.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): d/ holding f, and ro/ (0o555) — run as uid 1000,
// so ro/ really is unwritable.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Rename", SetLastError = true)]
    static extern unsafe int Rename(byte* oldPath, byte* newPath);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // `Interop.Sys.OpenFlags`, the PAL's portable numbering.
    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;
    const int O_CREAT = 0x0020;

    const int PAL_EACCES = 0x10002;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    // "d/" followed by one 0xFF byte, and so on: ASCII text, then raw bytes, then the NUL.
    static byte[] PathOf(string prefix, params byte[] tail)
    {
        byte[] bytes = new byte[prefix.Length + tail.Length + 1];
        for (int i = 0; i < prefix.Length; i++) bytes[i] = (byte)prefix[i];
        Array.Copy(tail, 0, bytes, prefix.Length, tail.Length);
        return bytes;
    }

    static unsafe int MkDirPath(byte[] path)
    {
        fixed (byte* p = path) return MkDir(p, 0x1FF /* 0o777 */);
    }

    static unsafe IntPtr OpenPath(byte[] path, int flags)
    {
        fixed (byte* p = path) return Open(p, flags, 0x1B6 /* 0o666 */);
    }

    static unsafe int RenamePath(byte[] source, byte[] destination)
    {
        fixed (byte* s = source)
        fixed (byte* d = destination)
            return Rename(s, d);
    }

    static int Main()
    {
        int check = 0;

        // ---- each binding succeeds.
        check++;
        if (MkDirPath(PathOf("d/", 0xFF)) != 0) return check;

        check++;
        IntPtr fd = OpenPath(PathOf("d/", 0xFE), O_WRONLY | O_CREAT);
        if (fd == (IntPtr)(-1)) return check;
        Close(fd);

        check++;
        if (RenamePath(PathOf("d/f"), PathOf("d/", 0xE4, 0xB8)) != 0) return check;

        // ---- ...and each name is there afterwards, byte for byte: the file
        // opens, and the moved file answers to its new name only.
        check++;
        fd = OpenPath(PathOf("d/", 0xFE), O_RDONLY);
        if (fd == (IntPtr)(-1)) return check;
        Close(fd);

        check++;
        fd = OpenPath(PathOf("d/", 0xE4, 0xB8), O_RDONLY);
        if (fd == (IntPtr)(-1)) return check;
        Close(fd);

        check++;
        if (OpenPath(PathOf("d/f"), O_RDONLY) != (IntPtr)(-1)) return check;

        // ---- the permission check still applies, whatever the name.
        check++;
        if (MkDirPath(PathOf("ro/", 0xFF)) != -1) return check;
        check++;
        if (LastPalError() != PAL_EACCES) return check;

        return 0;
    }
}
