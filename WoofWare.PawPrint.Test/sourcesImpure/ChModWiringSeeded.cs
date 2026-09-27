using System;
using System.IO;
using System.Runtime.InteropServices;

// `SystemNative_ChMod` and `SystemNative_FChMod` called directly, with the
// arguments the BCL never passes: a mode word with bits above 0o7777, the
// set-group-ID bit, the empty path, and descriptors that are not open.
//
// PawPrint-only, because the rows need a umask and a uid this suite chooses:
// registered once per flavour, each with a umask of 0o027 and a uid of 1000,
// neither of them `KernelConfig`'s default, so a handler that applied the umask
// to a mode change, or that assumed privilege, fails here. Every row answers
// the same on both flavours. sourcesPure/ChModSeeded.cs carries what the BCL
// reaches, differentially.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f and g (5 bytes each), d/ (a directory),
// dang -> nx, theirs (0o644, owned by root), and outside (0o644, owned by this
// process's user in a group it is not in).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ChMod", SetLastError = true)]
    static extern unsafe int ChMod(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FChMod", SetLastError = true)]
    static extern int FChMod(IntPtr fd, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EBADF = 0x10008;
    const int PAL_EPERM = 0x10042;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;

    const int O_RDONLY = 0x0000;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe int ChModPath(string name, int mode)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        fixed (byte* p = bytes) return ChMod(p, mode);
    }

    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        fixed (byte* p = bytes) return Open(p, flags, 0);
    }

    static int Main()
    {
        int check = 0;

        // Bits above 0o7777 are ignored rather than rejected, on both kernels:
        // this is 0o2755 with a file-type band and more on top.
        check++;
        if (ChModPath("f", unchecked((int)0x7FFF0000) | 0x5ED /* 0o2755 */) != 0) return check;
        check++;
        if (File.GetUnixFileMode("f") != (UnixFileMode.SetGroup | UnixFileMode.UserRead | UnixFileMode.UserWrite
                                          | UnixFileMode.UserExecute | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
                                          | UnixFileMode.OtherRead | UnixFileMode.OtherExecute)) return check;

        // -1 is every bit, and no umask narrows it.
        check++;
        if (ChModPath("f", -1) != 0) return check;
        check++;
        if ((int)File.GetUnixFileMode("f") != 0xFFF) return check;

        // A directory gets the set-group-ID bit too: the caller owns it and is
        // in its group.
        check++;
        if (ChModPath("d", 0x5ED /* 0o2755 */) != 0) return check;
        check++;
        if (((int)File.GetUnixFileMode("d") & 0xFFF) != 0x5ED) return check;
        ChModPath("d", 0x1ED /* 0o755 */);

        // The walk's own answers.
        check++;
        if (ChModPath("", 0x1A4) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;
        check++;
        if (ChModPath("dang", 0x1A4) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOENT) return check;
        check++;
        if (ChModPath("f/", 0x1A4) != -1) return check;
        check++;
        if (LastPalError() != PAL_ENOTDIR) return check;

        // Through a descriptor open only for reading, with the same high bits.
        check++;
        IntPtr fd = OpenPath("g", O_RDONLY);
        if (fd == new IntPtr(-1)) return check;
        check++;
        if (FChMod(fd, unchecked((int)0xFFFF0000) | 0x180 /* 0o600 */) != 0) return check;
        check++;
        if (File.GetUnixFileMode("g") != (UnixFileMode.UserRead | UnixFileMode.UserWrite)) return check;

        // A descriptor that is no longer open, and one that never was.
        Close(fd);
        check++;
        if (FChMod(fd, 0x1A4) != -1) return check;
        check++;
        if (LastPalError() != PAL_EBADF) return check;
        check++;
        if (FChMod(new IntPtr(1000), 0x1A4) != -1) return check;
        check++;
        if (LastPalError() != PAL_EBADF) return check;
        check++;
        if (File.GetUnixFileMode("g") != (UnixFileMode.UserRead | UnixFileMode.UserWrite)) return check;

        // Someone else's file: EPERM by path and through a descriptor opened
        // for reading, and the mode stays as it was.
        check++;
        if (ChModPath("theirs", 0x1A4) != -1) return check;
        check++;
        if (LastPalError() != PAL_EPERM) return check;
        check++;
        IntPtr theirs = OpenPath("theirs", O_RDONLY);
        if (theirs == new IntPtr(-1)) return check;
        check++;
        if (FChMod(theirs, 0x180) != -1) return check;
        check++;
        if (LastPalError() != PAL_EPERM) return check;
        Close(theirs);
        check++;
        if (File.GetUnixFileMode("theirs") != (UnixFileMode.UserRead | UnixFileMode.UserWrite
                                               | UnixFileMode.GroupRead | UnixFileMode.OtherRead)) return check;

        // This process's own file in a group it is not in: the set-group-ID bit
        // it asks for is silently dropped, and the rest is set.
        check++;
        if (ChModPath("outside", 0x5ED /* 0o2755 */) != 0) return check;
        check++;
        if (((int)File.GetUnixFileMode("outside") & 0xFFF) != 0x1ED /* 0o755 */) return check;

        return 0;
    }
}
