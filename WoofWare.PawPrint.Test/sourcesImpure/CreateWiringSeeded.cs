using System;
using System.IO;
using System.Runtime.InteropServices;

// That `SystemNative_Open`'s creating path actually reads the *configured*
// kernel: its umask and its Unix flavour.
//
// PawPrint-only, and necessarily so. The unit tests call
// `CreatingOpenRules.verdict` directly and pass the rules in by hand, so a
// handler that ignored `Kernel.Umask` and `Kernel.UnixPlatform` and hardcoded
// Linux-with-umask-022 would satisfy every one of them. Only a guest sees the
// values travel from `KernelConfig` to the syscall boundary. (The same reason
// `SpliceLengthSeeded.cs` exists for `PathLimits`.)
//
// Configured as **macOS with umask 0o077**, both deliberately non-default:
//
//   * the flavour, because Linux answers EISDIR for a creating open on a
//     directory while Darwin opens it, so a handler stuck on the default
//     flavour gives the opposite answer to every directory row here;
//   * the umask, because 0o022 is what `SeedEntry.defaultPermsForRegularFile`
//     already bakes in, so a created mode of 0o644 would be indistinguishable
//     from a handler that never consulted the umask at all.
//
// Both facts were measured against real `open(2)` on macOS 26.6/APFS: a
// creating open of a directory succeeds read-only, "/" is EEXIST even without
// O_EXCL, and XNU masks the mode argument with ACCESSPERMS so setuid/setgid/
// sticky cannot survive creation.
//
// Raw P/Invoke rather than FileStream, because the BCL turns these errnos into
// exceptions whose construction needs SystemNative_ConvertErrorPalToPlatform
// and SystemNative_StrErrorR, neither of which exists yet. Every errno asserted
// below has the same number on both platforms, so nothing here depends on
// Darwin's numbering.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f = "hello", d/ a directory.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    // `Interop.Sys.OpenFlags`, the PAL's portable numbering rather than any
    // platform's <fcntl.h>.
    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;
    const int O_CREAT = 0x0020;
    const int O_EXCL = 0x0040;

    // Identical on Linux and macOS, so the guest does not depend on the
    // configured flavour's errno numbering.
    const int ENOENT = 2;
    const int EEXIST = 17;
    const int EISDIR = 21;

    static unsafe IntPtr OpenPath(string name, int flags, int mode)
    {
        byte[] bytes = new byte[System.Text.Encoding.UTF8.GetByteCount(name) + 1];
        System.Text.Encoding.UTF8.GetBytes(name, 0, name.Length, bytes, 0);
        fixed (byte* p = bytes)
        {
            return Open(p, flags, mode);
        }
    }

    static int Main(string[] args)
    {
        int check;

        // --- the umask reaches the handler.
        // 0o666 under umask 0o077 is 0o600. A handler using the default 0o022
        // would produce 0o644, and one ignoring the umask entirely 0o666.
        check = 1;
        IntPtr fd = OpenPath("made", O_WRONLY | O_CREAT, 0x1B6 /* 0o666 */);
        if (fd == (IntPtr)(-1)) return check;
        Close(fd);
        check = 2;
        UnixFileMode made = File.GetUnixFileMode("made");
        if (made != (UnixFileMode.UserRead | UnixFileMode.UserWrite)) return check;

        // The umask masks all twelve bits, and Darwin drops the top three
        // before the umask ever sees them: 0o7777 becomes 0o700, not 0o7700.
        check = 3;
        fd = OpenPath("special", O_WRONLY | O_CREAT, 0xFFF /* 0o7777 */);
        if (fd == (IntPtr)(-1)) return check;
        Close(fd);
        check = 4;
        UnixFileMode special = File.GetUnixFileMode("special");
        if (special != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute)) return check;

        // --- the flavour reaches the handler.
        // On Darwin a creating open of an existing directory succeeds read-only.
        // On the default Linux kernel this is EISDIR, so a handler that ignored
        // KernelConfig.UnixPlatform fails here.
        check = 5;
        fd = OpenPath("d", O_RDONLY | O_CREAT, 0x1B6);
        if (fd == (IntPtr)(-1)) return check;
        Close(fd);

        // ...and a path that consumed no component at all is EEXIST on Darwin
        // even without O_EXCL, where Linux says EISDIR. The two flavours give
        // *different* errnos here rather than one succeeding, so this row
        // distinguishes them even if the one above were somehow satisfied.
        check = 6;
        if (OpenPath("/", O_RDONLY | O_CREAT, 0x1B6) != (IntPtr)(-1)) return check;
        check = 7;
        if (Marshal.GetLastSystemError() != EEXIST) return check;

        // A trailing separator on a free name creates nothing and is ENOENT on
        // Darwin; Linux refuses the path with EISDIR inside the walk.
        check = 8;
        if (OpenPath("nx/", O_WRONLY | O_CREAT, 0x1B6) != (IntPtr)(-1)) return check;
        check = 9;
        if (Marshal.GetLastSystemError() != ENOENT) return check;
        check = 10;
        if (File.Exists("nx")) return check;

        // O_EXCL is unanimous, and is here so that a wholesale flavour mix-up
        // cannot leave the file with nothing to fail on.
        check = 11;
        if (OpenPath("f", O_WRONLY | O_CREAT | O_EXCL, 0x1B6) != (IntPtr)(-1)) return check;
        check = 12;
        if (Marshal.GetLastSystemError() != EEXIST) return check;

        // Under Darwin's rules a *writing* open of a directory is still EISDIR,
        // which is the ordinary rule rather than the creating one -- so the
        // flavour switch must not have turned that off too.
        check = 13;
        if (OpenPath("d", O_WRONLY | O_CREAT, 0x1B6) != (IntPtr)(-1)) return check;
        check = 14;
        if (Marshal.GetLastSystemError() != EISDIR) return check;

        return 0;
    }
}
