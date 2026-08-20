using System;
using System.IO;
using System.Runtime.InteropServices;

// Truncation's one flavour-dependent fact, on a **Darwin**-configured kernel:
// clearing a truncated file's set-user-ID and set-group-ID bits.
//
// PawPrint-only for two independent reasons. The rule is uid-dependent — a
// privileged process strips nothing on either kernel, and this suite does not
// choose the uid its oracle runs as — and it is *flavour*-dependent, so a
// differential run would compare PawPrint's configured kernel against whichever
// kernel happened to run the oracle. `sourcesPure/TruncateSeeded.cs` carries
// everything the two platforms agree about.
//
// This file and its Linux twin exist as a **pair**, and neither alone is
// enough: the unit tests hand `PermissionBits.afterTruncation` its rule
// explicitly and the host oracle compares the pure function, so a handler that
// ignored `SimulatedUnixPlatform.setIdBitsOnTruncation` and hardcoded either
// answer would satisfy every one of them plus one of these two guests.
//
// Also here, for the same uid reason: the write permission `O_TRUNC` demands
// over and above its access mode. And the timestamps a truncation moves, which
// need PawPrint's deterministic clock to state without racing a real
// filesystem's granularity.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): suid 0o4755, sgid 0o2755, sgnox 0o2644,
// sticky 0o1755, noop 0o4755, readonly 0o444, otrunc 0o4755 — every one of them
// holding the five bytes "hello".
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FTruncate", SetLastError = true)]
    static extern int FTruncate(IntPtr fd, long length);

    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;
    const int O_TRUNC = 0x0080;

    const int EACCES = 13;

    // What a Linux kernel leaves behind. The Darwin twin differs from this file
    // in exactly these four constants and its `KernelConfig`.
    const UnixFileMode Rwxrxrx = UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
        | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
        | UnixFileMode.OtherRead | UnixFileMode.OtherExecute;                          // 0o0755
    const UnixFileMode SuidAfter = UnixFileMode.SetUser | Rwxrxrx;                      // 0o4755
    const UnixFileMode SgidAfter = UnixFileMode.SetGroup | Rwxrxrx;                     // 0o2755
    const UnixFileMode SgidNoExecAfter = UnixFileMode.SetGroup
        | UnixFileMode.UserRead | UnixFileMode.UserWrite
        | UnixFileMode.GroupRead | UnixFileMode.OtherRead;                             // 0o2644
    const UnixFileMode StickyAfter = UnixFileMode.StickyBit | Rwxrxrx;                  // 0o1755

    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte* path = stackalloc byte[32];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, flags, 0x1B6 /* 0o666 */);
    }

    /// Truncate `name` to `length` through its own descriptor, and report the
    /// mode it is left with.
    static unsafe UnixFileMode TruncateTo(string name, long length)
    {
        IntPtr fd = OpenPath(name, O_WRONLY);
        if (fd == new IntPtr(-1)) return (UnixFileMode)(-1);
        int result = FTruncate(fd, length);
        Close(fd);
        if (result != 0) return (UnixFileMode)(-1);
        return File.GetUnixFileMode(name);
    }

    static unsafe int Main(string[] args)
    {
        int check;

        // --- ftruncate and the set-ID bits ---

        check = 1;
        if (TruncateTo("suid", 0) != SuidAfter) return check;
        check = 2;
        if (TruncateTo("sgid", 0) != SgidAfter) return check;

        // Kept, like every other bit here — but kept for a different reason from
        // its Linux twin, where this row survives a *stripping* kernel because
        // the bit means mandatory locking without group-execute.
        check = 3;
        if (TruncateTo("sgnox", 0) != SgidNoExecAfter) return check;

        // The sticky bit is never touched, on either kernel.
        check = 4;
        if (TruncateTo("sticky", 0) != StickyAfter) return check;

        // --- a truncation that moves no bytes is still a truncation ---

        // Truncating to the length the file already has leaves every byte in place
        // but still stamps the inode. That the *timestamps* move is what this row
        // asserts here — the mode cannot show it on a kernel that never strips —
        // and it is what separates truncation from a write, where transferring
        // nothing is a complete no-op.
        DateTime before = File.GetLastWriteTimeUtc("noop");
        check = 5;
        if (TruncateTo("noop", 5) != SuidAfter) return check;
        check = 6;
        if (File.ReadAllBytes("noop").Length != 5) return check;
        check = 7;
        if (File.GetLastWriteTimeUtc("noop") <= before) return check;

        // --- O_TRUNC's permission requirement ---

        // `O_TRUNC` demands the write bit whatever the access mode says, so a
        // read-only *open* of a file this process may not write is EACCES — where
        // without O_TRUNC the same open succeeds.
        check = 8;
        Marshal.SetLastSystemError(0);
        IntPtr denied = OpenPath("readonly", O_RDONLY | O_TRUNC);
        if (denied != new IntPtr(-1)) { Close(denied); return check; }
        check = 9;
        if (Marshal.GetLastSystemError() != EACCES) return check;

        // ...and nothing was truncated on the way to that refusal.
        check = 10;
        if (File.ReadAllBytes("readonly").Length != 5) return check;

        // The control: the very same open without O_TRUNC succeeds.
        check = 11;
        IntPtr allowed = OpenPath("readonly", O_RDONLY);
        if (allowed == new IntPtr(-1)) return check;
        Close(allowed);

        // --- O_TRUNC treats the mode exactly as ftruncate does ---

        check = 12;
        IntPtr t = OpenPath("otrunc", O_WRONLY | O_TRUNC);
        if (t == new IntPtr(-1)) return check;
        Close(t);
        check = 13;
        if (File.GetUnixFileMode("otrunc") != SuidAfter) return check;
        check = 14;
        if (File.ReadAllBytes("otrunc").Length != 0) return check;

        return 0;
    }
}
