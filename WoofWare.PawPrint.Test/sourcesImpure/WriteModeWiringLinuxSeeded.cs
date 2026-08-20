using System;
using System.IO;
using System.Runtime.InteropServices;

// A content-changing write's one flavour-dependent fact, on a **Linux**-configured
// kernel: whether `S_ISGID` survives on a file that is not group-executable.
//
// PawPrint-only for two independent reasons, the same two the truncation pair
// gives. The rule is uid-dependent — a privileged writer strips nothing on
// either kernel, and this suite does not choose the uid its oracle runs as — and
// it is *flavour*-dependent, so a differential run would compare PawPrint's
// configured kernel against whichever kernel happened to run the oracle.
// `sourcesPure/WriteSeeded.cs` carries everything the two platforms agree about.
//
// This file and its Darwin twin exist as a **pair**, and neither alone is
// enough: the unit tests hand `PermissionBits.afterContentChangingWrite` its
// rule explicitly and the host oracle compares the pure function, so a handler
// that ignored `SimulatedUnixPlatform.setGroupIdOnWrite` and hardcoded either
// answer would satisfy every one of them plus one of these two guests.
//
// Measured non-root on Linux 6.18.5 (ext4) and macOS 26.6 (APFS), one byte
// written over the front of a file that was given the caller's own primary
// group first — without that, `chmod` drops `S_ISGID` silently and the
// measurement reads as agreement where there is none.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): suid 0o4755, sgid 0o2755, sgnox 0o2644,
// both 0o6644, sticky 0o1755, plain 0o0644, zerolen 0o4755 — every one of them
// holding the five bytes "hello".
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int O_WRONLY = 0x0001;

    const UnixFileMode Rwxrxrx = UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
        | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
        | UnixFileMode.OtherRead | UnixFileMode.OtherExecute;                          // 0o0755
    const UnixFileMode Rwrr = UnixFileMode.UserRead | UnixFileMode.UserWrite
        | UnixFileMode.GroupRead | UnixFileMode.OtherRead;                             // 0o0644
    const UnixFileMode SuidRwxrxrx = UnixFileMode.SetUser | Rwxrxrx;                   // 0o4755
    const UnixFileMode StickyRwxrxrx = UnixFileMode.StickyBit | Rwxrxrx;               // 0o1755

    // The two rows the flavours answer differently. This file and its Darwin
    // twin differ in exactly these two constants and their `KernelConfig`.
    //
    // Linux keeps the bit: without `S_IXGRP` it means mandatory locking rather
    // than privilege, so there is no privilege to drop.
    const UnixFileMode SgnoxAfter = UnixFileMode.SetGroup | Rwrr;                      // 0o2644
    const UnixFileMode BothAfter = UnixFileMode.SetGroup | Rwrr;                       // 0o2644

    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte* path = stackalloc byte[32];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, flags, 0x1B6 /* 0o666 */);
    }

    /// Write `count` bytes over the front of `name` through its own descriptor,
    /// and report the mode it is left with.
    static unsafe UnixFileMode WriteTo(string name, int count)
    {
        IntPtr fd = OpenPath(name, O_WRONLY);
        if (fd == new IntPtr(-1)) return (UnixFileMode)(-1);
        byte* buf = stackalloc byte[4];
        buf[0] = (byte)'x';
        int written = Write(fd, buf, count);
        Close(fd);
        if (written != count) return (UnixFileMode)(-1);
        return File.GetUnixFileMode(name);
    }

    static unsafe int Main(string[] args)
    {
        int check;

        // --- the rows both flavours agree on ---

        // `S_ISUID` goes whatever the execute bits say.
        check = 1;
        if (WriteTo("suid", 1) != Rwxrxrx) return check;

        // `S_ISGID` alongside `S_IXGRP` is a real privilege bit, and goes.
        check = 2;
        if (WriteTo("sgid", 1) != Rwxrxrx) return check;

        // The sticky bit is never touched, on either kernel.
        check = 3;
        if (WriteTo("sticky", 1) != StickyRwxrxrx) return check;

        // An ordinary mode has nothing to lose.
        check = 4;
        if (WriteTo("plain", 1) != Rwrr) return check;

        // --- the rows that separate the flavours ---

        check = 5;
        if (WriteTo("sgnox", 1) != SgnoxAfter) return check;

        // Both set-ID bits at once, with no group-execute bit. The strongest
        // single row: the two flavour rules and "strip nothing" each answer it
        // differently, so no two of them can be confused here.
        check = 6;
        if (WriteTo("both", 1) != BothAfter) return check;

        // --- a write that transfers nothing is not a write ---

        // The control that separates "a content-changing write strips" from
        // "any call to write(2) strips". A zero-length write reports 0 and
        // leaves the mode exactly as it was, on both kernels.
        check = 7;
        if (WriteTo("zerolen", 0) != SuidRwxrxrx) return check;

        // ...and the file is still there to be stripped, so the row above is
        // about the write's length rather than about an unwritable file.
        check = 8;
        if (WriteTo("zerolen", 1) != Rwxrxrx) return check;

        return 0;
    }
}
