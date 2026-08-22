using System;
using System.IO;
using System.Runtime.InteropServices;

// Directory enumeration -- `opendir`/`readdir`/`closedir` -- through the BCL and
// through the raw shim, in the rows Linux and macOS answer identically. This is
// a *pure* test, so it runs on the real CLR as well as under PawPrint, and every
// fact below is one both must agree on.
//
// **No row compares an order.** Measured on both kernels, the order `readdir`
// returns names in is arbitrary and the two disagree: the same seven names come
// back as `z é a sub ls C b` on APFS and `b a C é z sub ls` on the Linux overlay.
// Only `.` and `..` have a fixed position -- first, in that order, on both. So
// every listing here is sorted before it is compared, and PawPrint's own order
// (the `Map`'s) is pinned in the unit tier instead.
//
// The one divergence in this slice is `DirectoryEntry.NameLength`, which is -1
// on Linux and the name's byte length on Darwin; it lives in
// sourcesImpure/EnumerateWiring{Linux,Darwin}Seeded.cs.
//
// ENOENT is raised through the BCL, but ENOTDIR and EACCES are not: their arms
// of `GetExceptionForIoErrno` build a message through `SystemNative_StrErrorR`,
// which PawPrint does not implement, so a managed row for either would abort the
// run rather than fail it. Those go through the raw shim, exactly as
// sourcesPure/RmDirSeeded.cs and UnlinkSeeded.cs do.
//
// No permission row here, and deliberately: a run as root answers differently on
// the two sides, since PawPrint's uid is `KernelConfig`'s whatever the host's is.
// Those rows are in the unit tier, on both flavours.
//
// This guest *deletes* things. On the oracle side that is the per-run scratch
// directory, so every path below is a relative name the seed put there -- never
// an absolute path, never "..", and never the guest image. For the same reason
// nothing here enumerates ".": the scratch directory also holds the guest's own
// dll and runtimeconfig, which the emulated filesystem has no idea about.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): d/{a, sub/z, ls -> a}, f (a file),
// ld -> d, dang -> nx, del/{x, inner/y}, gone/ (an empty directory this guest
// opens and then removes). "nx" deliberately does not exist.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_OpenDir", SetLastError = true)]
    static extern unsafe IntPtr OpenDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseDir", SetLastError = true)]
    static extern int CloseDir(IntPtr dir);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ReadDir")]
    static extern unsafe int ReadDir(IntPtr dir, DirectoryEntry* entry);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_RmDir", SetLastError = true)]
    static extern unsafe int RmDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    // Must match `Interop.Sys.DirectoryEntry` exactly: a pointer then two 32-bit
    // fields, 16 bytes. `NameLength` is -1 on Linux and the name's byte length
    // on Darwin, so nothing here reads it -- walking to the terminator is valid
    // on both, and the divergence is asserted in
    // sourcesImpure/EnumerateWiring{Linux,Darwin}Seeded.cs.
    [StructLayout(LayoutKind.Sequential)]
    unsafe struct DirectoryEntry
    {
        public byte* Name;
        public int NameLength;
        public int InodeType;
    }

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Sys.OpenFlags, and the mode CoreLib's own OpenReadOnly passes.
    const int O_RDONLY = 0x0000;
    const int DefaultCreateMode = 438;

    // Interop.Sys.NodeType, which is the platform's own DT_*.
    const int DT_DIR = 4;
    const int DT_REG = 8;
    const int DT_LNK = 10;

    // Interop.Error, the PAL error enum.
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe void Ascii(string s, byte* dest)
    {
        for (int i = 0; i < s.Length; i++)
        {
            dest[i] = (byte)s[i];
        }

        dest[s.Length] = 0;
    }

    static unsafe IntPtr OpenDirPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        Marshal.SetLastSystemError(0);
        return OpenDir(buf);
    }

    /// The NUL-terminated name a `DirectoryEntry` points at. Walked to the
    /// terminator rather than sliced at `NameLength`, which is the only decode
    /// valid on both kernels.
    static unsafe string NameOf(byte* name)
    {
        int length = 0;
        while (name[length] != 0) length++;

        char[] chars = new char[length];
        for (int i = 0; i < length; i++) chars[i] = (char)name[i];

        return new string(chars);
    }

    /// TEMPORARY DIAGNOSTIC, to be reverted with its caller.
    static int Rank(string s) => s == null ? 0 : s == "." ? 1 : s == ".." ? 2 : s == "z" ? 3 : 4;

    /// Sorted, and with the directory prefix stripped, so a comparison is about
    /// the names rather than about how the BCL composed the paths.
    static string Names(string[] entries)
    {
        Array.Sort(entries, StringComparer.Ordinal);
        return string.Join(",", entries);
    }

    static int Main()
    {
        // 1-3: what a directory holds, split by kind. `ls` is a symlink to a
        // regular file, so it is a file; `sub` is the only directory.
        if (Names(Directory.GetFileSystemEntries("d")) != "d/a,d/ls,d/sub") return 1;
        if (Names(Directory.GetFiles("d")) != "d/a,d/ls") return 2;
        if (Names(Directory.GetDirectories("d")) != "d/sub") return 3;

        // 4: `.` and `..` are produced by the stream, not stored in the
        // directory, and this is the only managed way to see them.
        EnumerationOptions withSpecials = new EnumerationOptions { ReturnSpecialDirectories = true };
        if (Names(Directory.GetFileSystemEntries("d", "*", withSpecials)) != "d/.,d/..,d/a,d/ls,d/sub") return 4;

        // 5: enumeration follows a symlink to a directory, and reports the
        // entries under the name it was asked about.
        if (Names(Directory.GetFileSystemEntries("ld")) != "ld/a,ld/ls,ld/sub") return 5;

        // 6: a nested walk, which reopens each subdirectory by path.
        if (Names(Directory.GetFiles("d", "*", SearchOption.AllDirectories)) != "d/a,d/ls,d/sub/z") return 6;

        // 7: an empty enumeration is empty rather than a failure.
        if (Names(Directory.GetFileSystemEntries("d/sub")) != "d/sub/z") return 7;

        // 8-9: ENOENT reaches the BCL, because that arm of
        // `GetExceptionForIoErrno` builds its message from SR strings alone.
        try
        {
            Directory.GetFileSystemEntries("nx");
            return 8;
        }
        catch (DirectoryNotFoundException)
        {
        }

        try
        {
            Directory.GetFileSystemEntries("dang");
            return 9;
        }
        catch (DirectoryNotFoundException)
        {
        }

        // 10-13: the errnos, through the shim. A trailing separator changes
        // nothing on either kernel: a file is ENOTDIR with and without one.
        unsafe
        {
            if (OpenDirPath("nx") != IntPtr.Zero || LastPalError() != PAL_ENOENT) return 10;
            if (OpenDirPath("dang") != IntPtr.Zero || LastPalError() != PAL_ENOENT) return 11;
            if (OpenDirPath("f") != IntPtr.Zero || LastPalError() != PAL_ENOTDIR) return 12;
            if (OpenDirPath("f/") != IntPtr.Zero || LastPalError() != PAL_ENOTDIR) return 13;

            // 14: a stream really is opened and closed, and closing it succeeds.
            IntPtr dir = OpenDirPath("d");
            if (dir == IntPtr.Zero) return 14;
            if (CloseDir(dir) != 0) return 15;

            // 16: through a symlink, and with a trailing separator, which both
            // kernels follow.
            IntPtr viaLink = OpenDirPath("ld/");
            if (viaLink == IntPtr.Zero) return 16;
            if (CloseDir(viaLink) != 0) return 17;
        }

        // 18-21: the same walk through the shim, which is the only way to see
        // `.` and `..` in the order the stream produces them -- first, in that
        // order, on both kernels. The names after them are compared as a set,
        // because that order is arbitrary and the two kernels disagree.
        unsafe
        {
            IntPtr sub = OpenDirPath("d/sub");
            if (sub == IntPtr.Zero) return 18;

            DirectoryEntry entry;
            string first = null;
            string second = null;
            string rest = "";
            int count = 0;

            while (ReadDir(sub, &entry) == 0)
            {
                string got = NameOf(entry.Name);

                if (count == 0) first = got;
                else if (count == 1) second = got;
                else rest = rest.Length == 0 ? got : rest + "," + got;

                count++;
            }

            if (CloseDir(sub) != 0) return 19;

            // TEMPORARY DIAGNOSTIC, to be reverted. CI (Linux) reports 20 here
            // while macOS and a Linux container both report 0, so report *what*
            // the stream returned rather than merely that it was unexpected.
            // 40 + count when the count is not three; otherwise 60 + a rank for
            // each of the first two names (0 null, 1 ".", 2 "..", 3 "z", 4 other).
            if (count != 3) return 40 + (count > 9 ? 9 : count);
            if (first != "." || second != "..") return 60 + Rank(first) * 5 + Rank(second);
            if (rest != "z") return 21;

            // 22-24: `d_type`. Measured identical on both kernels for every
            // inode kind PawPrint can represent, and *not* implied by the
            // `GetFiles`/`GetDirectories` split above: `FileSystemEntry`
            // consults `InodeType` only when it is not DT_UNKNOWN, and falls
            // back to `stat` otherwise -- so a handler answering DT_UNKNOWN for
            // everything would pass every managed row in this file.
            IntPtr typed = OpenDirPath("d");
            if (typed == IntPtr.Zero) return 22;

            int sawRegular = 0;
            int sawDirectory = 0;
            int sawSymlink = 0;

            while (ReadDir(typed, &entry) == 0)
            {
                string got = NameOf(entry.Name);

                if (got == "a" && entry.InodeType == DT_REG) sawRegular++;
                if (got == "sub" && entry.InodeType == DT_DIR) sawDirectory++;
                if (got == "ls" && entry.InodeType == DT_LNK) sawSymlink++;
                // `.` and `..` are directories on both kernels.
                if ((got == "." || got == "..") && entry.InodeType != DT_DIR) return 23;
            }

            if (CloseDir(typed) != 0) return 24;
            if (sawRegular != 1 || sawDirectory != 1 || sawSymlink != 1) return 25;

            // 26: a stream over a directory removed *before* its first read is
            // at end of stream at once -- no `.`, no `..`. Measured on both
            // kernels. (Reading an entry first and then removing gives the whole
            // listing on both, which is why nothing here does that: it is an
            // artefact of when `getdents` ran, not a rule.)
            IntPtr doomed = OpenDirPath("gone");
            if (doomed == IntPtr.Zero) return 26;

            byte* path = stackalloc byte[256];
            Ascii("gone", path);
            if (RmDir(path) != 0) return 27;

            if (ReadDir(doomed, &entry) != -1) return 28;
            if (CloseDir(doomed) != 0) return 29;
        }

        // 32-33: `readdir` leaves errno at zero. The C sets `errno = 0` itself
        // before calling `readdir(3)`, so that it can tell end-of-stream from
        // failure by reading errno back -- and this import declares no
        // `SetLastError`, so nothing saves or restores errno around the call
        // and `Marshal.GetLastSystemError` reads exactly what the C left.
        // A cross-runtime fact for that reason: through a *flagged* import the
        // stub would have zeroed errno anyway and the row would prove nothing.
        unsafe
        {
            IntPtr errnoProbe = OpenDirPath("d");
            if (errnoProbe == IntPtr.Zero) return 32;

            DirectoryEntry probeEntry;
            Marshal.SetLastSystemError(4242);
            int got = ReadDir(errnoProbe, &probeEntry);
            int afterEntry = Marshal.GetLastSystemError();

            // ...and at end of stream too, which is the path that reads errno
            // back on a real kernel.
            while (ReadDir(errnoProbe, &probeEntry) == 0) { }
            int afterEnd = Marshal.GetLastSystemError();

            if (CloseDir(errnoProbe) != 0) return 33;
            if (got != 0 || afterEntry != 0) return 34;
            if (afterEnd != 0) return 35;
        }

        // 36-37: `opendir` consumes a file descriptor. Measured on both kernels:
        // an `open` either side of one returned fds 3 and 5, with the stream
        // holding 4. `dirfd(3)` is the only way to ask directly and appears
        // nowhere in CoreLib, so the numbering of a later `open` is how a guest
        // sees it -- and it is what makes the descriptor real rather than
        // bookkeeping.
        unsafe
        {
            byte* file = stackalloc byte[8];
            Ascii("f", file);

            IntPtr before = Open(file, O_RDONLY, DefaultCreateMode);
            if (before == new IntPtr(-1)) return 36;

            IntPtr stream = OpenDirPath("d");
            if (stream == IntPtr.Zero) return 37;

            IntPtr after = Open(file, O_RDONLY, DefaultCreateMode);
            if (after == new IntPtr(-1)) return 38;

            long gap = (long)after - (long)before;

            Close(before);
            Close(after);
            if (CloseDir(stream) != 0) return 39;
            if (gap != 2) return 40;
        }

        // 30-31: the payoff. `Directory.Delete(recursive: true)` enumerates and
        // deletes in the same pass -- CoreLib's `RemoveDirectoryRecursive`
        // removes each child inside the `foreach` over the live enumerator and
        // then `rmdir`s the parent -- so an enumeration that skipped anything
        // would answer ENOTEMPTY here.
        Directory.Delete("del", true);
        if (Directory.Exists("del")) return 30;
        if (File.Exists("del/x")) return 31;

        return 0;
    }
}
