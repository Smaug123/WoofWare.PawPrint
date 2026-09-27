using System;
using System.IO;
using Microsoft.Win32.SafeHandles;

// chmod(2) and fchmod(2) through the BCL: `File.SetUnixFileMode`, the
// `FileSystemInfo.UnixFileMode` setter and `File.SetAttributes`, each given a
// path (SystemNative_ChMod) and, where the BCL offers it, a SafeFileHandle
// (SystemNative_FChMod), with every mode read back.
//
// Every mode set is one the seed's default would never produce, so an
// implementation that answered success and changed nothing fails. Only facts
// that hold for the owner of every file, on both flavours, are asked: the
// set-group-ID bit is not, because whether an owner keeps it depends on
// whether it is in the file's group, which the oracle's scratch directory
// decides rather than the seed.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    const UnixFileMode RW = UnixFileMode.UserRead | UnixFileMode.UserWrite;
    const UnixFileMode RWR = RW | UnixFileMode.GroupRead;
    const UnixFileMode Default = RW | UnixFileMode.GroupRead | UnixFileMode.OtherRead;
    const UnixFileMode DirDefault = UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
                                    | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
                                    | UnixFileMode.OtherRead | UnixFileMode.OtherExecute;

    static int Main(string[] args)
    {
        int check = 0;

        // A plain file, by path, to a mode that clears group and other.
        check = 1;
        File.SetUnixFileMode("f", RW);
        if (File.GetUnixFileMode("f") != RW) return check;

        // To no bits at all, and back: the mode is taken verbatim, with no
        // umask applied to it.
        check = 2;
        File.SetUnixFileMode("f", UnixFileMode.None);
        if (File.GetUnixFileMode("f") != UnixFileMode.None) return check;
        check = 3;
        File.SetUnixFileMode("f", Default | UnixFileMode.OtherWrite | UnixFileMode.GroupWrite);
        if (File.GetUnixFileMode("f") != (Default | UnixFileMode.OtherWrite | UnixFileMode.GroupWrite)) return check;

        // The set-user-ID bit, and the sticky bit on a regular file: the owner
        // keeps both.
        check = 4;
        File.SetUnixFileMode("f", UnixFileMode.SetUser | RW | UnixFileMode.UserExecute);
        if (File.GetUnixFileMode("f") != (UnixFileMode.SetUser | RW | UnixFileMode.UserExecute)) return check;
        check = 5;
        File.SetUnixFileMode("f", UnixFileMode.StickyBit | Default);
        if (File.GetUnixFileMode("f") != (UnixFileMode.StickyBit | Default)) return check;

        // A mode change is not a content change: the file's last-write and
        // last-access times stay where they were, and so does its length.
        check = 6;
        DateTime written = File.GetLastWriteTimeUtc("g");
        DateTime accessed = File.GetLastAccessTimeUtc("g");
        File.SetUnixFileMode("g", RW);
        if (File.GetLastWriteTimeUtc("g") != written) return check;
        check = 7;
        if (File.GetLastAccessTimeUtc("g") != accessed) return check;
        check = 8;
        if (new FileInfo("g").Length != 5) return check;

        // A directory, by path, and back to where it started, so that the
        // directory stays writable.
        check = 9;
        File.SetUnixFileMode("d", UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute);
        if (File.GetUnixFileMode("d") != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute)) return check;
        check = 10;
        if (!File.Exists("d/g")) return check;
        File.SetUnixFileMode("d", DirDefault);

        // Through a symbolic link: the link's target changes, and reading the
        // mode through the link reads the target's.
        check = 11;
        File.SetUnixFileMode("lf", RWR);
        if (File.GetUnixFileMode("f") != RWR) return check;
        check = 12;
        if (File.GetUnixFileMode("lf") != RWR) return check;
        check = 13;
        File.SetUnixFileMode("ld", UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute | UnixFileMode.GroupExecute);
        if (File.GetUnixFileMode("d") != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute | UnixFileMode.GroupExecute)) return check;
        File.SetUnixFileMode("d", DirDefault);

        // Nothing to change: a dangling link and an absent name are both
        // "not found", and the file behind neither is created.
        check = 14;
        try
        {
            File.SetUnixFileMode("dang", RW);
            return check;
        }
        catch (FileNotFoundException)
        {
        }
        check = 15;
        try
        {
            File.SetUnixFileMode("absent", RW);
            return check;
        }
        catch (FileNotFoundException)
        {
        }
        check = 16;
        if (File.Exists("absent") || File.Exists("nx")) return check;

        // A regular file named as a directory, through a path that continues
        // past it.
        check = 17;
        try
        {
            File.SetUnixFileMode("f/under", RW);
            return check;
        }
        catch (IOException)
        {
        }

        // The same through a descriptor, which the BCL reaches with a
        // SafeFileHandle: one opened only for reading will do.
        check = 18;
        using (SafeFileHandle handle = File.OpenHandle("h", FileMode.Open, FileAccess.Read))
        {
            File.SetUnixFileMode(handle, RWR);
            if (File.GetUnixFileMode(handle) != RWR) return check;
            check = 19;
            if (File.GetUnixFileMode("h") != RWR) return check;
            check = 20;
            File.SetUnixFileMode(handle, UnixFileMode.SetUser | RW);
            if (File.GetUnixFileMode("h") != (UnixFileMode.SetUser | RW)) return check;
        }

        // The setter on FileSystemInfo, for a file and a directory.
        check = 21;
        new FileInfo("g").UnixFileMode = UnixFileMode.UserRead;
        if (File.GetUnixFileMode("g") != UnixFileMode.UserRead) return check;
        check = 22;
        new DirectoryInfo("d").UnixFileMode = UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute | UnixFileMode.OtherExecute;
        if (File.GetUnixFileMode("d") != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute | UnixFileMode.OtherExecute)) return check;
        new DirectoryInfo("d").UnixFileMode = DirDefault;

        // `File.SetAttributes`: ReadOnly takes away every write bit, and
        // clearing it gives the owner write back because the owner may read.
        check = 23;
        File.SetUnixFileMode("e", Default | UnixFileMode.GroupWrite);
        File.SetAttributes("e", FileAttributes.ReadOnly);
        if (File.GetUnixFileMode("e") != (UnixFileMode.UserRead | UnixFileMode.GroupRead | UnixFileMode.OtherRead)) return check;
        check = 24;
        if ((File.GetAttributes("e") & FileAttributes.ReadOnly) == 0) return check;
        check = 25;
        File.SetAttributes("e", FileAttributes.Normal);
        if (File.GetUnixFileMode("e") != Default) return check;

        // ...and through a handle.
        check = 26;
        using (SafeFileHandle handle = File.OpenHandle("e", FileMode.Open, FileAccess.Read))
        {
            File.SetAttributes(handle, FileAttributes.ReadOnly);
            if (File.GetUnixFileMode("e") != (UnixFileMode.UserRead | UnixFileMode.GroupRead | UnixFileMode.OtherRead)) return check;
        }

        // FileInfo.IsReadOnly, which is SetAttributes by another name.
        check = 27;
        FileInfo info = new FileInfo("e");
        info.IsReadOnly = false;
        if (File.GetUnixFileMode("e") != Default) return check;

        return 0;
    }
}
