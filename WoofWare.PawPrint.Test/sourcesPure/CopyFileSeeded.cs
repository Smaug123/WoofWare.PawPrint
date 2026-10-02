using System;
using System.IO;

// File.Copy, with and without overwrite: what the copy holds, the mode and the
// times it is given, and which exception each refusal throws.
//
// Only what holds on both flavours is asked, and no row that throws
// UnauthorizedAccessException: those are CopyFileUnauthorized.cs's. A Linux System.Native copies the
// bytes, then sets the destination's access and modification times and its
// permission bits from the source's; a Darwin CoreLib clones the file, which
// carries the same, and its birth time too. Neither applies the umask to the
// mode. Which errno a refusal had is not asked (EWOULDBLOCK is 11 on Linux and
// 35 on Darwin), only the exception CoreLib makes of it; nor is anything a
// privilege decides, nor a destination that is a symbolic link, where the two
// flavours part.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    const UnixFileMode Plain = UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.GroupRead;
    const UnixFileMode Open = UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.GroupRead
                              | UnixFileMode.GroupWrite | UnixFileMode.OtherRead | UnixFileMode.OtherWrite;

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

    static bool SameTimes(string a, string b)
    {
        return File.GetLastWriteTimeUtc(a) == File.GetLastWriteTimeUtc(b)
               && File.GetLastAccessTimeUtc(a) == File.GetLastAccessTimeUtc(b);
    }

    static int Main(string[] args)
    {
        int check = 0;

        // A new name: the source's times, its mode, and its bytes. The times
        // first, since reading either file can move its access time.
        check = 1;
        File.Copy("f", "copy");
        if (!SameTimes("f", "copy")) return check;
        check = 2;
        if (File.GetUnixFileMode("copy") != Plain) return check;
        check = 3;
        if (File.ReadAllText("copy") != "hello") return check;
        check = 4;
        if (File.ReadAllText("f") != "hello" || File.GetUnixFileMode("f") != Plain) return check;

        // A source written since it was last read, so that its two times
        // differ, and each must land in its own place.
        check = 28;
        File.AppendAllText("m", " more");
        File.Copy("m", "mcopy");
        if (!SameTimes("m", "mcopy")) return check;
        check = 29;
        if (File.GetLastWriteTimeUtc("mcopy") == File.GetLastAccessTimeUtc("mcopy")
            && File.GetLastWriteTimeUtc("m") != File.GetLastAccessTimeUtc("m")) return check;

        // A mode the umask would have narrowed is copied whole.
        check = 5;
        File.Copy("w", "wcopy");
        if (File.GetUnixFileMode("wcopy") != Open) return check;

        // An existing name without overwrite: refused, and untouched.
        check = 6;
        if (!Throws<IOException>(() => File.Copy("f", "g"))) return check;
        check = 7;
        if (File.ReadAllText("g") != "previous content that is longer") return check;

        // With overwrite: replaced whole, shorter than it was, with the
        // source's mode and times.
        check = 8;
        File.Copy("f", "g", true);
        if (!SameTimes("f", "g")) return check;
        check = 9;
        if (File.GetUnixFileMode("g") != Plain) return check;
        check = 10;
        if (File.ReadAllText("g") != "hello") return check;

        // An empty source.
        check = 11;
        File.Copy("empty", "emptycopy");
        if (new FileInfo("emptycopy").Length != 0) return check;

        // Through a symbolic link to the source: the copy is a regular file
        // holding the target's bytes.
        check = 12;
        File.Copy("lf", "fromlink");
        if (File.ReadAllText("fromlink") != "hello" || new FileInfo("fromlink").LinkTarget != null) return check;

        // A directory as the destination, without overwrite. (A directory as
        // the source, or as the destination with overwrite, is
        // UnauthorizedAccessException: CopyFileUnauthorized.cs.)
        check = 15;
        if (!Throws<IOException>(() => File.Copy("f", "d"))) return check;
        check = 17;
        if (!Directory.Exists("d") || !File.Exists("d/in")) return check;

        // What is not there.
        check = 18;
        if (!Throws<FileNotFoundException>(() => File.Copy("absent", "acopy"))) return check;
        check = 19;
        if (!Throws<DirectoryNotFoundException>(() => File.Copy("f", "nodir/copy"))) return check;
        check = 20;
        if (!Throws<DirectoryNotFoundException>(() => File.Copy("f/under", "ucopy"))) return check;
        check = 21;
        if (!Throws<DirectoryNotFoundException>(() => File.Copy("f", "f/under"))) return check;

        // The source onto itself, by its own name and through a link: the
        // copy's exclusive lock meets the source's shared one.
        check = 22;
        if (!Throws<IOException>(() => File.Copy("f", "f"))) return check;
        check = 23;
        if (!Throws<IOException>(() => File.Copy("f", "f", true))) return check;
        check = 24;
        if (!Throws<IOException>(() => File.Copy("lf", "f", true))) return check;
        check = 25;
        if (File.ReadAllText("f") != "hello") return check;

        // A destination someone holds open: the overwrite cannot lock it.
        check = 26;
        using (new FileStream("held", FileMode.Open, FileAccess.Read, FileShare.Read))
        {
            if (!Throws<IOException>(() => File.Copy("f", "held", true))) return check;
        }
        check = 27;
        if (File.ReadAllText("held") != "held") return check;

        return 0;
    }
}
