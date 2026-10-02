using System;
using System.IO;

// The File.Copy refusals CoreLib reports as UnauthorizedAccessException: a
// directory as the source, which SafeFileHandle refuses with EACCES once its
// read-only open of it has succeeded, and a directory as the destination of an
// overwrite, whose open for writing is EISDIR. Each holds on both flavours.
//
// Made here rather than seeded, so that the real-runtime half of the parked
// suite can run it in its scratch directory.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
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

    static int Main(string[] args)
    {
        Directory.CreateDirectory("d");
        File.WriteAllText("d/in", "in");
        File.WriteAllText("f", "hello");

        int check = 1;
        if (!Throws<UnauthorizedAccessException>(() => File.Copy("d", "dcopy"))) return check;
        check = 2;
        if (File.Exists("dcopy") || Directory.Exists("dcopy")) return check;
        check = 3;
        if (!Throws<UnauthorizedAccessException>(() => File.Copy("f", "d", true))) return check;
        check = 4;
        if (!Directory.Exists("d") || File.ReadAllText("d/in") != "in") return check;

        return 0;
    }
}
