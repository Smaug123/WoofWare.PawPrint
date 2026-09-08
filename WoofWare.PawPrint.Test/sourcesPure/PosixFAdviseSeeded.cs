using System;
using System.IO;
using System.Text;

// The path that reaches `SystemNative_PosixFAdvise` in ordinary BCL code:
// `SafeFileHandle.Init` issues the hint when a file is opened with
// `FileOptions.SequentialScan` or `FileOptions.RandomAccess`.
//
// What this can assert differentially is only that the hint is *ignored*: the
// number the shim returns is the one fact the two flavours disagree about (Linux
// answers 0, Darwin ENOTSUP, having no such libc call), and the caller passes it
// to `CheckFileCall(..., ignoreNotSupported: true)`, which inspects only
// `result < 0`. So on both, an open carrying either option behaves exactly like
// an open without it. `sourcesImpure/PosixFAdviseWiring{Linux,Darwin}Seeded.cs`
// pin the returned numbers themselves.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases): `f` and `g`, each holding the five bytes "hello".
class Program
{
    static int failure = 0;

    static void Check(int index, bool ok, string what)
    {
        if (ok) return;
        Console.WriteLine($"check {index}: {what}");
        if (failure == 0) failure = index;
    }

    /// Read `name` back through a stream opened with `options`.
    static string ReadWith(string name, FileOptions options)
    {
        using var stream = new FileStream(name, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, options);
        var buffer = new byte[16];
        int read = stream.Read(buffer, 0, buffer.Length);
        return Encoding.UTF8.GetString(buffer, 0, read);
    }

    static int Main(string[] args)
    {
        Check(1, ReadWith("f", FileOptions.SequentialScan) == "hello", "sequential read");
        Check(2, ReadWith("f", FileOptions.RandomAccess) == "hello", "random-access read");
        Check(3, ReadWith("f", FileOptions.None) == "hello", "unhinted read");

        // Both at once: the BCL prefers RandomAccess, and either way the open
        // must still succeed.
        Check(4, ReadWith("f", FileOptions.SequentialScan | FileOptions.RandomAccess) == "hello", "both hints");

        // Writing through a hinted handle, which reaches the same `Init`.
        using (var stream = new FileStream("g", FileMode.Open, FileAccess.Write, FileShare.None, 4096, FileOptions.SequentialScan))
        {
            stream.Write(Encoding.UTF8.GetBytes("world"), 0, 5);
        }

        Check(5, File.ReadAllText("g") == "world", "hinted write");

        // A hinted open of a file that does not exist must still fail for its
        // own reason, rather than for anything the hint did.
        try
        {
            ReadWith("nx", FileOptions.SequentialScan);
            Check(6, false, "opening a missing file should have thrown");
        }
        catch (FileNotFoundException)
        {
        }

        return failure;
    }
}
