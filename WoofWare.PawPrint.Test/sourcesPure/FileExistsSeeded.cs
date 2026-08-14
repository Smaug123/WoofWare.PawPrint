using System;
using System.IO;

// Probes File.Exists / Directory.Exists against a seeded filesystem, through
// SystemNative_LStat and SystemNative_Stat.
//
// Every path here is RELATIVE: the real runtime runs in a scratch directory
// holding the materialised seed, while PawPrint puts the same seed at the root
// of its filesystem with "/" as the current directory. Relative names agree;
// absolute ones do not, and the scratch directory also holds the guest image,
// so this must never enumerate a directory.
//
// The exit code is the index of the first check that disagreed with the
// expectation, so a failure says which one rather than just "false". Zero
// means every check passed. Kept below 128, since a process exit code is eight
// bits and the oracle cannot tell 130 from a signal.
//
// The seed (see TestPureCases.seededCases):
//   f          regular file, contents "hello"
//   d/         directory, containing g (regular file)
//   lf   -> f  symlink to the regular file
//   ld   -> d  symlink to the directory
//   dang -> nx symlink to nothing
//   cyc  -> cyc symlink to itself
class Program
{
    static int check = 0;

    static bool Is(bool actual, bool expected)
    {
        check++;
        return actual == expected;
    }

    static int Main(string[] args)
    {
        // A plain regular file and a plain directory: each API says yes to its
        // own kind and no to the other.
        if (!Is(File.Exists("f"), true)) return check;
        if (!Is(Directory.Exists("f"), false)) return check;
        if (!Is(Directory.Exists("d"), true)) return check;
        if (!Is(File.Exists("d"), false)) return check;

        // Nothing there at all.
        if (!Is(File.Exists("nx"), false)) return check;
        if (!Is(Directory.Exists("nx"), false)) return check;

        // Nested, so that the walk has to cross a directory rather than
        // stopping at the first component.
        if (!Is(File.Exists("d/g"), true)) return check;

        // ENOTDIR: a path cannot continue through a regular file.
        if (!Is(File.Exists("f/x"), false)) return check;
        if (!Is(Directory.Exists("f/x"), false)) return check;

        // Symlinks. Both APIs see through a link to what it names, which is
        // why File.Exists re-stats after an LStat that reported S_IFLNK.
        if (!Is(File.Exists("lf"), true)) return check;
        if (!Is(Directory.Exists("lf"), false)) return check;
        if (!Is(Directory.Exists("ld"), true)) return check;
        if (!Is(File.Exists("ld"), false)) return check;
        if (!Is(File.Exists("ld/g"), true)) return check;

        // A dangling link exists as a link. File.Exists says TRUE here: its
        // re-stat fails and the failure path returns true, not false. Worth
        // pinning precisely because it is the counter-intuitive answer.
        if (!Is(File.Exists("dang"), true)) return check;
        if (!Is(Directory.Exists("dang"), false)) return check;

        // Trailing separators. File.Exists refuses one outright, before any
        // syscall — a control rather than a test of the resolver. Directory
        // .Exists passes it through, where it means "must be a directory", so
        // it follows a link to a directory and gives ENOTDIR through a file.
        if (!Is(File.Exists("d/"), false)) return check;
        if (!Is(Directory.Exists("d/"), true)) return check;
        if (!Is(Directory.Exists("ld/"), true)) return check;
        if (!Is(Directory.Exists("lf/"), false)) return check;
        if (!Is(Directory.Exists("f/"), false)) return check;

        // "." and ".." are resolved by the walk, not by the seed.
        if (!Is(File.Exists("./f"), true)) return check;
        if (!Is(File.Exists("d/../f"), true)) return check;
        if (!Is(Directory.Exists("d/.."), true)) return check;

        // A symlink cycle. lstat sees the link, so File.Exists takes the same
        // re-stat path as the dangling case — and the re-stat fails with ELOOP
        // rather than ENOENT, which is the interesting part: ELOOP has no
        // platform-independent errno (raw 40 on Linux, 62 on Darwin), so
        // routing it to a guest at all requires the emulated kernel to have
        // committed to a numbering.
        if (!Is(File.Exists("cyc"), true)) return check;
        if (!Is(Directory.Exists("cyc"), false)) return check;

        // Not tested here: st_size, st_uid/st_gid, st_dev/st_ino and the
        // timestamps. FileInfo.Length would reach all of the machinery but
        // takes FileStatus.HasReadOnlyFlag on the way, which calls
        // SystemNative_GetEUid — not implemented, so it crashes rather than
        // answering. StatFieldsSeeded.cs reads those fields directly instead,
        // through a hand-rolled P/Invoke; it is a PawPrint-only test because
        // the uid and the timestamps of a real file cannot agree with a
        // simulated one's.
        return 0;
    }
}
