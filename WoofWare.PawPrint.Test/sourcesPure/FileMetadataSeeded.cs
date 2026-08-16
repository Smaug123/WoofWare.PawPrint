using System;
using System.IO;

// Probes FileInfo.Length and the FileAttributes surface against a seeded
// filesystem. Both reach SystemNative_LStat and SystemNative_Stat as
// FileExistsSeeded.cs does, but they also consult FileStatus.HasReadOnlyFlag,
// which is what brings SystemNative_GetEUid into the picture: whenever the
// user, group and other read-only verdicts are not unanimous — 0o644 and 0o755
// are both such modes — IsModeReadOnlyCore compares the file's owner against
// the effective uid (FileStatus.Unix.cs:106). Without that entry point,
// every check here aborts the interpreter.
//
// Every path is RELATIVE: the real runtime runs in a scratch directory holding
// the materialised seed, while PawPrint puts the same seed at the root of its
// filesystem with "/" as the current directory. Relative names agree; absolute
// ones do not, and the scratch directory also holds the guest image, so this
// must never enumerate a directory.
//
// Deliberately absent: File.GetUnixFileMode, and anything reading st_uid,
// st_gid or a timestamp. The oracle materialises the seed with
// File.WriteAllBytes under whatever umask and identity the suite runs as,
// whereas PawPrint's files are 0o644 owned by the configured kernel identity —
// so those are not cross-runtime facts and belong in sourcesImpure (see
// StatFieldsSeeded.cs). ReadOnly survives the same objection only because it is
// clear for 0o644, 0o600 and 0o666 alike, so no plausible umask splits it.
//
// The exit code is the index of the first check that disagreed with the
// expectation, so a failure says which one rather than just "false". Zero means
// every check passed. Kept below 128, since a process exit code is eight bits.
//
// The seed (see TestPureCases.seededCases):
//   f          regular file, contents "hello"   (5 bytes)
//   d/         directory, containing g          ("nested", 6 bytes)
//   lf   -> f  symlink to the regular file
//   ld   -> d  symlink to the directory
//   dang -> nx symlink to nothing
//   .hidden    regular file, contents "x"
class Program
{
    static int check = 0;

    static bool Is(bool condition)
    {
        check++;
        return condition;
    }

    static int Main(string[] args)
    {
        // ---- lengths ----
        if (!Is(new FileInfo("f").Length == 5)) return check;
        if (!Is(new FileInfo("d/g").Length == 6)) return check;

        // A symlink's Length is the length of its *target string*, not of the
        // file it names: RefreshCaches keeps the lstat result and stats the
        // target only to patch Mode and decide directory-ness
        // (FileStatus.Unix.cs:534-546). So "lf" is 1 and "dang" is 2, even
        // though "lf" resolves to a five-byte file.
        if (!Is(new FileInfo("lf").Length == 1)) return check;
        if (!Is(new FileInfo("dang").Length == 2)) return check;

        // ---- existence, through the FileInfo/DirectoryInfo caches rather
        // than through the static File.Exists that FileExistsSeeded.cs uses ----
        if (!Is(new FileInfo("f").Exists)) return check;
        if (!Is(!new FileInfo("d").Exists)) return check;
        if (!Is(!new FileInfo("nx").Exists)) return check;
        if (!Is(new DirectoryInfo("d").Exists)) return check;

        // ---- attributes ----
        // A plain file has no attribute bits at all, which GetAttributes
        // reports as Normal rather than as zero (FileStatus.Unix.cs:215).
        FileAttributes f = File.GetAttributes("f");
        if (!Is(f == FileAttributes.Normal)) return check;

        FileAttributes d = File.GetAttributes("d");
        if (!Is((d & FileAttributes.Directory) != 0)) return check;
        if (!Is((d & FileAttributes.ReparsePoint) == 0)) return check;

        // A symlink is a reparse point whichever kind of thing it names, and
        // Directory follows the link.
        FileAttributes lf = File.GetAttributes("lf");
        if (!Is((lf & FileAttributes.ReparsePoint) != 0)) return check;
        if (!Is((lf & FileAttributes.Directory) == 0)) return check;

        FileAttributes ld = File.GetAttributes("ld");
        if (!Is((ld & FileAttributes.ReparsePoint) != 0)) return check;
        if (!Is((ld & FileAttributes.Directory) != 0)) return check;

        // A broken link exists as far as GetAttributes is concerned: the
        // failing Stat leaves _state at InitializedExistsBrokenLink, which
        // EntryExists accepts.
        FileAttributes dang = File.GetAttributes("dang");
        if (!Is((dang & FileAttributes.ReparsePoint) != 0)) return check;
        if (!Is((dang & FileAttributes.Directory) == 0)) return check;

        // Hidden on Unix is a leading dot in the name, decided in managed code
        // before any syscall (IsNameHidden). The UF_HIDDEN flag PawPrint always
        // reports as zero is the *other* half of that test, and only macOS has
        // it at all.
        FileAttributes hidden = File.GetAttributes(".hidden");
        if (!Is((hidden & FileAttributes.Hidden) != 0)) return check;
        if (!Is((f & FileAttributes.Hidden) == 0)) return check;

        // The bit that actually depends on the effective uid.
        if (!Is((f & FileAttributes.ReadOnly) == 0)) return check;
        if (!Is((d & FileAttributes.ReadOnly) == 0)) return check;

        // The instance properties agree with the static ones.
        if (!Is(new DirectoryInfo("d").Attributes == d)) return check;
        if (!Is(new FileInfo("f").Attributes == f)) return check;

        // ---- absent ----
        // Length and GetAttributes throw where Exists merely answers false.
        try
        {
            long _ = new FileInfo("nx").Length;
            check++;
            return check;
        }
        catch (FileNotFoundException) { check++; }

        try
        {
            FileAttributes _ = File.GetAttributes("nx");
            check++;
            return check;
        }
        catch (FileNotFoundException) { check++; }

        return 0;
    }
}
