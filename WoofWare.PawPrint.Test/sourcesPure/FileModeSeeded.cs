using System;
using System.IO;

// A seeded file's permission bits, read back through the BCL.
//
// This is the check `FileMetadataSeeded.cs` excludes: with no seeded mode,
// PawPrint gives every file 0644 while the oracle's host gives it whatever
// `File.WriteAllBytes` produced under the machine's umask, so
// `File.GetUnixFileMode` would not be a cross-runtime fact. It is one because
// the seed names the mode and the harness chmods the host tree to match.
//
// Every mode below is chosen so that a *wrong* implementation is visible rather
// than merely different: 0644 is the default, so it is only a control; the
// others are modes the default would never produce. 0600 clears both group and
// other; 0666 sets a bit the umask would have cleared; 0444 clears owner write;
// 0711 is a directory whose group and other may search but not list.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    static int Main(string[] args)
    {
        int check = 0;

        // The control: the mode a `umask 022` process would have created, which
        // is what an implementation ignoring the seed would answer for
        // everything.
        check = 1;
        if (File.GetUnixFileMode("default") != (UnixFileMode.UserRead | UnixFileMode.UserWrite
                                                | UnixFileMode.GroupRead | UnixFileMode.OtherRead)) return check;

        // Owner-only. An implementation that ignored the seed would answer 0644
        // here, and the group/other bits are what would give it away.
        check = 2;
        if (File.GetUnixFileMode("private") != (UnixFileMode.UserRead | UnixFileMode.UserWrite)) return check;

        // World-writable: a bit the default umask *clears*, so this fails for
        // an implementation that applies a umask to the seeded mode rather than
        // taking it verbatim.
        check = 3;
        if (File.GetUnixFileMode("shared") != (UnixFileMode.UserRead | UnixFileMode.UserWrite
                                               | UnixFileMode.GroupRead | UnixFileMode.GroupWrite
                                               | UnixFileMode.OtherRead | UnixFileMode.OtherWrite)) return check;

        // Read-only for everyone, including the owner. This says nothing
        // about whether a *write* would be refused: PawPrint enforces no
        // permissions yet, and neither runtime is asked to write here.
        check = 4;
        if (File.GetUnixFileMode("readonly") != (UnixFileMode.UserRead | UnixFileMode.GroupRead
                                                 | UnixFileMode.OtherRead)) return check;

        // Directories carry their own mode, and the default differs from a
        // file's: 0755 rather than 0644.
        check = 5;
        if (File.GetUnixFileMode("dir") != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
                                            | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
                                            | UnixFileMode.OtherRead | UnixFileMode.OtherExecute)) return check;

        // ...and a directory's mode is seedable too. 0711: searchable by group
        // and other, listable by neither.
        check = 6;
        if (File.GetUnixFileMode("narrow") != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
                                               | UnixFileMode.GroupExecute | UnixFileMode.OtherExecute)) return check;

        // The same answer through the FileInfo/DirectoryInfo cache, which is a
        // different code path: it reads the mode from a cached stat rather than
        // performing one per call.
        check = 7;
        if (new FileInfo("private").UnixFileMode != (UnixFileMode.UserRead | UnixFileMode.UserWrite)) return check;
        check = 8;
        if (new DirectoryInfo("narrow").UnixFileMode != (UnixFileMode.UserRead | UnixFileMode.UserWrite
                                                         | UnixFileMode.UserExecute | UnixFileMode.GroupExecute
                                                         | UnixFileMode.OtherExecute)) return check;

        // A file's mode does not disturb the rest of its metadata: the mode
        // lives in the same st_mode word as the file-type band, so an
        // implementation that wrote the permission bits over the whole word
        // would break these rather than the checks above.
        check = 9;
        if (!new FileInfo("private").Exists) return check;
        check = 10;
        if (new FileInfo("private").Length != 5) return check;
        check = 11;
        if (!new DirectoryInfo("narrow").Exists) return check;
        check = 12;
        if ((File.GetAttributes("narrow") & FileAttributes.Directory) == 0) return check;

        // A read-only file *is* reported as ReadOnly through the DOS-attribute
        // projection, which FileStatus derives from the mode: with no write bit
        // anywhere, the answer is unanimous and so does not depend on which
        // user is asking.
        check = 13;
        if ((File.GetAttributes("readonly") & FileAttributes.ReadOnly) == 0) return check;
        check = 14;
        if ((File.GetAttributes("default") & FileAttributes.ReadOnly) != 0) return check;

        // A directory that its owner may read and search but not write, with a
        // child inside it. The child's presence is the check: the harness must
        // create it *before* applying the mode, since afterwards the host would
        // refuse. (Reading through such a directory is fine on both runtimes;
        // only creating and deleting entries is denied.)
        check = 15;
        if (File.GetUnixFileMode("locked") != (UnixFileMode.UserRead | UnixFileMode.UserExecute
                                               | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
                                               | UnixFileMode.OtherRead | UnixFileMode.OtherExecute)) return check;
        check = 16;
        if (!File.Exists("locked/inside")) return check;
        check = 17;
        if (new FileInfo("locked/inside").Length != 6) return check;
        check = 18;
        if (File.GetUnixFileMode("locked/inside") != (UnixFileMode.UserRead | UnixFileMode.UserWrite
                                                      | UnixFileMode.GroupRead | UnixFileMode.OtherRead)) return check;

        // The *current directory* has a mode too, and it is the one thing a
        // seed cannot name: the seed is a map of entries, so it describes the
        // root's contents but not the root. PawPrint's root is whatever
        // `VirtualFileSystem.empty` gives it, and the oracle's is a scratch
        // directory the host created — 0700 on macOS. They agree only because
        // the harness normalises it.
        check = 19;
        if (File.GetUnixFileMode(".") != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
                                          | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
                                          | UnixFileMode.OtherRead | UnixFileMode.OtherExecute)) return check;

        // ...and the same answer by the absolute path a guest would compute,
        // which is a different resolution (no "." component to consume).
        check = 20;
        if (File.GetUnixFileMode(Path.GetFullPath(".")) != File.GetUnixFileMode(".")) return check;

        return 0;
    }
}
