using System;
using System.IO;

// access(2) through the BCL. `Environment.GetFolderPath` asks
// `SystemNative_Access(folder, R_OK)` of the folder it is about to return,
// unless told not to verify it, and returns "" when that fails (or, under
// `SpecialFolderOption.Create`, creates the folder and returns it).
// `SpecialFolder.UserProfile` is $HOME on both flavours' CoreLib, and the
// registration (see TestPureCases) sets HOME to "s/h", relative to the seeded
// scratch directory; each row changes what is there and reads the folder back.
//
// Only facts that hold on both flavours, for an unprivileged caller who owns
// every file, are asked. Rows that need a permission bit to refuse are skipped
// for a privileged process, which no such bit refuses.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    const string Home = "s/h";

    static string Folder(Environment.SpecialFolderOption option = Environment.SpecialFolderOption.None) =>
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile, option);

    const UnixFileMode Rwx = UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute;

    static int Main(string[] args)
    {
        int check = 0;

        // A readable directory.
        check = 1;
        if (Folder() != Home) return check;

        if (!Environment.IsPrivilegedProcess)
        {
            // Its parent may be read and written but not searched, so the
            // walk to it is refused.
            check = 2;
            File.SetUnixFileMode("s", UnixFileMode.UserRead | UnixFileMode.UserWrite);
            if (Folder() != "") return check;
            check = 3;
            File.SetUnixFileMode("s", Rwx);
            if (Folder() != Home) return check;

            // The folder itself may be written and searched but not read.
            check = 4;
            File.SetUnixFileMode(Home, UnixFileMode.UserWrite | UnixFileMode.UserExecute);
            if (Folder() != "") return check;

            // Create on a folder that exists but cannot be read: creating it
            // changes nothing, and the folder is returned.
            check = 5;
            if (Folder(Environment.SpecialFolderOption.Create) != Home) return check;

            // Readable again, without write or search.
            check = 6;
            File.SetUnixFileMode(Home, UnixFileMode.UserRead);
            if (Folder() != Home) return check;
            File.SetUnixFileMode(Home, Rwx);
        }

        // Nothing there.
        check = 7;
        Directory.Delete(Home);
        if (Folder() != "") return check;

        // Not verified, so not asked: the absent folder comes back as it is.
        check = 8;
        if (Folder(Environment.SpecialFolderOption.DoNotVerify) != Home) return check;

        // Verified and absent under Create: the folder is made and returned.
        check = 9;
        if (Folder(Environment.SpecialFolderOption.Create) != Home) return check;
        check = 10;
        if (!Directory.Exists(Home)) return check;

        // A regular file will do: access(2) does not ask what kind of thing it
        // is.
        check = 11;
        Directory.Delete(Home);
        File.WriteAllText(Home, "not a directory");
        if (Folder() != Home) return check;

        if (!Environment.IsPrivilegedProcess)
        {
            // The owner's triple alone decides: group and other may read this
            // file, and its owner may not.
            check = 12;
            File.SetUnixFileMode(Home, UnixFileMode.GroupRead | UnixFileMode.OtherRead);
            if (Folder() != "") return check;
        }

        return 0;
    }
}
