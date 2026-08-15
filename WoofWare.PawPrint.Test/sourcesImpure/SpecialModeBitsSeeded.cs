using System;
using System.IO;

// The set-user-ID, set-group-ID and sticky bits, which `PermissionBits` models
// (it is `st_mode & 0o7777`, twelve bits, not nine) and which a guest reads
// back through `UnixFileMode`.
//
// PawPrint-only, and this file is the reason `RealRuntime.validateSeedForOracle`
// refuses these bits rather than PawPrint refusing to model them: a host
// `chmod` may silently drop them — Linux drops S_ISGID when the caller is not
// in the file's group — so a differential test would be comparing the harness's
// luck rather than the two runtimes. Refusing them *only* at the oracle keeps
// the model complete and confines the problem to where it arises.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): setuid 0o4755, setgid 0o2755, sticky dir 0o1777.
class Program
{
    static int Main(string[] args)
    {
        int check = 0;

        check = 1;
        UnixFileMode setuid = File.GetUnixFileMode("setuid");
        if ((setuid & UnixFileMode.SetUser) == 0) return check;
        check = 2;
        if ((setuid & UnixFileMode.SetGroup) != 0) return check;
        check = 3;
        if ((setuid & (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute))
            != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute)) return check;

        check = 4;
        UnixFileMode setgid = File.GetUnixFileMode("setgid");
        if ((setgid & UnixFileMode.SetGroup) == 0) return check;
        check = 5;
        if ((setgid & UnixFileMode.SetUser) != 0) return check;

        // The sticky bit on a directory, which is what /tmp has.
        check = 6;
        UnixFileMode sticky = File.GetUnixFileMode("sticky");
        if ((sticky & UnixFileMode.StickyBit) == 0) return check;
        check = 7;
        if ((sticky & UnixFileMode.OtherWrite) == 0) return check;

        // The special bits do not leak into the file-type band: a setuid file
        // is still a regular file, and a sticky directory still a directory.
        check = 8;
        if (!File.Exists("setuid")) return check;
        check = 9;
        if (!Directory.Exists("sticky")) return check;

        // ...nor into a file that did not ask for them.
        check = 10;
        UnixFileMode plain = File.GetUnixFileMode("plain");
        if ((plain & (UnixFileMode.SetUser | UnixFileMode.SetGroup | UnixFileMode.StickyBit)) != 0) return check;

        return 0;
    }
}
