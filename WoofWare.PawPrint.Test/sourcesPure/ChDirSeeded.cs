using System;
using System.IO;

// `chdir(2)` through `Environment.CurrentDirectory`, in the rows Linux and macOS
// answer identically — which, measured, is all of them: see
// docs/probes/chdir/, whose two columns agree on every row.
//
// This is a *pure* test, so it runs on the real CLR as well as under PawPrint,
// and every claim below has to hold on both. That rules out saying anything
// about the *absolute* current directory: the oracle starts in a private temp
// directory and PawPrint starts at `KernelConfig.CurrentDirectory`, so only
// relative facts, and suffixes of the reported path, are comparable.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): d/ containing g, f at the top, and
// ld -> d.
class Program
{
    static int Main()
    {
        // Where a relative name resolves is what this syscall changes.
        if (!File.Exists("f")) return 1;
        if (File.Exists("g")) return 2;

        Environment.CurrentDirectory = "d";

        if (!File.Exists("g")) return 3;
        // "f" lives where we came from, so it must no longer be visible.
        if (File.Exists("f")) return 4;

        Environment.CurrentDirectory = "..";

        if (!File.Exists("f")) return 5;
        if (File.Exists("g")) return 6;

        // A final symlink is followed, and what is recorded is where it landed:
        // entering "ld" leaves the process in "d". Compared as a suffix, since
        // the absolute path differs between the two runtimes.
        Environment.CurrentDirectory = "ld";

        string where = Environment.CurrentDirectory;
        if (!where.EndsWith("/d", StringComparison.Ordinal)) return 7;
        if (where.EndsWith("/ld", StringComparison.Ordinal)) return 8;
        if (!File.Exists("g")) return 9;

        return 0;
    }
}
