using System;
using System.ComponentModel;
using System.Diagnostics;

// Process.Start of an executable that is nowhere to be found. On Unix the BCL
// resolves a bare file name itself (Process.Unix.cs's ResolvePath: beside the
// running executable, then in the working directory, then along PATH), and
// when nothing matches it throws ENOENT before it forks anything. What it does
// reach first is the class's one-time initialisation: System.Native's
// terminal and signal handling, and the registration of its SIGCHLD callback.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    const string Missing = "pawprint-executable-does-not-exist";

    static int Main(string[] args)
    {
        int check = 0;

        var process = new Process
        {
            StartInfo = new ProcessStartInfo
            {
                FileName = Missing,
                Arguments = "ro.build.user",
                RedirectStandardOutput = true,
                UseShellExecute = false,
                CreateNoWindow = true,
            },
        };

        check = 1;
        try
        {
            process.Start();
            return check;
        }
        catch (Win32Exception e)
        {
            // ENOENT is 2 under Linux's numbering and Darwin's alike.
            check = 2;
            if (e.NativeErrorCode != 2) return check;
            // "An error occurred trying to start process '<name>' with working
            // directory '<cwd>'. No such file or directory": the working
            // directory differs between runs, so only the ends are checked.
            check = 3;
            if (!e.Message.Contains("'" + Missing + "'")) return check;
            check = 4;
            if (!e.Message.EndsWith("No such file or directory")) return check;
        }

        // The failed start associated no process with the object.
        check = 5;
        try
        {
            _ = process.Id;
            return check;
        }
        catch (InvalidOperationException) { }

        // A second start, after the one-time initialisation has run, fails
        // the same way.
        check = 6;
        try
        {
            Process.Start(Missing);
            return check;
        }
        catch (Win32Exception e)
        {
            check = 7;
            if (e.NativeErrorCode != 2) return check;
        }

        return 0;
    }
}
