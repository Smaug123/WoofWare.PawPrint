using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

// Process.Start of a rooted path that names nothing executable. The BCL hands
// a rooted path to SystemNative_ForkAndExecProcess unresolved, and on .NET 10
// that shim asks access(filename, X_OK) before it creates a pipe or forks: a
// path that fails that check fails the start with the check's errno, and no
// child ever exists. The seed (see TestPureCases) holds a regular file "f",
// without execute permission, and a directory "d".
//
// Every stream is redirected, so the start does not configure the terminal
// for a child. The last rows call the shim directly, for what the BCL does not
// look at: the -1 it stores through each out-pointer. They run on .NET 10
// alone, whose shim the declaration below matches; .NET 11's takes other
// arguments, and has no access check before it forks.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ForkAndExecProcess", SetLastError = true)]
    static extern unsafe int ForkAndExecProcess(
        byte* filename, byte** argv, byte** envp, byte* cwd,
        int redirectStdin, int redirectStdout, int redirectStderr,
        int setCredentials, uint userId, uint groupId, uint* groups, int groupsLength,
        int* childPid, int* stdinFd, int* stdoutFd, int* stderrFd);

    // 0 if the shim fails a start of fileName with errno, storing -1 through
    // every out-pointer; otherwise a nonzero offset saying which part did not
    // hold.
    static unsafe int ShimFailsWith(string fileName, int errno)
    {
        byte[] name = Encoding.UTF8.GetBytes(fileName + "\0");
        // Empty argv and envp: the shim reads neither before its access check.
        byte** argv = stackalloc byte*[1];
        argv[0] = null;
        byte** envp = stackalloc byte*[1];
        envp[0] = null;
        int childPid = 7, stdinFd = 7, stdoutFd = 7, stderrFd = 7;
        fixed (byte* filename = name)
        {
            int result = ForkAndExecProcess(
                filename, argv, envp, null,
                1, 1, 1,
                0, 0, 0, null, 0,
                &childPid, &stdinFd, &stdoutFd, &stderrFd);
            if (result != -1) return 1;
        }
        if (Marshal.GetLastPInvokeError() != errno) return 2;
        if (childPid != -1) return 3;
        if (stdinFd != -1) return 4;
        if (stdoutFd != -1) return 5;
        if (stderrFd != -1) return 6;
        return 0;
    }

    static ProcessStartInfo Info(string fileName) => new ProcessStartInfo
    {
        FileName = fileName,
        Arguments = "an argument",
        RedirectStandardInput = true,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false,
    };

    // 0 if starting fileName throws a Win32Exception carrying errno and naming
    // the path; otherwise a nonzero offset saying which part did not hold.
    static int FailsWith(string fileName, int errno)
    {
        var process = new Process { StartInfo = Info(fileName) };
        try
        {
            process.Start();
            return 1;
        }
        catch (Win32Exception e)
        {
            if (e.NativeErrorCode != errno) return 2;
            if (!e.Message.Contains("'" + fileName + "'")) return 3;
        }

        // The failed start associated no process with the object.
        try
        {
            _ = process.Id;
            return 4;
        }
        catch (InvalidOperationException) { }

        return 0;
    }

    static int Main(string[] args)
    {
        int result;

        // ENOENT, ENOTDIR and EACCES have these numbers under Linux's
        // numbering and Darwin's alike.
        const int ENOENT = 2;
        const int EACCES = 13;
        const int ENOTDIR = 20;

        // Nothing at that name in an existing directory.
        result = FailsWith(Path.GetFullPath("missing"), ENOENT);
        if (result != 0) return 10 + result;

        // Nothing at that name in the root directory.
        result = FailsWith("/pawprint-executable-does-not-exist", ENOENT);
        if (result != 0) return 20 + result;

        // A missing directory on the way.
        result = FailsWith(Path.GetFullPath("absent/x"), ENOENT);
        if (result != 0) return 30 + result;

        // A regular file on the way, where a directory is needed.
        result = FailsWith(Path.GetFullPath("f/x"), ENOTDIR);
        if (result != 0) return 40 + result;

        // A regular file that exists, without execute permission. Even a
        // privileged process is refused X_OK on a file with no execute bit.
        result = FailsWith(Path.GetFullPath("f"), EACCES);
        if (result != 0) return 50 + result;

        // A directory is refused by the BCL itself, before the shim.
        try
        {
            Process.Start(Info(Path.GetFullPath("d")));
            return 60;
        }
        catch (Win32Exception) { }

        if (Environment.Version.Major == 10)
        {
            result = ShimFailsWith("/pawprint-executable-does-not-exist", ENOENT);
            if (result != 0) return 70 + result;

            result = ShimFailsWith(Path.GetFullPath("f"), EACCES);
            if (result != 0) return 80 + result;
        }

        return 0;
    }
}
