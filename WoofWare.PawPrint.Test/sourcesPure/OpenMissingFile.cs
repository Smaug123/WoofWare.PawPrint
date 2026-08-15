using System;
using System.IO;

// The managed half of SystemNative_Open: the BCL paths that are complete once
// opening can *fail*, without needing anything that reads bytes.
//
// File.ReadAllBytes on a path that does not exist never gets past the open, so
// it reaches neither FStat, FLock nor PRead. What it does reach is
// Interop.CheckIo -> GetExceptionForIoErrno, whose ENOENT and ENOTDIR arms
// build their exceptions from SR resources alone -- no strerror, and hence no
// SystemNative_StrErrorR, which is not implemented.
//
// That distinction is the whole subject here: which *exception type* comes out
// depends on the errno the handler chose, so this file is what stops the open
// handler answering a plausible-but-wrong errno. ENOENT with an existing parent
// is FileNotFoundException; ENOENT with a missing parent, and ENOTDIR, are
// DirectoryNotFoundException -- and the BCL tells those apart by calling Stat
// on the parent directory, so the two are not interchangeable.
//
// Deliberately absent: File.ReadAllBytes("d"), which should raise
// UnauthorizedAccessException. Opening a directory succeeds (see
// SystemNativeOpen.cs) and CoreLib then rejects it on the FStat, but the EACCES
// arm of GetExceptionForIoErrno builds an inner exception through
// GetIOException, which needs both SystemNative_ConvertErrorPalToPlatform and
// SystemNative_StrErrorR. Neither exists, so that guest would abort
// mid-exception-construction rather than fail an assertion. It waits for those.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f (5 bytes), d/ (a directory holding
// g), lf -> f, ld -> d. "nx" deliberately does not exist.
class Program
{
    static int Main(string[] args)
    {
        int check = 0;

        // A missing name in a directory that exists.
        check = 1;
        try
        {
            File.ReadAllBytes("nx");
            return check;
        }
        catch (FileNotFoundException) { }

        // A missing name in a directory that does not: for Windows
        // compatibility the BCL reports the *directory* as missing, which it
        // decides by stat-ing the parent.
        check = 2;
        try
        {
            File.ReadAllBytes("nx/deeper");
            return check;
        }
        catch (DirectoryNotFoundException) { }

        // A path that runs through a regular file is ENOTDIR, which is
        // DirectoryNotFoundException too -- but by a different arm, and from a
        // different errno.
        check = 3;
        try
        {
            File.ReadAllBytes("f/g");
            return check;
        }
        catch (DirectoryNotFoundException) { }

        // The same three through FileStream, which reaches the open by a
        // different managed route.
        check = 4;
        try
        {
            using (FileStream _ = new FileStream("nx", FileMode.Open, FileAccess.Read))
            {
                return check;
            }
        }
        catch (FileNotFoundException) { }

        // File.Exists must keep answering from Stat rather than by opening:
        // it is false for the missing path and true for the real one, and
        // neither answer may become an exception now that Open exists.
        check = 5;
        if (File.Exists("nx")) return check;
        check = 6;
        if (!File.Exists("f")) return check;

        return 0;
    }
}
