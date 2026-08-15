using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_ReadLink PawPrint handler directly via a P/Invoke
// stub, mirroring the shape CoreLib's own [LibraryImport] generates
// (`(byte*, byte*, int) -> int`, SetLastError). The managed path that reaches
// it is covered by the sibling LinkTargetSeeded.cs.
//
// This is a *pure* test, so it runs on the real CLR as well as under PawPrint,
// and every fact below is one both must agree on. Three of them are only
// visible from here, because the BCL wrapper hides them:
//
//  - **Truncation.** Interop.Sys.ReadLink starts with a 256-byte stackalloc and
//    doubles while `result == buffer.Length`, so truncation is how it *sizes*
//    the buffer rather than an error case. A caller only ever sees the
//    successful final iteration.
//  - **The shim's own size guard.** `bufferSize <= 0` is refused by
//    pal_io.c:1188 before readlink(2) is called at all, and it has to be:
//    measured, the raw syscall answers 0 on macOS and EINVAL on Linux for
//    `bufsiz == 0`, so this guard is the only reason the entry point is
//    cross-platform at all. That is also why this file P/Invokes the *shim*
//    rather than readlink.
//  - **The order the failures are decided in.** Each of the last three checks
//    is an input that two different checks would both reject, so which errno
//    comes back names which one ran first. Without them, a handler that
//    decided them in any order would pass.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f (a regular file), d (a directory),
// lf -> f, five -> hello5. "nx" deliberately does not exist.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ReadLink", SetLastError = true)]
    static extern unsafe int ReadLink(byte* path, byte* buffer, int bufferSize);

    // Raw kernel errno values, not the Interop.Error PAL enum: a direct
    // P/Invoke like this one skips SystemNative_ConvertErrorPlatformToPal, so
    // Marshal.GetLastSystemError reports the raw number. All four are in the
    // 1-34 band that Linux and macOS number identically, which is what lets
    // this file be differential; ELOOP and ENAMETOOLONG are not, and are
    // deliberately absent.
    const int ENOENT = 2;
    const int ENOTDIR = 20;
    const int EINVAL = 22;

    const int BufSize = 64;

    // Not a valid address, and deliberately not one PawPrint could resolve to a
    // cell even in principle: a bare integer with no managed provenance. Not
    // NULL, which pal_io.c:1185 asserts against for a positive bufferSize -- a
    // checked build of libSystem.Native would abort rather than return, and
    // this test must hold against whatever runtime the harness loads.
    static unsafe byte* Bogus => (byte*)8;

    // Every path here is ASCII, so this is the whole of the encoding. Written
    // out rather than going through Encoding.UTF8 to keep the guest's
    // dependencies to the entry point under test.
    static unsafe void Ascii(string s, byte* dest)
    {
        for (int i = 0; i < s.Length; i++)
        {
            dest[i] = (byte)s[i];
        }

        dest[s.Length] = 0;
    }

    /// Fill with a sentinel that is neither NUL nor any byte a target below
    /// contains, so "the handler wrote here" and "the handler left this alone"
    /// are distinguishable.
    static unsafe void Poison(byte* buffer)
    {
        for (int i = 0; i < BufSize; i++)
        {
            buffer[i] = (byte)'#';
        }
    }

    static unsafe bool Is(byte* buffer, string expected)
    {
        for (int i = 0; i < expected.Length; i++)
        {
            if (buffer[i] != (byte)expected[i]) return false;
        }

        // readlink does not NUL-terminate, so the byte *after* what it wrote
        // must still be the sentinel. This is the check that a handler
        // terminating its output fails.
        return buffer[expected.Length] == (byte)'#';
    }

    static unsafe int Main(string[] args)
    {
        byte* path = stackalloc byte[BufSize];
        byte* buf = stackalloc byte[BufSize];
        int check = 0;

        // A comfortable buffer: the whole target, and nothing after it.
        check = 1;
        Ascii("lf", path);
        Poison(buf);
        if (ReadLink(path, buf, 16) != 1) return check;
        check = 2;
        if (!Is(buf, "f")) return check;

        // An exact fit. Unlike getcwd there is no terminator to make room for,
        // so "exactly the target's length" succeeds rather than being one short.
        check = 3;
        Ascii("five", path);
        Poison(buf);
        if (ReadLink(path, buf, 6) != 6) return check;
        check = 4;
        if (!Is(buf, "hello5")) return check;

        // One byte over: the return value is the target's length, not the
        // buffer's, which is exactly the test CoreLib's growth loop makes to
        // decide it is finished.
        check = 5;
        Poison(buf);
        if (ReadLink(path, buf, 7) != 6) return check;
        check = 6;
        if (!Is(buf, "hello5")) return check;

        // One byte short: truncated silently, and the return value is the
        // *buffer* size. A handler answering the target's true length here
        // would send the growth loop round again forever.
        check = 7;
        Poison(buf);
        if (ReadLink(path, buf, 5) != 5) return check;
        check = 8;
        if (!Is(buf, "hello")) return check;

        // Truncated to almost nothing, so that "wrote min(len, size) bytes" and
        // "wrote the whole target" are far apart.
        check = 9;
        Poison(buf);
        if (ReadLink(path, buf, 2) != 2) return check;
        check = 10;
        if (!Is(buf, "he")) return check;

        // A regular file is not a link: EINVAL, which is the errno
        // FileSystem.ResolveLinkTarget reads back to answer null rather than
        // throw. Any other errno there becomes an exception.
        check = 11;
        Ascii("f", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, buf, 16) != -1) return check;
        check = 12;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // A directory is not a link either. Distinct from the file case
        // because the two reach the check by different inode kinds.
        check = 13;
        Ascii("d", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, buf, 16) != -1) return check;
        check = 14;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        check = 15;
        Ascii("nx", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, buf, 16) != -1) return check;
        check = 16;
        if (Marshal.GetLastSystemError() != ENOENT) return check;

        // A trailing separator demands that the final component be a
        // directory, and it is imposed *after* the link is followed -- so this
        // is not a link-that-is-a-file question but a file-that-must-be-a-
        // directory one, and the errno is ENOTDIR rather than EINVAL.
        check = 17;
        Ascii("lf/", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, buf, 16) != -1) return check;
        check = 18;
        if (Marshal.GetLastSystemError() != ENOTDIR) return check;

        // Zero size on a perfectly good link. EINVAL from the shim's own
        // guard, and note this is the case where the shim and the kernel
        // disagree: macOS's readlink answers 0 here.
        check = 19;
        Ascii("lf", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, buf, 0) != -1) return check;
        check = 20;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // ---- ordering. Each input below is one that two checks would reject
        // differently, so the errno says which ran first.

        // Size guard before the path is looked at: a null path would fault, but
        // the size is refused first. (Assert-safe: pal_io.c asserts nothing
        // about the path, and its buffer assert permits any buffer when
        // bufferSize is 0.)
        check = 21;
        Marshal.SetLastSystemError(0);
        if (ReadLink(null, buf, 0) != -1) return check;
        check = 22;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // The is-it-a-link check before the buffer is written through: an
        // unwritable buffer is irrelevant, because nothing is going to be
        // written. Measured against the host kernel, which answers EINVAL.
        check = 23;
        Ascii("f", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, Bogus, 16) != -1) return check;
        check = 24;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // Likewise resolution before the write: a path that names nothing is
        // ENOENT rather than EFAULT.
        check = 25;
        Ascii("nx", path);
        Marshal.SetLastSystemError(0);
        if (ReadLink(path, Bogus, 16) != -1) return check;
        check = 26;
        if (Marshal.GetLastSystemError() != ENOENT) return check;

        // Deliberately absent: a negative bufferSize, and a null buffer with a
        // positive one. pal_io.c asserts against both before returning EINVAL,
        // so a checked libSystem.Native would abort instead -- the same
        // exclusion SystemNativeGetCwd.cs documents. The two calls that really
        // do provoke a fault -- a null path with room to write, and a good
        // link into an unmapped buffer, both EFAULT -- live in
        // sourcesImpure/ReadLinkRawSeeded.cs, which never hands them to a real
        // kernel.

        return 0;
    }
}
