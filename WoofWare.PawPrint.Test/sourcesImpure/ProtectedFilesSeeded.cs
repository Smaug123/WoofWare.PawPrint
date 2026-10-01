using System;
using System.Runtime.InteropServices;

// Another user's link and file in a sticky world-writable /tmp, opened by an
// unprivileged process, under whichever fs.protected_* sysctls the
// registration configures (see TestImpureCases).
//
// PawPrint-only: the oracle materialises a seed as whoever runs the tests and
// cannot give it another owner, and the answers here depend on the configured
// sysctls.
//
// Each open's answer is written to stdout as a little-endian uint32: 0 if it
// succeeded, or the PAL's error number if it failed, so that the registration
// asserts which opens each configuration refuses. Errnos come from the raw
// shim rather than from a caught exception, as in ForeignOwnedSeed.cs.
//
// Seed (see TestImpureCases.protectedFilesSeed): / is 0:0 and 0755; etc/ is
// 0:0 and 0755 holding passwd (0:0, 0644); tmp/ is 0:0 and 01777, holding
//   theirs   2000:2000  0666   a regular file
//   link     2000:2000         a symbolic link to /etc/passwd
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Sys.OpenFlags, the PAL's own numbering.
    const int O_RDONLY = 0x0000;
    const int O_CREAT = 0x0020;
    const int O_NOFOLLOW = 0x0200;

    static byte[] CString(string s)
    {
        byte[] bytes = new byte[s.Length + 1];
        for (int i = 0; i < s.Length; i++) bytes[i] = (byte)s[i];
        return bytes;
    }

    // 0 if the open succeeded (closing what it opened), else the PAL error.
    static unsafe uint Answer(string path, int flags)
    {
        IntPtr fd;
        fixed (byte* p = CString(path)) fd = Open(p, flags, 0x1A4 /* 0o644 */);
        if (fd == (IntPtr)(-1)) return (uint)ConvertErrorPlatformToPal(Marshal.GetLastSystemError());
        return Close(fd) == 0 ? 0u : 0xFFFFFFFFu;
    }

    static unsafe int Main()
    {
        uint[] answers =
        {
            // Following the link: fs.protected_symlinks.
            Answer("/tmp/link", O_RDONLY),
            // Its target, reached directly.
            Answer("/etc/passwd", O_RDONLY),
            // The file, opened without O_CREAT, then with: fs.protected_regular.
            Answer("/tmp/theirs", O_RDONLY),
            Answer("/tmp/theirs", O_RDONLY | O_CREAT),
            // The link left unfollowed by an O_CREAT open: screened whatever
            // the sysctls say, where it would otherwise be ELOOP.
            Answer("/tmp/link", O_RDONLY | O_CREAT | O_NOFOLLOW),
        };

        byte[] bytes = new byte[4 * answers.Length];
        for (int i = 0; i < answers.Length; i++)
        {
            bytes[4 * i] = (byte)(answers[i] & 0xFF);
            bytes[4 * i + 1] = (byte)((answers[i] >> 8) & 0xFF);
            bytes[4 * i + 2] = (byte)((answers[i] >> 16) & 0xFF);
            bytes[4 * i + 3] = (byte)((answers[i] >> 24) & 0xFF);
        }

        fixed (byte* p = bytes)
        {
            if (Write((IntPtr)1, p, bytes.Length) != bytes.Length) return 1;
        }

        return 0;
    }
}
