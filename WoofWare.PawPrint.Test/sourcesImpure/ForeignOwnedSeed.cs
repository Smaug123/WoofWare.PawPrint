using System;
using System.IO;
using System.Runtime.InteropServices;

// An unprivileged process on a filesystem it does not own: a root-owned "/" and
// "/etc", a root-owned /etc/passwd, and three files of root's whose groups the
// process is in by its effective group, in by a supplementary group, and not in.
//
// PawPrint-only: the oracle materialises a seed as whoever runs the tests and
// cannot give it another owner, and the answers here depend on the configured
// user and groups (see TestImpureCases).
//
// Errnos come from the raw shim rather than from a caught exception: CoreLib's
// UnauthorizedAccessException for EACCES carries an inner IOException whose
// message needs SystemNative_StrErrorR, which does not exist, so a managed row
// that throws would abort the run rather than fail it.
//
// The effective group and the reported groups are written to stdout, as
// little-endian uint32s (the effective group, the count, then each group), so
// that the registration asserts the exact list, which is sorted on Linux.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases.foreignOwnedSeed): / is 0:0 and 0755; etc/ is 0:0
// and 0755, holding
//   passwd       0:0     0644
//   egid-group   0:1500  0464   the process's effective group
//   extra-group  0:3000  0464   one of the process's supplementary groups
//   other-group  0:5000  0464   a group the process is not in
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetEGid")]
    static extern uint GetEGid();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetGroups", SetLastError = true)]
    static extern unsafe int GetGroups(int ngroups, uint* groups);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Sys.OpenFlags, the PAL's own numbering.
    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;
    const int O_CREAT = 0x0020;

    const int PAL_EACCES = 0x10002;
    const int PAL_EINVAL = 0x1001C;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static byte[] CString(string s)
    {
        byte[] bytes = new byte[s.Length + 1];
        for (int i = 0; i < s.Length; i++) bytes[i] = (byte)s[i];
        return bytes;
    }

    static unsafe IntPtr OpenPath(string path, int flags, int mode)
    {
        fixed (byte* p = CString(path)) return Open(p, flags, mode);
    }

    static unsafe int MkDirPath(string path, int mode)
    {
        fixed (byte* p = CString(path)) return MkDir(p, mode);
    }

    static unsafe bool WriteUInt32s(uint[] values)
    {
        byte[] bytes = new byte[4 * values.Length];
        for (int i = 0; i < values.Length; i++)
        {
            bytes[4 * i] = (byte)(values[i] & 0xFF);
            bytes[4 * i + 1] = (byte)((values[i] >> 8) & 0xFF);
            bytes[4 * i + 2] = (byte)((values[i] >> 16) & 0xFF);
            bytes[4 * i + 3] = (byte)((values[i] >> 24) & 0xFF);
        }

        fixed (byte* p = bytes) return Write((IntPtr)1, p, bytes.Length) == bytes.Length;
    }

    static bool IsReadOnly(string path) => (File.GetAttributes(path) & FileAttributes.ReadOnly) != 0;

    static unsafe int Main()
    {
        int check = 0;

        // Other's bits let anyone read /etc/passwd...
        check++;
        IntPtr readable = OpenPath("/etc/passwd", O_RDONLY, 0);
        if (readable == (IntPtr)(-1)) return check;
        check++;
        if (Close(readable) != 0) return check;
        check++;
        if (File.ReadAllText("/etc/passwd") != "root:x:0:0::/root:/bin/sh\n") return check;

        // ...and nobody but root write it.
        check++;
        if (OpenPath("/etc/passwd", O_WRONLY, 0) != (IntPtr)(-1)) return check;
        check++;
        if (LastPalError() != PAL_EACCES) return check;

        // Nor create anything in a directory root owns at 0755, the root
        // directory included.
        foreach (string created in new[] { "/created", "/etc/created" })
        {
            check++;
            if (OpenPath(created, O_WRONLY | O_CREAT, 0x1A4 /* 0o644 */) != (IntPtr)(-1)) return check;
            check++;
            if (LastPalError() != PAL_EACCES) return check;
        }

        check++;
        if (MkDirPath("/newdir", 0x1ED /* 0o755 */) != -1) return check;
        check++;
        if (LastPalError() != PAL_EACCES) return check;
        check++;
        if (File.Exists("/created") || File.Exists("/etc/created") || Directory.Exists("/newdir")) return check;

        // FileAttributes.ReadOnly, which CoreLib decides from the triple that
        // applies to this process (FileStatus.IsModeReadOnlyCore). For 0644 the
        // owner's triple is writable and the group's and other's are not, so a
        // process that does not own the file asks whether it is in the file's
        // group: GetEGid first, then GetGroups. It is not, so other's r-- makes
        // it read-only.
        check++;
        if (!IsReadOnly("/etc/passwd")) return check;

        // 0464: read-only to the owner and to other, writable to the group, so
        // the answer is whether the process is in the file's group. By its
        // effective group, which GetEGid answers alone...
        check++;
        if (IsReadOnly("/etc/egid-group")) return check;
        // ...by a supplementary group, which only GetGroups reports...
        check++;
        if (IsReadOnly("/etc/extra-group")) return check;
        // ...and not at all, when other's r-- applies.
        check++;
        if (!IsReadOnly("/etc/other-group")) return check;

        // The raw entry points, as a guest's own P/Invoke sees them.
        uint egid = GetEGid();

        check++;
        int count = GetGroups(0, null);
        if (count < 1) return check;

        // A buffer one short of the list is EINVAL, which is how CoreLib's
        // IsMemberOfGroup learns to grow its buffer.
        uint* shortBuffer = stackalloc uint[count - 1 > 0 ? count - 1 : 1];
        check++;
        if (GetGroups(count - 1, shortBuffer) != -1) return check;
        check++;
        if (LastPalError() != PAL_EINVAL) return check;

        uint[] groups = new uint[count];
        check++;
        fixed (uint* g = groups)
        {
            if (GetGroups(count, g) != count) return check;
        }

        uint[] report = new uint[2 + count];
        report[0] = egid;
        report[1] = (uint)count;
        Array.Copy(groups, 0, report, 2, count);
        check++;
        if (!WriteUInt32s(report)) return check;

        return 0;
    }
}
