using System;
using System.Runtime.InteropServices;

// `SystemNative_Access` called directly under a Darwin kernel, with uid 1000,
// which is not that flavour's default user. Darwin rejects no mode word: bits
// 3 to 8 and 22 to 31 are ignored, so a bad mode never hides a bad path. The
// caller owns everything in the seed, so its own triple decides each row.
// AccessWiringLinuxSeeded.cs is the Linux counterpart.
//
// Registered a second time with uid 0, where its first row, root's X_OK on a
// regular file, must stop the run: what Darwin answers there is unmeasured.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f (0o644), x (0o100), o (0o001), z (0o000),
// d/ (0o000), lf -> f.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Access", SetLastError = true)]
    static extern unsafe int Access(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EACCES = 0x10002;
    const int PAL_EFAULT = 0x10015;
    const int PAL_ENOENT = 0x1002D;

    const int F_OK = 0, X_OK = 1, W_OK = 2, R_OK = 4;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe int AccessPath(string name, int mode)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        fixed (byte* p = bytes) return Access(p, mode);
    }

    static int Expect(int result, int palError) => result == -1 && LastPalError() == palError ? 0 : 1;

    static unsafe int Main()
    {
        int check = 0;

        // The owner's execute bit decides X_OK on a regular file.
        check++;
        if (Expect(AccessPath("f", X_OK), PAL_EACCES) != 0) return check;
        check++;
        if (AccessPath("x", X_OK) != 0) return check;
        check++;
        if (Expect(AccessPath("o", X_OK), PAL_EACCES) != 0) return check;
        check++;
        if (Expect(AccessPath("d", X_OK), PAL_EACCES) != 0) return check;
        check++;
        if (AccessPath("lf", R_OK | W_OK) != 0) return check;

        // Bits 3 to 8 and 22 to 31 are ignored, alone and beside R_OK.
        check++;
        if (AccessPath("z", 8) != 0) return check;
        check++;
        if (AccessPath("z", 1 << 30) != 0) return check;
        check++;
        if (Expect(AccessPath("z", 8 | R_OK), PAL_EACCES) != 0) return check;

        // So the path decides: absent, or unreadable.
        check++;
        if (Expect(AccessPath("nx", 8), PAL_ENOENT) != 0) return check;
        check++;
        if (Expect(Access(null, 8), PAL_EFAULT) != 0) return check;
        check++;
        if (Expect(Access(null, F_OK), PAL_EFAULT) != 0) return check;

        return 0;
    }
}
