using System;
using System.Runtime.InteropServices;

// `SystemNative_Access` called directly, with what the BCL never passes: a
// mode word with bits above R_OK|W_OK|X_OK, X_OK itself, and an unreadable
// path. PawPrint-only, registered under a Linux kernel with uid 0, which is
// not `KernelConfig`'s default: root's X_OK on a non-directory needs an
// execute bit, and is the one question root can be refused, so a handler that
// assumed privilege or ignored the configured user fails here.
// AccessWiringDarwinSeeded.cs asks the same of a Darwin kernel, which screens
// the mode word differently.
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
    const int PAL_EINVAL = 0x1001C;
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

        // Root is granted X_OK on a regular file only when some execute bit is
        // set, in whichever triple.
        check++;
        if (Expect(AccessPath("f", X_OK), PAL_EACCES) != 0) return check;
        check++;
        if (AccessPath("x", X_OK) != 0) return check;
        check++;
        if (AccessPath("o", X_OK) != 0) return check;
        check++;
        if (Expect(AccessPath("z", X_OK), PAL_EACCES) != 0) return check;
        // Through a link, it is the target that is asked about.
        check++;
        if (Expect(AccessPath("lf", X_OK | R_OK), PAL_EACCES) != 0) return check;

        // A directory is search, which root is never refused; nor read nor
        // write.
        check++;
        if (AccessPath("d", X_OK | R_OK | W_OK) != 0) return check;
        check++;
        if (AccessPath("z", R_OK | W_OK) != 0) return check;

        // Any bit above the low three is EINVAL, ahead of the path: absent,
        // or unreadable.
        check++;
        if (Expect(AccessPath("f", 8), PAL_EINVAL) != 0) return check;
        check++;
        if (Expect(AccessPath("nx", 8), PAL_EINVAL) != 0) return check;
        check++;
        if (Expect(Access(null, 8), PAL_EINVAL) != 0) return check;
        // Nor is a pointer this interpreter could not read looked at.
        check++;
        if (Expect(Access((byte*)typeof(int).TypeHandle.Value, 8), PAL_EINVAL) != 0) return check;
        check++;
        if (Expect(Access(null, F_OK), PAL_EFAULT) != 0) return check;
        check++;
        if (Expect(AccessPath("nx", F_OK), PAL_ENOENT) != 0) return check;

        return 0;
    }
}
