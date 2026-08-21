using System;
using System.IO;
using System.Runtime.InteropServices;

// A relative path resolved from a current directory whose *ancestor* cannot be
// searched.
//
// Measured on both kernels: with the cwd at `outer/inner` and `outer` at 0o666,
// `lstat("target")` succeeds while `lstat("../inner/target")` is EACCES. A
// process holds its cwd as an open reference and does not re-walk it from the
// root, so the ancestors' modes never come into it — but climbing back out
// through one is an ordinary lookup and needs the bit like any other.
//
// PawPrint models the cwd as a *path* and re-resolves it on every relative
// lookup, so it has to resolve that path as privileged for this to work. This
// guest is the only thing that sees that: every unit test hands the walk a start
// directory directly, and the host oracle has no cwd concept at all. Remove the
// exemption and `resolveGuestPathFull` does not merely answer differently — it
// `failwith`s, because a cwd that will not resolve is a host misconfiguration
// rather than a guest error.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): outer/ (0o666) holding inner/ (0o755) holding
// target and sub/; the kernel's CurrentDirectory is /outer/inner.
class Program
{
    // Must match `Interop.Sys.FileStatus` exactly: 17 sequential fields, 120
    // bytes. See sourcesPure/SystemNativeOpen.cs, which declares the same shape.
    // Never read here — only the return code matters — but the handler checks
    // the declared layout, as it must.
    [StructLayout(LayoutKind.Sequential)]
    struct FileStatus
    {
        public int Flags;
        public int Mode;
        public uint Uid;
        public uint Gid;
        public long Size;
        public long ATime;
        public long ATimeNsec;
        public long MTime;
        public long MTimeNsec;
        public long CTime;
        public long CTimeNsec;
        public long BirthTime;
        public long BirthTimeNsec;
        public long Dev;
        public long RDev;
        public long Ino;
        public uint UserFlags;
    }

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EACCES = 0x10002;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static unsafe int StatPath(string name)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        FileStatus status;
        fixed (byte* p = bytes) return Stat(p, &status);
    }

    static int Main()
    {
        int check = 0;

        // Relative, from a cwd two levels down: the unsearchable `outer` is
        // never consulted, because the walk starts at `inner`.
        check++;
        if (StatPath("target") != 0) return check;
        check++;
        if (StatPath("sub") != 0) return check;
        check++;
        if (StatPath(".") != 0) return check;

        // ...but climbing out through `outer` and back in is a lookup in
        // `outer`, and needs its search bit.
        check++;
        if (StatPath("../inner/target") != -1) return check;
        check++;
        if (LastPalError() != PAL_EACCES) return check;

        // The same path spelled absolutely walks `outer` from the root, so it is
        // refused too — which is what says the exemption is confined to
        // resolving the cwd itself.
        check++;
        if (StatPath("/outer/inner/target") != -1) return check;
        check++;
        if (LastPalError() != PAL_EACCES) return check;

        return 0;
    }
}
