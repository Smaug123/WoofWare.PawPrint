using System;
using System.Runtime.InteropServices;

// `SystemNative_GetFileSystemType` for every kind of descriptor PawPrint's
// table holds, under the Darwin flavour.
//
// Not differential, and it could not be: the number this native reports for a
// *file* is a property of whichever mount the oracle's scratch directory
// happens to be on — ext4 in CI, APFS on a Mac, overlayfs in a container — so
// the real runtime would answer with a fact about the machine that ran it.
// PawPrint's answer is `KernelConfig.FileSystemType`, which is a choice the
// host makes. `sourcesPure/WriteSeeded.cs` and
// `sourcesPure/FlockContentionSeeded.cs` carry what *is* portable: that the
// reported filesystem is one CoreCLR will take a shared lock on, and hence that
// `File.WriteAllBytes` works.
//
// Darwin's `fstatfs(2)` fails outright for every object that is not on a
// filesystem — both ends of a pipe, an AF_INET socket, an AF_UNIX socket and a
// kqueue all answer EINVAL — where Linux succeeds and names a pseudo-filesystem
// for each. That is the whole of the divergence; see GetFileSystemTypeLinux.cs
// for the same checks in the same order.
//
// The Linux and Darwin files are deliberately the same checks in the same order,
// so that diffing them shows exactly the flavour column and nothing else.
//
// errno is read via `Marshal.GetLastSystemError`: with a raw `DllImport` there
// is no generated stub to copy it to `GetLastPInvokeError`. CoreLib's own
// declaration of this native has no `SetLastError` at all, so nothing in the
// BCL ever reads these numbers — but a guest that asks for them sees what the
// kernel left behind, and that is what these rows pin.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f = "hello", d/ a directory.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetFileSystemType", SetLastError = true)]
    static extern uint GetFileSystemType(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* created);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseSocketEventPort")]
    static extern int CloseSocketEventPort(IntPtr port);

    const int PAL_SUCCESS = 0;

    const int O_RDONLY = 0x0000;

    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;

    // Darwin numbering, which is what PawPrint reports under this flavour. These
    // two agree with Linux's; ELOOP and ENAMETOOLONG are the ones that do not.
    const int EBADF = 9;
    const int EINVAL = 22;

    // The filesystem PawPrint's mount claims to be under this flavour's default,
    // spelled as CoreLib's `Interop.Sys.UnixFileSystemTypes` spells it.
    const uint APFS = 0x1A;


    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte* path = stackalloc byte[32];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, flags, 0x1B6 /* 0o666 */);
    }

    // A descriptor's answer together with the errno it left behind. The pair
    // matters: this native folds every failure to 0, so a row asserting only
    // the 0 could not tell "no such descriptor" from "not on a filesystem".
    static bool Answers(IntPtr fd, uint expected, int expectedErrno)
    {
        Marshal.SetLastSystemError(0);
        if (GetFileSystemType(fd) != expected) return false;
        return Marshal.GetLastSystemError() == expectedErrno;
    }

    static unsafe int Main(string[] args)
    {
        int check;

        // A regular file is on the mount, and reports the mount's type.
        check = 1;
        IntPtr f = OpenPath("f", O_RDONLY);
        if (f == new IntPtr(-1)) return check;
        check = 2;
        if (!Answers(f, APFS, 0)) return check;

        // A directory is on the same mount and answers identically. This is not
        // a special case in the model and must not become one: `fstatfs` differs
        // from `ftruncate`, which refuses a directory descriptor.
        check = 3;
        IntPtr d = OpenPath("d", O_RDONLY);
        if (d == new IntPtr(-1)) return check;
        check = 4;
        if (!Answers(d, APFS, 0)) return check;

        Close(f);
        Close(d);

        // A closed descriptor is no longer held, so it answers as an unknown one
        // rather than keeping the mount's answer.
        check = 5;
        if (!Answers(f, 0, EBADF)) return check;

        // An fd the process never held. EBADF on both flavours, and the PAL
        // reports the failure as 0 rather than as -1.
        check = 6;
        if (!Answers(new IntPtr(4242), 0, EBADF)) return check;

        // The standard streams, which PawPrint models as pipes. Darwin's `fstatfs` refuses one.
        check = 7;
        if (!Answers(new IntPtr(0), 0, EINVAL)) return check;
        check = 8;
        if (!Answers(new IntPtr(1), 0, EINVAL)) return check;
        check = 9;
        if (!Answers(new IntPtr(2), 0, EINVAL)) return check;

        // A socket. Refused the same way, and with the same errno.
        check = 10;
        IntPtr sock;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &sock) != PAL_SUCCESS) return check;
        check = 11;
        if (!Answers(sock, 0, EINVAL)) return check;
        Close(sock);

        // A socket event port, which is an anonymous kernel object rather than
        // anything on a filesystem. A kqueue is refused too.
        check = 12;
        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return check;
        check = 13;
        if (!Answers(port, 0, EINVAL)) return check;
        CloseSocketEventPort(port);

        // A successful call leaves errno exactly as it was, as `fstatfs` does:
        // it is only ever *set* on failure. Without this, a handler that cleared
        // errno on the way out would pass every row above.
        check = 14;
        IntPtr g = OpenPath("f", O_RDONLY);
        if (g == new IntPtr(-1)) return check;
        check = 15;
        Marshal.SetLastSystemError(4242);
        GetFileSystemType(g);
        if (Marshal.GetLastSystemError() != 4242) return check;
        Close(g);

        return 0;
    }
}
