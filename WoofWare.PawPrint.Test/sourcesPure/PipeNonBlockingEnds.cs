using System;
using System.Reflection;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;

// The flags `SystemNative_Pipe` admits on the runtime under test, and what they
// do to the pipe's two ends.
//
//   * .NET 10's shim admits 0 and PAL_O_CLOEXEC alone, and answers EINVAL for
//     anything else before it looks at the array;
//   * .NET 11's admits any combination of PAL_O_CLOEXEC, PAL_O_NONBLOCK_READ
//     and PAL_O_NONBLOCK_WRITE, answering EINVAL for any other bit, and sets
//     O_NONBLOCK on the end each of the last two names and on no other;
//   * a non-blocking read end of an empty pipe answers EAGAIN while a writer
//     is open;
//   * .NET 11's public `SafeFileHandle.CreateAnonymousPipe`, which .NET 10
//     does not have, makes its ends close-on-exec, each non-blocking as asked.
//
// The guest is compiled against .NET 10's reference pack, so it reaches
// `CreateAnonymousPipe` by reflection.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    delegate void AnonymousPipeMaker(out SafeFileHandle readHandle, out SafeFileHandle writeHandle, bool asyncRead, bool asyncWrite);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetFD", SetLastError = true)]
    static extern int GetFD(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetIsNonBlocking", SetLastError = true)]
    static extern unsafe int GetIsNonBlocking(IntPtr fd, int* isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    // Interop.Error, as `pal_errno.h` numbers it.
    const int PalEAGAIN = 0x10006;
    const int PalEINVAL = 0x1001C;

    // Interop.Sys.PipeFlags.
    const int PalCloseOnExec = 0x0010;
    const int PalNonBlockRead = 0x0400;
    const int PalNonBlockWrite = 0x0800;

    // Interop.Sys.FileDescriptorFlags.
    const int PalFdCloseOnExec = 1;

    /// 1 if `fd` is non-blocking, 0 if not, -1 if the shim failed.
    static unsafe int IsNonBlocking(IntPtr fd)
    {
        int isNonBlocking = -1;
        if (GetIsNonBlocking(fd, &isNonBlocking) != 0) return -1;
        return isNonBlocking;
    }

    /// The shim's EINVAL for `flags`, with the array left as it was.
    static unsafe bool Refuses(int flags)
    {
        int* fds = stackalloc int[2];
        fds[0] = -7;
        fds[1] = -7;
        if (Pipe(fds, flags) != -1) return false;
        if (LastPalError() != PalEINVAL) return false;
        return fds[0] == -7 && fds[1] == -7;
    }

    /// 0 if a pipe made with `flags` has the ends they ask for; otherwise a
    /// nonzero code, offset by `offset`, naming what differed.
    static unsafe int Admits(int flags, int offset)
    {
        int* fds = stackalloc int[2];
        if (Pipe(fds, flags) != 0) return offset + 1;
        IntPtr readEnd = (IntPtr)fds[0];
        IntPtr writeEnd = (IntPtr)fds[1];

        int expectRead = (flags & PalNonBlockRead) != 0 ? 1 : 0;
        int expectWrite = (flags & PalNonBlockWrite) != 0 ? 1 : 0;
        int expectFd = (flags & PalCloseOnExec) != 0 ? PalFdCloseOnExec : 0;

        if (IsNonBlocking(readEnd) != expectRead) return offset + 2;
        if (IsNonBlocking(writeEnd) != expectWrite) return offset + 3;
        if (GetFD(readEnd) != expectFd) return offset + 4;
        if (GetFD(writeEnd) != expectFd) return offset + 5;

        if (Close(readEnd) != 0) return offset + 6;
        if (Close(writeEnd) != 0) return offset + 7;
        return 0;
    }

    static int CheckCreateAnonymousPipe(AnonymousPipeMaker make, bool asyncRead, bool asyncWrite, int offset)
    {
        make(out SafeFileHandle readHandle, out SafeFileHandle writeHandle, asyncRead, asyncWrite);
        using (readHandle)
        using (writeHandle)
        {
            IntPtr readEnd = readHandle.DangerousGetHandle();
            IntPtr writeEnd = writeHandle.DangerousGetHandle();
            if (IsNonBlocking(readEnd) != (asyncRead ? 1 : 0)) return offset + 1;
            if (IsNonBlocking(writeEnd) != (asyncWrite ? 1 : 0)) return offset + 2;
            if (GetFD(readEnd) != PalFdCloseOnExec) return offset + 3;
            if (GetFD(writeEnd) != PalFdCloseOnExec) return offset + 4;
            if (readHandle.IsAsync != asyncRead) return offset + 5;
            if (writeHandle.IsAsync != asyncWrite) return offset + 6;
        }

        return 0;
    }

    static unsafe int Main()
    {
        // Bits no runtime's shim admits.
        if (!Refuses(0x0020)) return 1;
        if (!Refuses(0x1000)) return 2;
        if (!Refuses(PalCloseOnExec | 0x0001)) return 3;
        if (!Refuses(unchecked((int)0x80000000))) return 4;

        MethodInfo create =
            typeof(SafeFileHandle).GetMethod("CreateAnonymousPipe", BindingFlags.Public | BindingFlags.Static);

        if (Environment.Version.Major == 10)
        {
            if (!Refuses(PalNonBlockRead)) return 10;
            if (!Refuses(PalNonBlockWrite)) return 11;
            if (!Refuses(PalNonBlockRead | PalNonBlockWrite)) return 12;
            if (!Refuses(PalCloseOnExec | PalNonBlockRead)) return 13;
            if (create != null) return 14;
            if (Admits(0, 100) != 0) return Admits(0, 100);
            if (Admits(PalCloseOnExec, 110) != 0) return Admits(PalCloseOnExec, 110);
            return 0;
        }

        if (!Refuses(PalNonBlockRead | 0x0020)) return 20;
        if (!Refuses(PalNonBlockWrite | 0x2000)) return 21;

        int[] admitted =
        {
            0,
            PalCloseOnExec,
            PalNonBlockRead,
            PalNonBlockWrite,
            PalNonBlockRead | PalNonBlockWrite,
            PalCloseOnExec | PalNonBlockRead,
            PalCloseOnExec | PalNonBlockWrite,
            PalCloseOnExec | PalNonBlockRead | PalNonBlockWrite,
        };

        for (int i = 0; i < admitted.Length; i++)
        {
            int code = Admits(admitted[i], 100 + 10 * i);
            if (code != 0) return code;
        }

        // The non-blocking read end of an empty pipe finds nothing while the
        // writer is open, and then reads what the blocking write end wrote.
        int* fds = stackalloc int[2];
        if (Pipe(fds, PalCloseOnExec | PalNonBlockRead) != 0) return 200;
        IntPtr readEnd = (IntPtr)fds[0];
        IntPtr writeEnd = (IntPtr)fds[1];
        byte* buffer = stackalloc byte[8];
        if (Read(readEnd, buffer, 8) != -1) return 201;
        if (LastPalError() != PalEAGAIN) return 202;
        buffer[0] = 42;
        if (Write(writeEnd, buffer, 1) != 1) return 203;
        buffer[0] = 0;
        if (Read(readEnd, buffer, 8) != 1) return 204;
        if (buffer[0] != 42) return 205;
        if (Close(readEnd) != 0) return 206;
        if (Close(writeEnd) != 0) return 207;

        if (create == null) return 300;
        var make = (AnonymousPipeMaker)Delegate.CreateDelegate(typeof(AnonymousPipeMaker), create);
        int result = CheckCreateAnonymousPipe(make, false, false, 310);
        if (result != 0) return result;
        result = CheckCreateAnonymousPipe(make, true, false, 320);
        if (result != 0) return result;
        result = CheckCreateAnonymousPipe(make, false, true, 330);
        if (result != 0) return result;
        result = CheckCreateAnonymousPipe(make, true, true, 340);
        if (result != 0) return result;

        return 0;
    }
}
