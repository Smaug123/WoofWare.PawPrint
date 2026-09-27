using System;
using System.Runtime.InteropServices;

// A pipe made through `SystemNative_Pipe` (pal_io.c:557), and what the other
// entry points answer of its two ends: the rows the kernel library's own tests
// hold to a real kernel (TestPipeAgainstHost), seen here through the handlers'
// wiring.
//
//   * `SystemNative_Pipe` takes 0 or PAL_O_CLOEXEC and answers EINVAL for
//     anything else, before it looks at the array;
//   * the read end is the lower descriptor, and `fstat` calls both ends FIFOs
//     owned by the process;
//   * bytes come out of the read end in the order they went into the write
//     end, a read taking what is there up to its count;
//   * a non-blocking read of the empty pipe is EAGAIN while a writer is open,
//     and 0 once the last one has closed;
//   * neither end is a terminal or seekable;
//   * a non-blocking write longer than an empty pipe holds is short, and reads
//     only the bytes the pipe takes: here 65536 of 65537 offered from a
//     65536-byte array, whose missing last byte a real kernel never touches;
//   * isatty of a Unix-domain socket is 0, with ENOTTY on Linux and
//     EOPNOTSUPP on Darwin.
//
// Impure because the registration reads the kernel's pipe table at the end.
// The registration runs it under both flavours; the checks here are the ones
// the two agree on.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FStat", SetLastError = true)]
    static extern unsafe int FStat(IntPtr fd, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking", SetLastError = true)]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_IsATty", SetLastError = true)]
    static extern int IsATty(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    // Interop.Error, as `pal_errno.h` numbers it.
    const int PalEAGAIN = 0x10006;
    const int PalEINVAL = 0x1001C;
    const int PalENOTTY = 0x1003E;
    const int PalESPIPE = 0x10049;
    const int PalEOPNOTSUPP = 0x1003D;

    const int PalCloseOnExec = 0x0010;
    const int FileTypeMask = 0xF000;
    const int Fifo = 0x1000;

    static unsafe int Main()
    {
        int* fds = stackalloc int[2];
        fds[0] = -1;
        fds[1] = -1;

        // An unknown flag is the shim's own EINVAL, and the array is untouched.
        if (Pipe(fds, 0x20) != -1) return 1;
        if (LastPalError() != PalEINVAL) return 2;
        if (fds[0] != -1 || fds[1] != -1) return 3;

        if (Pipe(fds, PalCloseOnExec) != 0) return 4;
        IntPtr readEnd = (IntPtr)fds[0];
        IntPtr writeEnd = (IntPtr)fds[1];
        if (fds[0] < 0 || fds[1] <= fds[0]) return 5;

        FileStatus status;
        if (FStat(readEnd, &status) != 0) return 10;
        if ((status.Mode & FileTypeMask) != Fifo) return 11;
        if (FStat(writeEnd, &status) != 0) return 12;
        if ((status.Mode & FileTypeMask) != Fifo) return 13;

        if (IsATty(readEnd) != 0) return 20;
        if (LastPalError() != PalENOTTY) return 21;
        if (IsATty(writeEnd) != 0) return 22;
        if (LSeek(readEnd, 0, 1) != -1) return 23;
        if (LastPalError() != PalESPIPE) return 24;

        byte* hello = stackalloc byte[5] { (byte)'h', (byte)'e', (byte)'l', (byte)'l', (byte)'o' };
        byte* world = stackalloc byte[5] { (byte)'w', (byte)'o', (byte)'r', (byte)'l', (byte)'d' };
        if (Write(writeEnd, hello, 5) != 5) return 30;
        if (Write(writeEnd, world, 5) != 5) return 31;

        byte* buffer = stackalloc byte[16];
        if (Read(readEnd, buffer, 3) != 3) return 32;
        if (buffer[0] != (byte)'h' || buffer[2] != (byte)'l') return 33;
        if (Read(readEnd, buffer, 16) != 7) return 34;
        if (buffer[0] != (byte)'l' || buffer[1] != (byte)'o' || buffer[6] != (byte)'d') return 35;

        // Empty, with the writer open: a non-blocking read finds nothing.
        if (SetIsNonBlocking(readEnd, 1) != 0) return 40;
        if (Read(readEnd, buffer, 16) != -1) return 41;
        if (LastPalError() != PalEAGAIN) return 42;

        // With the last writer gone, the empty pipe is at end of file.
        if (Close(writeEnd) != 0) return 50;
        if (Read(readEnd, buffer, 16) != 0) return 51;
        if (Close(readEnd) != 0) return 52;

        // A short non-blocking write reads no further than the pipe takes.
        if (Pipe(fds, 0) != 0) return 60;
        readEnd = (IntPtr)fds[0];
        writeEnd = (IntPtr)fds[1];
        if (SetIsNonBlocking(writeEnd, 1) != 0) return 61;
        byte[] block = new byte[65536];
        fixed (byte* start = block)
        {
            if (Write(writeEnd, start, 65537) != 65536) return 62;
        }
        if (Close(writeEnd) != 0) return 63;
        if (Close(readEnd) != 0) return 64;

        // A Unix-domain socket is no terminal either, whichever errno says so.
        IntPtr socket;
        if (Socket(1, 1, 0, &socket) != 0) return 70;
        if (IsATty(socket) != 0) return 71;
        int error = LastPalError();
        if (error != PalENOTTY && error != PalEOPNOTSUPP) return 72;
        if (Close(socket) != 0) return 73;

        return 0;
    }
}
