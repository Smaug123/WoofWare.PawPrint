using System;
using System.Runtime.InteropServices;
using System.Threading;

// `SystemNative_Read` and `SystemNative_Write` on sockets with no peer: a TCP
// socket fresh and listening, a UDP socket, and a Unix-domain stream socket,
// through hand-rolled P/Invokes, since CoreLib reaches a socket through
// `SystemNative_Send` and `SystemNative_Receive` instead.
//
// The rows the flavours agree on are checked first, and any failure exits with
// that check's index. Three rows then part the flavours, measured by
// socket-unconnected-transfer.c: a TCP write is EPIPE on Linux (raising SIGPIPE,
// which the runtime's startup ignores) and ENOTCONN on Darwin; a UDP write of
// more than 65535 bytes is EMSGSIZE on Linux and EDESTADDRREQ on Darwin; and a
// Unix-domain stream read is EINVAL on Linux and ENOTCONN on Darwin. The exit
// code is 0 if all three are Linux's and 100 if all three are Darwin's.
//
// Impure because which of the two it is depends on the kernel the registration
// simulates, not the host.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking", SetLastError = true)]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    // Interop.Error, as `pal_error_common.h` numbers it.
    const int PalEAGAIN = 0x10006;
    const int PalEDESTADDRREQ = 0x10011;
    const int PalEINVAL = 0x1001C;
    const int PalEMSGSIZE = 0x10023;
    const int PalENOTCONN = 0x10038;
    const int PalEPIPE = 0x10043;

    // The shim's own numbering of its arguments.
    const int AF_UNIX = 1;
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;
    const int PT_UNSPECIFIED = 0;
    const int PT_TCP = 6;
    const int PT_UDP = 17;

    static unsafe IntPtr Create(int addressFamily, int socketType, int protocolType)
    {
        IntPtr created = (IntPtr)(-1);
        if (Socket(addressFamily, socketType, protocolType, &created) != 0) return (IntPtr)(-1);
        return created;
    }

    /// The PAL error of a call that must fail, or -1 if it did not.
    static int FailedWith(int returned) => returned == -1 ? LastPalError() : -1;

    static unsafe int Main()
    {
        byte[] block = new byte[70000];
        byte* one = stackalloc byte[1];

        IntPtr tcp = Create(AF_INET, SOCK_STREAM, PT_TCP);
        if (tcp == (IntPtr)(-1)) return 1;
        if (FailedWith(Read(tcp, one, 1)) != PalENOTCONN) return 2;

        // The socket's own answer, at length zero too, where a file's write
        // would have been a no-op.
        int tcpWrite = FailedWith(Write(tcp, one, 1));
        if (FailedWith(Write(tcp, one, 0)) != tcpWrite) return 3;

        // From another thread, whose SIGPIPE Linux aims at that thread.
        int workerWrite = 0;
        Thread worker = new Thread(() =>
        {
            byte* b = stackalloc byte[1];
            workerWrite = FailedWith(Write(tcp, b, 1));
        });
        worker.Start();
        worker.Join();
        if (workerWrite != tcpWrite) return 4;

        IntPtr listener = Create(AF_INET, SOCK_STREAM, PT_TCP);
        if (listener == (IntPtr)(-1)) return 5;
        if (Listen(listener, 4) != 0) return 6;
        if (FailedWith(Read(listener, one, 1)) != PalENOTCONN) return 7;
        if (FailedWith(Write(listener, one, 1)) != tcpWrite) return 8;

        IntPtr udp = Create(AF_INET, SOCK_DGRAM, PT_UDP);
        if (udp == (IntPtr)(-1)) return 10;
        if (SetIsNonBlocking(udp, 1) != 0) return 11;
        if (FailedWith(Read(udp, one, 1)) != PalEAGAIN) return 12;
        if (Read(udp, one, 0) != 0) return 13;
        if (FailedWith(Write(udp, one, 1)) != PalEDESTADDRREQ) return 14;
        int udpLongWrite;
        fixed (byte* start = block)
        {
            udpLongWrite = FailedWith(Write(udp, start, block.Length));
        }

        IntPtr unix = Create(AF_UNIX, SOCK_STREAM, PT_UNSPECIFIED);
        if (unix == (IntPtr)(-1)) return 20;
        if (FailedWith(Write(unix, one, 1)) != PalENOTCONN) return 21;
        int unixRead = FailedWith(Read(unix, one, 1));

        if (Close(tcp) != 0) return 30;
        if (Close(listener) != 0) return 31;
        if (Close(udp) != 0) return 32;
        if (Close(unix) != 0) return 33;

        if (tcpWrite == PalEPIPE && udpLongWrite == PalEMSGSIZE && unixRead == PalEINVAL) return 0;
        if (tcpWrite == PalENOTCONN && udpLongWrite == PalEDESTADDRREQ && unixRead == PalENOTCONN) return 100;
        return 40;
    }
}
