using System;
using System.Net;
using System.Net.Sockets;
using System.Runtime.InteropServices;
using System.Threading;

// Bytes over a loopback connection to the guest's own listener, through the
// synchronous Socket API and through hand-rolled P/Invokes of the shim.
//
// Socket.Send reaches SystemNative_Send. Socket.Receive reaches
// SystemNative_Receive only on a socket whose Blocking is false (on a blocking
// one it goes through SystemNative_ReceiveMessage), so the receiving end is
// made non-blocking: a receive with nothing queued is WouldBlock, a peek leaves
// the bytes queued, and Available is FIONREAD. Then SystemNative_Read and
// SystemNative_Write on the sockets, which nothing in CoreLib calls; the shim's
// own screens (a null buffer, a negative length, a flag it does not convert);
// and a blocking peek and receive through SystemNative_Receive on a second
// thread, which sleep until the main thread sends.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Receive")]
    static extern unsafe int Receive(IntPtr socket, byte* buffer, int bufferLen, int flags, int* received);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Send")]
    static extern unsafe int Send(IntPtr socket, byte* buffer, int bufferLen, int flags, int* sent);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetBytesAvailable")]
    static extern unsafe int GetBytesAvailable(IntPtr socket, int* available);

    // Interop.Error, as `pal_error_common.h` numbers it.
    const int PalSuccess = 0;
    const int PalEAGAIN = 0x10006;
    const int PalEFAULT = 0x10015;
    const int PalENOTSUP = 0x1003D;

    // SocketFlags, as `pal_networking.h` numbers them.
    const int PalPeek = 0x0002;

    static bool Same(byte[] buffer, int offset, int count, byte first)
    {
        for (int i = 0; i < count; i++)
        {
            if (buffer[offset + i] != (byte)(first + i)) return false;
        }

        return true;
    }

    static byte[] Bytes(byte first, int count)
    {
        var bytes = new byte[count];
        for (int i = 0; i < count; i++) bytes[i] = (byte)(first + i);
        return bytes;
    }

    static unsafe int Main(string[] args)
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(1);

        using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect((IPEndPoint)listener.LocalEndPoint!);
        using var server = listener.Accept();
        server.Blocking = false;

        var buffer = new byte[64];

        // Nothing queued: WouldBlock, as the error code and as the exception.
        if (server.Receive(buffer, 0, buffer.Length, SocketFlags.None, out SocketError nothing) != 0) return 1;
        if (nothing != SocketError.WouldBlock) return 2;

        try
        {
            server.Receive(buffer);
            return 3;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.WouldBlock)
        {
        }

        if (server.Available != 0) return 4;

        if (client.Send(Bytes(1, 10)) != 10) return 5;

        // A real Darwin kernel's loopback delivers on a thread of its own, so
        // the bytes may not have arrived yet.
        if (!server.Poll(5_000_000, SelectMode.SelectRead)) return 6;
        if (server.Available != 10) return 32;

        // A peek answers the bytes and leaves them queued.
        if (server.Receive(buffer, 0, 4, SocketFlags.Peek) != 4) return 7;
        if (!Same(buffer, 0, 4, 1)) return 8;
        if (server.Available != 10) return 9;

        if (server.Receive(buffer) != 10) return 10;
        if (!Same(buffer, 0, 10, 1)) return 11;
        if (server.Available != 0) return 12;

        // The other way, through `SystemNative_Write` and `SystemNative_Read`.
        var payload = Bytes(40, 5);
        fixed (byte* p = payload)
        {
            if (Write(server.Handle, p, 5) != 5) return 13;
        }

        fixed (byte* p = buffer)
        {
            if (Read(client.Handle, p, buffer.Length) != 5) return 14;
        }

        if (!Same(buffer, 0, 5, 40)) return 15;

        // The shim's own screens, ahead of the descriptor and the kernel.
        int count = -7;
        fixed (byte* p = buffer)
        {
            if (Receive(client.Handle, null, 4, 0, &count) != PalEFAULT) return 16;
            if (Receive(client.Handle, p, -1, 0, &count) != PalEFAULT) return 17;
            if (Receive(client.Handle, p, 4, 0, null) != PalEFAULT) return 18;
            if (Send(client.Handle, null, 4, 0, &count) != PalEFAULT) return 19;
            if (count != -7) return 20;

            // 0x8 is no SocketFlags the shim converts.
            if (Receive(client.Handle, p, 4, 0x8, &count) != PalENOTSUP) return 21;
            if (Send(client.Handle, p, 4, 0x8, &count) != PalENOTSUP) return 22;
            if (count != -7) return 23;

            // MSG_DONTWAIT, which the shim converts too: nothing is queued.
            if (Receive(client.Handle, p, 4, 0x1000, &count) != PalEAGAIN) return 24;
            if (count != 0) return 25;
        }

        int available = -1;
        if (GetBytesAvailable(client.Handle, &available) != PalSuccess || available != 0) return 26;

        // A blocking peek and then a receive, on a thread of their own, through
        // the client, which nothing has made non-blocking: each sleeps until
        // the main thread sends.
        int peeked = -1;
        int peekError = -1;
        int received = -1;
        int receiveError = -1;
        var got = new byte[8];
        var peekStarted = new ManualResetEventSlim();
        var worker = new Thread(() =>
        {
            fixed (byte* p = got)
            {
                int n;
                peekStarted.Set();
                peekError = Receive(client.Handle, p, 8, PalPeek, &n);
                peeked = n;
                receiveError = Receive(client.Handle, p, 8, 0, &n);
                received = n;
            }
        });

        worker.Start();
        peekStarted.Wait();
        Thread.Sleep(50);

        fixed (byte* p = payload)
        {
            int n;
            if (Send(server.Handle, p, 3, 0, &n) != PalSuccess || n != 3) return 27;
        }

        worker.Join();
        if (peekError != PalSuccess || peeked != 3) return 28;
        if (receiveError != PalSuccess || received != 3) return 29;
        if (!Same(got, 0, 3, 40)) return 30;

        // The peer's close: end of file.
        server.Close();
        fixed (byte* p = buffer)
        {
            int n = -1;
            if (Receive(client.Handle, p, buffer.Length, 0, &n) != PalSuccess || n != 0) return 31;
        }

        return 0;
    }
}
