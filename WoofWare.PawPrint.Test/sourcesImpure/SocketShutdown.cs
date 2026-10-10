using System;
using System.Net;
using System.Net.Sockets;
using System.Runtime.InteropServices;

// Socket.Shutdown on loopback connections to the guest's own listener, as both
// flavours' real runtimes agree on it.
//
// First pair: the client shuts both sides before anything reaches it; the
// server then reads end of file, the client's send throws with
// SocketError.Shutdown and its receive answers 0, and neither a second
// shutdown of the client nor the server's shutdown of both sides throws
// (Darwin answers ENOTCONN to a side already shut, which .NET ignores on a
// socket that was connected). Second pair: the server shuts its send side
// alone; the client reads end of file, and can still send to the server,
// whose own send throws; the client's shutdown of its send side then gives
// the server end of file. Then the shim's own screen: a SocketShutdown value
// it does not convert is EINVAL, ahead of the descriptor.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Shutdown")]
    static extern int Shutdown(IntPtr socket, int socketShutdown);

    // Interop.Error, as `pal_error_common.h` numbers it.
    const int PalEBADF = 0x10008;
    const int PalEINVAL = 0x1001C;

    static (Socket client, Socket server) Pair(Socket listener)
    {
        var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect((IPEndPoint)listener.LocalEndPoint!);
        var server = listener.Accept();
        // Socket.Receive reaches SystemNative_Receive only on a non-blocking
        // socket.
        client.Blocking = false;
        server.Blocking = false;
        return (client, server);
    }

    // Waits for something to read, as a real Darwin kernel's loopback
    // delivers on a thread of its own, then reads.
    static int ReceiveAfterPoll(Socket socket, byte[] buffer)
    {
        if (!socket.Poll(5_000_000, SelectMode.SelectRead)) return -1;
        return socket.Receive(buffer);
    }

    static bool SendThrowsShutdown(Socket socket)
    {
        try
        {
            socket.Send(new byte[] { 1, 2, 3 });
            return false;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.Shutdown)
        {
            return true;
        }
    }

    static int Main(string[] args)
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(2);

        var buffer = new byte[64];

        {
            var (client, server) = Pair(listener);
            using (client)
            using (server)
            {
                if (client.Send(new byte[] { 10, 11, 12, 13, 14 }) != 5) return 1;
                if (ReceiveAfterPoll(server, buffer) != 5) return 2;
                if (buffer[0] != 10 || buffer[4] != 14) return 3;

                client.Shutdown(SocketShutdown.Both);

                // The client's FIN: end of file.
                if (ReceiveAfterPoll(server, buffer) != 0) return 4;

                // Its send side is shut: EPIPE, which raises no SIGPIPE in a
                // .NET process, and its receive side answers 0, never
                // WouldBlock.
                if (!SendThrowsShutdown(client)) return 5;
                if (client.Receive(buffer) != 0) return 6;

                // Neither throws, though Darwin answers ENOTCONN to both.
                client.Shutdown(SocketShutdown.Both);
                server.Shutdown(SocketShutdown.Both);

                if (client.Receive(buffer) != 0) return 7;
            }
        }

        {
            var (client, server) = Pair(listener);
            using (client)
            using (server)
            {
                server.Shutdown(SocketShutdown.Send);
                if (ReceiveAfterPoll(client, buffer) != 0) return 8;

                // The client can still send, and the server still receive.
                if (client.Send(new byte[] { 20, 21, 22 }) != 3) return 9;
                if (ReceiveAfterPoll(server, buffer) != 3) return 10;
                if (buffer[0] != 20 || buffer[2] != 22) return 11;

                if (!SendThrowsShutdown(server)) return 12;

                // A send side shut after the peer's FIN arrived: both answer 0.
                client.Shutdown(SocketShutdown.Send);
                if (ReceiveAfterPoll(server, buffer) != 0) return 13;
                if (!SendThrowsShutdown(client)) return 14;

                // The shim answers EINVAL for a SocketShutdown value it does
                // not convert, before it looks at the descriptor; a value it
                // converts reaches the kernel.
                if (Shutdown(client.Handle, 3) != PalEINVAL) return 15;
                if (Shutdown(new IntPtr(-1), 3) != PalEINVAL) return 16;
                if (Shutdown(new IntPtr(-1), 2) != PalEBADF) return 17;
            }
        }

        return 0;
    }
}
