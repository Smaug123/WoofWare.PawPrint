using System;
using System.Runtime.InteropServices;
using System.Threading;

// The main thread accepts on a blocking listener with nothing queued, so its
// accept sleeps. A second thread connects, and sends the process a SIGTERM,
// which a PosixSignalRegistration handler catches and cancels. Under some
// schedules both arrive before the accept finishes. Under Darwin the signal
// beats the connection, so the accept is interrupted and restarts once the
// handler has run; under Linux the connection beats the signal, so the accept
// takes it with the signal still to deliver.
unsafe class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    static extern int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept")]
    static extern int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    static extern int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    [DllImport("libc", EntryPoint = "kill")]
    static extern int Kill(int pid, int signo);

    static IntPtr Address = Marshal.AllocHGlobal(16);

    static int Main(string[] args)
    {
        using var handled = new ManualResetEventSlim(false);

        using var registration = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                context.Cancel = true;
                handled.Set();
            });

        IntPtr listener;
        if (Socket(2, 1, 6, &listener) != 0) return 1;
        byte* addr = (byte*)Address;
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, 2);
        SetIPv4Address(addr, 16, 0x0100007F);
        SetPort(addr, 16, 0);
        if (Bind(listener, 6, addr, 16) != 0) return 2;
        if (Listen(listener, 8) != 0) return 3;
        int addressLength = 16;
        if (GetSockName(listener, addr, &addressLength) != 0) return 4;

        int killed = -1;
        int connected = -1;
        var other = new Thread(() =>
        {
            Thread.Sleep(50);
            IntPtr client;
            if (Socket(2, 1, 6, &client) == 0) connected = Connect(client, (byte*)Address, 16);
            // SIGTERM is 15 on both Linux and Darwin.
            killed = Kill(Environment.ProcessId, 15);
        });
        other.Start();

        byte* peer = stackalloc byte[16];
        int peerLength = 16;
        IntPtr accepted;
        int result = Accept(listener, peer, &peerLength, &accepted);

        other.Join();
        if (killed != 0) return 5;
        if (connected != 0) return 6;
        if (result != 0) return 7;
        if ((long)accepted <= 0) return 8;
        if (peerLength != 16) return 9;
        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 10;
        return 0;
    }
}
