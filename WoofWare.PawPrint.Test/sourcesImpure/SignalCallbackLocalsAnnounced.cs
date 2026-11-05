using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;

// System.Native's signal-handling thread calls a callback installed with
// SystemNative_SetPosixSignalHandler, here one whose only mention of
// System.Collections is a local. The runtime loads System.Collections when it
// compiles the callback, so an AppDomain.AssemblyLoad handler subscribed
// beforehand sees it, once.
//
// Exit codes: 0 when the handler saw System.Collections once; 7 when the
// callback never ran; otherwise 10 plus the number of times it was seen.
unsafe class Program
{
    static volatile int s_seen;
    static volatile int s_done;

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPosixSignalHandler")]
    static extern void SetHandler(delegate* unmanaged<int, int, int> handler);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPlatformSignalNumber")]
    static extern int GetPlatformSignalNumber(PosixSignal signal);

    [DllImport("libc", EntryPoint = "kill")]
    static extern int Kill(int pid, int signal);

    [UnmanagedCallersOnly]
    static int Callback(int signo, int signal)
    {
        Stack<int> local = new Stack<int>();
        local.Push(1);
        s_done = local.Pop();
        return 1;
    }

    static int Main()
    {
        // Has System.Native catch SIGCONT, so that its thread calls the callback.
        using var registration = PosixSignalRegistration.Create(PosixSignal.SIGCONT, context => context.Cancel = true);

        AppDomain.CurrentDomain.AssemblyLoad += (sender, args) =>
        {
            if (args.LoadedAssembly.GetName().Name == "System.Collections")
                s_seen++;
        };

        SetHandler(&Callback);
        Kill(Environment.ProcessId, GetPlatformSignalNumber(PosixSignal.SIGCONT));

        for (int i = 0; i < 10000 && s_done == 0; i++)
            Thread.Yield();

        if (s_done == 0)
            return 7;

        return s_seen == 1 ? 0 : 10 + s_seen;
    }
}
