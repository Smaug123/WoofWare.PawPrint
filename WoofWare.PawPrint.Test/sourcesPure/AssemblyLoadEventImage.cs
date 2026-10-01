using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading;

// `AppDomain.AssemblyLoad` for images. CoreCLR raises it as the loader binds an assembly, which is
// usually while the JIT compiles the method that first names it; PawPrint interprets, so it binds
// later, at the instruction that first needs the assembly. *When* the event fires is therefore not
// asserted beyond what holds on both: by the time code after an assembly's first use runs, the
// handler has seen that assembly, exactly once, on the thread that used it. Each `Touch` method is
// kept out of line so that compiling its caller cannot bind the assembly early.
//
// Nothing here counts notifications in total, because the set of assemblies each runtime loads
// differs (see the `AppDomain.GetAssemblies()` entry in docs/divergences.md).
public class Program
{
    private static readonly object s_lock = new object ();
    private static readonly List<Assembly> s_announced = new List<Assembly> ();
    private static readonly List<int> s_threads = new List<int> ();
    private static readonly List<object> s_senders = new List<object> ();

    private static void OnLoad (object sender, AssemblyLoadEventArgs args)
    {
        lock (s_lock)
        {
            s_announced.Add (args.LoadedAssembly);
            s_threads.Add (Environment.CurrentManagedThreadId);
            s_senders.Add (sender);
        }
    }

    private static int Count (Assembly assembly)
    {
        lock (s_lock)
        {
            int count = 0;

            for (int i = 0; i < s_announced.Count; i++)
            {
                if (ReferenceEquals (s_announced[i], assembly))
                    count++;
            }

            return count;
        }
    }

    // The thread each announcement of `assembly` ran on; -1 if it was never announced.
    private static int ThreadOf (Assembly assembly)
    {
        lock (s_lock)
        {
            for (int i = 0; i < s_announced.Count; i++)
            {
                if (ReferenceEquals (s_announced[i], assembly))
                    return s_threads[i];
            }

            return -1;
        }
    }

    private static bool IsLoaded (string simpleName)
    {
        Assembly[] loaded = AppDomain.CurrentDomain.GetAssemblies ();

        for (int i = 0; i < loaded.Length; i++)
        {
            if (loaded[i].GetName ().Name == simpleName)
                return true;
        }

        return false;
    }

    private static bool Contains (Assembly[] haystack, Assembly needle)
    {
        for (int i = 0; i < haystack.Length; i++)
        {
            if (ReferenceEquals (haystack[i], needle))
                return true;
        }

        return false;
    }

    // System.Collections. True if the handler had already seen it by the line after its first use.
    [MethodImpl (MethodImplOptions.NoInlining)]
    private static bool TouchStack (out Assembly assembly)
    {
        var stack = new Stack<int> ();
        stack.Push (1);
        assembly = typeof (Stack<int>).Assembly;
        return Count (assembly) == 1 && stack.Count == 1;
    }

    // System.Collections.Specialized.
    [MethodImpl (MethodImplOptions.NoInlining)]
    private static bool TouchBitVector (out Assembly assembly)
    {
        var vector = new BitVector32 (5);
        assembly = typeof (BitVector32).Assembly;
        return Count (assembly) == 1 && vector.Data == 5;
    }

    // System.Collections.NonGeneric.
    [MethodImpl (MethodImplOptions.NoInlining)]
    private static Assembly TouchQueue ()
    {
        var queue = new Queue ();
        queue.Enqueue (1);
        return queue.Count == 1 ? typeof (Queue).Assembly : null;
    }

    public static int Main (string[] args)
    {
        // Each assembly must be loaded only after the subscription, or the test proves nothing.
        if (IsLoaded ("System.Collections"))
            return 1;
        if (IsLoaded ("System.Collections.Specialized"))
            return 2;
        if (IsLoaded ("System.Collections.NonGeneric"))
            return 3;

        int mainThread = Environment.CurrentManagedThreadId;

        AppDomain.CurrentDomain.AssemblyLoad += OnLoad;

        Assembly collections;
        if (!TouchStack (out collections))
            return 4;
        if (Count (collections) != 1)
            return 5;
        if (ThreadOf (collections) != mainThread)
            return 6;

        // CoreCLR raises the event on the thread that loaded the assembly.
        int workerThread = -1;
        bool workerSawIt = false;
        Assembly specialized = null;
        var worker = new Thread (() =>
        {
            workerThread = Environment.CurrentManagedThreadId;
            workerSawIt = TouchBitVector (out specialized);
        });
        worker.Start ();
        worker.Join ();

        if (!workerSawIt)
            return 7;
        if (Count (specialized) != 1)
            return 8;
        if (ThreadOf (specialized) != workerThread)
            return 9;

        Assembly[] loaded = AppDomain.CurrentDomain.GetAssemblies ();

        lock (s_lock)
        {
            for (int i = 0; i < s_announced.Count; i++)
            {
                Assembly announced = s_announced[i];

                // `RaiseLoadingAssemblyEvent` returns early for corelib.
                if (ReferenceEquals (announced, typeof (object).Assembly))
                    return 10;

                // `GetExposedObject()`, so the RuntimeAssembly every other route reports.
                if (announced.GetType ().FullName != "System.Reflection.RuntimeAssembly")
                    return 11;

                if (!Contains (loaded, announced))
                    return 12;

                // `OnAssemblyLoad` invokes the event with `AppDomain.CurrentDomain` as its sender.
                if (!ReferenceEquals (s_senders[i], AppDomain.CurrentDomain))
                    return 13;

                for (int j = i + 1; j < s_announced.Count; j++)
                {
                    if (ReferenceEquals (announced, s_announced[j]))
                        return 14;
                }
            }
        }

        // With its only subscriber gone the event's field is null again, and CoreCLR then runs no
        // managed code for a load at all.
        AppDomain.CurrentDomain.AssemblyLoad -= OnLoad;

        int before;
        lock (s_lock)
        {
            before = s_announced.Count;
        }

        Assembly nonGeneric = TouchQueue ();
        if (nonGeneric == null)
            return 15;
        if (Count (nonGeneric) != 0)
            return 16;

        lock (s_lock)
        {
            if (s_announced.Count != before)
                return 17;
        }

        return 0;
    }
}
