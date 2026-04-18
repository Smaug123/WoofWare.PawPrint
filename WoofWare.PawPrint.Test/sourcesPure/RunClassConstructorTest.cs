using System;
using System.Runtime.CompilerServices;

// Exercises RuntimeHelpers.RunClassConstructor — verifies the QCall is handled
// and that it correctly triggers the .cctor.

class Tracker
{
    public static int InitCount;

    static Tracker()
    {
        InitCount = 42;
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Before RunClassConstructor, the .cctor has not run (no static access yet).
        RuntimeHelpers.RunClassConstructor(typeof(Tracker).TypeHandle);

        // After RunClassConstructor, the .cctor must have run.
        if (Tracker.InitCount != 42)
            return 1;

        // Calling again is a no-op (cctor only runs once).
        RuntimeHelpers.RunClassConstructor(typeof(Tracker).TypeHandle);

        if (Tracker.InitCount != 42)
            return 2;

        return 0;
    }
}
