using System;
using System.Runtime.CompilerServices;

// Exercises RuntimeHelpers.RunClassConstructor — verifies the QCall is handled
// and that it correctly triggers the .cctor.
// The .cctor writes to a separate type (SideChannel) so we can observe the
// effect without accessing Tracker's statics (which would itself trigger the cctor).

static class SideChannel
{
    public static int CctorRanCount;
}

class Tracker
{
    public static int InitCount;

    static Tracker()
    {
        InitCount = 42;
        SideChannel.CctorRanCount++;
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Before RunClassConstructor, the .cctor has not run (no static access to Tracker yet).
        if (SideChannel.CctorRanCount != 0)
            return 1;

        RuntimeHelpers.RunClassConstructor(typeof(Tracker).TypeHandle);

        // After RunClassConstructor, the .cctor must have run exactly once.
        if (SideChannel.CctorRanCount != 1)
            return 2;

        // Calling again is a no-op (cctor only runs once).
        RuntimeHelpers.RunClassConstructor(typeof(Tracker).TypeHandle);

        if (SideChannel.CctorRanCount != 1)
            return 3;

        // Accessing the static field directly should not re-run the cctor.
        if (Tracker.InitCount != 42)
            return 4;

        if (SideChannel.CctorRanCount != 1)
            return 5;

        return 0;
    }
}
