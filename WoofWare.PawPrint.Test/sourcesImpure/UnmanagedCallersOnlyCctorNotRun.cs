using System;
using System.Reflection;
using System.Runtime.InteropServices;

// *When* the refusal happens, not merely that it happens.
//
// Real .NET refuses the managed entry into a `[UnmanagedCallersOnly]` method before running the
// declaring type's static constructor — measured, and pinned here — so the gate has to sit ahead of
// class initialisation rather than anywhere convenient inside the call. A gate placed after the
// callee's frame is armed would still abort, and would still pass every other guest in this set;
// this one is what distinguishes them.
//
// `Holder` has an explicit static constructor, so it is not `beforefieldinit` and its
// initialisation is precisely timed rather than at the runtime's discretion. The marker goes to
// *stderr* because that is what both oracles report: the real runtime's stderr is the string
// `RealRuntimeResult` carries, and PawPrint records it under `FileDescriptorRole.StandardError`.
//
// argv[0] selects:
//   "run"  — an ordinary read of `Holder`, which runs the static constructor. The marker must
//            appear. Without this the absence assertion below would pass for a guest that could
//            never have printed at all.
//   "call" — the managed entry, which must abort with the marker absent.

public static class Holder
{
    public static readonly int Sentinel;

    static Holder ()
    {
        Console.Error.WriteLine ("HOLDER CCTOR RAN");
        Console.Error.Flush ();
        Sentinel = 7;
    }

    [UnmanagedCallersOnly]
    public static int Doubler (int x)
    {
        return x * 2;
    }
}

public static class Program
{
    public static int Main (string[] args)
    {
        if (args.Length > 0 && args[0] == "run")
        {
            return Holder.Sentinel == 7 ? 0 : 1;
        }

        // Neither `GetMethod` nor `CreateDelegate` initialises the declaring type, so the static
        // constructor is still pending when the invocation below is refused.
        Func<int, int> bound =
            (Func<int, int>) typeof (Holder).GetMethod ("Doubler").CreateDelegate (typeof (Func<int, int>));

        return bound (21);
    }
}
