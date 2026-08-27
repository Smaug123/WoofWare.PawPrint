using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// `MethodBase.GetMethodImplementationFlags()`, which is `RuntimeMethodHandle.GetImplAttributes`
// verbatim on `RuntimeMethodInfo` (RuntimeMethodInfo.CoreCLR.cs:262) and on
// `RuntimeConstructorInfo` (RuntimeConstructorInfo.CoreCLR.cs:192) -- the only two callers CoreLib
// has, each passing `this`. Unlike its neighbouring natives that FCall takes the reflection object
// rather than a `RuntimeMethodHandleInternal`, so both of those types are exercised here.
//
// Every method reflected on is declared by this guest, and a method's ImplAttributes is a column of
// its own MethodDef row, so each expectation is a fact about this image rather than about whichever
// CoreLib flavour the run resolves.
//
// The comparisons are all made here rather than by handing a flags value back as the exit code: an
// exit code is eight bits, and AggressiveInlining (0x100) and InternalCall (0x1000) are both zero
// in the low byte, so an interpreter that answered 0 for everything would pass those two vacuously.
//
// Returns 0 on success, or the number of the first check that failed.

class Subject
{
    public Subject ()
    {
    }

    public static void Plain ()
    {
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public static void Inlining ()
    {
    }

    [MethodImpl (MethodImplOptions.NoInlining | MethodImplOptions.NoOptimization)]
    public static void NoInline ()
    {
    }

    [MethodImpl (MethodImplOptions.Synchronized)]
    public static void Sync ()
    {
    }

    public static void Generic<T> ()
    {
    }

    // Never called: this is here to be reflected on. `PreserveSig` defaults to true, so the
    // compiler emits `pinvokeimpl(...) preservesig` and the impl flags carry 0x80.
    [DllImport ("libc", EntryPoint = "getpid")]
    public static extern int GetPid ();

    // Also never called; `MethodImplOptions.InternalCall` goes straight into the ImplAttributes
    // column as 0x1000.
    [MethodImpl (MethodImplOptions.InternalCall)]
    public static extern void ImplementedByTheRuntime ();
}

delegate int Doubler (int x);

class Program
{
    const BindingFlags All =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance;

    static int Main (string[] args)
    {
        // MethodImplAttributes.IL is 0 and an ordinary method declares nothing else. This is the
        // check that a handler reporting `MethodAttributes` instead would fail: those are
        // Public|Static|HideBySig = 0x0096 for this method.
        if (!Check ("Plain", 0))
            return 1;

        // MethodImplAttributes.AggressiveInlining = 0x0100.
        if (!Check ("Inlining", 256))
            return 2;

        // NoInlining (0x08) | NoOptimization (0x40).
        if (!Check ("NoInline", 72))
            return 3;

        // MethodImplAttributes.Synchronized = 0x0020.
        if (!Check ("Sync", 32))
            return 4;

        // A generic method definition carries no impl flags of its own.
        if (!Check ("Generic", 0))
            return 5;

        // MethodImplAttributes.PreserveSig = 0x0080.
        if (!Check ("GetPid", 128))
            return 6;

        // MethodImplAttributes.InternalCall = 0x1000.
        if (!Check ("ImplementedByTheRuntime", 4096))
            return 7;

        // A delegate's members are declared `runtime managed`, i.e.
        // MethodImplAttributes.Runtime = 0x0003. `Invoke` reaches the native as a
        // `RuntimeMethodInfo`, like everything above...
        MethodInfo invoke = typeof (Doubler).GetMethod ("Invoke");

        if (invoke == null)
            return 8;

        if ((int) invoke.GetMethodImplementationFlags () != 3)
            return 9;

        // ...and the delegate's constructor reaches it as a `RuntimeConstructorInfo`, the other of
        // the two CoreLib types that call this native.
        ConstructorInfo[] delegateConstructors = typeof (Doubler).GetConstructors ();

        if (delegateConstructors.Length != 1)
            return 10;

        if ((int) delegateConstructors[0].GetMethodImplementationFlags () != 3)
            return 11;

        // An ordinary constructor has an IL body, so the constructor path has to report 0 for one
        // too: an implementation that answered every constructor the way it answers a delegate's
        // fails here rather than passing on the check above alone.
        ConstructorInfo constructor = typeof (Subject).GetConstructor (Type.EmptyTypes);

        if (constructor == null)
            return 12;

        if ((int) constructor.GetMethodImplementationFlags () != 0)
            return 13;

        // `MethodBase.MethodImplementationFlags` is the property spelling of the same call, and is
        // the spelling `StackTrace.ShowInStackTrace` uses; pin that it agrees.
        MethodInfo inlining = typeof (Subject).GetMethod ("Inlining", All);

        if (inlining == null)
            return 14;

        if (inlining.MethodImplementationFlags != MethodImplAttributes.AggressiveInlining)
            return 15;

        return 0;
    }

    static bool Check (string name, int expected)
    {
        MethodInfo method = typeof (Subject).GetMethod (name, All);

        // A missing method is a different failure from a wrong answer, but the caller reports one
        // number per check either way; the name is right here in the source if it ever fires.
        if (method == null)
            return false;

        return (int) method.GetMethodImplementationFlags () == expected;
    }
}
