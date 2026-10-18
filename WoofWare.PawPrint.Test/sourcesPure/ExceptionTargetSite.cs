using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.ExceptionServices;

// `Exception.TargetSite` and `Exception.Source` of exceptions raised by an IL `throw`. CoreCLR reads
// the method of the first frame of `_stackTrace` (`ExceptionNative_GetMethodFromStackTrace`), as its
// typical definition, and `Source` is that method's assembly's simple name. Every throwing method is
// `NoInlining`, because a JIT that inlined one would name its caller instead.

class Gen<T>
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void Throw<U>() { throw new InvalidOperationException("gen"); }
}

public static class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Thrower() { throw new InvalidOperationException("x"); }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Rethrower()
    {
        try { Thrower(); }
        catch { throw; }
    }

    static ExceptionDispatchInfo captured;

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Capture()
    {
        try { Thrower(); }
        catch (Exception e) { captured = ExceptionDispatchInfo.Capture(e); }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Replay() { captured.Throw(); }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void ThrowAgain(Exception e) { throw e; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Index(int[] a, int i) => a[i];

    public static int Main(string[] args)
    {
        try { Thrower(); return 1; }
        catch (Exception e)
        {
            MethodBase site = e.TargetSite;
            if (site == null) return 2;
            if (site.Name != "Thrower") return 3;
            if (site.DeclaringType != typeof(Program)) return 4;
            if (e.Source != typeof(Program).Assembly.GetName().Name) return 5;
            // `TargetSite` latches what it first read.
            if (!ReferenceEquals(site, e.TargetSite)) return 6;
        }

        // The typical definition: both instantiations are stripped.
        try { Gen<int>.Throw<string>(); return 10; }
        catch (Exception e)
        {
            MethodBase site = e.TargetSite;
            if (site.Name != "Throw") return 11;
            if (!site.DeclaringType.IsGenericTypeDefinition) return 12;
            if (site.DeclaringType != typeof(Gen<>)) return 13;
            if (!site.IsGenericMethodDefinition) return 14;
        }

        // A `[StackTraceHidden]` frame is still the first frame of the trace.
        try { ArgumentNullException.ThrowIfNull((object)null); return 20; }
        catch (Exception e)
        {
            if (e.TargetSite.Name != "Throw") return 21;
            if (e.TargetSite.DeclaringType != typeof(ArgumentNullException)) return 22;
            if (e.Source != "System.Private.CoreLib") return 23;
        }

        // A rethrow keeps the trace, and so its first frame.
        try { Rethrower(); return 30; }
        catch (Exception e)
        {
            if (e.TargetSite.Name != "Thrower") return 31;
        }

        // Throwing a caught exception again begins a new trace; `TargetSite` names its first frame
        // unless it was read, and latched, before.
        Exception unread = null;
        try { Thrower(); return 40; }
        catch (Exception e) { unread = e; }
        try { ThrowAgain(unread); return 41; }
        catch (Exception e)
        {
            if (e.TargetSite.Name != "ThrowAgain") return 42;
        }

        Exception read = null;
        try { Thrower(); return 43; }
        catch (Exception e)
        {
            read = e;
            if (e.TargetSite.Name != "Thrower") return 44;
        }
        try { ThrowAgain(read); return 45; }
        catch (Exception e)
        {
            if (e.TargetSite.Name != "Thrower") return 46;
        }

        // `ExceptionDispatchInfo.Throw` restores the captured trace in front of its own.
        Capture();
        try { Replay(); return 50; }
        catch (Exception e)
        {
            if (e.TargetSite.Name != "Thrower") return 51;
        }

        // Throwing again an exception the runtime raised begins a trace at the `throw`, whose
        // frame is then the first.
        Exception raised = null;
        try { Index(new int[1], 2); return 52; }
        catch (Exception e) { raised = e; }
        try { ThrowAgain(raised); return 53; }
        catch (Exception e)
        {
            if (e.TargetSite.Name != "ThrowAgain") return 54;
            if (e.Source != typeof(Program).Assembly.GetName().Name) return 55;
        }

        // `ExceptionDispatchInfo.Throw` of an exception that was never thrown restores no frames,
        // so its own `throw` is the first.
        try { ExceptionDispatchInfo.Capture(new InvalidOperationException()).Throw(); return 56; }
        catch (Exception e)
        {
            if (e.TargetSite.Name != "Throw") return 57;
            if (e.TargetSite.DeclaringType != typeof(ExceptionDispatchInfo)) return 58;
            if (e.Source != "System.Private.CoreLib") return 59;
        }

        // An exception that has not been thrown has neither.
        var fresh = new InvalidOperationException();
        if (fresh.TargetSite != null) return 60;
        if (fresh.Source != null) return 61;

        return 0;
    }
}
