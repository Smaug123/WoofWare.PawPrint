using System;
using System.Reflection;

// Calling through the function pointer `RuntimeMethodHandle.GetFunctionPointer` hands out for a
// method with no body. The pointer is real — every abstract method has an entry point, the
// prestub — and calling through it is not refused at the call site: the prestub finds nothing to
// compile and raises a catchable `BadImageFormatException` whose HResult is `COR_E_BADIMAGEFORMAT`.
// That holds whatever the receiver, including a subclass that overrides the method: a `calli` does
// not dispatch, so the override is never consulted.
//
// The exception is faithful; its *trace* is not. Real .NET names the abstract method as the top
// frame, because the failure happens while entering it; PawPrint raises it from the `calli`, so the
// trace starts at the caller. Nothing here asserts the trace, because that would pin a known
// divergence as though it were the intended answer — see docs/divergences.md, "A delegate invocation
// or `calli` that fails before entering its target names no frame for it".
//
// Returns 0 on success, or the number of the first check that failed.

public static class Log
{
    public static string Initialised = "";
}

public abstract class Abs
{
    public abstract int A();
}

public class Conc : Abs
{
    public override int A() => 9;
}

public interface I
{
    int M();

    static abstract int SA();
}

public class Impl : I
{
    public int M() => 8;

    public static int SA() => 7;
}

// Each has an explicit static constructor, so neither is `beforefieldinit`: were the call to enter
// the method, ECMA-335 I.8.9.5 would run the initialiser first.
public abstract class AbsWithCctor
{
    static AbsWithCctor()
    {
        Log.Initialised += "AbsWithCctor;";
    }

    public abstract int A();
}

public interface IWithCctor
{
    static IWithCctor()
    {
        Log.Initialised += "IWithCctor;";
    }

    static abstract int SA();
}

public static unsafe class Program
{
    static IntPtr Fp(Type t, string name) => t.GetMethod(name).MethodHandle.GetFunctionPointer();

    // 0 when `call` raised the runtime's `BadImageFormatException`; otherwise `ifReturned` if it
    // returned, or `ifWrong` if what it raised was not that.
    static int ExpectBadImage(Func<int> call, int ifReturned, int ifWrong)
    {
        try
        {
            call();
            return ifReturned;
        }
        catch (BadImageFormatException e)
        {
            // The HResult distinguishes the runtime's throw from one a guest could construct.
            if (e.HResult != unchecked((int)0x8007000B))
            {
                return ifWrong;
            }

            // The message is the CLR's HRESULT text rather than the parameterless constructor's,
            // which is a different string with no HRESULT in it. Only the numeral is checked: the
            // prose around it is localisable.
            if (!e.Message.Contains("0x8007000B"))
            {
                return ifWrong;
            }

            return 0;
        }
    }

    // The `calli` and its handler in one frame: the exception is raised at the call, so the
    // caller's own clause catches it.
    static int CaughtInCallingFrame(IntPtr p)
    {
        try
        {
            return ((delegate*<Abs, int>)p)(new Conc());
        }
        catch (BadImageFormatException)
        {
            return -1;
        }
    }

    public static int Main()
    {
        IntPtr abs = Fp(typeof(Abs), "A");
        IntPtr m = Fp(typeof(I), "M");
        IntPtr sa = Fp(typeof(I), "SA");
        int r;

        // An abstract class's method, over a receiver that overrides it.
        r = ExpectBadImage(() => ((delegate*<Abs, int>)abs)(new Conc()), 1, 2);
        if (r != 0) return r;

        // The receiver plays no part: a null one raises the same, rather than faulting on it.
        r = ExpectBadImage(() => ((delegate*<Abs, int>)abs)(null), 3, 4);
        if (r != 0) return r;

        // An interface's instance method, over a receiver that implements it.
        r = ExpectBadImage(() => ((delegate*<I, int>)m)(new Impl()), 5, 6);
        if (r != 0) return r;

        // A static abstract interface method, which has no receiver at all.
        r = ExpectBadImage(() => ((delegate*<int>)sa)(), 7, 8);
        if (r != 0) return r;

        // Raised at the call, and so catchable by the calling frame's own handler.
        if (CaughtInCallingFrame(abs) != -1) return 9;

        // Nothing is entered, so the declaring type's initialiser does not run.
        r = ExpectBadImage(() => ((delegate*<AbsWithCctor, int>)Fp(typeof(AbsWithCctor), "A"))(null), 10, 11);
        if (r != 0) return r;
        r = ExpectBadImage(() => ((delegate*<int>)Fp(typeof(IWithCctor), "SA"))(), 12, 13);
        if (r != 0) return r;
        if (Log.Initialised != "") return 14;

        // Controls: the overriding and implementing methods have bodies, and run.
        if (((delegate*<Abs, int>)Fp(typeof(Conc), "A"))(new Conc()) != 9) return 15;
        if (((delegate*<I, int>)Fp(typeof(Impl), "M"))(new Impl()) != 8) return 16;
        if (((delegate*<int>)Fp(typeof(Impl), "SA"))() != 7) return 17;

        return 0;
    }
}
