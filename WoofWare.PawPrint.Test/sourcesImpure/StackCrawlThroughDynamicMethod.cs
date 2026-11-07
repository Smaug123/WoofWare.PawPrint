using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;

public class Program
{
    // A stack crawl (`Assembly.GetExecutingAssembly`, `Assembly.GetCallingAssembly`) whose walk
    // passes a dynamic method's frame. CoreCLR's `SystemDomain::IsReflectionInvocationMethod` steps
    // over a dynamic method exactly when it is scoped to CoreLib's module *and* named
    // `InvokeStub_*`, which is how it recognises the stubs `MethodBase.Invoke` emits from its second
    // invocation onwards; any other dynamic method is a frame like any other, and answers with the
    // assembly of the module it is scoped to. The dynamic methods here are the guest's own, built to
    // sit on each side of that test. Every expected answer was measured on real .NET 10.
    //
    // Returns 0 on success, or the number of the first check that failed.

    private static readonly MethodInfo GetExecuting =
        typeof(Assembly).GetMethod("GetExecutingAssembly", BindingFlags.Static | BindingFlags.Public, null, Type.EmptyTypes, null);

    private static readonly MethodInfo GetCalling =
        typeof(Assembly).GetMethod("GetCallingAssembly", BindingFlags.Static | BindingFlags.Public, null, Type.EmptyTypes, null);

    private static readonly Module Guest = typeof(Program).Module;

    private static readonly Module CoreLib = typeof(object).Module;

    // A dynamic method `name`, scoped to `module`, that calls `target` and returns its answer. The
    // answer is passed to `GC.KeepAlive` first so that the call is not in tail position: optimised
    // code turns a bare `call; ret` into an implicit tail call, whose caller's frame is then gone
    // from the crawl (measured: `Outer` vanishes, and check 5 below answers this guest).
    private static DynamicMethod Calling(string name, Module module, MethodInfo target)
    {
        DynamicMethod dm = new DynamicMethod(name, typeof(Assembly), Type.EmptyTypes, module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Call, target);
        il.Emit(OpCodes.Dup);
        il.Emit(OpCodes.Call, typeof(GC).GetMethod("KeepAlive"));
        il.Emit(OpCodes.Ret);
        return dm;
    }

    // Invoked from a frame of this assembly, so that a crawl looking two frames out from the
    // dynamic method finds guest code rather than running off the outermost frame.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static Assembly Run(DynamicMethod dm)
    {
        return ((Func<Assembly>) dm.CreateDelegate(typeof(Func<Assembly>)))();
    }

    public static int Main(string[] args)
    {
        Assembly guest = typeof(Program).Assembly;
        Assembly coreLib = typeof(object).Assembly;

        // A dynamic method scoped to this guest's module is the caller, whatever its name: the
        // CoreLib test comes first. Each is called from `Outer`, scoped to CoreLib and not named as
        // a stub, so that stepping over it would answer CoreLib.
        if (!ReferenceEquals(Run(Calling("Outer", CoreLib, Calling("Probe", Guest, GetExecuting))), guest))
        {
            return 1;
        }

        if (!ReferenceEquals(Run(Calling("Outer", CoreLib, Calling("InvokeStub_Probe", Guest, GetExecuting))), guest))
        {
            return 2;
        }

        // Scoped to CoreLib's module but not named as an invoke stub, it is the caller, and its
        // assembly is CoreLib.
        if (!ReferenceEquals(Run(Calling("Probe", CoreLib, GetExecuting)), coreLib))
        {
            return 3;
        }

        // Scoped to CoreLib's module and named as an invoke stub, it is stepped over, and the
        // caller is `Run`.
        if (!ReferenceEquals(Run(Calling("InvokeStub_Probe", CoreLib, GetExecuting)), guest))
        {
            return 4;
        }

        // The same distinction two frames out. `Outer`, scoped to CoreLib and not a stub, calls
        // `Inner`, which calls `GetCallingAssembly`. When `Inner` is a frame, the caller's caller
        // is `Outer` and the answer is CoreLib; when `Inner` is stepped over, the caller is
        // `Outer` and the caller's caller is `Run`.
        DynamicMethod innerFrame = Calling("Inner", CoreLib, GetCalling);
        if (!ReferenceEquals(Run(Calling("Outer", CoreLib, innerFrame)), coreLib))
        {
            return 5;
        }

        DynamicMethod innerStub = Calling("InvokeStub_Inner", CoreLib, GetCalling);
        if (!ReferenceEquals(Run(Calling("Outer", CoreLib, innerStub)), guest))
        {
            return 6;
        }

        return 0;
    }
}
