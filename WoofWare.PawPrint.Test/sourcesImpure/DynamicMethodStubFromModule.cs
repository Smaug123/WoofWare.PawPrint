using System;
using System.Reflection.Emit;

public class Program
{
    // `ModuleHandle_GetDynamicMethod` is the QCall behind `DynamicMethod.GetMethodDescriptor()`:
    // it mints CoreCLR's no-metadata `DynamicMethodDesc` and writes back a `RuntimeMethodInfoStub`
    // naming it. PawPrint mints the method but cannot yet *execute* one, so a guest cannot observe
    // the QCall by calling the dynamic method: that path stops one primitive later, in
    // `RuntimeTypeHandle_InternalAlloc` (measured, not guessed — `Delegate.CreateDelegateNoSecurityCheck`
    // allocates the delegate before it binds anything).
    //
    // It is observable exactly, though, and this is why. `CreateDelegate` evaluates
    // `GetMethodDescriptor()` as an *argument expression*, so the QCall runs before
    // `CreateDelegateNoSecurityCheck` performs any of its checks (Delegate.CoreCLR.cs:367-393), and
    // every step between the QCall returning and the first check that can fail is pure managed
    // code. Handing it a non-delegate type therefore reaches
    // `ArgumentException(Arg_MustBeDelegate, "type")` — but only if the QCall dispatched,
    // unmarshalled all six of its arguments, and wrote a *non-null* stub through its `result`
    // handle. A handler that wrote nothing is caught by the arm above it: `method.IsNullHandle()`
    // is tested first and raises `ArgumentNullException(nameof(method))`, which is a subclass of
    // `ArgumentException` and so must be caught separately to be distinguished.
    //
    // Impure because PawPrint declares dynamic code unsupported by default, and the harness
    // registers this case with the switch overridden to true — standing in for a
    // `runtimeconfig.json` that says so, which is a supported configuration a real host honours.
    // Real .NET launched with that same override exits 0 here.
    public static int Main(string[] args)
    {
        DynamicMethod dm = new DynamicMethod("Probe", typeof(void), Type.EmptyTypes, typeof(Program).Module);

        // `GetMethodDescriptor` refuses a body it never wrote to (`ILOffset == 0`) with
        // `InvalidOperationException`, so the QCall is only reached if something is emitted.
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ret);

        try
        {
            dm.CreateDelegate(typeof(string));
            return 1;
        }
        catch (ArgumentNullException)
        {
            return 2;
        }
        catch (ArgumentException e)
        {
            return e.ParamName == "type" ? 0 : 3;
        }
    }
}
