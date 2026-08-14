using System;
using System.Reflection.Emit;

public class Program
{
    // What a frame belonging to a `Reflection.Emit` method looks like in a stack trace.
    //
    // This is the guest-observable consequence of the representation choice #988 made: a dynamic
    // method is owned by a synthetic per-module class with no TypeDef row, so there is no type name
    // to qualify the frame with — and real .NET agrees, rendering `at Thrower(Int32)` where an
    // ordinary method renders `at Ns.Type.M(Int32)`. Had PawPrint fabricated an owner (the scope
    // module's `<Module>`, say) this would read `at <Module>.Thrower(...)` and nothing else in the
    // suite would have noticed.
    //
    // The assertions are deliberately the *cross-runtime* ones. PawPrint renders a synthesised
    // method's parameter list as `(…)` because it has no metadata signature to walk, where real
    // .NET prints `(Int32)`; that gap is recorded in docs/divergences.md. Both runtimes agree on
    // the two things checked here — that the method's own name is present, and that no type name
    // precedes it — so this guest returns 0 on either.
    //
    // Returns 0 on success, or the number of the first check that failed.

    public static int Main(string[] args)
    {
        // `ldarg.0; throw` rethrows whatever the caller handed in. Both opcodes are token-free,
        // which is what makes this the available way to raise from inside a dynamic method: every
        // more natural route needs a metadata token (`newobj` for the exception, `callvirt` for a
        // member that throws), and a dynamic method carrying one is refused when it is minted.
        //
        // The parameter is typed `object` rather than `Exception` for the same class of reason:
        // `SignatureHelper` spells any type that is not a primitive, string or object as
        // `ELEMENT_TYPE_INTERNAL`, which the signature decoder refuses, so `Exception` cannot
        // appear in a dynamic method's signature at all today.
        DynamicMethod thrower =
            new DynamicMethod("Thrower", typeof(int), new Type[] { typeof(object) }, typeof(Program).Module);
        ILGenerator il = thrower.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Throw);

        Func<object, int> f = (Func<object, int>) thrower.CreateDelegate(typeof(Func<object, int>));

        string trace;

        try
        {
            f(new InvalidOperationException("raised from a dynamic method"));
            return 1;
        }
        catch (InvalidOperationException e)
        {
            trace = e.StackTrace;
        }

        if (trace == null)
        {
            return 2;
        }

        // The dynamic method's frame is present, named by the name it was minted with.
        if (trace.IndexOf("at Thrower(", StringComparison.Ordinal) < 0)
        {
            return 3;
        }

        // ...and carries no qualifying type. A fabricated owner would put one immediately before
        // the name, so a dot there is exactly the failure this is looking for.
        if (trace.IndexOf(".Thrower(", StringComparison.Ordinal) >= 0)
        {
            return 4;
        }

        // The caller's frame is still rendered the ordinary way, so this is not passing because
        // trace rendering has broken altogether.
        if (trace.IndexOf("Program.Main(", StringComparison.Ordinal) < 0)
        {
            return 5;
        }

        return 0;
    }
}
