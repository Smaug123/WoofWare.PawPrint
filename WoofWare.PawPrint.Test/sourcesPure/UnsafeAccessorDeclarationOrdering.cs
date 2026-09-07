using System;
using System.Runtime.CompilerServices;

// Which refusal a declaration gets depends on the order CoreCLR asks its questions, and two of
// those orderings are guest-visible.
//
// The accessor must be static, and that is asked before anything is parsed -- so an instance
// accessor is a catchable `BadImageFormatException` even when the declaration is unsupported in
// some other way as well.
//
// A member that does not exist is reported as missing even when the declaration's *shape* is one
// the body could not have executed: binding completes before the synthesised instruction runs.
// Measured on real .NET 10 for both.
//
// The `Name` is also compared as a UTF-8 buffer, so it ends at the first NUL: `Name = "M\0suffix"`
// binds the member called `M`.
public class TestUnsafeAccessorDeclarationOrdering
{
    private class Target
    {
        private int _f = 3;

        private int M(int x) => _f + x;
    }

    private class Instance
    {
        // Not static, which CoreCLR refuses before it reads the rest of the declaration.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "M")]
        internal extern int NotStatic(Target t, int x);
    }

    // A reference-typed receiver by `ref` is a shape whose *body* cannot run, but the member here
    // does not exist, and the lookup is what answers.
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NoSuchMethod")]
    private static extern int MissingViaRef(ref Target t, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "noSuchField")]
    private static extern ref int MissingFieldViaRef(ref Target t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "M\0suffix")]
    private static extern int NulTerminatedName(Target t, int x);

    private static int Check<TExpected>(int code, Action a)
        where TExpected : Exception
    {
        try
        {
            a();
            return code;
        }
        catch (TExpected)
        {
            return 0;
        }
    }

    private static int Run()
    {
        Target t = new Target();
        int r;

        r = Check<BadImageFormatException>(1, () => new Instance().NotStatic(t, 1));
        if (r != 0) return r;

        r = Check<MissingMethodException>(2, () => MissingViaRef(ref t, 1));
        if (r != 0) return r;

        r = Check<MissingFieldException>(3, () => MissingFieldViaRef(ref t));
        if (r != 0) return r;

        // The name ends at the NUL, so this binds `M` and returns `_f + 1`.
        if (NulTerminatedName(t, 1) != 4) return 4;

        return 0;
    }

    public static int Main() => Run();
}
