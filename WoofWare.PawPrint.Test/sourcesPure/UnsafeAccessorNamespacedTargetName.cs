using System;
using System.Runtime.CompilerServices;

// How the runtime names a target type in a missing-member report: namespace-qualified, as
// `'Outer.Inner.Missing.NoSuch'`, and taken from the *target's* namespace rather than the
// accessor's. Beside it, a value type's non-virtual instance method and its static method, both of
// which bind (its *virtual* methods are `sourcesPure/UnsafeAccessorStructVirtualIsAmbiguous.cs`).
// Measured on real .NET 10.
namespace Outer.Inner
{
    public struct Shape
    {
        public int X;

        private int Plain() => X + 1;

        private static int Stat(int x) => x + 2;
    }

    public class Missing
    {
        private int Present;
    }
}

public class TestUnsafeAccessorNamespacedTargetName
{
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Plain")]
    private static extern int Plain(ref Outer.Inner.Shape s);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Stat")]
    private static extern int Stat(Outer.Inner.Shape s, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NoSuch")]
    private static extern int MissingMethodOnNamespacedType(Outer.Inner.Missing m);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "NoSuchField")]
    private static extern ref int MissingFieldOnNamespacedType(Outer.Inner.Missing m);

    private static int Run()
    {
        Outer.Inner.Shape s = new Outer.Inner.Shape
        {
            X = 1,
        };

        if (Plain(ref s) != 2) return 1;
        if (Stat(default, 5) != 7) return 2;

        try
        {
            MissingMethodOnNamespacedType(null);
            return 3;
        }
        catch (MissingMethodException e)
        {
            if (!e.Message.Contains("Outer.Inner.Missing.NoSuch")) return 4;
        }

        try
        {
            MissingFieldOnNamespacedType(null);
            return 5;
        }
        catch (MissingFieldException e)
        {
            if (!e.Message.Contains("Outer.Inner.Missing.NoSuchField")) return 6;
        }

        return 0;
    }

    public static int Main() => Run();
}
