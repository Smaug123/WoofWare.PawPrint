using System;
using System.Runtime.CompilerServices;

// A *method* of a generic type is never reachable from an accessor declared on a non-generic type.
// The lookup can succeed -- `Get` takes nothing and returns `int32` on both sides -- and then
// `VerifyDeclarationSatisfiesTargetConstraints` (vm/unsafeaccessors.cpp) refuses it, because the
// declaration supplies no class instantiation for the target's type parameters. Measured on real
// .NET 10: `InvalidProgramException`, not `MissingMethodException`.
public class TestUnsafeAccessorMethodOfGenericType
{
    private class Boxed<T>
    {
        private int _plain = 17;

        private int Get() => _plain;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Get")]
    private static extern int MethodOfGenericType(Boxed<int> b);

    private static int Run()
    {
        try
        {
            MethodOfGenericType(new Boxed<int>());
            return 1;
        }
        catch (InvalidProgramException) { }

        return 0;
    }

    public static int Main() => Run();
}
