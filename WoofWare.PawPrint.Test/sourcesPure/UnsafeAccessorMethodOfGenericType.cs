using System;
using System.Runtime.CompilerServices;

// A *method* of a generic type is never reachable from an accessor declared on a non-generic type.
// The lookup can succeed -- `Get` takes nothing and returns `int32` on both sides -- and then
// `VerifyDeclarationSatisfiesTargetConstraints` (vm/unsafeaccessors.cpp) refuses it, because the
// declaration supplies no class instantiation for the target's type parameters. Measured on real
// .NET 10: `InvalidProgramException` ("Generic type constraints do not match.", the
// `COR_E_INVALIDPROGRAM` HResult) for every kind that looks up a method, not `MissingMethodException`.
public class TestUnsafeAccessorMethodOfGenericType
{
    private class Boxed<T>
    {
        private int _plain = 17;

        public Boxed()
        {
        }

        private int Get() => _plain;

        private static int StaticGet() => 3;
    }

    private abstract class AbstractBoxed<T>
    {
        private AbstractBoxed()
        {
        }
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Get")]
    private static extern int MethodOfGenericType(Boxed<int> b);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "StaticGet")]
    private static extern int StaticMethodOfGenericType(Boxed<int> b);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Boxed<int> ConstructorOfGenericType();

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern AbstractBoxed<int> ConstructorOfAbstractGenericType();

    private static int Check(int code, Action a)
    {
        try
        {
            a();
            return code;
        }
        catch (InvalidProgramException e)
        {
            if (e.HResult != unchecked((int) 0x8013153A)) return code + 1;
            if (e.Message != "Generic type constraints do not match.") return code + 2;
        }

        return 0;
    }

    private static int Run()
    {
        int r;

        r = Check(10, () => MethodOfGenericType(new Boxed<int>()));
        if (r != 0) return r;

        // The refusal is part of binding, so it comes before the stub's `callvirt` could fault on
        // a null receiver.
        r = Check(20, () => MethodOfGenericType(null));
        if (r != 0) return r;

        r = Check(30, () => StaticMethodOfGenericType(null));
        if (r != 0) return r;

        r = Check(40, () => ConstructorOfGenericType());
        if (r != 0) return r;

        // Binding refuses before the `newobj` of an abstract class could.
        r = Check(50, () => ConstructorOfAbstractGenericType());
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}
