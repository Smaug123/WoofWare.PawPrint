using System;
using System.Runtime.CompilerServices;

// An `[UnsafeAccessor]` searches the target type's *own* declared members and does not walk the
// base chain: CoreCLR iterates `MethodTable::IntroducedMethodIterator` and `ApproxFieldDescIterator`
// over the one type (vm/unsafeaccessors.cpp, `TrySetTargetMethod`/`TrySetTargetField`), unlike
// `MemberLoader::FindMethod` which it otherwise follows. Naming the base type in the accessor's
// first argument is how an inherited member is reached instead.
//
// Measured on real .NET 10: each of the three inherited lookups below raises, and the same member
// reached through `Base` succeeds.
public class TestUnsafeAccessorNoBaseClassWalk
{
    private class Base
    {
        private int _baseField = 3;
        private static int _baseStatic = 5;
        private int BaseMethod(int x) => x + 100;
    }

    private class Derived : Base
    {
    }

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_baseField")]
    private static extern ref int InheritedField(Derived d);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "_baseStatic")]
    private static extern ref int InheritedStatic(Derived d);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "BaseMethod")]
    private static extern int InheritedMethod(Derived d, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_baseField")]
    private static extern ref int DeclaredField(Base b);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "_baseStatic")]
    private static extern ref int DeclaredStatic(Base b);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "BaseMethod")]
    private static extern int DeclaredMethod(Base b, int x);

    public static int Main()
    {
        Derived d = new Derived();

        try
        {
            InheritedField(d);
            return 1;
        }
        catch (MissingFieldException) { }

        try
        {
            InheritedStatic(d);
            return 2;
        }
        catch (MissingFieldException) { }

        try
        {
            InheritedMethod(d, 1);
            return 3;
        }
        catch (MissingMethodException) { }

        // The same three members, named through the type that declares them. A `Derived` is a
        // `Base`, so the receiver is unchanged; only the accessor's declared parameter type moved.
        if (DeclaredField(d) != 3) return 4;
        if (DeclaredStatic(d) != 5) return 5;
        if (DeclaredMethod(d, 1) != 101) return 6;

        return 0;
    }
}
