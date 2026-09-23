using System;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace Outer.Inner
{
    public interface IFoo
    {
        int Impl();
    }

    public struct Shape : IFoo
    {
        public int X;

        public override string ToString() => "s" + X;

        // An implicit interface implementation is `virtual final newslot` in metadata, so it gets an
        // unboxing stub too -- this is the common way to trip the ambiguity, not just `ToString`.
        public int Impl() => X + 10;
    }

    public struct GenericShape<T>
    {
        public override string ToString() => "g";
    }
}

// CoreCLR generates an unboxing stub beside every *virtual* method a value type declares, and
// `IntroducedMethodIterator` yields both it and the method. Two candidates of one declaration
// cannot be told apart by any comparison, and the custom-modifier retry finds the same two, so the
// lookup is ambiguous. Measured on real .NET 10: `AmbiguousMatchException` ("Ambiguity in binding of
// UnsafeAccessorAttribute.") for an `override` and for an implicitly implemented interface method.
// (A non-virtual instance method and a static one bind: `sourcesPure/UnsafeAccessorNamespacedTargetName.cs`.)
public class TestUnsafeAccessorStructVirtualIsAmbiguous
{
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "ToString")]
    private static extern string Overridden(ref Outer.Inner.Shape s);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Impl")]
    private static extern int InterfaceImpl(ref Outer.Inner.Shape s);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "ToString")]
    private static extern string OverriddenOnGeneric(ref Outer.Inner.GenericShape<int> s);

    private static int Check(int code, Action a)
    {
        try
        {
            a();
            return code;
        }
        catch (AmbiguousMatchException e)
        {
            if (e.HResult != unchecked((int) 0x8000211D)) return code + 1;
            if (e.Message != "Ambiguity in binding of UnsafeAccessorAttribute.") return code + 2;
        }

        return 0;
    }

    private static int Run()
    {
        Outer.Inner.Shape s = new Outer.Inner.Shape
        {
            X = 1,
        };

        int r;

        r = Check(10, () => Overridden(ref s));
        if (r != 0) return r;

        r = Check(20, () => InterfaceImpl(ref s));
        if (r != 0) return r;

        // The ambiguity is found during the lookup, before the generic-type refusal that follows a
        // successful one (`sourcesPure/UnsafeAccessorMethodOfGenericType.cs`).
        Outer.Inner.GenericShape<int> g = default;
        r = Check(30, () => OverriddenOnGeneric(ref g));
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}
