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

        private int Plain() => X + 1;

        private static int Stat(int x) => x + 2;
    }

    public class Missing
    {
        private int Present;
    }
}

// CoreCLR generates an unboxing stub beside every *virtual* method a value type declares, and
// `IntroducedMethodIterator` yields both it and the method. Two candidates of one declaration
// cannot be told apart by any comparison, and the custom-modifier retry finds the same two, so the
// lookup is ambiguous. Measured on real .NET 10: `AmbiguousMatchException` for an `override` and
// for an implicitly implemented interface method, while a non-virtual instance method and a static
// one bind normally.
//
// The namespaced type here also pins how the runtime names a target in its diagnostics: the
// namespace is part of the name.
public class TestUnsafeAccessorStructVirtualIsAmbiguous
{
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "ToString")]
    private static extern string Overridden(ref Outer.Inner.Shape s);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Impl")]
    private static extern int InterfaceImpl(ref Outer.Inner.Shape s);

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

        try
        {
            Overridden(ref s);
            return 1;
        }
        catch (AmbiguousMatchException) { }

        try
        {
            InterfaceImpl(ref s);
            return 2;
        }
        catch (AmbiguousMatchException) { }

        // Neither of these has an unboxing stub, so both bind.
        if (Plain(ref s) != 2) return 3;
        if (Stat(default, 5) != 7) return 4;

        // The namespace is part of the name the runtime reports.
        try
        {
            MissingMethodOnNamespacedType(null);
            return 5;
        }
        catch (MissingMethodException e)
        {
            if (!e.Message.Contains("Outer.Inner.Missing.NoSuch")) return 6;
        }

        try
        {
            MissingFieldOnNamespacedType(null);
            return 7;
        }
        catch (MissingFieldException e)
        {
            if (!e.Message.Contains("Outer.Inner.Missing.NoSuchField")) return 8;
        }

        return 0;
    }

    public static int Main() => Run();
}
