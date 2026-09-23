using System;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace Outer.Inner
{
    public interface IFoo
    {
        int Impl();
    }

    public interface IEcho
    {
        U Echo<U>(U u);
    }

    public struct Shape : IFoo, IEcho
    {
        public int X;

        // A *generic* virtual method gets no unboxing stub (`NeedsTightlyBoundUnboxingStub`
        // exempts it), so it is one candidate and binds.
        public U Echo<U>(U u)
        {
            X++;
            return u;
        }

        public override string ToString() => "s" + X;

        // An implicit interface implementation is `virtual final newslot` in metadata, so it gets an
        // unboxing stub too -- this is the common way to trip the ambiguity, not just `ToString`.
        public int Impl() => X + 10;
    }

    public struct GenericShape<T> : IEcho
    {
        public override string ToString() => "g";

        public U Echo<U>(U u) => u;
    }
}

// CoreCLR generates an unboxing stub beside every *virtual* method a value type declares, and
// `IntroducedMethodIterator` yields both it and the method. Two candidates of one declaration
// cannot be told apart by any comparison, and the custom-modifier retry finds the same two, so the
// lookup is ambiguous. Measured on real .NET 10: `AmbiguousMatchException` ("Ambiguity in binding of
// UnsafeAccessorAttribute.") for an `override` and for an implicitly implemented interface method.
// (A non-virtual instance method and a static one bind: `sourcesPure/UnsafeAccessorNamespacedTargetName.cs`.
// So does a generic virtual method, which gets no unboxing stub, when instantiated over a value type;
// over one CoreCLR shares over `System.__Canon`, real .NET 10 crashes the process calling it:
// `TestUnsafeAccessorSharedGenericVirtual.fs`.)
public class TestUnsafeAccessorStructVirtualIsAmbiguous
{
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "ToString")]
    private static extern string Overridden(ref Outer.Inner.Shape s);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Impl")]
    private static extern int InterfaceImpl(ref Outer.Inner.Shape s);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "ToString")]
    private static extern string OverriddenOnGeneric(ref Outer.Inner.GenericShape<int> s);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Echo")]
    private static extern U Echo<U>(ref Outer.Inner.Shape s, U u);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Echo")]
    private static extern U EchoOnGeneric<U>(ref Outer.Inner.GenericShape<int> s, U u);

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

        // The generic virtual method binds, and runs on the struct the byref addresses.
        if (Echo<long>(ref s, 5L) != 5L) return 40;
        if (s.X != 2) return 41;

        // With no ambiguity, the lookup succeeds and the generic-type refusal is what follows.
        try
        {
            EchoOnGeneric<int>(ref g, 1);
            return 50;
        }
        catch (InvalidProgramException) { }

        return 0;
    }

    public static int Main() => Run();
}
