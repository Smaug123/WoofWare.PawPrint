using System.Runtime.CompilerServices;

// CoreCLR's synthesised body for `UnsafeAccessorKind.Method` is a `callvirt`, not a `call`
// (vm/unsafeaccessors.cpp:968). Two things follow, and both are checked here: a virtual target
// named through the type that declares it dispatches to the runtime type's override, and a null
// receiver faults at the accessor rather than inside the target.
//
// The private target is the control: a private method is not virtual, so it runs whatever the
// accessor named regardless of the receiver's runtime type.
public class TestUnsafeAccessorVirtualTarget
{
    private class Base
    {
        protected virtual int Virt(int x) => x + 1;

        private int NonVirtual(int x) => x + 10;
    }

    private class Derived : Base
    {
        protected override int Virt(int x) => x + 2;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Virt")]
    private static extern int Virt(Base b, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NonVirtual")]
    private static extern int NonVirtual(Base b, int x);

    private static int Run()
    {
        Base b = new Base();
        Derived d = new Derived();

        if (Virt(b, 10) != 11) return 1;

        // The accessor names `Base.Virt`; the receiver is a `Derived`, and `callvirt` resolves to
        // the override.
        if (Virt(d, 10) != 12) return 2;

        // A private method has no override to find.
        if (NonVirtual(b, 10) != 20) return 3;
        if (NonVirtual(d, 10) != 20) return 4;

        try
        {
            Virt(null, 1);
            return 5;
        }
        catch (System.NullReferenceException) { }

        try
        {
            NonVirtual(null, 1);
            return 6;
        }
        catch (System.NullReferenceException) { }

        return 0;
    }

    // `Main` only delegates. An accessor that pushed the wrong number of arguments would leave the
    // extra one on its *caller's* evaluation stack, and the entry frame is never checked for a
    // clean stack on return -- it has nowhere to return to -- so the leak would go unnoticed if the
    // accessors were called from `Main` itself.
    public static int Main() => Run();
}
