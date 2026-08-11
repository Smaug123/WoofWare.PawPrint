using System;

// A `callvirt` names the interface at the call site but resolves to the implementing class's
// method, and it is the *resolved callee's* declaring type whose initialiser runs — the check
// lives in that method's prologue. Measured on .NET 10: calling `IFace.M()` through an
// interface reference never runs `IFace`'s own initialiser, so the marker below is still unset
// when the call returns.
//
// An interface is the only receiver shape that can tell the two apart. In an ordinary class
// hierarchy, constructing the receiver has already initialised both the derived type and its
// bases — `new Derived()` runs `Derived..cctor` and then `Base..cctor`, the latter from
// `Base..ctor`'s prologue as `Derived..ctor` chains to it — so nothing is left for the call to
// trigger either way. Constructing an implementor touches nothing of the interface's own
// static state.
class InterfaceCallDoesNotInitialiseInterface
{
    static int marker = 0;

    interface IFace
    {
        static readonly int Initialised = Mark();

        static int Mark()
        {
            marker = 1;
            return 1;
        }

        void M();
    }

    class Impl : IFace
    {
        public void M() { }
    }

    static IFace Make() => new Impl();

    static int Main(string[] args)
    {
        IFace f = Make();

        if (marker != 0)
        {
            // Constructing the implementor must not have run the interface's initialiser.
            return 1;
        }

        f.M();

        if (marker != 0)
        {
            // The call resolved to `Impl.M`, so `IFace`'s initialiser has no reason to run.
            return 2;
        }

        // Reading the interface's own static state is what triggers it, and proves the field
        // really is initialiser-backed rather than a constant the check could never observe.
        int seen = IFace.Initialised;

        if (seen != 1 || marker != 1)
        {
            return 3;
        }

        return 0;
    }
}
