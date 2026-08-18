// A function pointer's unmanaged calling convention is carried two different ways, and only one of
// them is part of the type as concretisation sees it.
//
// A single nameable convention (`unmanaged[Cdecl]`) goes in the signature header's CallKind byte,
// which `ConcreteTypeHandle.FunctionPointer` preserves. Any *combination* -- here adding
// `SuppressGCTransition` -- is spelled as `modopt`s inside the function pointer's own signature,
// with CallKind left as plain `unmanaged`. Concretisation looks through custom modifiers, so the two
// `Combined` overloads below collapse onto one `ConcreteTypeHandle` and a comparison of concretised
// signatures cannot tell them apart, where CoreCLR's `MetaSig::CompareMethodSigs` compares the
// modifier tokens (siginfo.cpp:4082-4100).
//
// The `Single` overloads are the control: they differ in the CallKind byte alone, which is preserved,
// so they discriminate even under a comparison that drops modifiers. A failure confined to
// `Combined` is therefore attributable to the modifiers rather than to function pointer overloads in
// general.
//
// The pointers are never invoked -- only passed -- so no unmanaged code is reached.

unsafe class Base
{
    public virtual int Combined (delegate* unmanaged[Cdecl, SuppressGCTransition]<void> f) => 1;

    public virtual int Combined (delegate* unmanaged[Stdcall, SuppressGCTransition]<void> f) => 2;

    public virtual int Single (delegate* unmanaged[Cdecl]<void> f) => 3;

    public virtual int Single (delegate* unmanaged[Stdcall]<void> f) => 4;
}

unsafe class Derived : Base
{
    public override int Combined (delegate* unmanaged[Cdecl, SuppressGCTransition]<void> f) => 11;

    public override int Combined (delegate* unmanaged[Stdcall, SuppressGCTransition]<void> f) => 12;

    public override int Single (delegate* unmanaged[Cdecl]<void> f) => 13;

    public override int Single (delegate* unmanaged[Stdcall]<void> f) => 14;
}

unsafe class Program
{
    static int Main (string[] args)
    {
        // Dispatched through the base, so each call has to find the override that fills the slot
        // its own overload occupies.
        Base b = new Derived ();

        if (b.Combined ((delegate* unmanaged[Cdecl, SuppressGCTransition]<void>) null) != 11)
        {
            return 1;
        }

        if (b.Combined ((delegate* unmanaged[Stdcall, SuppressGCTransition]<void>) null) != 12)
        {
            return 2;
        }

        if (b.Single ((delegate* unmanaged[Cdecl]<void>) null) != 13)
        {
            return 3;
        }

        if (b.Single ((delegate* unmanaged[Stdcall]<void>) null) != 14)
        {
            return 4;
        }

        // Called on the exact type as well, so that a failure above is known to be about slot
        // matching rather than about overload resolution at the call site.
        Derived d = new Derived ();

        if (d.Combined ((delegate* unmanaged[Stdcall, SuppressGCTransition]<void>) null) != 12)
        {
            return 5;
        }

        return 0;
    }
}
