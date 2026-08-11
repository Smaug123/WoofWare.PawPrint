using System;

// Calling an *instance* method does not trigger its declaring type's initialiser.
//
// ECMA-335 II.10.5.3.1 lists the triggers as first access to a static field, first invocation of
// a static method, and first invocation of an instance constructor. An instance method call on an
// object that already exists is not among them, and the difference is observable exactly when the
// initialiser has failed and an instance escaped before the failure — which a `.cctor` can arrange
// by publishing one into another type before it throws.
//
// Measured on .NET 10: the virtual call below returns `Derived`'s override, while constructing
// another `Derived` throws `TypeInitializationException`. Both halves are asserted, so an
// implementation that simply stopped initialising types would fail the second.
class InstanceCallDoesNotTriggerTypeInit
{
    class Base
    {
        public virtual int M() => 1;
    }

    // A separate type, so reading the published instance does not itself touch Derived.
    static class Holder
    {
        public static Base Published;
    }

    class Derived : Base
    {
        static Derived()
        {
            // Constructing Derived here is legal: its initialiser is in progress on this thread,
            // so the constructor's own check proceeds rather than recursing.
            Holder.Published = new Derived();
            throw new InvalidOperationException("boom");
        }

        public override int M() => 2;
    }

    static int Main(string[] args)
    {
        bool failed = false;

        try
        {
            Base unused = new Derived();
        }
        catch (TypeInitializationException)
        {
            failed = true;
        }

        if (!failed)
        {
            return 2;
        }

        if (Holder.Published == null)
        {
            return 1;
        }

        // The call site names Base; the override lives in Derived, whose initialiser has failed.
        // Resolving the callee is not the same as deciding to initialise it.
        int seen;

        try
        {
            seen = Holder.Published.M();
        }
        catch (TypeInitializationException)
        {
            return 3;
        }

        if (seen != 2)
        {
            return 4;
        }

        // The contrast: a constructor call *is* a trigger, and still reports the failure.
        try
        {
            Base again = new Derived();
            return 5;
        }
        catch (TypeInitializationException)
        {
            return 0;
        }
    }
}
