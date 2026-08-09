using System;

// Invoking a delegate must run the target's type initializer. `ldftn` itself deliberately does
// not — taking a function pointer is not a use of the type — but the *call* through it is, and
// CoreCLR runs the `.cctor` at that point like any other static call.
//
// Both halves below reach the same code path (`AbstractMachine.dispatchDelegateInvoke`), by the
// two routes that can produce a pointer to a static method: a plain `ldftn` of an ordinary
// static method, and a `constrained. ldftn` of a static abstract interface member. The direct
// calls are controls: they run the `.cctor` correctly today, which is what makes this about
// delegate invocation rather than about type initialisation generally.

static class CctorWitness
{
    public static int PlainRan;
    public static int ConstrainedRan;
}

class PlainWithCctor
{
    static PlainWithCctor()
    {
        CctorWitness.PlainRan += 1;
    }

    // Deliberately does not touch its own statics, so nothing inside the body can trigger the
    // initializer; only the runtime's rule for calling a static method can.
    public static int Value()
    {
        return 42;
    }
}

interface ILabelledCctor<T> where T : ILabelledCctor<T>
{
    static abstract int Label();
}

class ImplWithCctor : ILabelledCctor<ImplWithCctor>
{
    static ImplWithCctor()
    {
        CctorWitness.ConstrainedRan += 1;
    }

    public static int Label()
    {
        return 7;
    }
}

class Program
{
    private static Func<int> LabelDelegate<T> ()
        where T : ILabelledCctor<T>
    {
        return T.Label;
    }

    static int Main(string[] args)
    {
        // Route 1: plain `ldftn` of an ordinary static method.
        Func<int> plain = PlainWithCctor.Value;
        if (plain () != 42)
        {
            return 1;
        }

        if (CctorWitness.PlainRan != 1)
        {
            return 2;
        }

        // Route 2: `constrained. ldftn` of a static abstract interface member.
        Func<int> constrained = LabelDelegate<ImplWithCctor> ();
        if (constrained () != 7)
        {
            return 3;
        }

        if (CctorWitness.ConstrainedRan != 1)
        {
            return 4;
        }

        return 0;
    }
}
