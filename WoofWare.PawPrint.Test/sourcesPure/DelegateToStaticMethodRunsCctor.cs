using System;

// Invoking a delegate must run the target's type initializer, and must not run it any
// earlier. `ldftn` deliberately does not — taking a function pointer is not a use of the
// type — but the *call* through it is, and CoreCLR runs the `.cctor` at that point like any
// other static call.
//
// Both halves below reach the same code path (`AbstractMachine.dispatchDelegateInvoke`), by
// the two routes that can produce a pointer to a static method: a plain `ldftn` of an
// ordinary static method, and a `constrained. ldftn` of a static abstract interface member.
// The direct calls are controls: they run the `.cctor` correctly today, which is what makes
// this about delegate invocation rather than about type initialisation generally.
//
// Each half checks the witness *twice*: once after building the delegate and once after
// invoking it. The "after building" read is what distinguishes running the initialiser at
// invocation from running it eagerly at `ldftn`; both timings satisfy an invoke-only check,
// so without it this file cannot tell a correct fix from an over-eager one. Verified on
// real .NET 10 in both Debug and Release: the witness reads 0 after construction and 1
// after invocation, for every route here.

static class CctorWitness
{
    public static int PlainRan;
    public static int ConstrainedRan;
}

// Every witnessed type below declares an explicit static constructor, so none of them is
// `beforefieldinit`. That matters: it obliges the runtime to precise-init, firing the
// initialiser at exactly the triggering event rather than at any earlier point of its
// choosing, which is what makes the "not yet" reads meaningful rather than merely lucky.
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

        if (CctorWitness.PlainRan != 0)
        {
            return 1;
        }

        if (plain () != 42)
        {
            return 2;
        }

        if (CctorWitness.PlainRan != 1)
        {
            return 3;
        }

        // Route 2: `constrained. ldftn` of a static abstract interface member.
        Func<int> constrained = LabelDelegate<ImplWithCctor> ();

        if (CctorWitness.ConstrainedRan != 0)
        {
            return 4;
        }

        if (constrained () != 7)
        {
            return 5;
        }

        if (CctorWitness.ConstrainedRan != 1)
        {
            return 6;
        }

        // Invoking again must not re-run either initializer: the type is already
        // initialized, so the second invocation takes the "nothing to do" path.
        if (plain () != 42 || constrained () != 7)
        {
            return 7;
        }

        if (CctorWitness.PlainRan != 1 || CctorWitness.ConstrainedRan != 1)
        {
            return 8;
        }

        return 0;
    }
}
