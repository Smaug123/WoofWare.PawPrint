using System;

// A delegate over an *instance* method must also run the declaring type's initializer at
// invocation, so the rule cannot be narrowed to "static targets only".
//
// This is the one shape where that distinction is observable. ECMA-335 II.10.5.3.1 makes
// "first invocation of any instance method of a value type" a trigger in its own right, and
// a value type is the only receiver whose existence does not already imply initialization:
// for a class, holding an instance means `base..ctor()` ran up the whole chain, and
// constructor invocation is itself a trigger, so the declaring type is initialized long
// before any delegate can name it. `default(S)` runs nothing.
//
// Verified on real .NET 10 in both Debug and Release: the witness reads 0 after
// `default(S)`, still 0 after building the delegate (which boxes the receiver), and 1 after
// invoking it.

static class StructCctorWitness
{
    public static int Ran;
}

// Explicit static constructor, so `S` is not `beforefieldinit` and the runtime must
// precise-init it rather than picking its own earlier moment.
struct S
{
    static S()
    {
        StructCctorWitness.Ran += 1;
    }

    // Touches no static of its own, so only the runtime's rule can fire the initializer.
    public int M()
    {
        return 11;
    }
}

class Program
{
    static int Main(string[] args)
    {
        S s = default(S);

        // Merely having a value of the type is not a trigger.
        if (StructCctorWitness.Ran != 0)
        {
            return 1;
        }

        // Boxes `s` into the delegate's target field. Still not an invocation.
        Func<int> f = s.M;

        if (StructCctorWitness.Ran != 0)
        {
            return 2;
        }

        if (f () != 11)
        {
            return 3;
        }

        if (StructCctorWitness.Ran != 1)
        {
            return 4;
        }

        // Already initialized; the second invocation must not re-run it.
        if (f () != 11)
        {
            return 5;
        }

        if (StructCctorWitness.Ran != 1)
        {
            return 6;
        }

        return 0;
    }
}
