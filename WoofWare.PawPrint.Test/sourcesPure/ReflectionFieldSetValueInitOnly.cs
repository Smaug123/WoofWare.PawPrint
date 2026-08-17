using System;
using System.Reflection;

// The static init-only gate inside the RuntimeFieldHandle_SetValue QCall. Managed FieldAccessor
// does *not* perform this check itself while its accessor is still in its first-call state
// (VerifyInitOnly, FieldAccessor.cs) — it delegates to the QCall — so the QCall's own gate is what
// a guest sees.
//
// The two halves are a pair on purpose. CoreCLR keys the gate on "the initialiser has *finished*",
// not on "the initialiser has started", so setting a static readonly field from inside its own
// declaring type's initialiser is legal while setting it afterwards is not. A runtime that
// collapsed those two states would pass one half and fail the other, whichever way it collapsed
// them.

static class Settled
{
    public static readonly int Value = 5;
}

static class SelfSetting
{
    public static readonly int Value;

    static SelfSetting()
    {
        // Legal: the initialiser has not finished, so the field is not yet sealed.
        typeof(SelfSetting).GetField("Value").SetValue(null, 99);
    }
}

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    static int Main()
    {
        // Force the initialiser to completion before the reflective set.
        Check(Settled.Value == 5);

        bool threw = false;
        try
        {
            typeof(Settled).GetField("Value").SetValue(null, 6);
        }
        catch (FieldAccessException)
        {
            threw = true;
        }

        Check(threw);
        // The refused write must not have landed.
        Check(Settled.Value == 5);

        // The other half: the same shape, written from inside the initialiser, sticks.
        Check(SelfSetting.Value == 99);

        return firstFailure;
    }
}
