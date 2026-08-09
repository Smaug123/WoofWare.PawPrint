using System;

// A `constrained.` prefix applies to exactly one instruction. `ldftn` is one of the three
// opcodes ECMA lets it precede, so `ldftn` must *consume* it — an armed prefix left behind is
// picked up by whatever call-like instruction runs next.
//
// The unrelated instruction has to be in the *same method body* for this to bite:
// `MethodState`'s frame constructor starts every frame at `PrefixState.empty`, so a leaked
// prefix dies with the frame that armed it. Hence each case below takes the delegate and then
// does something call-like before returning — which is also exactly what Roslyn's own
// delegate-caching sequence does (`ldnull; constrained. T; ldftn; newobj; dup; stsfld;
// callvirt Invoke`). A test that returned the delegate for another frame to invoke would pass
// against an implementation that resolved correctly but never cleared the prefix.

public interface ILabelled<T> where T : ILabelled<T>
{
    static abstract int Label();
}

public class LabelledThing : ILabelled<LabelledThing>
{
    public static int Label() => 4;

    public virtual int Instance() => 5;
}

public struct LabelledStruct : ILabelled<LabelledStruct>
{
    public static int Label() => 6;
}

public static class ConstrainedLdftnPrefixNotLeaked
{
    // Take the delegate, then invoke it — the invocation is a `callvirt Func::Invoke` in this
    // same body, so a stale prefix lands on it.
    private static int TakeThenInvoke<T> ()
        where T : ILabelled<T>
    {
        Func<int> f = T.Label;
        return f ();
    }

    // Take the delegate, then make an unrelated virtual call on a reference type before using
    // it. The stale prefix would be applied to `thing.Instance()`, whose receiver is an
    // ordinary object reference rather than the managed pointer a `constrained.` callvirt
    // expects.
    private static int TakeThenUnrelatedCall<T> (LabelledThing thing)
        where T : ILabelled<T>
    {
        Func<int> f = T.Label;
        int fromInstance = thing.Instance ();
        return f () + fromInstance;
    }

    public static int Main (string[] argv)
    {
        if (TakeThenInvoke<LabelledThing> () != 4)
        {
            return 1;
        }

        if (TakeThenInvoke<LabelledStruct> () != 6)
        {
            return 2;
        }

        if (TakeThenUnrelatedCall<LabelledThing> (new LabelledThing ()) != 9)
        {
            return 3;
        }

        return 0;
    }
}
