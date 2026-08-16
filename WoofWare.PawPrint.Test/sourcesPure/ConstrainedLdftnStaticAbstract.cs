using System;

// Roslyn emits `constrained. T; ldftn IFace<T>::M()` for a delegate to a static abstract
// interface member. The prefix is not decoration: it is what directs resolution from the
// interface's abstract declaration to T's implementation, exactly as it does for
// `constrained. call` (CoreCLR routes both through `getCallInfo` with the constrained token,
// and switches on `pConstrainedResolvedToken != NULL && pMD->IsInterface() && pMD->IsStatic()`).
//
// Each delegate below is created in one method and invoked from another, so the failure this
// isolates is the resolution one: a leaked prefix would die with the popped frame. The
// same-frame leak is the sibling `ConstrainedLdftnPrefixNotLeaked.cs`.

public interface ITagged<T> where T : ITagged<T>
{
    static abstract int Tag();
    static abstract T Make();
}

public class RefWidget : ITagged<RefWidget>
{
    public int Which = 1;

    public static int Tag() => 11;

    public static RefWidget Make() => new RefWidget ();
}

public class OtherRefWidget : ITagged<OtherRefWidget>
{
    public static int Tag() => 22;

    public static OtherRefWidget Make() => new OtherRefWidget ();
}

public struct StructWidget : ITagged<StructWidget>
{
    public static int Tag() => 33;

    public static StructWidget Make() => new StructWidget ();
}

public static class ConstrainedLdftnStaticAbstract
{
    // `constrained. ldftn` — the shape under test. Returns the delegate so the caller invokes
    // it in a different frame.
    private static Func<int> TagDelegate<T> ()
        where T : ITagged<T>
    {
        return T.Tag;
    }

    // Static abstract member with a generic return type.
    private static Func<T> MakeDelegate<T> ()
        where T : ITagged<T>
    {
        return T.Make;
    }

    // `constrained. call` — the control: the ldftn and call paths share a resolution
    // helper, so if this breaks, that helper is what broke it.
    private static int TagDirect<T> ()
        where T : ITagged<T>
    {
        return T.Tag ();
    }

    public static int Main (string[] argv)
    {
        // One generic method, two reference-type implementers. An implementation that ignored
        // the constrained type and picked any implementation of the interface would answer the
        // same for both.
        if (TagDelegate<RefWidget> () () != 11)
        {
            return 1;
        }

        if (TagDelegate<OtherRefWidget> () () != 22)
        {
            return 2;
        }

        // A value-type implementer: the constrained type being a struct is the case the prefix
        // exists for elsewhere.
        if (TagDelegate<StructWidget> () () != 33)
        {
            return 3;
        }

        // Generic return type.
        RefWidget made = MakeDelegate<RefWidget> () ();
        if (made == null || made.Which != 1)
        {
            return 4;
        }

        StructWidget madeStruct = MakeDelegate<StructWidget> () ();
        if (!madeStruct.Equals (default (StructWidget)))
        {
            return 5;
        }

        // Controls: the direct `constrained. call` path.
        if (TagDirect<RefWidget> () != 11)
        {
            return 6;
        }

        if (TagDirect<StructWidget> () != 33)
        {
            return 7;
        }

        return 0;
    }
}
