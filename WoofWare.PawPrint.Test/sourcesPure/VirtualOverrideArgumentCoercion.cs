// Regression guard for the argument-coercion basis in
// `IlMachineStateExecution.callMethod`. `argZeroObjects` supplies the coercion target for
// every popped argument, and it must be derived from the method that virtual/interface
// resolution SELECTED, not from the declaration named at the call site.
//
// The sharpest case — where the declaration and the selected body genuinely disagree on a
// parameter type — is contravariant dispatch, and that is already pinned by
// `ContravariantExplicitMethodImpl.cs` (call site `IContravariant<string>::Set(string)`,
// body `Set(object)`). This file covers the complementary shape: generic virtual methods
// reached through an abstract declaration, where `methodGenerics` must be threaded through
// resolution before the parameter zero-values are computed.
//
// See also `ContravariantImplicitInterfaceDispatch.cs`, which covers the contravariant case
// for an *implicit* interface implementation and does not yet pass.

using System;

struct Extent
{
    public int Lo;
    public long Hi;
}

abstract class Runner
{
    public abstract long Run<U>(U item, int scale, string label, int[] data);
}

sealed class DerivedRunner : Runner
{
    public object SeenItem;
    public string SeenLabel;
    public int SeenData;

    public override long Run<U>(U item, int scale, string label, int[] data)
    {
        SeenItem = item;
        SeenLabel = label;
        SeenData = data[data.Length - 1];
        return scale * 2L;
    }
}

class Program
{
    // Generic virtual method reached through the abstract declaration, with a value-type
    // and then a reference-type instantiation.
    static int TestGenericVirtualOverride()
    {
        DerivedRunner runner = new DerivedRunner();
        Runner asBase = runner;

        Extent extent = new Extent { Lo = 11, Hi = 22L };
        long result = asBase.Run<Extent>(extent, 5, "struct-call", new int[] { 1, 2, 3 });

        if (result != 10L) return 1;
        if (!(runner.SeenItem is Extent seen)) return 2;
        if (seen.Lo != 11 || seen.Hi != 22L) return 3;
        if (runner.SeenLabel != "struct-call") return 4;
        if (runner.SeenData != 3) return 5;

        result = asBase.Run<string>("ref-call", -4, "ref-label", new int[] { 9 });
        if (result != -8L) return 6;
        if (!(runner.SeenItem is string item) || item != "ref-call") return 7;
        if (runner.SeenLabel != "ref-label") return 8;
        if (runner.SeenData != 9) return 9;

        // A null reference argument must survive coercion to the override's parameter.
        result = asBase.Run<string>(null, 0, null, new int[] { -1 });
        if (result != 0L) return 10;
        if (runner.SeenItem != null) return 11;
        if (runner.SeenLabel != null) return 12;
        if (runner.SeenData != -1) return 13;

        return 0;
    }

    static int Main(string[] args)
    {
        int result = TestGenericVirtualOverride();
        if (result != 0) return 1000 + result;

        return 0;
    }
}
