// Interface slot *ownership*: which type's method implements a given interface-map entry's slot.
// A same-signature method on the receiver is not enough on its own; what decides it is which type
// declares the interface (see `InterfaceDispatch.ownDispatchMapOf`).
//
// The two cases below are the two directions of that rule, so that getting one right cannot be
// done by biasing towards the base or towards the derived type:
//
//   * `Hidden` — the derived type must NOT take the slot. It only hides (`new`) the base's
//     implicit implementation and never declares the interface, so the slot stays on the base.
//     This half involves no variance at all.
//
//   * `Redeclared` — the derived type MUST take the slot. It re-declares the base's
//     instantiation *and* supplies a matching method, which re-implements the slot. (The
//     metadata cannot be read by entry order alone here: `Redeclared` and the `InheritedParent`
//     case in `VariantInterfaceMapOrder.cs` have the same InterfaceImpl row shape and opposite
//     correct answers, because the C# compiler flattens the interface closure into the row list.
//     Only slot ownership separates them.)

using System;

interface ISlot<in T> { long Accept(T value); }

// --- The derived type must NOT take the slot. -------------------------------------------

class HiddenBase : ISlot<object>
{
    public long Accept(object value) => 1;
}

sealed class Hidden : HiddenBase
{
    public new long Accept(object value) => 2;
}

// --- The derived type MUST take the slot. -----------------------------------------------

class RedeclaredBase : ISlot<object>
{
    public long Accept(object value) => 3;
}

sealed class Redeclared : RedeclaredBase, ISlot<object>, ISlot<Exception>
{
    public new long Accept(object value) => 4;
    public long Accept(Exception value) => 5;
}

class Program
{
    static long CallExact(ISlot<object> sink, object value) => sink.Accept(value);
    static long CallVariant(ISlot<ArgumentException> sink, ArgumentException value) => sink.Accept(value);

    static int Main(string[] args)
    {
        ArgumentException e = new ArgumentException("boom");

        // No variance: the base keeps the slot.
        if (CallExact(new Hidden(), e) != 1) return 1;

        // `Redeclared` re-implements ISlot<object>, so that slot binds to its own Accept(object)
        // and wins over its ISlot<Exception> slot.
        if (CallVariant(new Redeclared(), e) != 4) return 2;

        return 0;
    }
}
