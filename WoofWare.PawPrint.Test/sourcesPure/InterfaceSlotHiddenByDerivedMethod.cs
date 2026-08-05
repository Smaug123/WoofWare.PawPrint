// PawPrint does not model interface *slot ownership* — which type's method implements a given
// interface-map entry's slot. Its class walk (`IlMachineStateExecution.findClassImplementation`)
// starts at the receiver and takes the first method matching the target's name and signature,
// and when the call target's declaring type is an interface, `methodMatches` sets
// `allowImplicitInterfaceImplementation`, which skips the guard that would otherwise reject
// non-virtual and `newslot` candidates. So any same-signature method on the way down wins,
// whether or not it has anything to do with the slot.
//
// Both cases below are that gap. They are deliberately the two directions of it, so that a fix
// has to get ownership genuinely right rather than flip a bias:
//
//   * `Hidden` — the derived type must NOT take the slot. It only hides (`new`) the base's
//     implicit implementation and never declares the interface, so the slot stays on the base.
//     We answer with the derived method. This half involves no variance at all and fails
//     identically on `main`.
//
//   * `Redeclared` — the derived type MUST take the slot. It re-declares the base's
//     instantiation *and* supplies a matching method, which re-implements the slot. We answer
//     with the derived type's other overload instead, because we pick the interface-map entry by
//     order and never ask which type implements each slot. (Note the metadata cannot be read by
//     entry order alone here: `Redeclared` and the *passing* `InheritedParent` case in
//     `VariantInterfaceMapOrder.cs` have the same InterfaceImpl row shape and opposite correct
//     answers, because the C# compiler flattens the interface closure into the row list. Only
//     slot ownership separates them.)
//
// The variance work deliberately did not try to fix this: `tryResolveVirtualImplementation`
// scopes each retargeted entry to the entry's owner, which is as far as the interface map alone
// can go (see the cases it *does* fix in `VariantInterfaceSlotOwnership.cs`). Getting the rest
// right needs a real dispatch map — slot to implementing method, per interface entry — which
// changes ordinary non-variant interface dispatch too and so wants its own change.

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

        // No variance: the root cause, reproducible without any of the variant-dispatch code.
        // PawPrint answers 2.
        if (CallExact(new Hidden(), e) != 1) return 1;

        // `Redeclared` re-implements ISlot<object>, so that slot binds to its own Accept(object)
        // and wins over its ISlot<Exception> slot. PawPrint answers 5.
        if (CallVariant(new Redeclared(), e) != 4) return 2;

        return 0;
    }
}
