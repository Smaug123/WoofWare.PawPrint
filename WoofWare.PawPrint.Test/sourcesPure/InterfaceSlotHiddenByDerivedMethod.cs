// A derived class that merely *hides* (`new`) a base class's implicit interface implementation
// does not take over the interface slot. `BaseSink` implements `ISink<object>`; `DerivedSink`
// does not list the interface at all, so its `new Accept(object)` is an unrelated method and
// dispatch through the interface must still reach `BaseSink.Accept` (1, not 2).
//
// PawPrint gets this wrong, and does so with no variance involved at all — see `CallExact`
// below, which is a plain `callvirt ISink<object>::Accept` on a `DerivedSink`. The cause is in
// `IlMachineStateExecution.methodMatches`: when the call target's declaring type is an interface
// it sets `allowImplicitInterfaceImplementation`, which skips the guard rejecting non-virtual
// and `newslot` candidates entirely, so `DerivedSink.Accept` matches by name and signature.
// `findClassImplementation` then walks the receiver before its base and picks it. Fixing this
// means teaching the class walk which type actually *owns* the interface slot rather than
// matching any name-and-signature-compatible method on the way down, which changes ordinary
// (non-variant) interface dispatch and so belongs in its own change.
//
// The variant call in `CallVariant` reaches the same wrong answer through
// `tryRetargetToVariantInterfaceMapEntry`, which retargets to `ISink<object>` and then hands off
// to exactly the same class walk. So this file will start passing when the class walk is fixed;
// nothing about the variance retarget needs to change.

using System;

interface ISink<in T> { long Accept(T value); }

class BaseSink : ISink<object>
{
    public long Accept(object value) => 1;
}

sealed class DerivedSink : BaseSink
{
    public new long Accept(object value) => 2;
}

class Program
{
    static long CallExact(ISink<object> sink, object value) => sink.Accept(value);
    static long CallVariant(ISink<ArgumentException> sink, ArgumentException value) => sink.Accept(value);

    static int Main(string[] args)
    {
        ArgumentException e = new ArgumentException("boom");
        DerivedSink sink = new DerivedSink();

        // No variance: the root cause, reproducible without any of the variant-dispatch code.
        if (CallExact(sink, e) != 1) return 1;

        // Under `in`-variance, via the interface-map retarget onto ISink<object>.
        if (CallVariant(sink, e) != 1) return 2;

        // Calling the hidden method directly still reaches the derived one, of course.
        if (sink.Accept(e) != 2) return 3;

        return 0;
    }
}
