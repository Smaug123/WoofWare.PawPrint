// Regression guard for `[Intrinsic]` classification running AFTER virtual/interface
// resolution (IlMachineStateExecution.callMethod).
//
// `IEquatable<string>::Equals` is abstract and not intrinsic, so it clears the intrinsic
// check; resolution then selects the instance `String.Equals(string)`, which IS
// `[Intrinsic]`. Once classification moved post-resolution, this call started being
// dispatched as an intrinsic — and the hand-written `String.Equals` arm in Intrinsics.fs
// only accepts the STATIC two-argument overload, so the call failed outright until the
// instance overload was added to the safe-intrinsic allowlist.
//
// The direct (non-interface) call is exercised too: `String.Equals(string)` is virtual
// final, so it is never subject to virtual resolution and reaches the classifier directly.

using System;

class Program
{
    // Runtime concatenation, so the result is a fresh instance rather than the
    // interned literal the compiler would fold a constant expression into.
    static string BuildHello()
    {
        string tail = "lo";
        return "hel" + tail;
    }

    static string BuildValue()
    {
        string tail = "ue";
        return "val" + tail;
    }

    static int TestThroughInterface()
    {
        string a = "hello";
        IEquatable<string> eq = a;

        if (!eq.Equals("hello")) return 1;
        if (eq.Equals("world")) return 2;
        if (eq.Equals(null)) return 3;
        // Same content, distinct instance (built at runtime so it is not the interned
        // literal): must compare by value, so EqualsHelper actually runs and returns true.
        if (!eq.Equals(BuildHello())) return 4;
        // Same length, different content.
        if (eq.Equals("hellp")) return 5;
        // Different length.
        if (eq.Equals("hell")) return 6;

        return 0;
    }

    static int TestDirectCall()
    {
        string a = "value";

        if (!a.Equals("value")) return 10;
        if (a.Equals("other")) return 11;
        if (a.Equals(null)) return 12;
        if (!a.Equals(BuildValue())) return 13;

        // The static two-argument overload keeps its own dedicated handling.
        if (!string.Equals("x", "x")) return 14;
        if (string.Equals("x", "y")) return 15;
        if (!string.Equals(null, null)) return 16;

        return 0;
    }

    static int Main(string[] args)
    {
        int result;

        result = TestThroughInterface();
        if (result != 0) return 1000 + result;

        result = TestDirectCall();
        if (result != 0) return 2000 + result;

        return 0;
    }
}
