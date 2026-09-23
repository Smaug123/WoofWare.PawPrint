using System;
using System.Diagnostics;
using System.Reflection;

// Invoking a multicast delegate, beyond the combining that `DelegateCombine.cs` covers: what the
// runtime's multicast invoke stub does when an element throws, how it passes a byref, that it
// leaves no frame of its own in a stack trace or a caller-sensitive walk, and the managed surface (`Method`, `Target`,
// `DynamicInvoke`, `Remove`, `Equals`) that CoreLib answers from the invocation list.
//
// Each check returns its own index on failure, and 0 means every check held.
public static class DelegateMulticastInvoke
{
    private static int _log;

    private static void AddOne() => _log = _log * 10 + 1;
    private static void AddThree() => _log = _log * 10 + 3;

    private static void Throws()
    {
        _log = _log * 10 + 2;
        throw new InvalidOperationException("boom");
    }

    private delegate void RefAction(ref int x);

    private static void TimesTenPlusOne(ref int x) => x = x * 10 + 1;
    private static void TimesTenPlusTwo(ref int x) => x = x * 10 + 2;

    private static StackTrace _captured;

    private static void Capture() => _captured = new StackTrace();

    private static Assembly _callingAssembly;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static void CaptureCallingAssembly() => _callingAssembly = Assembly.GetCallingAssembly();

    private sealed class Counter
    {
        public int Value;

        public int Bump(int by)
        {
            Value += by;
            return Value;
        }
    }

    private static int Negate(int x) => -x;

    public static int Main()
    {
        Action one = AddOne;
        Action three = AddThree;
        Action throws = Throws;

        // An element that throws ends the invocation: later elements do not run, and the
        // exception reaches the caller of `Invoke`.
        Action oneThrowsThree = one + throws + three;
        _log = 0;
        try
        {
            oneThrowsThree();
            return 1;
        }
        catch (InvalidOperationException e)
        {
            if (_log != 12) return 2;

            // The stub has no frame in the trace: the throwing element is reported, then its
            // caller's caller, which is this method.
            string st = e.StackTrace;
            if (st == null) return 3;
            if (!st.Contains("Throws")) return 4;
            if (!st.Contains("Main")) return 5;
            if (st.Contains("Invoke()")) return 6;
            if (st.Split('\n').Length != 2) return 7;
        }

        // The same holds for a stack captured while an element is running.
        Action captureTwice = (Action)Capture + (Action)Capture;
        captureTwice();
        MethodBase inner = _captured.GetFrame(0).GetMethod();
        MethodBase outer = _captured.GetFrame(1).GetMethod();
        if (inner == null || inner.Name != "Capture") return 8;
        if (outer == null || outer.Name != "Main") return 9;

        // A caller-sensitive method sees through the stub too: the stub's own frame would
        // belong to `System.Action`, in CoreLib, where the true caller is in this assembly.
        Action callingAssemblyTwice = (Action)CaptureCallingAssembly + (Action)CaptureCallingAssembly;
        callingAssemblyTwice();
        if (!ReferenceEquals(_callingAssembly, typeof(DelegateMulticastInvoke).Assembly)) return 23;

        // A byref argument is the same location for every element, so each sees the previous
        // one's write.
        RefAction byref = TimesTenPlusOne;
        byref += TimesTenPlusTwo;
        int v = 5;
        byref(ref v);
        if (v != 512) return 10;

        // Instance and static targets mix, each element keeping its own receiver, and the result
        // is the last element's.
        Counter counter = new Counter();
        Func<int, int> bump = counter.Bump;
        Func<int, int> negate = Negate;
        Func<int, int> bumpThenNegate = bump + negate;
        if (bumpThenNegate(7) != -7) return 11;
        if (counter.Value != 7) return 12;
        Func<int, int> negateThenBump = negate + bump;
        if (negateThenBump(3) != 10) return 13;

        // `Method` and `Target` report the last element.
        if (bumpThenNegate.Method.Name != "Negate") return 14;
        if (bumpThenNegate.Target != null) return 15;
        if (!ReferenceEquals(negateThenBump.Target, counter)) return 16;

        // `DynamicInvoke` goes through `Invoke`, and so through the stub.
        Action oneThree = one + three;
        _log = 0;
        oneThree.DynamicInvoke();
        if (_log != 13) return 17;

        // Two multicasts with equal invocation lists are equal, and removal takes the last
        // occurrence.
        if (!oneThree.Equals(one + three)) return 18;
        // Unequal only in length. Deliberately not unequal element-wise: comparing two distinct
        // single-cast delegates over different methods reaches `Delegate_InternalEqualMethodHandles`,
        // a QCall PawPrint does not implement, and which has nothing to do with multicast.
        if (oneThree.Equals(one + three + one)) return 19;
        Action removed = (one + three + one) - one;
        _log = 0;
        removed();
        if (_log != 13) return 20;
        if (!removed.Equals(oneThree)) return 21;

        // Removing all but one element collapses to that element itself, not a one-element
        // multicast.
        if (!ReferenceEquals(oneThree - three, one)) return 22;

        return 0;
    }
}
