using System;

// Delegate.Combine, i.e. multicast delegates.
//
// Each check returns its own index on failure, and 0 means every check held. Deliberately
// *not* a bitmask: the harness observes the guest as a process, and a Unix exit code is eight
// bits, so a bitmask wide enough for these checks would silently truncate the high ones — and
// the real-runtime oracle that validates a parked file's expectations would then pass
// vacuously for exactly the checks least likely to be obvious.
public static class DelegateCombine
{
    private static int _log;

    private static void AddOne() => _log = _log * 10 + 1;
    private static void AddTwo() => _log = _log * 10 + 2;
    private static void AddThree() => _log = _log * 10 + 3;

    private static int _sum;

    private static int ReturnFour(int x)
    {
        _sum += x;
        return 4;
    }

    private static int ReturnFive(int x)
    {
        _sum += x * 2;
        return 5;
    }

    public static int Main()
    {
        // Combining with null on either side is the identity, and does not allocate a
        // multicast wrapper: the original delegate object comes back.
        Action one = AddOne;
        if (!ReferenceEquals(Delegate.Combine(one, null), one)) return 1;
        if (!ReferenceEquals(Delegate.Combine(null, one), one)) return 2;

        // Two single-cast delegates combine into a multicast which invokes both, in order.
        Action two = AddTwo;
        Action oneThenTwo = (Action) Delegate.Combine(one, two);
        _log = 0;
        oneThenTwo();
        if (_log != 12) return 3;

        // The invocation list is the flattened sequence, and holds the original delegate
        // objects rather than copies (`CombineImpl` stores `this` and `follow` directly, and
        // `GetInvocationList` copies the array, not its elements).
        Delegate[] list = oneThenTwo.GetInvocationList();
        if (list.Length != 2) return 4;
        if (!ReferenceEquals(list[0], one)) return 5;
        if (!ReferenceEquals(list[1], two)) return 6;

        // Combining a multicast with a single-cast appends, and combining in the other order
        // prepends.
        //
        // The shape here matters: `CombineImpl` grows the backing array by
        // doubling, so `allThree` has an invocation *count* of 3 in an array of *length* 4,
        // whose fourth slot is null. An implementation that walked the raw `_invocationList`
        // array instead of honouring `_invocationCount` — the single most plausible way to get
        // multicast dispatch wrong — hits that null and cannot pass this check. Keep the three-
        // from-two construction if this file is ever refactored.
        Action three = AddThree;
        Action allThree = (Action) Delegate.Combine(oneThenTwo, three);
        _log = 0;
        allThree();
        if (_log != 123) return 7;

        if (allThree.GetInvocationList().Length != 3) return 8;

        Action threeFirst = (Action) Delegate.Combine(three, oneThenTwo);
        _log = 0;
        threeFirst();
        if (_log != 312) return 9;

        // Neither operand was mutated by being combined, on either side.
        _log = 0;
        oneThenTwo();
        if (_log != 12) return 10;

        _log = 0;
        three();
        if (_log != 3) return 11;

        // Combining two multicasts concatenates both invocation lists.
        Action twoThenThree = (Action) Delegate.Combine(two, three);
        Action fourLong = (Action) Delegate.Combine(oneThenTwo, twoThenThree);
        _log = 0;
        fourLong();
        if (_log != 1223) return 12;

        if (fourLong.GetInvocationList().Length != 4) return 13;

        _log = 0;
        twoThenThree();
        if (_log != 23) return 14;

        // The C# `+` operator on delegates is Delegate.Combine.
        Action viaOperator = one + two + three;
        _log = 0;
        viaOperator();
        if (_log != 123) return 15;

        // A multicast with a return value invokes every target but yields the last one's
        // result. `_sum` witnesses that the earlier target really ran, so an implementation
        // that invoked only the last target cannot pass.
        Func<int, int> four = ReturnFour;
        Func<int, int> five = ReturnFive;
        Func<int, int> both = (Func<int, int>) Delegate.Combine(four, five);
        _sum = 0;
        if (both(10) != 5) return 16;
        if (_sum != 30) return 17;

        // The same target combined with itself appears twice; an invocation list is not a set.
        Action twice = (Action) Delegate.Combine(one, one);
        _log = 0;
        twice();
        if (_log != 11) return 18;

        if (twice.GetInvocationList().Length != 2) return 19;

        // Delegates of different types cannot combine.
        try
        {
            Delegate.Combine(one, four);
            return 20;
        }
        catch (ArgumentException)
        {
        }

        return 0;
    }
}
