using System;

// Comparing a runtime handle against the result of an identity bit-operation on that same
// handle. `h ^ 0`, `~~h` and friends are the identity on a real address, so every comparison
// here is true on any runtime — none of them names an address, and none of them assumes an
// address is small, large, or shaped in any particular way.
//
// The zeroes are derived from `args.Length` rather than written as literals so that the
// compiler cannot fold the operation away and leave the file testing nothing.
class HandleBitsIdentityComparison
{
    struct First
    {
        public int A;
    }

    struct Second
    {
        public long B;
    }

    static int TestXorZeroIsIdentity(ulong zero)
    {
        IntPtr h = typeof(First).TypeHandle.Value;
        ulong bits = (ulong)h;

        if ((bits ^ zero) != bits)
            return 1;

        // Twice, so the comparison is between two values that have *both* been through the
        // operation rather than one of each.
        if (((bits ^ zero) ^ zero) != (bits ^ zero))
            return 2;

        return 0;
    }

    static int TestIdentityAtNativeIntWidth(ulong zero)
    {
        IntPtr h = typeof(First).TypeHandle.Value;

        if ((IntPtr)((ulong)h ^ zero) != h)
            return 10;

        return 0;
    }

    static int TestDoubleComplementIsIdentity()
    {
        IntPtr h = typeof(First).TypeHandle.Value;
        ulong bits = (ulong)h;

        if (~(~bits) != bits)
            return 20;

        return 0;
    }

    // The comparison is about the handle's identity, not about the expression that produced
    // it: a second, independently obtained handle for the same type must compare equal, and a
    // different type's handle must not.
    static int TestIdentityNotExpression(ulong zero)
    {
        ulong first = (ulong)typeof(First).TypeHandle.Value;
        ulong firstAgain = (ulong)typeof(First).TypeHandle.Value;
        ulong second = (ulong)typeof(Second).TypeHandle.Value;

        if ((first ^ zero) != firstAgain)
            return 30;

        if ((first ^ zero) == second)
            return 31;

        if ((second ^ zero) == first)
            return 32;

        if ((first ^ zero) == (second ^ zero))
            return 33;

        return 0;
    }

    static int Main(string[] args)
    {
        ulong zero = (ulong)args.Length;

        int result = TestXorZeroIsIdentity(zero);
        if (result != 0)
            return result;

        result = TestIdentityAtNativeIntWidth(zero);
        if (result != 0)
            return result;

        result = TestDoubleComplementIsIdentity();
        if (result != 0)
            return result;

        result = TestIdentityNotExpression(zero);
        if (result != 0)
            return result;

        return 0;
    }
}
