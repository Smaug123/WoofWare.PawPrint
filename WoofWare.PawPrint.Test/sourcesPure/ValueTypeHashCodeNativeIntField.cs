using System;

// A native-int field does not stop a struct being bit-comparable: CoreCLR's field loop rejects
// only floats and non-bit-comparable nested value types. So this struct never reaches
// `ValueType_GetHashCodeStrategy` at all — it takes the whole-image fast path, and its hash
// therefore tracks *every* field rather than just the first.
public class Program
{
    private struct NativeIntThenLong
    {
        public IntPtr P;
        public long A;
    }

    public static int Main(string[] args)
    {
        NativeIntThenLong same1 = new NativeIntThenLong { P = (IntPtr)7, A = 1 };
        NativeIntThenLong same2 = new NativeIntThenLong { P = (IntPtr)7, A = 1 };
        if (same1.GetHashCode() != same2.GetHashCode())
        {
            return 1;
        }

        // The second field contributes. Were the struct wrongly held not to be bit-comparable,
        // the strategy walk would commit to `P` alone and this would be invariant under `A`.
        NativeIntThenLong otherLong = new NativeIntThenLong { P = (IntPtr)7, A = 2 };
        if (same1.GetHashCode() == otherLong.GetHashCode())
        {
            return 2;
        }

        NativeIntThenLong otherNativeInt = new NativeIntThenLong { P = (IntPtr)8, A = 1 };
        if (same1.GetHashCode() == otherNativeInt.GetHashCode())
        {
            return 3;
        }

        // `Equals` reads the same predicate, and takes the byte-comparing path for the same reason.
        if (!same1.Equals(same2))
        {
            return 4;
        }

        if (same1.Equals(otherLong))
        {
            return 5;
        }

        return 0;
    }
}
