// Tests that delegate invocation does not leave the delegate instance on the evaluation stack.
// When a delegate's Invoke is called, the first argument (the delegate "this") should NOT be
// forwarded to the underlying target method; only the Invoke parameters should be.
//
// If the delegate instance leaks onto the eval stack, calling the delegate twice in the same
// method will accumulate garbage on the stack and ultimately produce wrong results or crash.

public delegate int IntTransform(int x);

public class DelegateInvokeArgs
{
    private static int Double(int x)
    {
        return x * 2;
    }

    private static int AddOne(int x)
    {
        return x + 1;
    }

    public static int Main(string[] argv)
    {
        IntTransform doubler = Double;
        IntTransform incrementer = AddOne;

        // Call delegate, use its return value, then call another delegate.
        // If the delegate instance is left on the stack after each call,
        // these intermediate results will be wrong.
        int a = doubler(5);    // should be 10
        int b = incrementer(a); // should be 11

        if (b != 11) return 1;

        // Call the same delegate multiple times in sequence.
        int c = doubler(1);  // should be 2
        int d = doubler(c);  // should be 4
        int e = doubler(d);  // should be 8

        if (e != 8) return 2;

        // Use delegate result in arithmetic
        int f = doubler(3) + incrementer(7);  // should be 6 + 8 = 14
        if (f != 14) return 3;

        return 0;
    }
}
