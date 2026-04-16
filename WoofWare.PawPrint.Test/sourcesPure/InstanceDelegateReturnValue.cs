// Tests that invoking a delegate bound to an instance method correctly
// returns the method's actual return value, rather than the target object.
// This catches a bug where the delegate invocation path sets `constructing = Some target`,
// causing `returnStackFrame` to treat the call like a constructor and push
// the target object instead of propagating the callee's real return value.

public delegate int IntComputer(int x);

public class Calculator
{
    private int _factor;

    public Calculator(int factor)
    {
        _factor = factor;
    }

    public int Multiply(int x)
    {
        return x * _factor;
    }

    public int Add(int x)
    {
        return x + _factor;
    }

    public static int Main(string[] argv)
    {
        var calc = new Calculator(3);

        // Instance delegate: target is the Calculator object
        IntComputer tripler = calc.Multiply;
        int result = tripler(5);  // should be 15

        if (result != 15) return 1;

        IntComputer adder = calc.Add;
        int result2 = adder(10);  // should be 13

        if (result2 != 13) return 2;

        // Chain: use the return value of one instance delegate call
        // as input to another
        int chained = adder(tripler(4));  // tripler(4)=12, adder(12)=15
        if (chained != 15) return 3;

        return 0;
    }
}
