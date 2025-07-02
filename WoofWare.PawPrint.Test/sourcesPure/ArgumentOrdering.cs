public class Program
{
    public struct TestStruct
    {
        public int Value;

        public TestStruct(ref int x)
        {
            Value = x;
        }
    }

    public struct Calculator
    {
        private int baseValue;

        public Calculator(int initial)
        {
            baseValue = initial;
        }

        public int Add(int a, int b, int c)
        {
            return baseValue + a + b + c;
        }

        public int SubtractIsh(int a, int b)
        {
            return baseValue - a + b;
        }
    }

    public static int Main(string[] args)
    {
        int localVar = 42;
        TestStruct t = new TestStruct(ref localVar);
        if (t.Value != 42) return 1;

        Calculator calc = new Calculator(10);
        int addResult = calc.Add(1, 2, 3);  // Should be 10 + 1 + 2 + 3 = 16
        if (addResult != 16) return 2;

        // Test 2: Verify order matters
        int subResult = calc.SubtractIsh(3, 2);  // Should be 10 - 3 + 2 = 9
        if (subResult != 9) return 3;

        return 0;
    }
}
