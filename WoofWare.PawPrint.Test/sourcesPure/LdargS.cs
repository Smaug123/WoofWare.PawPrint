public class Program
{
    // Static method with many parameters to force ldarg.s for indices >= 4
    public static int Add(int a, int b, int c, int d, int e, int f)
    {
        // a=ldarg.0, b=ldarg.1, c=ldarg.2, d=ldarg.3, e=ldarg.s 4, f=ldarg.s 5
        return a + b + c + d + e + f;
    }

    // Instance method: 'this' is arg 0, so named params start at arg 1
    // Parameters d and e will use ldarg.s (indices 4 and 5)
    public int InstanceAdd(int a, int b, int c, int d, int e)
    {
        return a + b + c + d + e;
    }

    public static int Main(string[] args)
    {
        // Test 1: static method with 6 params
        int result1 = Add(1, 2, 3, 4, 5, 6);
        if (result1 != 21) return 1;

        // Test 2: instance method with 5 params (this + 5 = 6 args total)
        Program p = new Program();
        int result2 = p.InstanceAdd(10, 20, 30, 40, 50);
        if (result2 != 150) return 2;

        // Test 3: verify argument ordering is correct
        int result3 = Add(100, 0, 0, 0, 0, 1);
        if (result3 != 101) return 3;

        int result4 = Add(0, 0, 0, 0, 0, 77);
        if (result4 != 77) return 4;

        return 0;
    }
}
