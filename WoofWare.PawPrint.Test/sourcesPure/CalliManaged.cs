using System;

public class Program
{
    // `calli` with the managed calling convention. C#'s `&Method` emits `ldftn`,
    // and invoking the resulting `delegate*<...>` emits `calli` against a
    // StandaloneSignature token describing the call site.

    static int Add(int a, int b) => a + b;

    static int Negate(int a) => -a;

    static int Zero() => 0;

    static long Widen(int a, long b, short c) => a + b + c;

    static double Mix(double a, float b) => a + b;

    static int sideEffect;

    static void SetSideEffect(int v) => sideEffect = v;

    static string Concat(string a, string b) => a + b;

    struct Point
    {
        public int X;
        public int Y;
    }

    static Point MakePoint(int x, int y) => new Point { X = x, Y = y };

    static int SumPoint(Point p) => p.X + p.Y;

    static void AddOne(ref int slot) => slot += 1;

    static int Recurse(int n)
    {
        if (n <= 0) return 0;
        unsafe
        {
            delegate*<int, int> self = &Recurse;
            return n + self(n - 1);
        }
    }

    static int Throws(int n) => throw new InvalidOperationException("boom " + n);

    public static unsafe int Main(string[] args)
    {
        // Two arguments, int return.
        delegate*<int, int, int> add = &Add;
        if (add(2, 3) != 5) return 1;
        if (add(-4, 4) != 0) return 2;

        // One argument.
        delegate*<int, int> negate = &Negate;
        if (negate(7) != -7) return 3;

        // Zero arguments.
        delegate*<int> zero = &Zero;
        if (zero() != 0) return 4;

        // Mixed integer widths; verifies the call-site signature drives argument
        // coercion rather than the eval stack's natural widths.
        delegate*<int, long, short, long> widen = &Widen;
        if (widen(1, 2L, 3) != 6L) return 5;

        // Floating point.
        delegate*<double, float, double> mix = &Mix;
        if (Math.Abs(mix(1.5, 2.25f) - 3.75) > 0.00001) return 6;

        // Void return: nothing must be pushed to the eval stack.
        delegate*<int, void> setter = &SetSideEffect;
        setter(42);
        if (sideEffect != 42) return 7;

        // Reference-typed arguments and return.
        delegate*<string, string, string> concat = &Concat;
        if (concat("ab", "cd") != "abcd") return 8;

        // Value-type return and value-type argument.
        delegate*<int, int, Point> makePoint = &MakePoint;
        Point p = makePoint(3, 4);
        if (p.X != 3) return 9;
        if (p.Y != 4) return 10;

        delegate*<Point, int> sumPoint = &SumPoint;
        if (sumPoint(p) != 7) return 11;

        // Byref argument: the callee must mutate the caller's local.
        int slot = 10;
        delegate*<ref int, void> addOne = &AddOne;
        addOne(ref slot);
        if (slot != 11) return 12;

        // A function pointer stored in a local, reassigned, and called again:
        // the target is a runtime value, not a compile-time constant.
        delegate*<int, int> chosen = &Negate;
        if (chosen(5) != -5) return 13;
        chosen = &Zero_Ignore;
        if (chosen(5) != 99) return 14;

        // Function pointer passed as an argument and invoked by the callee.
        if (Apply(&Negate, 8) != -8) return 15;

        // Recursion through a function pointer.
        delegate*<int, int> recurse = &Recurse;
        if (recurse(4) != 10) return 16;

        // Function pointers stored in an array.
        delegate*<int, int>[] table = new delegate*<int, int>[2];
        table[0] = &Negate;
        table[1] = &Zero_Ignore;
        if (table[0](3) != -3) return 17;
        if (table[1](3) != 99) return 18;

        // Exceptions propagate out of a calli frame like any other call.
        delegate*<int, int> throws = &Throws;
        try
        {
            throws(5);
            return 19;
        }
        catch (InvalidOperationException e)
        {
            if (e.Message != "boom 5") return 20;
        }

        // Equality of function pointers taken twice from the same method.
        delegate*<int, int> a1 = &Negate;
        delegate*<int, int> a2 = &Negate;
        if (a1 != a2) return 21;
        if (a1 == chosen) return 22;

        return 0;
    }

    static int Zero_Ignore(int _) => 99;

    static unsafe int Apply(delegate*<int, int> f, int x) => f(x);
}
