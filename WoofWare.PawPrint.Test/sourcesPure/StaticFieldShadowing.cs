class Base
{
    public static int X = 1;
}

class Derived : Base
{
    public new static int X = 2;
}

class Program
{
    static int Main(string[] argv)
    {
        // Verify base and derived each see their own static field
        if (Base.X != 1) return 1;
        if (Derived.X != 2) return 2;

        // Mutate derived, base should be unaffected
        Derived.X = 42;
        if (Base.X != 1) return 3;
        if (Derived.X != 42) return 4;

        // Mutate base, derived should be unaffected
        Base.X = 99;
        if (Base.X != 99) return 5;
        if (Derived.X != 42) return 6;

        return 0;
    }
}
