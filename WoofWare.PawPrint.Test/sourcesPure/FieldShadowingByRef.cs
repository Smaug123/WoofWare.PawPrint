public class Program
{
    public class Base
    {
        public int Value;
    }

    public class Derived : Base
    {
        public new int Value;
    }

    private static void Set(ref int target, int value)
    {
        target = value;
    }

    public static int Main(string[] args)
    {
        Derived d = new Derived();

        Set(ref d.Value, 42);
        Set(ref ((Base)d).Value, 99);

        if (d.Value != 42) return 1;
        if (((Base)d).Value != 99) return 2;

        return 0;
    }
}
