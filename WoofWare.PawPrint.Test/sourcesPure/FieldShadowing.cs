public class Program
{
    public class Base
    {
        public int Value { get; set; }
    }

    public class Derived : Base
    {
        public new int Value { get; set; }
    }

    public static int Main(string[] args)
    {
        Derived d = new Derived();
        d.Value = 42;
        ((Base)d).Value = 99;

        // The two Value properties should be independent: Derived.Value and Base.Value
        // have separate backing fields even though both are named <Value>k__BackingField.
        if (d.Value != 42) return 1;
        if (((Base)d).Value != 99) return 2;

        return 0;
    }
}
