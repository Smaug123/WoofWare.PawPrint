public class Program
{
    public class Outer
    {
        public class Inner
        {
            public int Value { get; set; }
        }
    }

    public static int Main(string[] args)
    {
        Outer.Inner inner = new Outer.Inner { Value = 42 };

        // Cast nested type to object
        object obj = (object)inner;

        // Cast back
        Outer.Inner casted = (Outer.Inner)obj;

        return casted.Value;
    }
}
