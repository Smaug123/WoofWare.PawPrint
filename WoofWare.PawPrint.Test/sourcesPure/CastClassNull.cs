public class Program
{
    public class MyClass
    {
        public int Value { get; set; }
    }

    public static int Main(string[] args)
    {
        MyClass obj = null;

        // Cast null reference - should succeed and remain null
        object result = (object)obj;

        return result == null ? 42 : 0;
    }
}
