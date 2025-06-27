public class Program
{
    public class MyType
    {
        public int Value { get; set; }
    }

    public static int Main(string[] args)
    {
        MyType obj = null;

        // isinst on null should return null
        object result = obj as object;

        return result == null ? 42 : 0;
    }
}
