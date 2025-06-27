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

    public static int Main(string[] args)
    {
        int localVar = 42;
        TestStruct t = new TestStruct(ref localVar);
        return t.Value;
    }
}
