public class Program
{
    public struct TestStruct
    {
        public int Value;

        public TestStruct(ref int x)
        {
            // Store the value from the ref parameter
            // If args are correct: this.Value = x (which should be 42)
            // If args are reversed: might crash or get wrong value
            Value = x;
        }
    }

    public static int Main(string[] args)
    {
        int localVar = 42;

        // This will compile to:
        // 1. ldc.i4.s 42
        // 2. stloc.0
        // 3. ldloca.s 0  (push address of localVar)
        // 4. newobj TestStruct::.ctor

        TestStruct t = new TestStruct(ref localVar);

        // Return the value stored in the struct
        // Should be 42 if arguments are correct
        return t.Value;
    }
}
