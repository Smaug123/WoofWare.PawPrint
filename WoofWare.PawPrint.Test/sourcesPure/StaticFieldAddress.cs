class Program
{
    private static int value;

    private static ref int ValueRef()
    {
        return ref value;
    }

    static int Main(string[] args)
    {
        ref int valueRef = ref ValueRef();
        valueRef = 41;

        return value == 41 ? 0 : 1;
    }
}
