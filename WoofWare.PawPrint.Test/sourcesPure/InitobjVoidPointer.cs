unsafe class Program
{
    static void Clear(out void* value)
    {
        value = default;
    }

    static int Main(string[] args)
    {
        void* value = (void*)123;
        Clear(out value);

        return 0;
    }
}
