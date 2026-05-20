namespace BitConverterIsLittleEndian
{
    class Program
    {
        static int Main(string[] args)
        {
            if (!System.BitConverter.IsLittleEndian) return 1;
            return 0;
        }
    }
}
