unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[4];
        bytes[0] = 10;
        bytes[1] = 20;
        bytes[2] = (byte)(bytes[0] + bytes[1]);
        bytes[3] = 40;

        return bytes[2] == 30 && bytes[3] == 40 ? 0 : 1;
    }
}
