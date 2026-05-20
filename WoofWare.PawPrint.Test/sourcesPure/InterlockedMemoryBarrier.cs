using System.Threading;

class Program
{
    static int Main(string[] args)
    {
        int x = 41;
        Interlocked.MemoryBarrier();
        x++;
        Interlocked.MemoryBarrier();
        return x == 42 ? 0 : 1;
    }
}
