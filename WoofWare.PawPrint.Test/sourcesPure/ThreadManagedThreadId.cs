using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            int id1 = Thread.CurrentThread.ManagedThreadId;
            if (id1 <= 0) return 1;

            int id2 = Thread.CurrentThread.ManagedThreadId;
            if (id1 != id2) return 2;

            return 0;
        }
    }
}
