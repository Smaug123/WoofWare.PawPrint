using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int shared = 1;

        static void Worker()
        {
            shared = 0;
        }

        static int Main(string[] args)
        {
            Thread t = new Thread(Worker);
            t.Start();
            t.Join();
            // Second Join: the worker is already Terminated, so this should return true
            // immediately without blocking.
            t.Join();
            return shared;
        }
    }
}
