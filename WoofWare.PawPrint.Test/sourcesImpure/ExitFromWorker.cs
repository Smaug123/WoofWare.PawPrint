using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static void Worker()
        {
            Environment.Exit(7);
        }

        static int Main(string[] args)
        {
            Thread t = new Thread(Worker);
            t.Start();
            t.Join();
            // Unreachable: the worker calls Environment.Exit before we rejoin.
            return 1;
        }
    }
}
