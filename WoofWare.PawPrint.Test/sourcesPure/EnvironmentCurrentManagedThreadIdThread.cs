using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int mainEnvironmentId;
        static int constructedThreadId;
        static int workerEnvironmentId;
        static int workerEnvironmentIdAgain;

        static void Worker()
        {
            workerEnvironmentId = Environment.CurrentManagedThreadId;
            workerEnvironmentIdAgain = Environment.CurrentManagedThreadId;
        }

        static int Main(string[] args)
        {
            mainEnvironmentId = Environment.CurrentManagedThreadId;

            Thread t = new Thread(Worker);
            constructedThreadId = t.ManagedThreadId;
            if (constructedThreadId == mainEnvironmentId)
            {
                return 1;
            }

            t.Start();
            t.Join();

            if (workerEnvironmentId <= 0)
            {
                return 2;
            }

            if (workerEnvironmentId != constructedThreadId)
            {
                return 3;
            }

            if (workerEnvironmentId != workerEnvironmentIdAgain)
            {
                return 4;
            }

            return 0;
        }
    }
}
