using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            int environmentId = Environment.CurrentManagedThreadId;
            if (environmentId <= 0)
            {
                return 1;
            }

            int threadId = Thread.CurrentThread.ManagedThreadId;
            if (environmentId != threadId)
            {
                return 2;
            }

            int environmentIdAgain = Environment.CurrentManagedThreadId;
            if (environmentId != environmentIdAgain)
            {
                return 3;
            }

            return 0;
        }
    }
}
