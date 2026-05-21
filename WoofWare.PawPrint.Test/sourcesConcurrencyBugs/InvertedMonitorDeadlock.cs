using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static object locker1 = new object ();
        static object locker2 = new object ();

        static void Worker1()
        {
            Monitor.Enter(locker1);
            Monitor.Enter(locker2);

            Monitor.Exit(locker1);
            Monitor.Exit(locker2);
        }

        static void Worker2()
        {
            Monitor.Enter(locker2);
            Monitor.Enter(locker1);

            Monitor.Exit(locker2);
            Monitor.Exit(locker1);
        }

        static int Main(string[] args)
        {
            Thread t1 = new Thread(Worker1);
            Thread t2 = new Thread(Worker2);
            t1.Start();
            t2.Start();
            t1.Join();
            t2.Join();
            return 0;
        }
    }
}


