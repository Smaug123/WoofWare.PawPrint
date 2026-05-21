using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int counter1;
        static int counter2;

        static void Worker1()
        {
            counter1 += 1;
            counter2 += 1;
            if (counter1 < counter2) {
                throw new Exception("counter2 was bigger than counter1!");
            }
        }

        static void Worker2()
        {
            counter1 += 1;
            counter2 += 1;
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

