using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int counter;
        static bool isWorking;

        static void Worker1()
        {
            while (counter < 2) {
                counter += 1;
                if (counter == 2) {
                    if (isWorking) {
                        throw new Exception("we were not the first");
                    }
                    isWorking = true;
                    for (int i = 0; i < 1000; i += 1) {}
                }
            }
        }

        static void Worker2()
        {
            while (true) {
                counter += 1;
                if (counter == 3) {
                    if (isWorking) {
                        throw new Exception("Worker1 is in the critical section at the same time as us");
                    }
                    isWorking = true;
                    return;
                }
            }
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

