using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int[] locker = new int[1];
        static int shared = 0;

        static void Worker()
        {
            lock (locker)
            {
                shared = 99;
            }
        }

        static int Main(string[] args)
        {
            Thread t = new Thread(Worker);
            lock (locker)
            {
                t.Start();
                shared = 7;
            }
            t.Join();
            return shared == 99 ? 0 : 1;
        }
    }
}
