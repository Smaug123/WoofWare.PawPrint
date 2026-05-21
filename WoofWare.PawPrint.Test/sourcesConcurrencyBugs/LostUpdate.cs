using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int counter;

        static void Worker()
        {
            int local = counter;
            counter = local + 1;
        }

        static int Main(string[] args)
        {
            counter = 0;
            Thread t1 = new Thread(Worker);
            Thread t2 = new Thread(Worker);
            t1.Start();
            t2.Start();
            t1.Join();
            t2.Join();
            return counter == 2 ? 0 : 1;
        }
    }
}
