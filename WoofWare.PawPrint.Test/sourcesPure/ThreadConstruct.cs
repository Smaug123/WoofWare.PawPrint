using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            Thread t = new Thread(() => { });
            if (t == null) return 1;

            int id = t.ManagedThreadId;
            if (id <= 0) return 2;

            if (id == Thread.CurrentThread.ManagedThreadId) return 3;

            // Main thread's managed ID must be stable regardless of Thread construction ordering.
            int mainId = Thread.CurrentThread.ManagedThreadId;
            if (mainId <= 0) return 4;
            if (mainId >= id) return 5;

            return 0;
        }
    }
}
