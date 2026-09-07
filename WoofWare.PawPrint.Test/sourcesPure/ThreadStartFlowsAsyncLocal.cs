using System.Threading;

namespace HelloWorldApp
{
    // `Thread.Start()` captures the starter's ExecutionContext into the thread's StartHelper,
    // and the new thread runs its delegate inside `ExecutionContext.RunInternal` over that
    // capture, so an AsyncLocal value set before `Start()` is visible to the worker. The
    // capture is copy-on-write: a write on the worker does not reach the starter.
    class Program
    {
        static readonly AsyncLocal<int> al = new AsyncLocal<int>();
        static int seenByWorker = -1;

        static void Worker()
        {
            seenByWorker = al.Value;
            al.Value = 7;
        }

        static int Main(string[] args)
        {
            al.Value = 42;
            Thread t = new Thread(Worker);
            t.Start();
            t.Join();
            if (seenByWorker != 42)
            {
                return 1;
            }
            if (al.Value != 42)
            {
                return 2;
            }
            return 0;
        }
    }
}
