using System.Threading;

namespace HelloWorldApp
{
    // `Thread.UnsafeStart()` is `Start()` without the ExecutionContext capture, so the worker
    // begins with no context and reads the AsyncLocal's default. This is the counterpart of
    // ThreadStartFlowsAsyncLocal.cs: the value flows because `Start()` captured it, not because
    // every new thread inherits its starter's context.
    class Program
    {
        static readonly AsyncLocal<int> al = new AsyncLocal<int>();
        static int seenByWorker = -1;

        static void Worker()
        {
            seenByWorker = al.Value;
        }

        static int Main(string[] args)
        {
            al.Value = 42;
            Thread t = new Thread(Worker);
            t.UnsafeStart();
            t.Join();
            if (seenByWorker != 0)
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
