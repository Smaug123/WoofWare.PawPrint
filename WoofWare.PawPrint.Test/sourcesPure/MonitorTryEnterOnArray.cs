using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // The `ref bool` and timeout-carrying entry points are separate fastpaths
            // from plain `Monitor.Enter`, so exercise them against an array target too.
            byte[] locker = new byte[4];

            bool taken = false;
            Monitor.Enter(locker, ref taken);
            if (!taken)
            {
                return 1;
            }

            if (!Monitor.IsEntered(locker))
            {
                return 2;
            }

            // Reentrant TryEnter on a monitor we already own succeeds immediately.
            if (!Monitor.TryEnter(locker, 0))
            {
                return 3;
            }

            Monitor.Exit(locker);

            if (!Monitor.IsEntered(locker))
            {
                return 4;
            }

            Monitor.Exit(locker);

            if (Monitor.IsEntered(locker))
            {
                return 5;
            }

            return 0;
        }
    }
}
