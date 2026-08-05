using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            // Arrays are ordinary heap objects and carry an object header just like any
            // other reference type, so they are legal monitor targets. Exercise a
            // single-dimensional array, a multi-dimensional array (a separate allocation
            // path in the runtime), and reentrant acquisition of the same array.
            int[] szArray = new int[3];
            int[,] multiDim = new int[2, 2];

            lock (szArray)
            {
                if (!Monitor.IsEntered(szArray))
                {
                    return 1;
                }

                lock (szArray)
                {
                    if (!Monitor.IsEntered(szArray))
                    {
                        return 2;
                    }
                }

                // Still held after the inner lock released one level of reentrancy.
                if (!Monitor.IsEntered(szArray))
                {
                    return 3;
                }

                // Locking one array must not lock a different one.
                if (Monitor.IsEntered(multiDim))
                {
                    return 4;
                }

                lock (multiDim)
                {
                    if (!Monitor.IsEntered(multiDim))
                    {
                        return 5;
                    }
                }
            }

            if (Monitor.IsEntered(szArray))
            {
                return 6;
            }

            return 0;
        }
    }
}
