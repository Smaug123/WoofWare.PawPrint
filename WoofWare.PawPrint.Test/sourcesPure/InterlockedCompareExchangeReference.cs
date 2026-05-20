using System.Threading;

namespace InterlockedCompareExchangeReference
{
    class Marker
    {
    }

    class Program
    {
        static int Main(string[] args)
        {
            Marker a = new Marker();
            Marker b = new Marker();
            Marker location = null;

            Marker old = Interlocked.CompareExchange(ref location, a, null);
            if (!ReferenceEquals(old, null) || !ReferenceEquals(location, a))
            {
                return 1;
            }

            old = Interlocked.CompareExchange(ref location, b, null);
            if (!ReferenceEquals(old, a) || !ReferenceEquals(location, a))
            {
                return 2;
            }

            old = Interlocked.CompareExchange(ref location, b, a);
            if (!ReferenceEquals(old, a) || !ReferenceEquals(location, b))
            {
                return 3;
            }

            return 0;
        }
    }
}
