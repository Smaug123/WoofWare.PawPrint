using System;
using System.Collections.Generic;

namespace ArrayGetInterfacesByName
{
    class Program
    {
        // GetInterface(name) drives PopulateInterfaces down its name-filtering path, which
        // calls RuntimeTypeHandle.GetUtf8Name on each candidate — including the synthesised
        // generic ones. That is a different code path from the unfiltered GetInterfaces().
        static int Main(string[] args)
        {
            Type found = typeof(int[]).GetInterface("IList`1");

            if (found == null)
            {
                return 1;
            }

            if (found != typeof(IList<int>))
            {
                return 2;
            }

            // The non-generic IList is a distinct interface, reachable under its own name.
            Type nonGeneric = typeof(int[]).GetInterface("IList");

            if (nonGeneric != typeof(System.Collections.IList))
            {
                return 3;
            }

            if (typeof(int[]).GetInterface("IReadOnlyCollection`1") != typeof(IReadOnlyCollection<int>))
            {
                return 4;
            }

            if (typeof(string[]).GetInterface("IEnumerable`1") != typeof(IEnumerable<string>))
            {
                return 5;
            }

            // A multi-dimensional array has no generic interfaces, so the lookup misses.
            if (typeof(int[,]).GetInterface("IList`1") != null)
            {
                return 6;
            }

            // ... but still finds the ones inherited from System.Array.
            if (typeof(int[,]).GetInterface("ICloneable") != typeof(ICloneable))
            {
                return 7;
            }

            // A genuine miss is null rather than a fault.
            if (typeof(int[]).GetInterface("INotAnInterface") != null)
            {
                return 8;
            }

            return 0;
        }
    }
}
