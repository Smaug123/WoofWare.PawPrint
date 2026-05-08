using System;

namespace RuntimeTypeHandleGetInstantiationClosedGeneric
{
    class Box<T> { }

    class Pair<T, U> { }

    class Program
    {
        static int Main(string[] args)
        {
            Type[] none = typeof(int).GetGenericArguments();
            if (none.Length != 0) return 1;

            Type[] one = typeof(Box<int>).GetGenericArguments();
            if (one.Length != 1) return 2;
            if (one[0] != typeof(int)) return 3;

            Type[] two = typeof(Pair<string, int>).GetGenericArguments();
            if (two.Length != 2) return 4;
            if (two[0] != typeof(string)) return 5;
            if (two[1] != typeof(int)) return 6;

            return 0;
        }
    }
}
