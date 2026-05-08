using System;

namespace RuntimeTypeHandleGetInstantiationOpenGeneric
{
    class Box<T> { }

    class Pair<T, U> { }

    class Program
    {
        static int Main(string[] args)
        {
            Type[] one = typeof(Box<>).GetGenericArguments();
            if (one == null) return 1;
            if (one.Length != 1) return 2;
            if (one[0] == null) return 3;

            Type[] two = typeof(Pair<,>).GetGenericArguments();
            if (two == null) return 4;
            if (two.Length != 2) return 5;
            if (two[0] == null) return 6;
            if (two[1] == null) return 7;

            // Each parameter slot must be a distinct RuntimeType instance: the two parameters
            // of Pair<,> have different positions, so they cannot share an allocation.
            if (object.ReferenceEquals(two[0], two[1])) return 8;

            // Generic-parameter RuntimeTypes must report IsGenericParameter = true. A closed
            // type's GetGenericArguments() entries (e.g. typeof(Box<int>).GetGenericArguments()[0])
            // are not parameters and must report false.
            if (!one[0].IsGenericParameter) return 9;
            if (!two[0].IsGenericParameter) return 10;
            if (!two[1].IsGenericParameter) return 11;
            if (typeof(Box<int>).GetGenericArguments()[0].IsGenericParameter) return 12;

            return 0;
        }
    }
}
