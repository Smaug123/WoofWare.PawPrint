using System;
using System.Reflection;

namespace MethodReflectionProbe
{
    class Foo
    {
        public static int Add(int a, int b) => a + b;
    }

    class Program
    {
        static int Main(string[] args)
        {
            MethodInfo m = typeof(Foo).GetMethod("Add");
            if (m == null) return 1;
            if (m.Name != "Add") return 2;
            return 0;
        }
    }
}
