using System;

namespace ArraySortHelperDefaultIntTest
{
    class Program
    {
        static int Main(string[] args)
        {
            // Array.Sort<int>(int[]) routes through ArraySortHelper<int>.Default,
            // whose cctor calls RuntimeTypeHandle.CreateInstanceForAnotherGenericParameter
            // with (RuntimeType)typeof(GenericArraySortHelper<string>) and
            // (RuntimeType)typeof(int) to obtain a GenericArraySortHelper<int>. The
            // QCall is the load-bearing piece under test; the sort operating on the
            // returned helper is the observable confirmation.
            int[] arr = { 3, 1, 2 };
            Array.Sort(arr);

            if (arr[0] != 1) return 1;
            if (arr[1] != 2) return 2;
            if (arr[2] != 3) return 3;
            return 0;
        }
    }
}
