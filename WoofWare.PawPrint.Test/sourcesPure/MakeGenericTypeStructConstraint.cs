using System;

namespace MakeGenericTypeStructConstraint
{
    // CoreCLR's RuntimeTypeHandle.Instantiate validates the where-T:struct
    // (NotNullableValueTypeConstraint) flag and throws ArgumentException
    // when a reference-type argument is supplied. PawPrint must do the same.
    public class WhereStruct<T> where T : struct { }

    public class Program
    {
        public static int Main(string[] args)
        {
            // Negative: typeof(WhereStruct<>).MakeGenericType(typeof(string)) must throw.
            try
            {
                typeof(WhereStruct<>).MakeGenericType(typeof(string));
                return 1;
            }
            catch (ArgumentException)
            {
                // expected
            }

            // Positive: int satisfies the constraint.
            Type closed = typeof(WhereStruct<>).MakeGenericType(typeof(int));
            if (closed != typeof(WhereStruct<int>)) return 2;

            return 0;
        }
    }
}
