using System;

namespace MakeGenericTypeClassConstraint
{
    // CoreCLR's RuntimeTypeHandle.Instantiate validates the where-T:class
    // (ReferenceTypeConstraint) flag and throws ArgumentException when a
    // value-type argument is supplied. PawPrint must do the same.
    public class WhereClass<T> where T : class { }

    public class Program
    {
        public static int Main(string[] args)
        {
            // Negative: typeof(WhereClass<>).MakeGenericType(typeof(int)) must throw.
            try
            {
                typeof(WhereClass<>).MakeGenericType(typeof(int));
                return 1;
            }
            catch (ArgumentException)
            {
                // expected
            }

            // Positive: string satisfies the constraint.
            Type closed = typeof(WhereClass<>).MakeGenericType(typeof(string));
            if (closed != typeof(WhereClass<string>)) return 2;

            return 0;
        }
    }
}
