using System;

namespace MakeGenericTypeNewConstraint
{
    // CoreCLR's RuntimeTypeHandle.Instantiate validates the where-T:new()
    // (DefaultConstructorConstraint) flag and throws ArgumentException
    // when the supplied type has no public parameterless ctor.
    public class WhereNew<T> where T : new() { }

    public class WithPublicDefault
    {
        public WithPublicDefault() { }
    }

    public class WithoutPublicDefault
    {
        // Only a private parameterless ctor — does not satisfy `new()`.
        private WithoutPublicDefault() { }
        public WithoutPublicDefault(int unused) { }
    }

    public class Program
    {
        public static int Main(string[] args)
        {
            // Negative: typeof(WhereNew<>).MakeGenericType(typeof(WithoutPublicDefault)) must throw.
            try
            {
                typeof(WhereNew<>).MakeGenericType(typeof(WithoutPublicDefault));
                return 1;
            }
            catch (ArgumentException)
            {
                // expected
            }

            // Positive: a class with a public parameterless ctor satisfies the constraint.
            Type closedClass = typeof(WhereNew<>).MakeGenericType(typeof(WithPublicDefault));
            if (closedClass != typeof(WhereNew<WithPublicDefault>)) return 2;

            // Positive: value types implicitly satisfy the new() constraint.
            Type closedStruct = typeof(WhereNew<>).MakeGenericType(typeof(int));
            if (closedStruct != typeof(WhereNew<int>)) return 3;

            return 0;
        }
    }
}
