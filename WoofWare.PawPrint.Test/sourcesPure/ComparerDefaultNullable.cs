using System.Collections.Generic;

// The `Nullable<T>` branch of `ComparerHelpers.CreateDefaultComparer`: `int?` does not implement
// `IComparable<int?>` (that would cost an interface call per comparison), so the selection falls
// through to the `type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Nullable<>)`
// test and yields a `NullableComparer<int>`. That test is the only one of the four branches that
// needs the `RuntimeTypeHandle_GetGenericTypeDefinition` QCall; see the sibling
// `ComparerDefault.cs` for the three that need no reflection beyond what is already modelled,
// and `TypeGetGenericTypeDefinition.cs` for the QCall's own coverage.

namespace ComparerDefaultNullableTest
{
    class Program
    {
        static int Main(string[] args)
        {
            if (!(Comparer<int?>.Default is NullableComparer<int>)) return 1;
            // null sorts before every value, and two nulls compare equal.
            if (Comparer<int?>.Default.Compare(null, 5) >= 0) return 2;
            if (Comparer<int?>.Default.Compare(5, null) <= 0) return 3;
            if (Comparer<int?>.Default.Compare(null, null) != 0) return 4;
            if (Comparer<int?>.Default.Compare(3, 5) >= 0) return 5;
            return 0;
        }
    }
}
