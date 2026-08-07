using System.Collections.Generic;

// `Comparer<T>.Default`'s getter is [Intrinsic], but its IL body is just
// `ldsfld <Default>k__BackingField`; the comparer itself is chosen by the .cctor calling
// `ComparerHelpers.CreateDefaultComparer(typeof(T))`. Which of the four comparer shapes that
// picks is observable, so assert the selection as well as the resulting comparison behaviour:
// getting it wrong would silently change ordering semantics rather than fail loudly.
//
// The `Nullable<T>` branch of the selection, and `EnumComparer<T>.Compare` itself, are each
// blocked on an unrelated gap; they are covered by the parked `ComparerDefaultNullable.cs` and
// `ComparerDefaultEnumCompare.cs`.

namespace ComparerDefaultTest
{
    enum Colour
    {
        Red = 1,
        Green = 2,
    }

    // Deliberately implements neither IComparable nor IComparable<T>, so it must fall through
    // to the ObjectComparer<T> default.
    class Plain
    {
    }

    class Program
    {
        static int Main(string[] args)
        {
            // int implements IComparable<int>, so takes the GenericComparer<T> branch.
            if (!(Comparer<int>.Default is GenericComparer<int>)) return 1;
            // string implements IComparable<string> likewise.
            if (!(Comparer<string>.Default is GenericComparer<string>)) return 2;
            // Plain implements nothing, so falls through to the ObjectComparer<T> default.
            if (!(Comparer<Plain>.Default is ObjectComparer<Plain>)) return 3;
            // An enum takes the boxing-avoidance EnumComparer<T> branch, so must *not* have
            // fallen through to ObjectComparer<T>. EnumComparer<T> is internal to CoreLib, so
            // this negative test is as close as a guest can get to naming it.
            if (Comparer<Colour>.Default is ObjectComparer<Colour>) return 4;

            // The getter reads a static field, so it is the same instance every time.
            if (!object.ReferenceEquals(Comparer<int>.Default, Comparer<int>.Default)) return 5;

            if (Comparer<int>.Default.Compare(3, 5) >= 0) return 6;
            if (Comparer<int>.Default.Compare(5, 3) <= 0) return 7;
            if (Comparer<int>.Default.Compare(4, 4) != 0) return 8;

            return 0;
        }
    }
}
