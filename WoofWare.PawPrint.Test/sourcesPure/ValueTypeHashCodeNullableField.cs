using System;

// A `Nullable<T>` field is a value type that overrides `GetHashCode`, so the strategy is
// `ValueTypeOverride`: the guest boxes the field through its MethodTable and asks it. Boxing a
// `Nullable<T>` with no value produces a null box, which the caller folds to a hash contribution
// of zero.
public class Program
{
    // The `Nullable<int>` alone is enough to make this struct non-bit-comparable, so no object
    // reference is needed — and without one, the guest can byte-address the field in order to box
    // it.
    private struct NullableThenInt
    {
        public int? N;
        public int A;
    }

    public static int Main(string[] args)
    {
        // Only `N` contributes.
        NullableThenInt sameValue1 = new NullableThenInt { N = 5, A = 1 };
        NullableThenInt sameValue2 = new NullableThenInt { N = 5, A = 2 };
        if (sameValue1.GetHashCode() != sameValue2.GetHashCode())
        {
            return 1;
        }

        NullableThenInt otherValue = new NullableThenInt { N = 6, A = 1 };
        if (sameValue1.GetHashCode() == otherValue.GetHashCode())
        {
            return 2;
        }

        // A `Nullable<int>` with no value hashes as zero rather than as its raw bytes, so it is
        // distinguishable from a present value and stable across the second field.
        NullableThenInt noValue1 = new NullableThenInt { N = null, A = 1 };
        NullableThenInt noValue2 = new NullableThenInt { N = null, A = 2 };
        if (noValue1.GetHashCode() != noValue2.GetHashCode())
        {
            return 3;
        }

        if (noValue1.GetHashCode() == sameValue1.GetHashCode())
        {
            return 4;
        }

        // `Nullable<int>.GetHashCode` returns 0 both for no value and for the value 0, so these
        // two structs must hash equal — while their raw bytes differ in the `hasValue` flag. This
        // is what separates "boxed the field and asked it" from "hashed the field's bytes".
        if (((int?)null).GetHashCode() != ((int?)0).GetHashCode())
        {
            return 5;
        }

        NullableThenInt zeroValue = new NullableThenInt { N = 0, A = 1 };
        if (noValue1.GetHashCode() != zeroValue.GetHashCode())
        {
            return 6;
        }

        return 0;
    }
}
