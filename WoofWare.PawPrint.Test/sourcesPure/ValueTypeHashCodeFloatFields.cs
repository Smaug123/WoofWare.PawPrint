using System;

// A struct with a floating-point field is never bit-comparable (-0.0 and 0.0 compare equal but
// have different bits), so it always reaches `ValueType_GetHashCodeStrategy`, which reports the
// float's width so the guest can hash it through `Double.GetHashCode`/`Single.GetHashCode`
// rather than as raw bytes.
public class Program
{
    private struct DoubleThenInt
    {
        public double D;
        public int A;
    }

    private struct FloatThenInt
    {
        public float F;
        public int A;
    }

    public static int Main(string[] args)
    {
        // DoubleField: only `D` contributes.
        DoubleThenInt sameDouble1 = new DoubleThenInt { D = 1.5, A = 1 };
        DoubleThenInt sameDouble2 = new DoubleThenInt { D = 1.5, A = 2 };
        if (sameDouble1.GetHashCode() != sameDouble2.GetHashCode())
        {
            return 1;
        }

        DoubleThenInt otherDouble = new DoubleThenInt { D = 2.5, A = 1 };
        if (sameDouble1.GetHashCode() == otherDouble.GetHashCode())
        {
            return 2;
        }

        // `Double.GetHashCode` folds the two zeroes together, which raw bytes would not.
        DoubleThenInt positiveZero = new DoubleThenInt { D = 0.0, A = 1 };
        DoubleThenInt negativeZero = new DoubleThenInt { D = -0.0, A = 1 };
        if (positiveZero.GetHashCode() != negativeZero.GetHashCode())
        {
            return 3;
        }

        // SingleField: only `F` contributes.
        FloatThenInt sameFloat1 = new FloatThenInt { F = 1.5f, A = 1 };
        FloatThenInt sameFloat2 = new FloatThenInt { F = 1.5f, A = 2 };
        if (sameFloat1.GetHashCode() != sameFloat2.GetHashCode())
        {
            return 4;
        }

        FloatThenInt otherFloat = new FloatThenInt { F = 2.5f, A = 1 };
        if (sameFloat1.GetHashCode() == otherFloat.GetHashCode())
        {
            return 5;
        }

        FloatThenInt positiveZeroSingle = new FloatThenInt { F = 0.0f, A = 1 };
        FloatThenInt negativeZeroSingle = new FloatThenInt { F = -0.0f, A = 1 };
        if (positiveZeroSingle.GetHashCode() != negativeZeroSingle.GetHashCode())
        {
            return 6;
        }

        return 0;
    }
}
