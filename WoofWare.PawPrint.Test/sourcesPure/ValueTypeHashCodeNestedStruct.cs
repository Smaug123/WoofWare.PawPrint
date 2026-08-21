using System;

// The three value-type arms of `ValueType_GetHashCodeStrategy`: a nested struct that *is*
// bit-comparable is hashed whole; one that overrides `GetHashCode` is boxed and asked; otherwise
// the walk recurses into it, accumulating the byte offset as it goes.
public class Program
{
    private sealed class Ref
    {
        private readonly int hash;

        public Ref(int hash)
        {
            this.hash = hash;
        }

        public override int GetHashCode() => hash;

        public override bool Equals(object obj) => obj is Ref other && other.hash == hash;
    }

    private struct Point
    {
        public int X;
        public int Y;
    }

    private struct PointThenDouble
    {
        public Point P;
        public double D;
    }

    private struct DoubleThenInt
    {
        public double D;
        public int A;
    }

    // Auto layout puts `R` at offset 0 and `I` after it, so recursing into `I` must accumulate a
    // non-zero outer offset before adding `D`'s offset within `I`.
    private struct NestedAfterRef
    {
        public DoubleThenInt I;
        public Ref R;
    }

    private struct OneRef
    {
        public Ref R;
    }

    // The walk descends into `I`, finds nothing there, and stops: it does *not* come back out and
    // try `B`.
    private struct EmptyNestedThenInt
    {
        public OneRef I;
        public int B;
    }

    // Collapses distinct bit patterns of the same size, so hashing this struct's bytes and
    // hashing it through the override give different answers.
    private struct SignOnly
    {
        public double D;

        public override int GetHashCode() => D >= 0.0 ? 1 : 2;

        public override bool Equals(object obj) => obj is SignOnly other && other.D == D;
    }

    private struct SignOnlyThenInt
    {
        public SignOnly O;
        public int A;
    }

    public static int Main(string[] args)
    {
        // FastGetHashCode over the whole of `P`, which is bit-comparable; `D` never contributes.
        PointThenDouble samePoint1 = new PointThenDouble { P = new Point { X = 1, Y = 2 }, D = 1.5 };
        PointThenDouble samePoint2 = new PointThenDouble { P = new Point { X = 1, Y = 2 }, D = 2.5 };
        if (samePoint1.GetHashCode() != samePoint2.GetHashCode())
        {
            return 1;
        }

        PointThenDouble otherY = new PointThenDouble { P = new Point { X = 1, Y = 3 }, D = 1.5 };
        if (samePoint1.GetHashCode() == otherY.GetHashCode())
        {
            return 2;
        }

        PointThenDouble otherX = new PointThenDouble { P = new Point { X = 2, Y = 2 }, D = 1.5 };
        if (samePoint1.GetHashCode() == otherX.GetHashCode())
        {
            return 3;
        }

        // Recursion into `I`: `DoubleThenInt` is neither bit-comparable nor overriding, so the
        // walk descends and lands on its `double`. Neither `I.A` nor `R` contributes.
        NestedAfterRef sameNested1 = new NestedAfterRef
        {
            I = new DoubleThenInt { D = 1.5, A = 1 },
            R = new Ref(7),
        };
        NestedAfterRef sameNested2 = new NestedAfterRef
        {
            I = new DoubleThenInt { D = 1.5, A = 2 },
            R = new Ref(8),
        };
        if (sameNested1.GetHashCode() != sameNested2.GetHashCode())
        {
            return 4;
        }

        NestedAfterRef otherNested = new NestedAfterRef
        {
            I = new DoubleThenInt { D = 2.5, A = 1 },
            R = new Ref(7),
        };
        if (sameNested1.GetHashCode() == otherNested.GetHashCode())
        {
            return 5;
        }

        // The recursion reached a `double`, so the two zeroes fold together here too. Hashing
        // eight raw bytes at the same offset would keep them apart.
        NestedAfterRef nestedPositiveZero = new NestedAfterRef
        {
            I = new DoubleThenInt { D = 0.0, A = 1 },
            R = new Ref(7),
        };
        NestedAfterRef nestedNegativeZero = new NestedAfterRef
        {
            I = new DoubleThenInt { D = -0.0, A = 1 },
            R = new Ref(7),
        };
        if (nestedPositiveZero.GetHashCode() != nestedNegativeZero.GetHashCode())
        {
            return 6;
        }

        // The recursion into `I` runs out of fields (its only field is a null reference), and the
        // outer walk commits to that answer rather than resuming at `B`. So `B` cannot contribute,
        // even though it is a perfectly hashable `int`.
        EmptyNestedThenInt exhausted1 = new EmptyNestedThenInt { I = new OneRef { R = null }, B = 1 };
        EmptyNestedThenInt exhausted2 = new EmptyNestedThenInt { I = new OneRef { R = null }, B = 2 };
        if (exhausted1.GetHashCode() != exhausted2.GetHashCode())
        {
            return 7;
        }

        // ... and once that reference is non-null the recursion does find it.
        EmptyNestedThenInt found = new EmptyNestedThenInt { I = new OneRef { R = new Ref(11) }, B = 1 };
        if (exhausted1.GetHashCode() == found.GetHashCode())
        {
            return 8;
        }

        // ValueTypeOverride: `SignOnly` is boxed and asked, so two different `double` bit
        // patterns of the same sign give one hash ...
        SignOnlyThenInt sameSign1 = new SignOnlyThenInt { O = new SignOnly { D = 1.0 }, A = 1 };
        SignOnlyThenInt sameSign2 = new SignOnlyThenInt { O = new SignOnly { D = 2.0 }, A = 2 };
        if (sameSign1.GetHashCode() != sameSign2.GetHashCode())
        {
            return 9;
        }

        // ... and the override is genuinely consulted, rather than the field being skipped.
        SignOnlyThenInt otherSign = new SignOnlyThenInt { O = new SignOnly { D = -1.0 }, A = 1 };
        if (sameSign1.GetHashCode() == otherSign.GetHashCode())
        {
            return 10;
        }

        return 0;
    }
}
