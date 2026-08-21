using System;

// `value__` is only special on a type that actually derives from `System.Enum`. A plain struct is
// free to declare a field of that name, and CoreCLR reports it as the value type it is — so a
// `GetHashCode` override on such a struct must be reached, not bypassed in favour of hashing the
// field's raw bytes.
public class Program
{
    // Deliberately shaped like an enum's storage: one integer instance field named `value__`.
    // Collapses distinct bit patterns of the same width, so hashing the bytes and asking the
    // override give different answers.
    private struct Fake
    {
        public long value__;

        public override int GetHashCode() => value__ >= 0L ? 1 : 2;

        public override bool Equals(object obj) => obj is Fake other && other.value__ == value__;
    }

    private struct FakeThenInt
    {
        public Fake F;
        public int A;
    }

    public static int Main(string[] args)
    {
        // ValueTypeOverride: two different `long` bit patterns of the same sign give one hash, and
        // `A` never contributes.
        FakeThenInt sameSign1 = new FakeThenInt { F = new Fake { value__ = 1L }, A = 1 };
        FakeThenInt sameSign2 = new FakeThenInt { F = new Fake { value__ = 2L }, A = 2 };
        if (sameSign1.GetHashCode() != sameSign2.GetHashCode())
        {
            return 1;
        }

        // ... and the override is genuinely consulted rather than the field skipped.
        FakeThenInt otherSign = new FakeThenInt { F = new Fake { value__ = -1L }, A = 1 };
        if (sameSign1.GetHashCode() == otherSign.GetHashCode())
        {
            return 2;
        }

        return 0;
    }
}
