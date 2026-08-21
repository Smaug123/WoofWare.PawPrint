using System;

// `ValueType_GetHashCodeStrategy` classifies a generic field by its *instantiated* type, so the
// same declared field is hashed as raw bytes for one instantiation and as an object reference for
// another.
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

    private struct Holder<T>
    {
        public T V;
        public Ref R;
    }

    public static int Main(string[] args)
    {
        // T = int: the field is a primitive, hashed as its four bytes.
        Holder<int> intHolder1 = new Holder<int> { V = 1, R = new Ref(7) };
        Holder<int> intHolder2 = new Holder<int> { V = 1, R = new Ref(8) };
        if (intHolder1.GetHashCode() != intHolder2.GetHashCode())
        {
            return 1;
        }

        Holder<int> otherInt = new Holder<int> { V = 2, R = new Ref(7) };
        if (intHolder1.GetHashCode() == otherInt.GetHashCode())
        {
            return 2;
        }

        // T = Ref: the same field is now an object reference, hashed through its own GetHashCode.
        Holder<Ref> refHolder1 = new Holder<Ref> { V = new Ref(3), R = new Ref(7) };
        Holder<Ref> refHolder2 = new Holder<Ref> { V = new Ref(3), R = new Ref(8) };
        if (refHolder1.GetHashCode() != refHolder2.GetHashCode())
        {
            return 3;
        }

        Holder<Ref> otherRef = new Holder<Ref> { V = new Ref(4), R = new Ref(7) };
        if (refHolder1.GetHashCode() == otherRef.GetHashCode())
        {
            return 4;
        }

        // ... and being a reference, it can be null, in which case the walk moves on to `R`.
        Holder<Ref> nullFirst1 = new Holder<Ref> { V = null, R = new Ref(7) };
        Holder<Ref> nullFirst2 = new Holder<Ref> { V = null, R = new Ref(8) };
        if (nullFirst1.GetHashCode() == nullFirst2.GetHashCode())
        {
            return 5;
        }

        return 0;
    }
}
