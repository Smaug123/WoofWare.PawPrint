using System;

// `ValueType_GetHashCodeStrategy` walks the struct's fields in *metadata declaration* order, not
// in layout order, and reports an enum-typed field as its underlying primitive rather than as a
// value type. Auto layout moves object references to the front, so for the first two structs here
// the two orders disagree and the hash must follow the declared-first field.
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

    private enum Small : byte
    {
        Zero = 0,
        One = 1,
        Two = 2,
    }

    // `Int64.GetHashCode` xors the two halves together, so `Enum.GetHashCode` gives these two
    // members the same hash (measured: both 0) while their eight raw bytes differ.
    private enum Folding : long
    {
        Zero = 0L,
        Folded = 0x0000000100000001L,
    }

    private struct IntThenRef
    {
        public int A;
        public Ref R;
    }

    private struct EnumThenRef
    {
        public Small E;
        public Ref R;
    }

    // The `double` is what makes this struct non-bit-comparable, so no object reference is needed
    // — and without one the guest can byte-address the eight-byte enum field, which
    // `HashCode.AddBytes` reads four bytes at a time.
    private struct FoldingEnumThenDouble
    {
        public Folding E;
        public double D;
    }

    public static int Main(string[] args)
    {
        // FastGetHashCode over the four bytes of `A`, which auto layout places *after* the
        // reference. Layout order would have selected `R`.
        IntThenRef sameInt1 = new IntThenRef { A = 1, R = new Ref(7) };
        IntThenRef sameInt2 = new IntThenRef { A = 1, R = new Ref(8) };
        if (sameInt1.GetHashCode() != sameInt2.GetHashCode())
        {
            return 1;
        }

        IntThenRef otherInt = new IntThenRef { A = 2, R = new Ref(7) };
        if (sameInt1.GetHashCode() == otherInt.GetHashCode())
        {
            return 2;
        }

        // An enum field is reported as its underlying primitive rather than as a value type, so
        // this is FastGetHashCode over one byte rather than a recursion into `Small`.
        EnumThenRef sameEnum1 = new EnumThenRef { E = Small.One, R = new Ref(7) };
        EnumThenRef sameEnum2 = new EnumThenRef { E = Small.One, R = new Ref(8) };
        if (sameEnum1.GetHashCode() != sameEnum2.GetHashCode())
        {
            return 3;
        }

        EnumThenRef otherEnum = new EnumThenRef { E = Small.Two, R = new Ref(7) };
        if (sameEnum1.GetHashCode() == otherEnum.GetHashCode())
        {
            return 4;
        }

        // The two `Folding` members hash equal *as an enum*, so a struct whose first field is one
        // of them can only tell them apart if the field was reported as its underlying primitive
        // and hashed as raw bytes. Boxing it and asking `Enum.GetHashCode` would collapse them.
        if (Folding.Zero.GetHashCode() != Folding.Folded.GetHashCode())
        {
            return 5;
        }

        FoldingEnumThenDouble foldZero = new FoldingEnumThenDouble { E = Folding.Zero, D = 1.5 };
        FoldingEnumThenDouble foldOther = new FoldingEnumThenDouble { E = Folding.Folded, D = 1.5 };
        if (foldZero.GetHashCode() == foldOther.GetHashCode())
        {
            return 6;
        }

        FoldingEnumThenDouble foldZeroOtherDouble = new FoldingEnumThenDouble { E = Folding.Zero, D = 2.5 };
        if (foldZero.GetHashCode() != foldZeroOtherDouble.GetHashCode())
        {
            return 7;
        }

        return 0;
    }
}
