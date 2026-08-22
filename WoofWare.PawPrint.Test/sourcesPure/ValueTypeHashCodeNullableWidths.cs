using System;

// `ValueType.GetHashCode` on a struct whose first field is a `Nullable<T>` boxes that field
// through `CastHelpers.Box_Nullable`, which recovers `T` from the nullable's MethodTable
// (`InstantiationArg0`) and reads the payload at `NullableValueAddrOffset`. Both answers are
// per-`T`, so this file walks a range of underlying types rather than one: the payload sits one
// byte in for `byte?`, four for `int?`, eight for `long?` and `double?`, and a struct payload
// brings its own alignment.
//
// Each width is checked for the same three properties:
//
//  * only `N` contributes, so the trailing `A` must not reach the hash — an offset past the
//    payload would read into it;
//  * distinct payloads hash distinctly — an offset short of the payload would read the has-value
//    flag instead, which is 1 for both;
//  * "no value" hashes as "the default value" — `Nullable<T>.GetHashCode` answers 0 for both,
//    while their raw bytes differ in the has-value flag, so an implementation that hashed the
//    field's bytes instead of boxing it fails here.
//
// Every check compares two hashes produced in the same process, because a struct's hash mixes in
// its MethodTable pointer: an absolute value would be neither stable nor comparable.
public class Program
{
    private enum Colour : byte
    {
        Black = 0,
        Red = 7,
        Blue = 9,
    }

    private struct Pair
    {
        public int X;
        public int Y;
    }

    private struct ByteNullable
    {
        public byte? N;
        public int A;
    }

    private struct IntNullable
    {
        public int? N;
        public int A;
    }

    private struct LongNullable
    {
        public long? N;
        public int A;
    }

    private struct DoubleNullable
    {
        public double? N;
        public int A;
    }

    private struct ColourNullable
    {
        public Colour? N;
        public int A;
    }

    private struct PairNullable
    {
        public Pair? N;
        public int A;
    }

    public static int Main(string[] args)
    {
        if (new ByteNullable { N = 5, A = 1 }.GetHashCode() != new ByteNullable { N = 5, A = 2 }.GetHashCode())
        {
            return 1;
        }
        if (new ByteNullable { N = 5, A = 1 }.GetHashCode() == new ByteNullable { N = 6, A = 1 }.GetHashCode())
        {
            return 2;
        }
        if (new ByteNullable { N = null, A = 1 }.GetHashCode() != new ByteNullable { N = 0, A = 1 }.GetHashCode())
        {
            return 3;
        }

        if (new IntNullable { N = 5, A = 1 }.GetHashCode() != new IntNullable { N = 5, A = 2 }.GetHashCode())
        {
            return 11;
        }
        if (new IntNullable { N = 5, A = 1 }.GetHashCode() == new IntNullable { N = 6, A = 1 }.GetHashCode())
        {
            return 12;
        }
        if (new IntNullable { N = null, A = 1 }.GetHashCode() != new IntNullable { N = 0, A = 1 }.GetHashCode())
        {
            return 13;
        }

        if (new LongNullable { N = 5L, A = 1 }.GetHashCode() != new LongNullable { N = 5L, A = 2 }.GetHashCode())
        {
            return 21;
        }
        if (new LongNullable { N = 5L, A = 1 }.GetHashCode() == new LongNullable { N = 6L, A = 1 }.GetHashCode())
        {
            return 22;
        }
        if (new LongNullable { N = null, A = 1 }.GetHashCode() != new LongNullable { N = 0L, A = 1 }.GetHashCode())
        {
            return 23;
        }

        if (new DoubleNullable { N = 5.5, A = 1 }.GetHashCode() != new DoubleNullable { N = 5.5, A = 2 }.GetHashCode())
        {
            return 31;
        }
        if (new DoubleNullable { N = 5.5, A = 1 }.GetHashCode() == new DoubleNullable { N = 6.5, A = 1 }.GetHashCode())
        {
            return 32;
        }
        if (new DoubleNullable { N = null, A = 1 }.GetHashCode() != new DoubleNullable { N = 0.0, A = 1 }.GetHashCode())
        {
            return 33;
        }

        if (new ColourNullable { N = Colour.Red, A = 1 }.GetHashCode()
            != new ColourNullable { N = Colour.Red, A = 2 }.GetHashCode())
        {
            return 41;
        }
        if (new ColourNullable { N = Colour.Red, A = 1 }.GetHashCode()
            == new ColourNullable { N = Colour.Blue, A = 1 }.GetHashCode())
        {
            return 42;
        }
        if (new ColourNullable { N = null, A = 1 }.GetHashCode()
            != new ColourNullable { N = Colour.Black, A = 1 }.GetHashCode())
        {
            return 43;
        }

        // A multi-field payload: the box has to carry every byte of `Pair`, not only its first
        // field, and `Pair.GetHashCode` is itself `ValueType.GetHashCode` over those bytes.
        if (new PairNullable { N = new Pair { X = 5, Y = 6 }, A = 1 }.GetHashCode()
            != new PairNullable { N = new Pair { X = 5, Y = 6 }, A = 2 }.GetHashCode())
        {
            return 51;
        }
        if (new PairNullable { N = new Pair { X = 5, Y = 6 }, A = 1 }.GetHashCode()
            == new PairNullable { N = new Pair { X = 5, Y = 7 }, A = 1 }.GetHashCode())
        {
            return 52;
        }
        // Unlike every width above, `default(Pair)` does *not* hash as zero: `Pair.GetHashCode` is
        // itself `ValueType.GetHashCode`, which mixes in `Pair`'s own MethodTable pointer before
        // hashing its bytes. A null box still contributes 0, so these two must differ — the
        // opposite of the primitive cases, and measured on real .NET rather than assumed.
        if (new PairNullable { N = null, A = 1 }.GetHashCode()
            == new PairNullable { N = default(Pair), A = 1 }.GetHashCode())
        {
            return 53;
        }

        // An eight-byte payload starts eight bytes in, past seven bytes of padding, so an offset
        // that is short by four still reads eight zero bytes for the small values above: measured,
        // `long?`'s three checks all pass under exactly that mistake. Two payloads whose *low* four
        // bytes are zero are what separate it, one pair per eight-byte width.
        if (new LongNullable { N = 1L << 32, A = 1 }.GetHashCode()
            == new LongNullable { N = 2L << 32, A = 1 }.GetHashCode())
        {
            return 61;
        }

        if (new DoubleNullable { N = BitConverter.Int64BitsToDouble(1L << 32), A = 1 }.GetHashCode()
            == new DoubleNullable { N = BitConverter.Int64BitsToDouble(2L << 32), A = 1 }.GetHashCode())
        {
            return 62;
        }

        return 0;
    }
}
