// CoreCLR `CastHelpers.Unbox_Helper`: `unbox`/`unbox.any` to a non-Nullable value type succeeds
// when the handles are identical, OR when both types are in the primitive-value-type category
// (which includes enums) and their primitive CorElementType are *equal*.
//
// An enum reports the CorElementType of its underlying integer, so a boxed enum unboxes to that
// exact underlying type and vice versa. This is the positive half of the rule; the negatives —
// including the ones that make this narrower than ECMA's "verification type" equivalence — live
// in UnboxAnyEnumUnderlyingNegative.cs.

public enum IntEnum : int
{
    A = 1,
    B = 70000,
}

public enum ByteEnum : byte
{
    A = 1,
}

public enum LongEnum : long
{
    A = 5000000000L,
}

public class TestUnboxAnyEnumUnderlying
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    public static int Main(string[] argv)
    {
        // Boxed enum -> underlying, via a direct `unbox.any int32` token.
        object boxedIntEnum = IntEnum.B;
        int asInt = (int) boxedIntEnum;
        if (asInt != 70000) return 1;

        // Underlying -> enum, the other direction.
        object boxedInt = 70000;
        IntEnum asEnum = (IntEnum) boxedInt;
        if (asEnum != IntEnum.B) return 2;

        // Both directions again through the `unbox.any !!T` generic token form.
        if (Cast<int>(boxedIntEnum) != 70000) return 3;
        if (Cast<IntEnum>(boxedInt) != IntEnum.B) return 4;

        // Narrower underlying types take the same path.
        object boxedByteEnum = ByteEnum.A;
        if ((byte) boxedByteEnum != 1) return 5;

        object boxedByte = (byte) 1;
        if ((ByteEnum) boxedByte != ByteEnum.A) return 6;

        // Wider underlying types too, including a value that does not fit in 32 bits.
        object boxedLongEnum = LongEnum.A;
        if ((long) boxedLongEnum != 5000000000L) return 7;

        object boxedLong = 5000000000L;
        if ((LongEnum) boxedLong != LongEnum.A) return 8;

        // The unboxed value must be usable as its target type, not merely produced: the result
        // of unboxing an enum to its underlying participates in ordinary integer arithmetic.
        int sum = (int) boxedIntEnum + 1;
        if (sum != 70001) return 9;

        // And the enum direction round-trips back through boxing.
        object reboxed = (object) ((IntEnum) boxedInt);
        if ((int) reboxed != 70000) return 10;

        return 0;
    }
}
