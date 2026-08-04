// The enum/underlying relaxation in `unbox.any` is *equality of primitive CorElementType*, not
// ECMA-335's verification-type equivalence and not the array-element rule. Those two are strictly
// wider, and the difference is observable:
//
//   - verification types collapse signedness (int32 and uint32 share one), but unbox does not:
//     I4 != U4, so `(uint)(object)1` throws;
//   - the array-element rule (CoreCLR `CanCastParam`) *does* collapse signedness, which is why
//     `(uint[])(object)new int[1]` succeeds while `(uint)(object)1` does not.
//
// This file pins the negatives, so that a future implementation cannot drift towards either of
// the wider rules without a test failing.

using System;

public enum SignedEnum : int
{
    A = 1,
}

public enum UnsignedEnum : uint
{
    A = 1,
}

public enum ShortEnum : short
{
    A = 1,
}

public class TestUnboxAnyEnumUnderlyingNegative
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    private static bool Throws<T>(object o)
    {
        try
        {
            T _ = Cast<T>(o);
            return false;
        }
        catch (InvalidCastException)
        {
            return true;
        }
    }

    public static int Main(string[] argv)
    {
        // Signedness is NOT collapsed, for plain primitives ...
        if (!Throws<uint>((object) 1)) return 1;
        if (!Throws<int>((object) 1u)) return 2;
        if (!Throws<sbyte>((object) (byte) 1)) return 3;
        if (!Throws<byte>((object) (sbyte) 1)) return 4;
        if (!Throws<ulong>((object) 1L)) return 5;
        if (!Throws<UIntPtr>((object) (IntPtr) 1)) return 6;

        // ... nor when an enum is on one side.
        if (!Throws<uint>((object) SignedEnum.A)) return 7;
        if (!Throws<int>((object) UnsignedEnum.A)) return 8;
        if (!Throws<SignedEnum>((object) 1u)) return 9;
        if (!Throws<UnsignedEnum>((object) 1)) return 10;

        // Two enums sharing a category but not an element type.
        if (!Throws<UnsignedEnum>((object) SignedEnum.A)) return 11;

        // ... but two *different* enums that share an underlying element type ARE
        // interconvertible: the rule is element-type equality between two primitive-category
        // types, and it never asks whether either side is nominally an enum. (Verified against
        // the real runtime, which accepts this.)
        if (Cast<SignedEnum2>((object) SignedEnum.A) != SignedEnum2.A) return 12;

        // Distinct element types of the same width are distinct: CHAR != U2, BOOLEAN != U1.
        if (!Throws<ushort>((object) 'a')) return 13;
        if (!Throws<char>((object) (ushort) 97)) return 14;
        if (!Throws<byte>((object) true)) return 15;
        if (!Throws<bool>((object) (byte) 1)) return 16;

        // Native ints are their own element type, distinct from the fixed-width integers.
        if (!Throws<long>((object) (IntPtr) 1)) return 17;
        if (!Throws<IntPtr>((object) 1L)) return 18;

        // No widening or narrowing at all.
        if (!Throws<long>((object) 1)) return 19;
        if (!Throws<int>((object) (short) 1)) return 20;
        if (!Throws<int>((object) ShortEnum.A)) return 21;
        if (!Throws<double>((object) 1.0f)) return 22;
        if (!Throws<float>((object) 1.0)) return 23;

        // The relaxation does not reach Nullable: `Nullable<T>` matches its argument by exact
        // equivalence, so a boxed enum is not a `T?` of its underlying type, or vice versa.
        if (!Throws<int?>((object) SignedEnum.A)) return 24;
        if (!Throws<SignedEnum?>((object) 1)) return 25;

        // Sanity: the permitted case really does pass, so the above are not all throwing for
        // some unrelated reason.
        if (Cast<int>((object) SignedEnum.A) != 1) return 26;

        return 0;
    }
}

public enum SignedEnum2 : int
{
    A = 1,
}
