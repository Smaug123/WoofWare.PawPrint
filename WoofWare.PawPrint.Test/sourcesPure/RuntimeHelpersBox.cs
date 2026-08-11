using System;
using System.Runtime.CompilerServices;

public class Program
{
    private struct Pair
    {
        public int A;
        public long B;
    }

    private enum Colour : short
    {
        Red = 1,
        Green = 2
    }

    public static int Main(string[] args)
    {
        // A primitive: allocator plus a four-byte copy, no nullable offset.
        int x = 42;
        object boxed = RuntimeHelpers.Box(ref Unsafe.As<int, byte>(ref x), typeof(int).TypeHandle);
        if (!(boxed is int)) return 1;
        if ((int)boxed != 42) return 2;

        // A multi-field struct, so the copy is wider than one field.
        Pair p = new Pair { A = 7, B = 9000000000L };
        object boxedPair = RuntimeHelpers.Box(ref Unsafe.As<Pair, byte>(ref p), typeof(Pair).TypeHandle);
        if (!(boxedPair is Pair)) return 3;
        Pair q = (Pair)boxedPair;
        if (q.A != 7) return 4;
        if (q.B != 9000000000L) return 5;

        // The box is a copy, not an alias of the source.
        p.A = 8;
        if (((Pair)boxedPair).A != 7) return 6;

        // An enum whose underlying type is narrower than int.
        Colour c = Colour.Green;
        object boxedColour = RuntimeHelpers.Box(ref Unsafe.As<Colour, byte>(ref c), typeof(Colour).TypeHandle);
        if (!(boxedColour is Colour)) return 7;
        if ((Colour)boxedColour != Colour.Green) return 8;

        // Nullable<T> with a value boxes as a T, from the value offset the QCall reported.
        int? some = 5;
        object boxedSome = RuntimeHelpers.Box(ref Unsafe.As<int?, byte>(ref some), typeof(int?).TypeHandle);
        if (boxedSome == null) return 9;
        if (!(boxedSome is int)) return 10;
        if ((int)boxedSome != 5) return 11;

        // Nullable<T> without one boxes as null, which is decided by reading hasValue at offset 0.
        int? none = null;
        object boxedNone = RuntimeHelpers.Box(ref Unsafe.As<int?, byte>(ref none), typeof(int?).TypeHandle);
        if (boxedNone != null) return 12;

        // The other reachable caller: Enum.InternalBoxEnum boxes out of an eight-byte scratch
        // slot, so the copy is narrower than the source rather than the same width.
        object fromEnum = Enum.ToObject(typeof(Colour), 2);
        if (!(fromEnum is Colour)) return 13;
        if ((Colour)fromEnum != Colour.Green) return 14;

        byte scratch = 0;

        // The two rejections below discriminate between exception types rather than just naming
        // the expected one. The plausible wrong answers are the *other* exceptions
        // ValidateTypeAbleToBeInstantiated throws, which is what reading across from the sibling
        // RuntimeTypeHandle_GetActivationInfo classifier would produce: that call passes
        // fGetUninitializedObject: false, so the same checks throw MissingMethodException and
        // MemberAccessException there instead of ArgumentException.

        // System.Void is a value type with a MethodTable, so it passes every check BoxCache's
        // constructor makes and reaches the QCall's own ELEMENT_TYPE_VOID check.
        try
        {
            RuntimeHelpers.Box(ref scratch, typeof(void).TypeHandle);
            return 15;
        }
        catch (MissingMethodException) { return 16; }
        catch (MemberAccessException) { return 17; }
        catch (NotSupportedException) { return 18; }
        catch (ArgumentException) { }

        // A ref struct is rejected by ValidateTypeAbleToBeInstantiated's allowRefLike: false.
        try
        {
            RuntimeHelpers.Box(ref scratch, typeof(Span<int>).TypeHandle);
            return 19;
        }
        catch (MissingMethodException) { return 20; }
        catch (MemberAccessException) { return 21; }
        catch (ArgumentException) { return 22; }
        catch (NotSupportedException) { }

        return 0;
    }
}
