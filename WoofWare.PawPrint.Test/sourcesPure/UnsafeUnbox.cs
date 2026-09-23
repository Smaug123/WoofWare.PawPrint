// `Unsafe.Unbox<T>(object)`. CoreLib's C# body is `throw new PlatformNotSupportedException()`;
// the runtime substitutes `ldarg.0; unbox !!T; ret` (jitinterface.cpp,
// `getILIntrinsicImplementationForUnsafe`), so its semantics are exactly the `unbox` opcode's:
// the returned `ref T` aliases the box's payload, null raises NullReferenceException, and the
// type test is `CastHelpers.Unbox_Helper`'s (identity, or the same primitive element type).
//
// Unlike `((Point) o).X`, which is the only way C# reaches a bare `unbox`, this hands the guest
// the byref itself, so a write through it is observable via the box.

using System;
using System.Runtime.CompilerServices;
using System.Threading;

public struct Point
{
    public int X;
    public int Y;
}

public struct WithRef
{
    public string S;
    public int N;
}

public struct Wrapper<T>
{
    public T Item;
}

public enum IntEnum
{
    A = 1,
    B = 2,
    C = 7,
}

public enum OtherIntEnum
{
    P = 3,
}

public enum LongEnum : long
{
    X = 3,
}

public enum ByteEnum : byte
{
    Q = 5,
}

public class TestUnsafeUnbox
{
    static int Bump(ref int x)
    {
        x += 100;
        return x;
    }

    public static int Main(string[] argv)
    {
        // A boxed struct: field writes and whole-value writes through the byref land in the box.
        object boxedPoint = new Point { X = 3, Y = 4 };
        ref Point p = ref Unsafe.Unbox<Point>(boxedPoint);
        if (p.X != 3 || p.Y != 4) return 1;
        p.X = 30;
        if (((Point) boxedPoint).X != 30) return 2;
        p = new Point { X = 5, Y = 6 };
        Point readBack = (Point) boxedPoint;
        if (readBack.X != 5 || readBack.Y != 6) return 3;

        // Two calls on the same box return the same address.
        if (!Unsafe.AreSame(ref Unsafe.Unbox<Point>(boxedPoint), ref Unsafe.Unbox<Point>(boxedPoint))) return 4;

        // A boxed struct holding a reference.
        object boxedWithRef = new WithRef { S = "hello", N = 1 };
        ref WithRef w = ref Unsafe.Unbox<WithRef>(boxedWithRef);
        if (w.S != "hello" || w.N != 1) return 5;
        w.S = "goodbye";
        w.N = 2;
        WithRef wBack = (WithRef) boxedWithRef;
        if (wBack.S != "goodbye" || wBack.N != 2) return 6;
        w = new WithRef { S = null, N = 3 };
        wBack = (WithRef) boxedWithRef;
        if (wBack.S != null || wBack.N != 3) return 7;

        // A boxed generic struct.
        object boxedWrapper = new Wrapper<long> { Item = 9L };
        ref Wrapper<long> wl = ref Unsafe.Unbox<Wrapper<long>>(boxedWrapper);
        wl.Item = -9L;
        if (((Wrapper<long>) boxedWrapper).Item != -9L) return 8;

        // Boxed primitives: reads, writes, and read-modify-write through the byref.
        object boxedInt = 41;
        ref int i = ref Unsafe.Unbox<int>(boxedInt);
        if (i != 41) return 10;
        i++;
        if ((int) boxedInt != 42) return 11;
        if (Bump(ref Unsafe.Unbox<int>(boxedInt)) != 142) return 12;
        if ((int) boxedInt != 142) return 13;
        if (!Unsafe.AreSame(ref Unsafe.Unbox<int>(boxedInt), ref i)) return 14;

        object boxedBool = false;
        ref bool b = ref Unsafe.Unbox<bool>(boxedBool);
        if (b) return 15;
        b = true;
        if (!(bool) boxedBool) return 16;

        object boxedDouble = 1.5;
        ref double d = ref Unsafe.Unbox<double>(boxedDouble);
        if (d != 1.5) return 17;
        d *= 4.0;
        if ((double) boxedDouble != 6.0) return 18;

        object boxedLong = long.MaxValue;
        ref long l = ref Unsafe.Unbox<long>(boxedLong);
        l = long.MinValue;
        if ((long) boxedLong != long.MinValue) return 19;

        object boxedByte = (byte) 200;
        ref byte by = ref Unsafe.Unbox<byte>(boxedByte);
        by = 7;
        if ((byte) boxedByte != 7) return 20;

        object boxedChar = 'a';
        ref char c = ref Unsafe.Unbox<char>(boxedChar);
        c = 'z';
        if ((char) boxedChar != 'z') return 21;

        // A boxed enum through its own type.
        object boxedEnum = IntEnum.A;
        ref IntEnum e = ref Unsafe.Unbox<IntEnum>(boxedEnum);
        e = IntEnum.B;
        if ((IntEnum) boxedEnum != IntEnum.B) return 22;

        // `unbox`'s relaxation: a boxed enum unboxes as its underlying integer, and the byref
        // still aliases the box, whose runtime type is unchanged.
        ref int underlying = ref Unsafe.Unbox<int>(boxedEnum);
        if (underlying != 2) return 23;
        underlying = 7;
        if ((IntEnum) boxedEnum != IntEnum.C) return 24;
        if (boxedEnum.GetType() != typeof(IntEnum)) return 25;

        // ... and the other way: a boxed int unboxes as an enum over int.
        object boxedIntForEnum = 1;
        ref IntEnum asEnum = ref Unsafe.Unbox<IntEnum>(boxedIntForEnum);
        if (asEnum != IntEnum.A) return 26;
        asEnum = IntEnum.C;
        if ((int) boxedIntForEnum != 7) return 27;
        if (boxedIntForEnum.GetType() != typeof(int)) return 28;

        // ... and across two enums over the same integer.
        ref OtherIntEnum other = ref Unsafe.Unbox<OtherIntEnum>(boxedEnum);
        if ((int) other != 7) return 29;
        other = OtherIntEnum.P;
        if ((int) (IntEnum) boxedEnum != 3) return 30;

        // A byte-backed enum and byte.
        object boxedByteEnum = ByteEnum.Q;
        ref byte enumByte = ref Unsafe.Unbox<byte>(boxedByteEnum);
        if (enumByte != 5) return 31;
        enumByte = 6;
        if ((byte) (ByteEnum) boxedByteEnum != 6) return 32;

        // Instance methods and atomics through the byref: `Int32.CompareTo` reads `m_value`
        // through `this`, which is the unboxed byref itself.
        object boxedCounter = 10;
        if (Unsafe.Unbox<int>(boxedCounter).CompareTo(10) != 0) return 33;
        if (Interlocked.Increment(ref Unsafe.Unbox<int>(boxedCounter)) != 11) return 34;
        if (Interlocked.Exchange(ref Unsafe.Unbox<int>(boxedCounter), 20) != 11) return 35;
        if ((int) boxedCounter != 20) return 36;
        if (Unsafe.Unbox<int>(boxedEnum).CompareTo(3) != 0) return 37;
        if (Unsafe.Unbox<double>(boxedDouble).CompareTo(6.0) != 0) return 38;
        if (Unsafe.Unbox<Point>(boxedPoint).GetHashCode() != ((Point) boxedPoint).GetHashCode()) return 39;

        // The two byrefs a relaxed pair of unboxes hands out address the same storage, and the
        // underlying-integer view supports the byte-level and atomic operations an `int` does.
        object boxedEnumForViews = IntEnum.B;
        if (!Unsafe.AreSame(ref Unsafe.Unbox<int>(boxedEnumForViews), ref Unsafe.As<IntEnum, int>(ref Unsafe.Unbox<IntEnum>(boxedEnumForViews)))) return 50;
        object boxedIntForViews = 5;
        if (!Unsafe.AreSame(ref Unsafe.Unbox<IntEnum>(boxedIntForViews), ref Unsafe.As<int, IntEnum>(ref Unsafe.Unbox<int>(boxedIntForViews)))) return 51;
        ref int viewed = ref Unsafe.Unbox<int>(boxedEnumForViews);
        Unsafe.As<int, uint>(ref viewed) = 7u;
        if ((int) (IntEnum) boxedEnumForViews != 7) return 52;
        if (Unsafe.ReadUnaligned<int>(ref Unsafe.As<int, byte>(ref viewed)) != 7) return 53;
        Unsafe.WriteUnaligned<int>(ref Unsafe.As<int, byte>(ref viewed), 9);
        if ((int) (IntEnum) boxedEnumForViews != 9) return 54;
        if (Interlocked.Increment(ref viewed) != 10) return 55;
        if (Interlocked.CompareExchange(ref viewed, 20, 10) != 10) return 56;
        if ((int) (IntEnum) boxedEnumForViews != 20) return 57;
        object boxedLongEnum = LongEnum.X;
        if (Interlocked.Add(ref Unsafe.Unbox<long>(boxedLongEnum), 5) != 8) return 58;
        if ((long) (LongEnum) boxedLongEnum != 8) return 59;

        // Null raises NullReferenceException.
        try
        {
            Unsafe.Unbox<int>(null);
            return 40;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Unsafe.Unbox<Point>(null);
            return 41;
        }
        catch (NullReferenceException)
        {
        }

        // A box of a different type raises InvalidCastException, with no relaxation between
        // integers of different widths or between signed and unsigned element types.
        try
        {
            Unsafe.Unbox<int>(boxedLong);
            return 42;
        }
        catch (InvalidCastException)
        {
        }

        try
        {
            Unsafe.Unbox<uint>(boxedInt);
            return 43;
        }
        catch (InvalidCastException)
        {
        }

        try
        {
            Unsafe.Unbox<int>(boxedByteEnum);
            return 44;
        }
        catch (InvalidCastException)
        {
        }

        try
        {
            Unsafe.Unbox<Point>(boxedWithRef);
            return 45;
        }
        catch (InvalidCastException)
        {
        }

        // A reference-typed operand is not a box at all.
        try
        {
            Unsafe.Unbox<int>("not a box");
            return 46;
        }
        catch (InvalidCastException)
        {
        }

        try
        {
            Unsafe.Unbox<int>(new int[1]);
            return 47;
        }
        catch (InvalidCastException)
        {
        }

        return 0;
    }
}
