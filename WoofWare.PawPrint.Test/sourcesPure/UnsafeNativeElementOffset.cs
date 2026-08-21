using System;
using System.Runtime.CompilerServices;

// `Unsafe.Add<T>` and `Unsafe.Subtract<T>` each have three byref overloads, taking the element
// offset as `int`, `IntPtr` or `nuint`. `UnsafeIntrinsics.EmitAdd`/`EmitSubtract` generate a
// single IL body per member, shared by all three: `ldarg.1; sizeof !!T; conv.i; mul; add`/`sub`.
// `mul` and `sub` are two's-complement, so the declared parameter's signedness never enters the
// arithmetic — only its bits do. These pin that: the offset is read at native-int width, an
// `int` one sign-extended into it, and a `nuint` one taken as the bits it is rather than as an
// unsigned magnitude.
//
// `UnsafeSubtractArithmetic.cs` covers the `(ref T, int)` overload; this is its native-width
// sibling. `UnsafeArrayArithmetic.cs` covers `Add`'s three overloads at small offsets.
public class TestUnsafeNativeElementOffset
{
    // The blocking shape: an `IntPtr` element offset on a plain byref into an int[].
    // `HashCode.AddBytes` forms exactly this to round its end pointer down to a 16-byte block.
    public static int Test1()
    {
        int[] a = { 10, 20, 30, 40 };
        ref int q = ref Unsafe.Subtract(ref a[3], (IntPtr)2);
        if (q != 20)
            return 1;
        if (!Unsafe.AreSame(ref q, ref a[1]))
            return 2;
        if (!Unsafe.AreSame(ref q, ref Unsafe.Add(ref a[3], (IntPtr)(-2))))
            return 3;
        return 0;
    }

    // A `nuint` element offset reaches the same place.
    public static int Test2()
    {
        int[] a = { 10, 20, 30, 40 };
        ref int q = ref Unsafe.Subtract(ref a[3], (nuint)2);
        if (q != 20)
            return 4;
        if (!Unsafe.AreSame(ref q, ref a[1]))
            return 5;
        if (!Unsafe.AreSame(ref q, ref Unsafe.Add(ref a[1], (nuint)0)))
            return 6;
        return 0;
    }

    // The discriminating case. `unchecked((nuint)(-1))` is 2^64-1, so an implementation that
    // read a `nuint` offset as an unsigned *magnitude* would walk 2^64-1 elements backwards
    // (and refuse, having nowhere to land). The IL multiplies its bits, so this moves exactly
    // one element forwards.
    public static int Test3()
    {
        int[] a = { 10, 20, 30, 40 };
        nuint minusOne = unchecked((nuint)(-1));
        ref int q = ref Unsafe.Subtract(ref a[1], minusOne);
        if (q != 30)
            return 7;
        if (!Unsafe.AreSame(ref q, ref a[2]))
            return 8;
        // The mirror: `Add` by the same bits moves one element backwards.
        if (!Unsafe.AreSame(ref Unsafe.Add(ref a[1], minusOne), ref a[0]))
            return 9;
        // Two elements' worth of the same shape.
        nuint minusTwo = unchecked((nuint)(-2));
        if (!Unsafe.AreSame(ref Unsafe.Subtract(ref a[1], minusTwo), ref a[3]))
            return 10;
        return 0;
    }

    // Zero is the identity under both native-width overloads, and must not perturb the byref's
    // identity.
    public static int Test4()
    {
        int[] a = { 10, 20, 30 };
        if (!Unsafe.AreSame(ref Unsafe.Subtract(ref a[1], (IntPtr)0), ref a[1]))
            return 11;
        if (!Unsafe.AreSame(ref Unsafe.Subtract(ref a[1], (nuint)0), ref a[1]))
            return 12;
        if (!Unsafe.AreSame(ref Unsafe.Add(ref a[1], (IntPtr)0), ref a[1]))
            return 13;
        if (!Unsafe.AreSame(ref Unsafe.Add(ref a[1], (nuint)0), ref a[1]))
            return 14;
        return 0;
    }

    // Writing through a byref obtained at native width lands in the right slot and leaves its
    // neighbours alone.
    public static int Test5()
    {
        int[] a = { 10, 20, 30 };
        ref int q = ref Unsafe.Subtract(ref a[2], (IntPtr)1);
        q = 222;
        if (a[0] != 10)
            return 15;
        if (a[1] != 222)
            return 16;
        if (a[2] != 30)
            return 17;
        return 0;
    }

    // The three overloads agree with each other and with `Add`'s negation across the whole index
    // range, in both orders. This is the round-trip property: whichever internal representation
    // the intermediate byref takes, coming back must reach the original byref rather than merely
    // an address that reads the same value.
    public static int Test6()
    {
        int[] a = { 10, 20, 30, 40, 50, 60 };
        for (int i = 0; i < a.Length; i++)
        {
            for (int k = -a.Length; k <= a.Length; k++)
            {
                // Both endpoints must stay in range for the intermediate byref to be one we may
                // compare and dereference.
                if (i - k < 0 || i - k >= a.Length)
                    continue;

                ref int start = ref a[i];
                ref int viaInt = ref Unsafe.Subtract(ref start, k);
                ref int viaNint = ref Unsafe.Subtract(ref start, (IntPtr)k);
                ref int viaNuint = ref Unsafe.Subtract(ref start, unchecked((nuint)(long)k));

                if (!Unsafe.AreSame(ref viaNint, ref a[i - k]))
                    return 18;
                if (!Unsafe.AreSame(ref viaNuint, ref a[i - k]))
                    return 19;
                if (!Unsafe.AreSame(ref viaNint, ref viaInt))
                    return 20;
                if (viaNint != a[i - k])
                    return 21;
                // Mirror against `Add` with the negated offset, at each width.
                if (!Unsafe.AreSame(ref viaNint, ref Unsafe.Add(ref start, (IntPtr)(-k))))
                    return 22;
                if (!Unsafe.AreSame(ref viaNuint, ref Unsafe.Add(ref start, unchecked((nuint)(long)(-k)))))
                    return 23;
                // Round trip back.
                if (!Unsafe.AreSame(ref Unsafe.Add(ref viaNint, (IntPtr)k), ref start))
                    return 24;
                if (!Unsafe.AreSame(ref Unsafe.Subtract(ref viaNint, (IntPtr)(-k)), ref start))
                    return 25;
                // The byte distance must be exactly -k elements' worth.
                if ((long)Unsafe.ByteOffset(ref start, ref viaNint) != -(long)k * sizeof(int))
                    return 26;
            }
        }
        return 0;
    }

    // A struct element type, so `sizeof(T)` is neither 1 nor a machine word.
    public static int Test7()
    {
        Pair[] a = new Pair[4];
        for (int i = 0; i < a.Length; i++)
            a[i] = new Pair { X = i, Y = i * 10 };

        ref Pair p = ref Unsafe.Subtract(ref a[3], (IntPtr)3);
        if (p.X != 0 || p.Y != 0)
            return 27;
        if (!Unsafe.AreSame(ref p, ref a[0]))
            return 28;
        ref Pair q = ref Unsafe.Subtract(ref a[3], (nuint)1);
        q.Y = 999;
        if (a[2].Y != 999)
            return 29;
        if (a[3].Y != 30)
            return 30;
        return 0;
    }

    // A size-preserving reinterpret keeps the underlying int stride, so the walk still steps
    // whole array cells; a size-*changing* one makes it a byte cursor over the array.
    public static int Test8()
    {
        int[] a = { 0x01020304, 0x05060708, 0x090A0B0C, 0x0D0E0F10 };

        ref uint u3 = ref Unsafe.As<int, uint>(ref a[3]);
        ref uint u1 = ref Unsafe.Subtract(ref u3, (IntPtr)2);
        if (u1 != 0x05060708u)
            return 31;
        if ((long)Unsafe.ByteOffset(ref u1, ref u3) != 2L * sizeof(int))
            return 32;

        if (!BitConverter.IsLittleEndian)
            return 0;

        ref byte b = ref Unsafe.As<int, byte>(ref a[2]);
        ref byte back = ref Unsafe.Subtract(ref b, (IntPtr)sizeof(int));
        if (back != 0x08)
            return 33;
        if ((long)Unsafe.ByteOffset(ref back, ref b) != sizeof(int))
            return 34;
        return 0;
    }

    // A byref to a stack local: the identity walk must keep the local addressable for reads and
    // writes at native width too.
    public static int Test9()
    {
        int value = 77;
        ref int r = ref Unsafe.Subtract(ref value, (nuint)0);
        if (r != 77)
            return 35;
        r = 88;
        if (value != 88)
            return 36;
        if (!Unsafe.AreSame(ref r, ref value))
            return 37;
        return 0;
    }

    // A synthesised bit-pattern byref, walked further than an int32 element offset could express.
    // Nothing is dereferenced: only the distances are meaningful. An implementation that narrowed
    // the native-width offset to an int32 could not express this walk at all.
    public static unsafe int Test10()
    {
        ref byte p = ref Unsafe.AsRef<byte>((void*)8);
        ref byte far = ref Unsafe.Add(ref p, (IntPtr)3000000000L);
        if ((long)Unsafe.ByteOffset(ref p, ref far) != 3000000000L)
            return 38;
        if ((long)Unsafe.AsPointer(ref far) != 3000000008L)
            return 39;
        if (!Unsafe.AreSame(ref Unsafe.Subtract(ref far, (IntPtr)3000000000L), ref p))
            return 40;
        // The same distance in the other direction, and back.
        ref byte behind = ref Unsafe.Subtract(ref p, (IntPtr)3000000000L);
        if ((long)Unsafe.AsPointer(ref behind) != 8L - 3000000000L)
            return 41;
        if (!Unsafe.AreSame(ref Unsafe.Add(ref behind, (IntPtr)3000000000L), ref p))
            return 42;
        // Scaled by a wider element type, so the *product* also exceeds int32 range.
        ref long q = ref Unsafe.AsRef<long>((void*)0);
        ref long qFar = ref Unsafe.Add(ref q, (IntPtr)1000000000L);
        if ((long)Unsafe.AsPointer(ref qFar) != 8000000000L)
            return 43;
        return 0;
    }

    // `long.MinValue` is the one element offset whose negation is not itself representable. The
    // IL negates at native-int width, where two's-complement negation wraps it back to itself —
    // and that is faithful rather than a defect, because negation commutes with the wrapping
    // multiply. Subtracting it from null must therefore land on 0x8000000000000000, exactly as
    // adding it would.
    public static unsafe int Test11()
    {
        ref byte z = ref Unsafe.NullRef<byte>();
        nuint minValueBits = unchecked((nuint)long.MinValue);
        ref byte huge = ref Unsafe.Subtract(ref z, minValueBits);
        if ((long)Unsafe.AsPointer(ref huge) != long.MinValue)
            return 44;
        if (!Unsafe.AreSame(ref huge, ref Unsafe.Add(ref z, minValueBits)))
            return 45;
        // And the same for a two-byte element type, where the product wraps to zero.
        ref char c = ref Unsafe.NullRef<char>();
        if (!Unsafe.IsNullRef(ref Unsafe.Subtract(ref c, minValueBits)))
            return 46;
        return 0;
    }

    private struct Pair
    {
        public int X;
        public int Y;
    }

    public static int Main(string[] argv)
    {
        int r = Test1();
        if (r != 0) return r;
        r = Test2();
        if (r != 0) return r;
        r = Test3();
        if (r != 0) return r;
        r = Test4();
        if (r != 0) return r;
        r = Test5();
        if (r != 0) return r;
        r = Test6();
        if (r != 0) return r;
        r = Test7();
        if (r != 0) return r;
        r = Test8();
        if (r != 0) return r;
        r = Test9();
        if (r != 0) return r;
        r = Test10();
        if (r != 0) return r;
        r = Test11();
        if (r != 0) return r;
        return 0;
    }
}
