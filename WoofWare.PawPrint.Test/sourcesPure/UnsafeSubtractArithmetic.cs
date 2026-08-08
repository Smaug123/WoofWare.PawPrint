using System;
using System.Runtime.CompilerServices;

// `Unsafe.Subtract<T>(ref T, int)` is a JIT intrinsic: the managed body under CORECLR
// throws PlatformNotSupportedException and the JIT substitutes
// `ldarg.0; ldarg.1; sizeof !!T; conv.i; mul; sub`. It is the exact mirror of
// `Unsafe.Add<T>(ref T, int)` (covered by UnsafeArrayArithmetic.cs), so each case below
// pins the mirror relationship as well as the absolute answer: whatever
// `Unsafe.Add(ref p, -k)` does, `Unsafe.Subtract(ref p, k)` must do.
public class TestUnsafeSubtractArithmetic
{
    // Subtract by a positive element offset on a plain byref into an int[].
    public static int Test1()
    {
        int[] a = { 10, 20, 30, 40 };
        ref int p = ref a[3];
        ref int q = ref Unsafe.Subtract(ref p, 2);
        if (q != 20)
            return 1;
        if (!Unsafe.AreSame(ref q, ref a[1]))
            return 2;
        return 0;
    }

    // Element offset zero is the identity, and must not perturb the byref's identity.
    public static int Test2()
    {
        int[] a = { 10, 20, 30 };
        ref int q = ref Unsafe.Subtract(ref a[1], 0);
        if (q != 20)
            return 3;
        if (!Unsafe.AreSame(ref q, ref a[1]))
            return 4;
        return 0;
    }

    // A negative element offset moves forwards, exactly as `Add` with the negation would.
    public static int Test3()
    {
        int[] a = { 10, 20, 30 };
        ref int q = ref Unsafe.Subtract(ref a[0], -2);
        if (q != 30)
            return 5;
        if (!Unsafe.AreSame(ref q, ref a[2]))
            return 6;
        return 0;
    }

    // Writing through a byref obtained by Subtract must land in the right slot and
    // leave its neighbours alone.
    public static int Test4()
    {
        int[] a = { 10, 20, 30 };
        ref int q = ref Unsafe.Subtract(ref a[2], 1);
        q = 222;
        if (a[0] != 10)
            return 7;
        if (a[1] != 222)
            return 8;
        if (a[2] != 30)
            return 9;
        return 0;
    }

    // Subtract and Add are inverses over the whole index range of an array, in both
    // orders. This is the round-trip property: whichever internal representation the
    // intermediate byref takes, coming back must reach the original byref, not merely
    // an address that reads the same value.
    public static int Test5()
    {
        int[] a = { 10, 20, 30, 40, 50, 60 };
        for (int i = 0; i < a.Length; i++)
        {
            for (int k = -a.Length; k <= a.Length; k++)
            {
                // Both endpoints must stay in range for the intermediate byref to be
                // one we may compare and dereference.
                if (i - k < 0 || i - k >= a.Length)
                    continue;

                ref int start = ref a[i];
                ref int moved = ref Unsafe.Subtract(ref start, k);
                if (!Unsafe.AreSame(ref moved, ref a[i - k]))
                    return 10;
                if (moved != a[i - k])
                    return 11;
                // Mirror against Add with the negated offset.
                if (!Unsafe.AreSame(ref moved, ref Unsafe.Add(ref start, -k)))
                    return 12;
                // Round trip back.
                if (!Unsafe.AreSame(ref Unsafe.Add(ref moved, k), ref start))
                    return 13;
                if (!Unsafe.AreSame(ref Unsafe.Subtract(ref moved, -k), ref start))
                    return 14;
                // The byte distance must be exactly -k elements' worth.
                if ((long)Unsafe.ByteOffset(ref start, ref moved) != -(long)k * sizeof(int))
                    return 15;
            }
        }
        return 0;
    }

    // A size-preserving reinterpret (int -> uint) keeps the underlying int stride,
    // so Subtract still steps whole array cells.
    public static int Test6()
    {
        int[] a = { 100, 200, 300, 400 };
        ref uint u3 = ref Unsafe.As<int, uint>(ref a[3]);
        ref uint u1 = ref Unsafe.Subtract(ref u3, 2);
        if (u1 != 200u)
            return 16;
        if ((long)Unsafe.ByteOffset(ref u1, ref u3) != 2L * sizeof(int))
            return 17;
        // Writing through the reinterpreted byref lands in the underlying storage.
        u1 = 0xDEADBEEFu;
        if (a[1] != unchecked((int)0xDEADBEEFu))
            return 18;
        if (a[0] != 100 || a[2] != 300 || a[3] != 400)
            return 19;
        return 0;
    }

    // A size-*changing* reinterpret (int -> byte) makes Subtract a byte-cursor walk
    // over the array rather than a cell-index step.
    public static int Test7()
    {
        int[] a = { 0x01020304, 0x05060708, 0x090A0B0C };
        ref byte b = ref Unsafe.As<int, byte>(ref a[2]);
        ref byte back = ref Unsafe.Subtract(ref b, sizeof(int));
        // Little-endian: the first byte of a[1] is the low byte of 0x05060708.
        if (!BitConverter.IsLittleEndian)
            return 0;
        if (back != 0x08)
            return 20;
        if ((long)Unsafe.ByteOffset(ref back, ref b) != sizeof(int))
            return 21;
        // One byte further back is still inside a[1].
        ref byte back2 = ref Unsafe.Subtract(ref b, sizeof(int) - 1);
        if (back2 != 0x07)
            return 22;
        return 0;
    }

    // A byref to a stack local: Subtract by zero is the identity and must keep the
    // local addressable for reads and writes.
    public static int Test8()
    {
        int value = 77;
        ref int r = ref Unsafe.Subtract(ref value, 0);
        if (r != 77)
            return 23;
        r = 88;
        if (value != 88)
            return 24;
        if (!Unsafe.AreSame(ref r, ref value))
            return 25;
        return 0;
    }

    // A struct element type, so `sizeof(T)` is neither 1 nor a machine word.
    public static int Test9()
    {
        Pair[] a = new Pair[4];
        for (int i = 0; i < a.Length; i++)
            a[i] = new Pair { X = i, Y = i * 10 };

        ref Pair p = ref Unsafe.Subtract(ref a[3], 3);
        if (p.X != 0 || p.Y != 0)
            return 26;
        if (!Unsafe.AreSame(ref p, ref a[0]))
            return 27;
        ref Pair q = ref Unsafe.Subtract(ref a[3], 1);
        q.Y = 999;
        if (a[2].Y != 999)
            return 28;
        if (a[3].Y != 30)
            return 29;
        return 0;
    }

    // Bit-pattern byrefs: `NullRef` is the bit pattern 0, and Subtract on it is plain
    // native-int arithmetic. Landing back on zero must renormalise to a null ref, or
    // `IsNullRef` would disagree with the CLR's bit-pattern definition.
    public static int Test10()
    {
        ref byte nullRef = ref Unsafe.NullRef<byte>();
        ref byte negative = ref Unsafe.Subtract(ref nullRef, 4);
        if (Unsafe.IsNullRef(ref negative))
            return 30;
        if ((long)Unsafe.ByteOffset(ref nullRef, ref negative) != -4L)
            return 31;
        if (!Unsafe.IsNullRef(ref Unsafe.Subtract(ref negative, -4)))
            return 32;
        if (!Unsafe.IsNullRef(ref Unsafe.Add(ref negative, 4)))
            return 33;

        // A wider element type scales by its size.
        ref int nullInt = ref Unsafe.NullRef<int>();
        ref int intOffset = ref Unsafe.Subtract(ref nullInt, 3);
        if ((long)Unsafe.ByteOffset(ref nullInt, ref intOffset) != -12L)
            return 34;
        return 0;
    }

    // A synthesised bit-pattern byref that is not anchored to storage: Subtract is
    // bit arithmetic on it, and must agree with the byte-offset route.
    public static unsafe int Test11()
    {
        ref long p = ref Unsafe.AsRef<long>((void*)4096);
        ref long q = ref Unsafe.Subtract(ref p, 2);
        if ((long)Unsafe.AsPointer(ref q) != 4096L - 2L * sizeof(long))
            return 35;
        if (!Unsafe.AreSame(ref q, ref Unsafe.AddByteOffset(ref p, (IntPtr)(-2 * sizeof(long)))))
            return 36;
        if (!Unsafe.AreSame(ref p, ref Unsafe.Subtract(ref q, -2)))
            return 37;
        return 0;
    }

    // `Int32.MinValue` is the one offset whose negation is not itself an int32. The IL negates at
    // native-int width, so subtracting it moves *forwards* by 2^31 elements; an interpreter that
    // negated in int32 would wrap back to Int32.MinValue and move backwards instead.
    public static int Test12()
    {
        // A bit-pattern byref has no storage behind it, so the result is well-defined however far
        // it moves: 2^31 bytes forward from null.
        ref byte n = ref Unsafe.NullRef<byte>();
        ref byte far = ref Unsafe.Subtract(ref n, int.MinValue);
        if ((long)Unsafe.ByteOffset(ref n, ref far) != 2147483648L)
            return 38;
        // And back again, via the mirror.
        if (!Unsafe.IsNullRef(ref Unsafe.Add(ref far, int.MinValue)))
            return 39;

        // The same offset applied to an anchored byref one element *before* an array lands exactly
        // Int32.MaxValue elements past it. Never dereferenced — only its distance is meaningful.
        int[] a = new int[4];
        ref int back = ref Unsafe.Subtract(ref a[0], 1);
        ref int fwd = ref Unsafe.Subtract(ref back, int.MinValue);
        if ((long)Unsafe.ByteOffset(ref a[0], ref fwd) != 2147483647L * sizeof(int))
            return 40;
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
        r = Test12();
        if (r != 0) return r;
        return 0;
    }
}
