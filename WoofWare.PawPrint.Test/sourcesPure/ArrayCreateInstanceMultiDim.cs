using System;

// `Array.CreateInstance` and `Array.CreateInstanceFromArrayType` above rank 1. Every
// overload bottoms out in the `Array_CreateInstance` QCall with a `rank`, an `int*` of
// lengths, and an `int*` of lower bounds that is null unless the caller supplied one.
// The lengths reach the QCall in two shapes: the fixed-arity overloads `stackalloc` them,
// and the `int[]` overloads pin the caller's array with `fixed`. The lengths are chosen
// asymmetric so that a stride or ordering mistake on the native side reads as a wrong
// shape rather than passing by accident.
//
// Only zero lower bounds are exercised: an array with a non-zero lower bound is a
// different runtime shape, and this guest is about the rank.
//
// CoreCLR's `CheckElementType` runs on this QCall whenever the caller named an element
// type rather than an array type, so the element types it forbids are exercised too, at
// rank 1 as well as above it. Each is checked by the exact message CoreCLR's resource
// string renders, so a refusal for some other reason does not pass.
public class TestArrayCreateInstanceMultiDim
{
    private struct Pair
    {
        public int A;
        public long B;
    }

    // stackalloc'd lengths, rank 2, primitive element.
    private static int Int2D()
    {
        Array created = Array.CreateInstance(typeof(int), 2, 3);
        if (created.Rank != 2) return 1;
        if (created.Length != 6) return 2;
        if (created.GetLength(0) != 2) return 3;
        if (created.GetLength(1) != 3) return 4;
        if (created.GetLowerBound(1) != 0) return 5;
        if (created.GetUpperBound(1) != 2) return 6;
        if (created.GetType() != typeof(int[,])) return 7;

        int[,] typed = created as int[,];
        if (typed == null) return 8;

        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                typed[i, j] = 10 * i + j + 1;

        if (typed[1, 2] != 13) return 9;
        if ((int)created.GetValue(1, 2) != 13) return 10;
        if ((int)created.GetValue(0, 1) != 2) return 11;

        created.SetValue(77, 1, 0);
        if (typed[1, 0] != 77) return 12;

        return 0;
    }

    // stackalloc'd lengths, rank 3, reference element.
    private static int String3D()
    {
        Array created = Array.CreateInstance(typeof(string), 1, 2, 3);
        if (created.Rank != 3) return 1;
        if (created.Length != 6) return 2;
        if (created.GetLength(0) != 1) return 3;
        if (created.GetLength(1) != 2) return 4;
        if (created.GetLength(2) != 3) return 5;
        if (created.GetType() != typeof(string[,,])) return 6;

        string[,,] typed = (string[,,])created;
        if (typed[0, 1, 2] != null) return 7;
        typed[0, 1, 2] = "corner";
        typed[0, 0, 0] = "origin";
        if ((string)created.GetValue(0, 1, 2) != "corner") return 8;
        if ((string)created.GetValue(0, 0, 0) != "origin") return 9;
        if (created.GetValue(0, 1, 1) != null) return 10;

        return 0;
    }

    // Pinned `int[]` of lengths, rank 4, struct element.
    private static int Struct4D()
    {
        Array created = Array.CreateInstance(typeof(Pair), new int[] { 3, 1, 2, 4 });
        if (created.Rank != 4) return 1;
        if (created.Length != 24) return 2;
        if (created.GetLength(0) != 3) return 3;
        if (created.GetLength(1) != 1) return 4;
        if (created.GetLength(2) != 2) return 5;
        if (created.GetLength(3) != 4) return 6;
        if (created.GetType() != typeof(Pair[,,,])) return 7;

        Pair[,,,] typed = (Pair[,,,])created;
        if (typed[2, 0, 1, 3].A != 0 || typed[2, 0, 1, 3].B != 0L) return 8;
        typed[2, 0, 1, 3].A = 5;
        typed[2, 0, 1, 3].B = -6L;
        typed[0, 0, 0, 0].A = 1;
        if (typed[2, 0, 1, 3].A != 5 || typed[2, 0, 1, 3].B != -6L) return 9;
        if (typed[0, 0, 0, 0].A != 1) return 10;
        if (typed[1, 0, 1, 3].A != 0) return 11;

        return 0;
    }

    // Pinned lengths and pinned lower bounds, every lower bound zero, rank 2.
    private static int ZeroLowerBounds2D()
    {
        Array created = Array.CreateInstance(typeof(byte), new int[] { 4, 2 }, new int[] { 0, 0 });
        if (created.Rank != 2) return 1;
        if (created.Length != 8) return 2;
        if (created.GetLength(0) != 4) return 3;
        if (created.GetLength(1) != 2) return 4;
        if (created.GetLowerBound(0) != 0) return 5;
        if (created.GetLowerBound(1) != 0) return 6;
        if (created.GetType() != typeof(byte[,])) return 7;

        byte[,] typed = (byte[,])created;
        typed[3, 1] = 200;
        if ((byte)created.GetValue(3, 1) != 200) return 8;

        return 0;
    }

    // A zero-length dimension empties the array without disturbing the other lengths.
    private static int ZeroDimension()
    {
        Array created = Array.CreateInstance(typeof(int), 2, 0);
        if (created.Rank != 2) return 1;
        if (created.Length != 0) return 2;
        if (created.GetLength(0) != 2) return 3;
        if (created.GetLength(1) != 0) return 4;
        if (created.GetUpperBound(1) != -1) return 5;
        if (!(created is int[,])) return 6;

        Array created3 = Array.CreateInstance(typeof(string), new int[] { 0, 5, 7 });
        if (created3.Length != 0) return 7;
        if (created3.GetLength(1) != 5) return 8;
        if (created3.GetLength(2) != 7) return 9;

        return 0;
    }

    // From the array type rather than the element type: the QCall's fromArrayType branch.
    private static int FromArrayType()
    {
        Array created = Array.CreateInstanceFromArrayType(typeof(int[,]), 2, 5);
        if (created.Rank != 2) return 1;
        if (created.Length != 10) return 2;
        if (created.GetLength(0) != 2) return 3;
        if (created.GetLength(1) != 5) return 4;
        if (created.GetType() != typeof(int[,])) return 5;

        int[,] typed = (int[,])created;
        typed[1, 4] = 42;
        if ((int)created.GetValue(1, 4) != 42) return 6;

        Array withBounds = Array.CreateInstanceFromArrayType(typeof(long[,,]), new int[] { 1, 2, 3 }, new int[] { 0, 0, 0 });
        if (withBounds.Rank != 3) return 7;
        if (withBounds.Length != 6) return 8;
        if (withBounds.GetLength(2) != 3) return 9;
        if (withBounds.GetType() != typeof(long[,,])) return 10;

        long[,,] typedBounds = (long[,,])withBounds;
        typedBounds[0, 1, 2] = long.MinValue;
        if ((long)withBounds.GetValue(0, 1, 2) != long.MinValue) return 11;

        return 0;
    }

    // Rank 1 through the same overloads must still make a szarray, not a rank-1
    // multi-dimensional array.
    private static int Rank1StaysSzArray()
    {
        Array created = Array.CreateInstance(typeof(int), new int[] { 3 });
        if (created.Rank != 1) return 1;
        if (created.GetType() != typeof(int[])) return 2;
        if (!(created is int[])) return 3;

        Array withBounds = Array.CreateInstance(typeof(int), new int[] { 3 }, new int[] { 0 });
        if (withBounds.GetType() != typeof(int[])) return 4;

        // The single-length overload passes the address of its own parameter.
        Array single = Array.CreateInstance(typeof(int), 3);
        if (single.GetType() != typeof(int[])) return 5;
        if (single.Length != 3) return 6;

        return 0;
    }

    private static string Refusal(Func<object> f)
    {
        try
        {
            f();
            return "no exception";
        }
        catch (NotSupportedException e)
        {
            return e.Message;
        }
        catch (Exception e)
        {
            return e.GetType().Name;
        }
    }

    // The element types CoreCLR forbids, at rank 1 and above it: the screen precedes the
    // rank, so both must refuse and with the same message.
    private static int ForbiddenElementTypes()
    {
        const string voidMessage = "Arrays of System.Void are not supported.";
        const string byRefLikeMessage = "Cannot create arrays of ByRef-like values.";

        if (Refusal(() => Array.CreateInstance(typeof(void), 2, 3)) != voidMessage) return 1;
        if (Refusal(() => Array.CreateInstance(typeof(void), new int[] { 2, 3, 4 })) != voidMessage) return 2;
        if (Refusal(() => Array.CreateInstance(typeof(void), 2)) != voidMessage) return 3;

        if (Refusal(() => Array.CreateInstance(typeof(Span<int>), 2, 3)) != byRefLikeMessage) return 4;
        if (Refusal(() => Array.CreateInstance(typeof(Span<int>), 2)) != byRefLikeMessage) return 5;
        if (Refusal(() => Array.CreateInstance(typeof(TypedReference), 2, 3)) != byRefLikeMessage) return 6;

        // The refusal is a guest-catchable exception, so execution continues normally: a
        // legal creation right after a refused one still works.
        Array afterwards = Array.CreateInstance(typeof(int), 2, 3);
        if (afterwards.Length != 6) return 7;

        return 0;
    }

    // A pointer element is a `TypeDesc` the screen deliberately lets through, as is an
    // array element. Both must still allocate.
    private static unsafe int AllowedExoticElementTypes()
    {
        Array pointers = Array.CreateInstance(typeof(int*), 2, 3);
        if (pointers.Rank != 2) return 1;
        if (pointers.Length != 6) return 2;
        if (pointers.GetType() != typeof(int*[,])) return 3;

        Array jagged = Array.CreateInstance(typeof(int[]), 2, 3);
        if (jagged.Rank != 2) return 4;
        if (jagged.Length != 6) return 5;
        // Reflection and C# order the suffixes oppositely: this is `System.Int32[][,]` to
        // reflection and `int[,][]` in C#, a rank-2 array whose elements are `int[]`.
        if (jagged.GetType() != typeof(int[,][])) return 6;

        int[,][] typedJagged = (int[,][])jagged;
        if (typedJagged[1, 2] != null) return 7;
        typedJagged[1, 2] = new int[1];
        if (typedJagged[1, 2].Length != 1) return 8;

        return 0;
    }

    private static string OomRefusal(Func<object> f)
    {
        try
        {
            f();
            return "no exception";
        }
        catch (OutOfMemoryException e)
        {
            return e.Message;
        }
        catch (Exception e)
        {
            return e.GetType().Name;
        }
    }

    // `AllocateArrayEx` refuses dimensions by two independent rules, and neither can be
    // rescued by a zero dimension elsewhere: a single dimension above `MaxArrayLength`
    // (2147483591), recorded and raised after the whole walk, and a running element count
    // that overflows UInt32 at some multiply. A product that merely passes above
    // Int32.MaxValue and comes back down is fine, which is what separates the two.
    //
    // None of these allocate anything, so the guest can assert them cheaply.
    private static int DimensionLimits()
    {
        const string exceeded = "Array dimensions exceeded supported range.";

        // One over MaxArrayLength, in the first dimension and in a later one.
        if (OomRefusal(() => Array.CreateInstance(typeof(byte), new int[] { 0x7FFFFFC8, 0 })) != exceeded) return 1;
        if (OomRefusal(() => Array.CreateInstance(typeof(byte), new int[] { 0, 0x7FFFFFC8 })) != exceeded) return 2;
        if (OomRefusal(() => Array.CreateInstance(typeof(byte), new int[] { int.MaxValue, 0 })) != exceeded) return 3;

        // The same rule reaches the rank-1 overloads, which allocate a szarray.
        if (OomRefusal(() => Array.CreateInstance(typeof(byte), 0x7FFFFFC8)) != exceeded) return 4;

        // MaxArrayLength itself is allowed: the boundary is inclusive, and the zero dimension
        // makes the array empty, so nothing is actually allocated.
        Array atLimit = Array.CreateInstance(typeof(byte), new int[] { 0x7FFFFFC7, 0 });
        if (atLimit.Length != 0) return 5;
        if (atLimit.GetLength(0) != 0x7FFFFFC7) return 6;
        if (atLimit.GetLength(1) != 0) return 7;

        // The running product overflows UInt32 at the second multiply, so the trailing zero
        // does not rescue it.
        if (OomRefusal(() => Array.CreateInstance(typeof(int), new int[] { 65536, 65536, 0 })) != exceeded) return 8;

        // But a prefix that merely exceeds Int32.MaxValue does come back down: 50000 * 50000
        // is 2.5e9, which fits in UInt32, and the trailing zero empties the array.
        Array transient = Array.CreateInstance(typeof(int), new int[] { 50000, 50000, 0 });
        if (transient.Length != 0) return 9;
        if (transient.GetLength(1) != 50000) return 10;

        return 0;
    }

    public static int Main()
    {
        int r;
        if ((r = Int2D()) != 0) return 100 + r;
        if ((r = String3D()) != 0) return 120 + r;
        if ((r = Struct4D()) != 0) return 140 + r;
        if ((r = ZeroLowerBounds2D()) != 0) return 160 + r;
        if ((r = ZeroDimension()) != 0) return 180 + r;
        if ((r = FromArrayType()) != 0) return 200 + r;
        if ((r = Rank1StaysSzArray()) != 0) return 220 + r;
        if ((r = ForbiddenElementTypes()) != 0) return 230 + r;
        if ((r = AllowedExoticElementTypes()) != 0) return 240 + r;
        if ((r = DimensionLimits()) != 0) return 250 + r;
        return 0;
    }
}
