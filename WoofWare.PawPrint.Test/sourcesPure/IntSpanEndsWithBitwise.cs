using System;

namespace IntSpanEndsWithBitwiseTest
{
    enum SmallEnum : short
    {
        First = 1,
        Second = 2,
        Third = -3,
    }

    class Program
    {
        static int Main(string[] args)
        {
            int[] empty = new int[0];

            // Every span ends with the empty span, including the empty span itself.
            if (!((ReadOnlySpan<int>)empty).EndsWith((ReadOnlySpan<int>)new int[0]))
            {
                return 1;
            }

            int[] left = new int[3];
            left[0] = 123456;
            left[1] = -789;
            left[2] = 42;

            int[] suffix = new int[2];
            suffix[0] = -789;
            suffix[1] = 42;

            int[] notSuffix = new int[2];
            notSuffix[0] = -790;
            notSuffix[1] = 42;

            int[] prefix = new int[2];
            prefix[0] = 123456;
            prefix[1] = -789;

            int[] longer = new int[4];
            longer[0] = 7;
            longer[1] = 123456;
            longer[2] = -789;
            longer[3] = 42;

            ReadOnlySpan<int> leftSpan = left;

            if (!leftSpan.EndsWith((ReadOnlySpan<int>)suffix))
            {
                return 2;
            }

            // A span ends with itself.
            if (!leftSpan.EndsWith(leftSpan))
            {
                return 3;
            }

            if (leftSpan.EndsWith((ReadOnlySpan<int>)notSuffix))
            {
                return 4;
            }

            // Matching elements, but not at the end.
            if (leftSpan.EndsWith((ReadOnlySpan<int>)prefix))
            {
                return 5;
            }

            // A value longer than the span can never be a suffix.
            if (leftSpan.EndsWith((ReadOnlySpan<int>)longer))
            {
                return 6;
            }

            // The empty span ends with nothing but the empty span.
            if (((ReadOnlySpan<int>)empty).EndsWith((ReadOnlySpan<int>)suffix))
            {
                return 7;
            }

            // Slicing shifts which elements are compared. Dropping the last element
            // makes `prefix` the suffix and `suffix` no longer one.
            if (!leftSpan.Slice(0, 2).EndsWith((ReadOnlySpan<int>)prefix))
            {
                return 8;
            }

            if (leftSpan.Slice(0, 2).EndsWith((ReadOnlySpan<int>)suffix))
            {
                return 9;
            }

            // A mutable Span<T> receiver: MemoryExtensions declares a Span<T> overload,
            // but it carries [OverloadResolutionPriority(-1)], so C# 13 and later always
            // pick the ReadOnlySpan<T> overload and insert Span<T>.op_Implicit here.
            Span<int> mutableSpan = left;

            if (!mutableSpan.EndsWith((ReadOnlySpan<int>)suffix))
            {
                return 10;
            }

            if (mutableSpan.EndsWith((ReadOnlySpan<int>)prefix))
            {
                return 11;
            }

            bool[] boolLeft = new bool[3];
            boolLeft[0] = true;
            boolLeft[1] = false;
            boolLeft[2] = true;

            bool[] boolSuffix = new bool[2];
            boolSuffix[0] = false;
            boolSuffix[1] = true;

            bool[] boolNotSuffix = new bool[2];
            boolNotSuffix[0] = true;
            boolNotSuffix[1] = true;

            if (!((ReadOnlySpan<bool>)boolLeft).EndsWith((ReadOnlySpan<bool>)boolSuffix))
            {
                return 12;
            }

            if (((ReadOnlySpan<bool>)boolLeft).EndsWith((ReadOnlySpan<bool>)boolNotSuffix))
            {
                return 13;
            }

            char[] charLeft = new char[3];
            charLeft[0] = 'a';
            charLeft[1] = '☃';
            charLeft[2] = 'z';

            char[] charSuffix = new char[2];
            charSuffix[0] = '☃';
            charSuffix[1] = 'z';

            char[] charNotSuffix = new char[2];
            charNotSuffix[0] = '☄';
            charNotSuffix[1] = 'z';

            if (!((ReadOnlySpan<char>)charLeft).EndsWith((ReadOnlySpan<char>)charSuffix))
            {
                return 14;
            }

            if (((ReadOnlySpan<char>)charLeft).EndsWith((ReadOnlySpan<char>)charNotSuffix))
            {
                return 15;
            }

            SmallEnum[] enumLeft = new SmallEnum[3];
            enumLeft[0] = SmallEnum.First;
            enumLeft[1] = SmallEnum.Second;
            enumLeft[2] = SmallEnum.Third;

            SmallEnum[] enumSuffix = new SmallEnum[2];
            enumSuffix[0] = SmallEnum.Second;
            enumSuffix[1] = SmallEnum.Third;

            SmallEnum[] enumNotSuffix = new SmallEnum[2];
            enumNotSuffix[0] = SmallEnum.First;
            enumNotSuffix[1] = SmallEnum.Third;

            if (!((ReadOnlySpan<SmallEnum>)enumLeft).EndsWith((ReadOnlySpan<SmallEnum>)enumSuffix))
            {
                return 16;
            }

            if (((ReadOnlySpan<SmallEnum>)enumLeft).EndsWith((ReadOnlySpan<SmallEnum>)enumNotSuffix))
            {
                return 17;
            }

            long[] longLeft = new long[3];
            longLeft[0] = 5;
            longLeft[1] = 1L << 40;
            longLeft[2] = -(1L << 33);

            long[] longSuffix = new long[2];
            longSuffix[0] = 1L << 40;
            longSuffix[1] = -(1L << 33);

            long[] longNotSuffix = new long[2];
            longNotSuffix[0] = 1L << 40;
            longNotSuffix[1] = -(1L << 34);

            if (!((ReadOnlySpan<long>)longLeft).EndsWith((ReadOnlySpan<long>)longSuffix))
            {
                return 18;
            }

            if (((ReadOnlySpan<long>)longLeft).EndsWith((ReadOnlySpan<long>)longNotSuffix))
            {
                return 19;
            }

            // String-backed char spans reach the element data through a different byref
            // root than array-backed ones do.
            //
            // Null-backed spans — `default` and `ReadOnlySpan<T>.Empty`, which is `default` —
            // are deliberately not covered here. Every span intrinsic of this shape
            // reinterprets `MemoryMarshal.GetReference(span)` via `Unsafe.As<T, byte>` before
            // looking at the length, and that reinterpret cannot yet be applied to a null
            // byref. The gap predates EndsWith (`SequenceEqual` hits it too) and is recorded
            // separately in sourcesPure/DefaultSpanSequenceEqual.cs. The zero-length cases
            // above are array-backed, so their `_reference` points at the array rather than
            // being null, and they do exercise the length logic.
            ReadOnlySpan<char> strSpan = "xabcabcx".AsSpan(1, 6);

            if (!strSpan.EndsWith("abc".AsSpan()))
            {
                return 20;
            }

            if (strSpan.EndsWith("xbc".AsSpan()))
            {
                return 21;
            }

            // A string-backed value, and a suffix that spans the slice exactly.
            if (!strSpan.EndsWith("abcabc".AsSpan()))
            {
                return 22;
            }

            if (strSpan.EndsWith("xabcabc".AsSpan()))
            {
                return 23;
            }

            return 0;
        }
    }
}
