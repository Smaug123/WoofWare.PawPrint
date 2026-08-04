using System;

namespace IntSpanStartsWithBitwiseTest
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

            // Every span starts with the empty span, including the empty span itself.
            if (!((ReadOnlySpan<int>)empty).StartsWith((ReadOnlySpan<int>)new int[0]))
            {
                return 1;
            }

            int[] left = new int[3];
            left[0] = 123456;
            left[1] = -789;
            left[2] = 42;

            int[] prefix = new int[2];
            prefix[0] = 123456;
            prefix[1] = -789;

            int[] notPrefix = new int[2];
            notPrefix[0] = 123456;
            notPrefix[1] = -790;

            int[] suffix = new int[2];
            suffix[0] = -789;
            suffix[1] = 42;

            int[] longer = new int[4];
            longer[0] = 123456;
            longer[1] = -789;
            longer[2] = 42;
            longer[3] = 7;

            ReadOnlySpan<int> leftSpan = left;

            if (!leftSpan.StartsWith((ReadOnlySpan<int>)prefix))
            {
                return 2;
            }

            // A span starts with itself.
            if (!leftSpan.StartsWith(leftSpan))
            {
                return 3;
            }

            if (leftSpan.StartsWith((ReadOnlySpan<int>)notPrefix))
            {
                return 4;
            }

            // Matching elements, but not at the start.
            if (leftSpan.StartsWith((ReadOnlySpan<int>)suffix))
            {
                return 5;
            }

            // A value longer than the span can never be a prefix.
            if (leftSpan.StartsWith((ReadOnlySpan<int>)longer))
            {
                return 6;
            }

            // The empty span starts with nothing but the empty span.
            if (((ReadOnlySpan<int>)empty).StartsWith((ReadOnlySpan<int>)prefix))
            {
                return 7;
            }

            // Slicing shifts which elements are compared.
            if (!leftSpan.Slice(1).StartsWith((ReadOnlySpan<int>)suffix))
            {
                return 8;
            }

            if (leftSpan.Slice(1).StartsWith((ReadOnlySpan<int>)prefix))
            {
                return 9;
            }

            bool[] boolLeft = new bool[3];
            boolLeft[0] = true;
            boolLeft[1] = false;
            boolLeft[2] = true;

            bool[] boolPrefix = new bool[2];
            boolPrefix[0] = true;
            boolPrefix[1] = false;

            bool[] boolNotPrefix = new bool[2];
            boolNotPrefix[0] = true;
            boolNotPrefix[1] = true;

            if (!((ReadOnlySpan<bool>)boolLeft).StartsWith((ReadOnlySpan<bool>)boolPrefix))
            {
                return 10;
            }

            if (((ReadOnlySpan<bool>)boolLeft).StartsWith((ReadOnlySpan<bool>)boolNotPrefix))
            {
                return 11;
            }

            char[] charLeft = new char[3];
            charLeft[0] = 'a';
            charLeft[1] = '☃';
            charLeft[2] = 'z';

            char[] charPrefix = new char[2];
            charPrefix[0] = 'a';
            charPrefix[1] = '☃';

            char[] charNotPrefix = new char[2];
            charNotPrefix[0] = 'a';
            charNotPrefix[1] = '☄';

            if (!((ReadOnlySpan<char>)charLeft).StartsWith((ReadOnlySpan<char>)charPrefix))
            {
                return 12;
            }

            if (((ReadOnlySpan<char>)charLeft).StartsWith((ReadOnlySpan<char>)charNotPrefix))
            {
                return 13;
            }

            SmallEnum[] enumLeft = new SmallEnum[3];
            enumLeft[0] = SmallEnum.First;
            enumLeft[1] = SmallEnum.Second;
            enumLeft[2] = SmallEnum.Third;

            SmallEnum[] enumPrefix = new SmallEnum[2];
            enumPrefix[0] = SmallEnum.First;
            enumPrefix[1] = SmallEnum.Second;

            SmallEnum[] enumNotPrefix = new SmallEnum[2];
            enumNotPrefix[0] = SmallEnum.First;
            enumNotPrefix[1] = SmallEnum.Third;

            if (!((ReadOnlySpan<SmallEnum>)enumLeft).StartsWith((ReadOnlySpan<SmallEnum>)enumPrefix))
            {
                return 14;
            }

            if (((ReadOnlySpan<SmallEnum>)enumLeft).StartsWith((ReadOnlySpan<SmallEnum>)enumNotPrefix))
            {
                return 15;
            }

            long[] longLeft = new long[3];
            longLeft[0] = 1L << 40;
            longLeft[1] = -(1L << 33);
            longLeft[2] = 5;

            long[] longPrefix = new long[2];
            longPrefix[0] = 1L << 40;
            longPrefix[1] = -(1L << 33);

            long[] longNotPrefix = new long[2];
            longNotPrefix[0] = 1L << 40;
            longNotPrefix[1] = -(1L << 34);

            if (!((ReadOnlySpan<long>)longLeft).StartsWith((ReadOnlySpan<long>)longPrefix))
            {
                return 16;
            }

            if (((ReadOnlySpan<long>)longLeft).StartsWith((ReadOnlySpan<long>)longNotPrefix))
            {
                return 17;
            }

            return 0;
        }
    }
}
