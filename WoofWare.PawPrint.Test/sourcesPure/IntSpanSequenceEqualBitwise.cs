using System;

namespace IntSpanSequenceEqualBitwiseTest
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
            int[] emptyLeft = new int[0];
            int[] emptyRight = new int[0];

            if (!((ReadOnlySpan<int>)emptyLeft).SequenceEqual((ReadOnlySpan<int>)emptyRight))
            {
                return 1;
            }

            int[] left = new int[3];
            left[0] = 123456;
            left[1] = -789;
            left[2] = 42;

            int[] same = new int[3];
            same[0] = 123456;
            same[1] = -789;
            same[2] = 42;

            int[] different = new int[3];
            different[0] = 123456;
            different[1] = -789;
            different[2] = 43;

            int[] shorter = new int[2];
            shorter[0] = 123456;
            shorter[1] = -789;

            ReadOnlySpan<int> leftSpan = left;
            ReadOnlySpan<int> sameSpan = same;
            ReadOnlySpan<int> differentSpan = different;
            ReadOnlySpan<int> shorterSpan = shorter;

            if (!leftSpan.SequenceEqual(sameSpan))
            {
                return 2;
            }

            if (leftSpan.SequenceEqual(differentSpan))
            {
                return 3;
            }

            if (leftSpan.SequenceEqual(shorterSpan))
            {
                return 4;
            }

            bool[] boolLeft = new bool[3];
            boolLeft[0] = true;
            boolLeft[1] = false;
            boolLeft[2] = true;

            bool[] boolSame = new bool[3];
            boolSame[0] = true;
            boolSame[1] = false;
            boolSame[2] = true;

            bool[] boolDifferent = new bool[3];
            boolDifferent[0] = true;
            boolDifferent[1] = true;
            boolDifferent[2] = true;

            if (!((ReadOnlySpan<bool>)boolLeft).SequenceEqual((ReadOnlySpan<bool>)boolSame))
            {
                return 5;
            }

            if (((ReadOnlySpan<bool>)boolLeft).SequenceEqual((ReadOnlySpan<bool>)boolDifferent))
            {
                return 6;
            }

            char[] charLeft = new char[3];
            charLeft[0] = 'a';
            charLeft[1] = '\u2603';
            charLeft[2] = 'z';

            char[] charSame = new char[3];
            charSame[0] = 'a';
            charSame[1] = '\u2603';
            charSame[2] = 'z';

            char[] charDifferent = new char[3];
            charDifferent[0] = 'a';
            charDifferent[1] = '\u2603';
            charDifferent[2] = 'y';

            if (!((ReadOnlySpan<char>)charLeft).SequenceEqual((ReadOnlySpan<char>)charSame))
            {
                return 7;
            }

            if (((ReadOnlySpan<char>)charLeft).SequenceEqual((ReadOnlySpan<char>)charDifferent))
            {
                return 8;
            }

            SmallEnum[] enumLeft = new SmallEnum[3];
            enumLeft[0] = SmallEnum.First;
            enumLeft[1] = SmallEnum.Second;
            enumLeft[2] = SmallEnum.Third;

            SmallEnum[] enumSame = new SmallEnum[3];
            enumSame[0] = SmallEnum.First;
            enumSame[1] = SmallEnum.Second;
            enumSame[2] = SmallEnum.Third;

            SmallEnum[] enumDifferent = new SmallEnum[3];
            enumDifferent[0] = SmallEnum.First;
            enumDifferent[1] = SmallEnum.Third;
            enumDifferent[2] = SmallEnum.Third;

            if (!((ReadOnlySpan<SmallEnum>)enumLeft).SequenceEqual((ReadOnlySpan<SmallEnum>)enumSame))
            {
                return 9;
            }

            if (((ReadOnlySpan<SmallEnum>)enumLeft).SequenceEqual((ReadOnlySpan<SmallEnum>)enumDifferent))
            {
                return 10;
            }

            return 0;
        }
    }
}
