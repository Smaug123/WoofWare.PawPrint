using System;

namespace SpanFillTest
{
    struct Pair
    {
        public int A;
        public long B;
    }

    class Program
    {
        // SpanHelpers.Fill's scalar path is an unrolled loop that writes 8, then 4, then 2,
        // then 1 element at a time, so the interesting lengths are the ones that exercise
        // each tail combination.
        static readonly int[] Lengths = new int[] { 0, 1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17, 23 };

        static int FillIntsOfEveryLength()
        {
            for (int idx = 0; idx < Lengths.Length; idx++)
            {
                int length = Lengths[idx];
                int[] backing = new int[length + 2];

                // Sentinels either side, so an over-long fill is caught.
                for (int i = 0; i < backing.Length; i++)
                {
                    backing[i] = -1;
                }

                Span<int> target = ((Span<int>)backing).Slice(1, length);
                target.Fill(0x5A5A5A);

                if (backing[0] != -1)
                {
                    return 1;
                }

                if (backing[backing.Length - 1] != -1)
                {
                    return 2;
                }

                for (int i = 0; i < length; i++)
                {
                    if (backing[i + 1] != 0x5A5A5A)
                    {
                        return 3;
                    }
                }
            }

            return 0;
        }

        static int Main(string[] args)
        {
            int lengthSweep = FillIntsOfEveryLength();

            if (lengthSweep != 0)
            {
                return lengthSweep;
            }

            // Filling a whole span, then refilling with a different value.
            int[] ints = new int[5];
            Span<int> intSpan = ints;
            intSpan.Fill(7);

            for (int i = 0; i < ints.Length; i++)
            {
                if (ints[i] != 7)
                {
                    return 4;
                }
            }

            intSpan.Fill(-9);

            for (int i = 0; i < ints.Length; i++)
            {
                if (ints[i] != -9)
                {
                    return 5;
                }
            }

            // A zero-length fill must leave the backing store alone.
            intSpan.Slice(2, 0).Fill(123);

            for (int i = 0; i < ints.Length; i++)
            {
                if (ints[i] != -9)
                {
                    return 6;
                }
            }

            // One byte per element: the smallest element stride.
            byte[] bytes = new byte[9];
            ((Span<byte>)bytes).Fill(0xAB);

            for (int i = 0; i < bytes.Length; i++)
            {
                if (bytes[i] != 0xAB)
                {
                    return 7;
                }
            }

            // Eight bytes per element, with a value that needs all of them.
            long[] longs = new long[6];
            ((Span<long>)longs).Fill(-(1L << 40));

            for (int i = 0; i < longs.Length; i++)
            {
                if (longs[i] != -(1L << 40))
                {
                    return 8;
                }
            }

            char[] chars = new char[4];
            ((Span<char>)chars).Fill('☃');

            for (int i = 0; i < chars.Length; i++)
            {
                if (chars[i] != '☃')
                {
                    return 9;
                }
            }

            // An enum element type is deliberately not covered here: writing one through a
            // span byref leaves an EnumLike ValueType in a cell whose storage form is the
            // underlying primitive, so a subsequent ldelem fails. That is not specific to
            // Fill — the natively-implemented Span<T>.Clear corrupts an enum array the same
            // way — and is recorded in sourcesPure/SpanClearEnumArray.cs.

            // A multi-field struct: still no references, so it takes the same path,
            // but the element stride is not a primitive width.
            Pair[] pairs = new Pair[3];
            Pair pair = new Pair();
            pair.A = 11;
            pair.B = 1L << 33;
            ((Span<Pair>)pairs).Fill(pair);

            for (int i = 0; i < pairs.Length; i++)
            {
                if (pairs[i].A != 11 || pairs[i].B != (1L << 33))
                {
                    return 10;
                }
            }

            // A reference element type leaves via the IsReferenceOrContainsReferences
            // check at the very top of SpanHelpers.Fill, rather than the vectorisation
            // checks below it.
            string[] strings = new string[4];
            ((Span<string>)strings).Fill("hello");

            for (int i = 0; i < strings.Length; i++)
            {
                if (strings[i] != "hello")
                {
                    return 11;
                }
            }

            ((Span<string>)strings).Fill(null);

            for (int i = 0; i < strings.Length; i++)
            {
                if (strings[i] != null)
                {
                    return 12;
                }
            }

            return 0;
        }
    }
}
