using System;

public class Int128NarrowingTests
{
    // `Int128` carries a *type-level* [Intrinsic], so every member of it reaches PawPrint's
    // intrinsic dispatcher. The allowlisted members are `.ctor(ulong, ulong)`, `op_Equality`,
    // `op_Inequality`, `get_MinValue`, `get_MaxValue`, the widening `op_Implicit` overloads,
    // `op_Addition`, `op_LessThan`, `op_GreaterThan`, and now the narrowing `op_Explicit` *to
    // Int64 only*. So this file may cast an `Int128` to `long` but to nothing else, and may not
    // spell `<=` or `>=` between two `Int128`s.
    //
    // `op_Explicit(Int128) -> long` is `ldarg.0; ldfld _lower; ret`: it returns the low half
    // reinterpreted as signed and never looks at the high half. That makes it unchecked by
    // construction -- there is no branch in which it could throw -- and it is the reason a value
    // outside `long`'s range narrows to its low 64 bits rather than faulting. C#'s `checked` cast
    // calls `op_CheckedExplicit`, a different method that is not allowlisted and does not appear
    // here.

    public static int TestSmallValuesRoundTrip()
    {
        // For anything that fits in a long, widening and then narrowing is the identity.
        long[] values = new long[]
        {
            0L, 1L, -1L, 2L, -2L, 127L, -128L, 1000000L, -1000000L,
            long.MaxValue, long.MinValue, long.MaxValue - 1L, long.MinValue + 1L,
        };

        for (int i = 0; i < values.Length; i++)
        {
            if ((long)(Int128)values[i] != values[i]) return 1;
        }

        return 0;
    }

    public static int TestReturnsTheLowHalfReinterpreted()
    {
        // The body is a field read, so the answer is the low half's bits read as a signed long --
        // whatever the high half holds.
        if ((long)new Int128(0ul, 0ul) != 0L) return 1;
        if ((long)new Int128(0ul, 1ul) != 1L) return 2;
        // High half nonzero, low half zero: the high half is discarded entirely.
        if ((long)new Int128(0xDEADBEEFul, 0ul) != 0L) return 3;
        if ((long)new Int128(ulong.MaxValue, 0ul) != 0L) return 4;
        // The low half's top bit is the *sign* of the result, not part of its magnitude.
        if ((long)new Int128(0ul, 0x8000000000000000ul) != long.MinValue) return 5;
        if ((long)new Int128(0ul, ulong.MaxValue) != -1L) return 6;
        if ((long)new Int128(0ul, 0x7FFFFFFFFFFFFFFFul) != long.MaxValue) return 7;
        // The same low halves with a different high half must give the same answers, which is what
        // pins that the high half is not consulted at all.
        if ((long)new Int128(0x1234ul, 0x8000000000000000ul) != long.MinValue) return 8;
        if ((long)new Int128(ulong.MaxValue, ulong.MaxValue) != -1L) return 9;
        if ((long)new Int128(0x5555ul, 0x7FFFFFFFFFFFFFFFul) != long.MaxValue) return 10;
        // A value whose two halves differ, so a conversion that read the *wrong* field fails.
        if ((long)new Int128(0x1111111111111111ul, 0x2222222222222222ul) != 0x2222222222222222L)
            return 11;
        return 0;
    }

    public static int TestOutOfRangeTruncatesRatherThanFaulting()
    {
        // `op_Explicit` is the unchecked operator: values far outside `long`'s range narrow to
        // their low 64 bits instead of throwing. `Int128.MaxValue` is 2^127-1, whose low half is
        // all ones, so it narrows to -1; `Int128.MinValue` is 2^127, whose low half is zero.
        if ((long)Int128.MaxValue != -1L) return 1;
        if ((long)Int128.MinValue != 0L) return 2;
        // long.MaxValue + 1, as an Int128, is not representable as a long; it narrows to
        // long.MinValue, which is the low half's bits read signed.
        Int128 justPastLongMax = (Int128)long.MaxValue + (Int128)1L;
        if ((long)justPastLongMax != long.MinValue) return 3;
        // ... and one more takes it to long.MinValue + 1, still by truncation.
        if ((long)(justPastLongMax + (Int128)1L) != long.MinValue + 1L) return 4;
        return 0;
    }

    public static int TestAgreesWithTheOrderingAndAddition()
    {
        // The three allowlisted operations have to tell one story. Adding 1 to a value whose low
        // half is all ones carries into the high half, so the narrowed result drops from -1 to 0
        // even though the 128-bit value went *up*.
        Int128 lowAllOnes = new Int128(0ul, ulong.MaxValue);
        Int128 carried = lowAllOnes + (Int128)1L;

        if (!(carried > lowAllOnes)) return 1;
        if ((long)lowAllOnes != -1L) return 2;
        if ((long)carried != 0L) return 3;
        return 0;
    }
}

public class TimeSpanFromMillisecondsTests
{
    // The reason the four Int128 members above were allowlisted at all.
    // `TimeSpan.FromMilliseconds(long milliseconds, long microseconds)` computes
    //   Int128 totalMicroseconds = Math.BigMul(milliseconds, 1000) + microseconds;
    // and then `FromMicroseconds(Int128)` bound-checks it with `>` and `<` before narrowing with
    // `(long)` and scaling to ticks. So one call exercises op_Implicit, op_Addition,
    // op_GreaterThan, op_LessThan and op_Explicit together, on values a guest would really produce.

    public static int TestKnownValues()
    {
        // 1 ms = 10_000 ticks; 1 microsecond = 10 ticks.
        if (TimeSpan.FromMilliseconds(0L, 0L).Ticks != 0L) return 1;
        if (TimeSpan.FromMilliseconds(1L, 0L).Ticks != 10000L) return 2;
        if (TimeSpan.FromMilliseconds(0L, 1L).Ticks != 10L) return 3;
        if (TimeSpan.FromMilliseconds(3L, 4L).Ticks != 30040L) return 4;
        // Negative values on both sides, and the two sides cancelling.
        if (TimeSpan.FromMilliseconds(-1L, 0L).Ticks != -10000L) return 5;
        if (TimeSpan.FromMilliseconds(0L, -1L).Ticks != -10L) return 6;
        if (TimeSpan.FromMilliseconds(1L, -1000L).Ticks != 0L) return 7;
        if (TimeSpan.FromMilliseconds(-1L, 1000L).Ticks != 0L) return 8;
        // Large enough that the Int128 product genuinely needs more than 64 bits of headroom
        // during the multiply, which is why CoreLib reaches for Int128 here at all.
        if (TimeSpan.FromMilliseconds(1000000000L, 0L).Ticks != 10000000000000L) return 9;
        return 0;
    }

    public static int TestOutOfRangeThrows()
    {
        // The bound check is what `op_GreaterThan`/`op_LessThan` are for: a value past
        // TimeSpan.MaxValue must raise rather than silently narrowing. This also pins that the
        // comparison is reached at all -- if it answered "in range" for everything, this would
        // return a TimeSpan instead of throwing.
        bool threw = false;
        try
        {
            TimeSpan.FromMilliseconds(long.MaxValue, long.MaxValue);
        }
        catch (ArgumentOutOfRangeException)
        {
            threw = true;
        }

        if (!threw) return 1;

        threw = false;
        try
        {
            TimeSpan.FromMilliseconds(long.MinValue, long.MinValue);
        }
        catch (ArgumentOutOfRangeException)
        {
            threw = true;
        }

        if (!threw) return 2;
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Failure codes stay inside a single byte: a process exit code is truncated to its
        // low 8 bits, and a code congruent to 0 would be indistinguishable from success.
        int result;

        result = Int128NarrowingTests.TestSmallValuesRoundTrip();
        if (result != 0) return result;

        result = Int128NarrowingTests.TestReturnsTheLowHalfReinterpreted();
        if (result != 0) return 10 + result;

        result = Int128NarrowingTests.TestOutOfRangeTruncatesRatherThanFaulting();
        if (result != 0) return 30 + result;

        result = Int128NarrowingTests.TestAgreesWithTheOrderingAndAddition();
        if (result != 0) return 40 + result;

        result = TimeSpanFromMillisecondsTests.TestKnownValues();
        if (result != 0) return 50 + result;

        result = TimeSpanFromMillisecondsTests.TestOutOfRangeThrows();
        if (result != 0) return 70 + result;

        return 0;
    }
}
