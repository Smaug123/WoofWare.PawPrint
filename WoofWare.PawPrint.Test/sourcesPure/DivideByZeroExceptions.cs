using System;

// `div`, `div.un`, `rem` and `rem.un` are the only IL arithmetic instructions that
// fault on operand *values* rather than on the result: a zero divisor raises
// DivideByZeroException, and the signed forms additionally raise OverflowException
// for MinValue op -1 (the one quotient that has no two's-complement representation).
// Every divisor here comes out of a [MethodImpl(NoInlining)] helper so Roslyn cannot
// constant-fold the instruction away; a literal `1 / 0` is a compile error anyway,
// but a `const` folded through a local would silently stop emitting the opcode.
public class DivideByZeroExceptions
{
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static int Zero() => 0;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static long ZeroL() => 0L;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static int MinusOne() => -1;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static long MinusOneL() => -1L;

    // `div`
    private static int SignedDivideByZero()
    {
        try
        {
            int q = 7 / Zero();
            return 1 + q - q; // unreachable; the arithmetic keeps `q` live
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    private static int SignedLongDivideByZero()
    {
        try
        {
            long q = 7L / ZeroL();
            return 2 + (int)(q - q);
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    // `div.un`
    private static int UnsignedDivideByZero()
    {
        try
        {
            uint q = 7u / (uint)Zero();
            return 3 + (int)(q - q);
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    private static int UnsignedLongDivideByZero()
    {
        try
        {
            ulong q = 7ul / (ulong)ZeroL();
            return 4 + (int)(q - q);
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    // `rem`
    private static int SignedRemainderByZero()
    {
        try
        {
            int r = 7 % Zero();
            return 5 + r - r;
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    private static int SignedLongRemainderByZero()
    {
        try
        {
            long r = 7L % ZeroL();
            return 6 + (int)(r - r);
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    // `rem.un`
    private static int UnsignedRemainderByZero()
    {
        try
        {
            uint r = 7u % (uint)Zero();
            return 7 + (int)(r - r);
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    private static int UnsignedLongRemainderByZero()
    {
        try
        {
            ulong r = 7ul % (ulong)ZeroL();
            return 8 + (int)(r - r);
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    // The other fault the signed forms carry. This is *not* DivideByZeroException,
    // and a fix that mapped every arithmetic fault onto the same guest exception
    // would pass everything above while failing here.
    private static int SignedDivideOverflow()
    {
        try
        {
            int q = int.MinValue / MinusOne();
            return 9 + q - q;
        }
        catch (OverflowException)
        {
            return 0;
        }
    }

    private static int SignedLongDivideOverflow()
    {
        try
        {
            long q = long.MinValue / MinusOneL();
            return 10 + (int)(q - q);
        }
        catch (OverflowException)
        {
            return 0;
        }
    }

    private static int SignedRemainderOverflow()
    {
        try
        {
            int r = int.MinValue % MinusOne();
            return 11 + r - r;
        }
        catch (OverflowException)
        {
            return 0;
        }
    }

    private static int SignedLongRemainderOverflow()
    {
        try
        {
            long r = long.MinValue % MinusOneL();
            return 12 + (int)(r - r);
        }
        catch (OverflowException)
        {
            return 0;
        }
    }

    // Floating point `div`/`rem` never fault: they are the control that says the
    // fix keys on the operand *type* and not merely on "the divisor was zero".
    private static int FloatDivideByZeroDoesNotThrow()
    {
        try
        {
            double q = 7.0 / (double)Zero();
            if (!double.IsPositiveInfinity(q)) return 13;

            double r = 7.0 % (double)Zero();
            if (!double.IsNaN(r)) return 14;

            return 0;
        }
        catch (ArithmeticException)
        {
            return 15;
        }
    }

    // A fault raised inside a `try` whose handler does not match must keep
    // unwinding rather than be swallowed by the first frame that has any handler.
    private static int DivideByZeroUnwindsPastNonMatchingHandler()
    {
        try
        {
            try
            {
                return 16 + 7 / Zero();
            }
            catch (InvalidOperationException)
            {
                return 17;
            }
        }
        catch (DivideByZeroException)
        {
            return 0;
        }
    }

    private static int DivideByZeroRunsFinally()
    {
        int ran = 0;

        try
        {
            try
            {
                return 18 + 7 / Zero();
            }
            finally
            {
                ran = 1;
            }
        }
        catch (DivideByZeroException)
        {
            return ran == 1 ? 0 : 19;
        }
    }

    // The exception the runtime manufactures must be a real, fully constructed
    // DivideByZeroException, not a bare allocation: the guest can read it.
    private static int DivideByZeroExceptionIsWellFormed()
    {
        try
        {
            return 20 + 7 / Zero();
        }
        catch (DivideByZeroException e)
        {
            if (e.Message == null) return 21;
            if (e.Message.Length == 0) return 22;
            if (!(e is ArithmeticException)) return 23;
            if (e.InnerException != null) return 24;
            return 0;
        }
    }

    // Execution must resume normally after the handler: dispatch has to leave the
    // evaluation stack of the resuming frame in a state the rest of the method can use.
    private static int ExecutionContinuesAfterCatch()
    {
        int total = 0;

        for (int i = 0; i < 3; i++)
        {
            try
            {
                total += 100 / (i - 1 == 0 ? Zero() : 5);
            }
            catch (DivideByZeroException)
            {
                total += 1;
            }
        }

        // i=0: 100/5 = 20; i=1: throws, +1; i=2: 100/5 = 20.
        return total == 41 ? 0 : 25;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = SignedDivideByZero();
        if (result != 0) return result;

        result = SignedLongDivideByZero();
        if (result != 0) return result;

        result = UnsignedDivideByZero();
        if (result != 0) return result;

        result = UnsignedLongDivideByZero();
        if (result != 0) return result;

        result = SignedRemainderByZero();
        if (result != 0) return result;

        result = SignedLongRemainderByZero();
        if (result != 0) return result;

        result = UnsignedRemainderByZero();
        if (result != 0) return result;

        result = UnsignedLongRemainderByZero();
        if (result != 0) return result;

        result = SignedDivideOverflow();
        if (result != 0) return result;

        result = SignedLongDivideOverflow();
        if (result != 0) return result;

        result = SignedRemainderOverflow();
        if (result != 0) return result;

        result = SignedLongRemainderOverflow();
        if (result != 0) return result;

        result = FloatDivideByZeroDoesNotThrow();
        if (result != 0) return result;

        result = DivideByZeroUnwindsPastNonMatchingHandler();
        if (result != 0) return result;

        result = DivideByZeroRunsFinally();
        if (result != 0) return result;

        result = DivideByZeroExceptionIsWellFormed();
        if (result != 0) return result;

        result = ExecutionContinuesAfterCatch();
        if (result != 0) return result;

        return 0;
    }
}
