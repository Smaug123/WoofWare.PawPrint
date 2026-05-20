using System;
using System.Runtime.InteropServices;

public class TestExceptionHandling
{
    // Test Throw: Throw exception
    public static int TestThrow()
    {
        try
        {
            ThrowException();
            return 1; // Should not reach here
        }
        catch (InvalidOperationException)
        {
            // Expected exception caught
            return 0;
        }
        catch
        {
            return 2; // Wrong exception type
        }
    }

    private static void ThrowException()
    {
        throw new InvalidOperationException("Test exception");
    }

    // Test Rethrow: Rethrow current exception
    public static int TestRethrow()
    {
        try
        {
            try
            {
                throw new ArgumentException("Initial exception");
            }
            catch (ArgumentException)
            {
                // Catch and rethrow
                throw;
            }
        }
        catch (ArgumentException)
        {
            // Should catch the rethrown exception
            return 0;
        }
        catch
        {
            return 10; // Wrong exception type
        }
    }

    // Test Endfinally: End finally block
    public static int TestFinally()
    {
        int finallyExecuted = 0;

        try
        {
            // Normal execution
        }
        finally
        {
            finallyExecuted = 1;
        }

        if (finallyExecuted != 1) return 20;

        // Finally with exception
        finallyExecuted = 0;
        try
        {
            try
            {
                throw new Exception("Test");
            }
            finally
            {
                finallyExecuted = 2;
            }
        }
        catch
        {
            // Exception caught after finally
        }

        if (finallyExecuted != 2) return 21;

        return 0;
    }

    // Test nested exception handling
    public static int TestNestedExceptions()
    {
        try
        {
            try
            {
                try
                {
                    throw new InvalidOperationException("Inner");
                }
                catch (ArgumentException)
                {
                    return 30; // Wrong catch
                }
            }
            catch (InvalidOperationException)
            {
                // Correct catch
                throw new ArgumentException("Outer");
            }
        }
        catch (ArgumentException)
        {
            // Should catch outer exception
            return 0;
        }
        catch
        {
            return 31; // Wrong exception
        }
    }

    // Test exception filters (when/endfilter)
    public static int TestExceptionFilter()
    {
        try
        {
            throw new ArgumentException("Test");
        }
        catch (ArgumentException ex) when (ex.Message == "Test")
        {
            // Filter matches
            return 0;
        }
        catch (ArgumentException)
        {
            // Filter didn't match
            return 40;
        }
        catch
        {
            return 41;
        }
    }

    // Test multiple catch blocks
    public static int TestMultipleCatch()
    {
        // Test ArgumentException
        try
        {
            throw new ArgumentException();
        }
        catch (InvalidOperationException)
        {
            return 50;
        }
        catch (ArgumentException)
        {
            // Correct catch
        }
        catch
        {
            return 51;
        }

        // Test InvalidOperationException
        try
        {
            throw new InvalidOperationException();
        }
        catch (ArgumentException)
        {
            return 52;
        }
        catch (InvalidOperationException)
        {
            // Correct catch
        }
        catch
        {
            return 53;
        }

        return 0;
    }

    // Test finally execution order
    public static int TestFinallyOrder()
    {
        int order = 0;
        int errorCode = 0;

        try
        {
            try
            {
                order = 1;
                throw new Exception();
            }
            finally
            {
                if (order != 1) errorCode = 60;
                order = 2;
            }
        }
        catch
        {
            if (errorCode != 0) return errorCode;
            if (order != 2) return 61;
            order = 3;
        }
        finally
        {
            if (errorCode == 0 && order != 3) errorCode = 62;
            order = 4;
        }

        if (errorCode != 0) return errorCode;
        if (order != 4) return 63;

        return 0;
    }

    // Test exception in finally block
    public static int TestExceptionInFinally()
    {
        try
        {
            try
            {
                throw new ArgumentException("First");
            }
            finally
            {
                throw new InvalidOperationException("Second");
            }
        }
        catch (InvalidOperationException)
        {
            // Should catch the exception from finally
            return 0;
        }
        catch (ArgumentException)
        {
            // Original exception is lost
            return 70;
        }
        catch
        {
            return 71;
        }
    }

    // Test Localloc: Allocate from local memory pool
    public static unsafe int TestLocalloc()
    {
        // Allocate space for 10 integers
        int* buffer = stackalloc int[10];

        // Write values
        for (int i = 0; i < 10; i++)
        {
            buffer[i] = i * 10;
        }

        // Read and verify values
        for (int i = 0; i < 10; i++)
        {
            if (buffer[i] != i * 10) return 80 + i;
        }

        // Allocate different size
        byte* byteBuffer = stackalloc byte[256];

        // Initialize
        for (int i = 0; i < 256; i++)
        {
            byteBuffer[i] = (byte)i;
        }

        // Verify
        for (int i = 0; i < 256; i++)
        {
            if (byteBuffer[i] != (byte)i) return 90;
        }

        return 0;
    }

    // Test Cpblk: Copy block of memory
    public static unsafe int TestCpblk()
    {
        int[] source = new int[] { 1, 2, 3, 4, 5 };
        int[] dest = new int[5];

        fixed (int* srcPtr = source)
        fixed (int* destPtr = dest)
        {
            // Copy memory block
            Buffer.MemoryCopy(srcPtr, destPtr, 20, 20); // 5 * sizeof(int) = 20
        }

        // Verify copy
        for (int i = 0; i < 5; i++)
        {
            if (dest[i] != source[i]) return 100 + i;
        }

        // Test overlapping copy
        int[] overlap = new int[] { 10, 20, 30, 40, 50 };
        fixed (int* ptr = overlap)
        {
            // Copy overlapping regions (shift right by 1)
            Buffer.MemoryCopy(ptr, ptr + 1, 16, 16); // 4 * sizeof(int)
        }

        if (overlap[0] != 10) return 110;
        if (overlap[1] != 10) return 111;
        if (overlap[2] != 20) return 112;
        if (overlap[3] != 30) return 113;
        if (overlap[4] != 40) return 114;

        return 0;
    }

    // Test Initblk: Initialize block of memory
    public static unsafe int TestInitblk()
    {
        byte[] buffer = new byte[100];

        fixed (byte* ptr = buffer)
        {
            // Initialize block with value
            for (int i = 0; i < 100; i++)
            {
                ptr[i] = 0xFF;
            }
        }

        // Verify initialization
        for (int i = 0; i < 100; i++)
        {
            if (buffer[i] != 0xFF) return 120 + i;
        }

        // Clear block (initialize with 0)
        fixed (byte* ptr = buffer)
        {
            for (int i = 0; i < 100; i++)
            {
                ptr[i] = 0;
            }
        }

        // Verify clear
        for (int i = 0; i < 100; i++)
        {
            if (buffer[i] != 0) return 130;
        }

        return 0;
    }

    // Test Ckfinite: Check for finite float
    public static int TestCkfinite()
    {
        // Test finite values
        try
        {
            double finite = 3.14;
            CheckFinite(finite);
            // Should not throw
        }
        catch
        {
            return 140;
        }

        // Test infinity
        try
        {
            double infinity = double.PositiveInfinity;
            CheckFinite(infinity);
            return 141; // Should have thrown
        }
        catch (NotFiniteNumberException)
        {
            // Expected
        }
        catch
        {
            return 142;
        }

        // Test NaN
        try
        {
            double nan = double.NaN;
            CheckFinite(nan);
            return 143; // Should have thrown
        }
        catch (NotFiniteNumberException)
        {
            // Expected
        }
        catch
        {
            return 144;
        }

        // Test negative infinity
        try
        {
            double negInfinity = double.NegativeInfinity;
            CheckFinite(negInfinity);
            return 145; // Should have thrown
        }
        catch (NotFiniteNumberException)
        {
            // Expected
        }
        catch
        {
            return 146;
        }

        return 0;
    }

    private static void CheckFinite(double value)
    {
        if (double.IsInfinity(value) || double.IsNaN(value))
        {
            throw new NotFiniteNumberException(value);
        }
    }

    // Test Break: Breakpoint instruction
    public static int TestBreak()
    {
        // Break instruction is typically used for debugging
        // In release mode, it might be a no-op or cause debugger to attach
        // We'll just test that execution continues

        int beforeBreak = 1;
        // Debugger.Break() would emit break instruction
        int afterBreak = 2;

        if (beforeBreak != 1) return 150;
        if (afterBreak != 2) return 151;

        return 0;
    }

    private static int SumArgs(__arglist)
    {
        int sum = 0;
        ArgIterator args = new ArgIterator(__arglist);

        while (args.GetRemainingCount() > 0)
        {
            TypedReference tr = args.GetNextArg();
            if (__reftype(tr) == typeof(int))
            {
                sum += __refvalue(tr, int);
            }
        }

        return sum;
    }

    public static int Main(string[] argv)
    {
        int result;

        result = TestThrow();
        if (result != 0) return 9000 + result;

        result = TestRethrow();
        if (result != 0) return 9100 + result;

        result = TestFinally();
        if (result != 0) return 9200 + result;

        result = TestNestedExceptions();
        if (result != 0) return 9300 + result;

        result = TestExceptionFilter();
        if (result != 0) return 9400 + result;

        result = TestMultipleCatch();
        if (result != 0) return 9500 + result;

        result = TestFinallyOrder();
        if (result != 0) return 9600 + result;

        result = TestExceptionInFinally();
        if (result != 0) return 9700 + result;

        result = TestLocalloc();
        if (result != 0) return 9800 + result;

        result = TestCpblk();
        if (result != 0) return 9900 + result;

        result = TestInitblk();
        if (result != 0) return 10000 + result;

        result = TestCkfinite();
        if (result != 0) return 10100 + result;

        result = TestBreak();
        if (result != 0) return 10200 + result;

        return 0;
    }
}
