using System;

/// <summary>
/// Regression test: when a call instruction is the last "real" instruction
/// before the leave that exits a try block, the caller's handler must still
/// be found even though the return-resume PC points at the leave (which is
/// the very last byte of the protected region).
///
/// We structure this so the try body is as short as possible:
///   try { Helper(); }          // call + leave
///   catch (Exception) { ... }
///
/// If the runtime uses the *resumed* PC (pointing at the leave or past it)
/// instead of the call-site PC for exception-handler lookup, the catch
/// will be missed and the exception will propagate unhandled.
/// </summary>
class Program
{
    static void AlwaysThrows()
    {
        throw new InvalidOperationException("boom");
    }

    // Use noinlining to guarantee the call instruction stays in the IL.
    [System.Runtime.CompilerServices.MethodImpl(
        System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    static int TryCatchCallAtBoundary()
    {
        try
        {
            AlwaysThrows();
        }
        catch (InvalidOperationException)
        {
            return 42;
        }

        return -1;
    }

    // Same test but with a finally block.
    static int flag = 0;

    [System.Runtime.CompilerServices.MethodImpl(
        System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    static int TryFinallyCallAtBoundary()
    {
        flag = 0;

        try
        {
            try
            {
                AlwaysThrows();
            }
            finally
            {
                flag = 99;
            }
        }
        catch (InvalidOperationException)
        {
            // The finally must have run before we get here.
        }

        return flag;
    }

    static int Main(string[] args)
    {
        if (TryCatchCallAtBoundary() != 42)
        {
            return 1;
        }

        if (TryFinallyCallAtBoundary() != 99)
        {
            return 2;
        }

        return 0;
    }
}
