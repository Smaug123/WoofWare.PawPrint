using System;

/// <summary>
/// Verifies that the CLR caches the outer TypeInitializationException
/// for a failed .cctor so that repeated accesses rethrow the *same*
/// instance (both the outer TIE and its InnerException).
/// On real .NET 9, ReferenceEquals(ex1, ex2) and
/// ReferenceEquals(ex1.InnerException, ex2.InnerException) are both true.
/// </summary>
class ThrowingStaticInit3
{
    public static int Value = 1;

    static ThrowingStaticInit3()
    {
        throw new InvalidOperationException("cctor boom");
    }
}

class Program
{
    static int Main(string[] args)
    {
        TypeInitializationException first = null;
        TypeInitializationException second = null;

        try
        {
            int v = ThrowingStaticInit3.Value;
            return 1; // should not reach
        }
        catch (TypeInitializationException ex)
        {
            first = ex;
        }

        try
        {
            int v = ThrowingStaticInit3.Value;
            return 2; // should not reach
        }
        catch (TypeInitializationException ex)
        {
            second = ex;
        }

        if (first == null || second == null)
        {
            return 3;
        }

        // The CLR caches the outer TIE: same instance on repeated access.
        if (!ReferenceEquals(first, second))
        {
            return 4;
        }

        // The inner exception is also the same instance.
        if (!ReferenceEquals(first.InnerException, second.InnerException))
        {
            return 5;
        }

        // Sanity: the inner exception is the original InvalidOperationException.
        if (!(first.InnerException is InvalidOperationException))
        {
            return 6;
        }

        return 0;
    }
}
