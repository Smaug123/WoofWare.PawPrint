using System;
using System.Reflection;

public class Program
{
    class ThrowingCtor
    {
        public ThrowingCtor()
        {
            throw new InvalidOperationException("boom");
        }
    }

    public static int Main(string[] args)
    {
        try
        {
            var x = Activator.CreateInstance<ThrowingCtor>();
            // Ctor must have thrown; reaching here is itself a failure.
            return 1;
        }
        catch (TargetInvocationException ex)
        {
            // CoreCLR's RuntimeType.CreateInstanceOfT wraps the original
            // (RuntimeType.CoreCLR.cs:4045-4048). Verify we got the wrapper
            // with the correct inner exception.
            if (ex.InnerException is InvalidOperationException ioe && ioe.Message == "boom")
            {
                return 0;
            }
            return 2;
        }
        catch (InvalidOperationException)
        {
            // PawPrint currently lets the raw constructor exception propagate
            // unwrapped, so this is the branch we hit today.
            return 3;
        }
        catch (Exception)
        {
            return 4;
        }
    }
}
