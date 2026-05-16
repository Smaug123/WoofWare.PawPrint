using System;
using System.Reflection;

public class Program
{
    class BoomCctor
    {
        static BoomCctor()
        {
            throw new InvalidOperationException("cctor boom");
        }

        public BoomCctor() { }
    }

    public static int Main(string[] args)
    {
        // CoreCLR wraps any exception escaping the T..ctor or T..cctor path inside
        // RuntimeType.CreateInstanceOfT in a TargetInvocationException. The cctor case
        // produces TargetInvocationException whose InnerException is the
        // TypeInitializationException (which itself wraps the original InvalidOperationException).
        try
        {
            var x = Activator.CreateInstance<BoomCctor>();
            return 1;
        }
        catch (TargetInvocationException tie)
        {
            if (tie.InnerException is TypeInitializationException typeInit)
            {
                if (typeInit.InnerException is InvalidOperationException)
                {
                    return 0;
                }
                return 2;
            }
            return 3;
        }
        catch (TypeInitializationException)
        {
            // If we see a raw TypeInitializationException here, the wrap didn't happen.
            return 4;
        }
        catch (Exception)
        {
            return 5;
        }
    }
}
