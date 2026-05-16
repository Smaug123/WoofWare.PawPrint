using System;
using System.Reflection;

public class Program
{
    class CtorWithLocalCatch
    {
        public int Tag;

        public CtorWithLocalCatch()
        {
            try
            {
                throw new InvalidOperationException("local");
            }
            catch (InvalidOperationException)
            {
                Tag = 17;
            }
        }
    }

    public static int Main(string[] args)
    {
        try
        {
            var x = Activator.CreateInstance<CtorWithLocalCatch>();
            // A try/catch *inside* the ctor that handles the exception should not trigger
            // CreateInstanceOfT's TargetInvocationException wrap — control returns normally
            // and the object should be observable.
            return x.Tag == 17 ? 0 : 1;
        }
        catch (TargetInvocationException)
        {
            return 2;
        }
        catch (InvalidOperationException)
        {
            return 3;
        }
        catch (Exception)
        {
            return 4;
        }
    }
}
