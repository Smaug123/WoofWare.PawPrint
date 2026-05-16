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
        // First touch: triggers the failing cctor. We swallow the wrap so the runtime
        // caches the TypeInitializationException for subsequent accesses.
        try
        {
            var _ = Activator.CreateInstance<BoomCctor>();
            return 1;
        }
        catch (TargetInvocationException)
        {
            // Expected.
        }
        catch (Exception)
        {
            return 2;
        }

        // Second touch: the cctor is NOT re-run. CoreCLR still rewraps the cached
        // TypeInitializationException in a fresh TargetInvocationException because the
        // wrap fires inside CreateInstanceOfT independently of the cache state.
        try
        {
            var _ = Activator.CreateInstance<BoomCctor>();
            return 3;
        }
        catch (TargetInvocationException tie)
        {
            if (tie.InnerException is TypeInitializationException typeInit)
            {
                if (typeInit.InnerException is InvalidOperationException)
                {
                    return 0;
                }
                return 4;
            }
            return 5;
        }
        catch (TypeInitializationException)
        {
            return 6;
        }
        catch (Exception)
        {
            return 7;
        }
    }
}
