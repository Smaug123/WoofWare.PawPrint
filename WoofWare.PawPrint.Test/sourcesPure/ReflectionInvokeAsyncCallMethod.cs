using System;
using System.Reflection;
using System.Threading.Tasks;

// On .NET 11, `MethodBase.Invoke` refuses a method the runtime calls with runtime async's calling
// convention (`RuntimeMethodHandle.IsAsyncMethod`), before it counts the arguments. CoreLib's
// `AsyncHelpers.Await` carries `MethodImplAttributes.Async` and returns no promise type, so it is
// such a method; a method compiled with runtime async that returns a `Task` or `ValueTask` is not,
// since the runtime calls it as an ordinary method and calls its async variant only from runtime
// async code. Each method is invoked with no arguments, so a method reflection agrees to call
// fails the argument count and its body never runs.
//
// .NET 10's reflection asks no such question, so it counts `Await`'s arguments and exits 10.
public class Program
{
    public static int Main (string[] args)
    {
        MethodInfo awaitTask = typeof (object).Assembly
            .GetType ("System.Runtime.CompilerServices.AsyncHelpers", true)
            .GetMethod ("Await", BindingFlags.Public | BindingFlags.Static, new[] { typeof (Task) });

        try
        {
            awaitTask.Invoke (null, new object[0]);
            return 1;
        }
        catch (NotSupportedException)
        {
        }
        catch (TargetParameterCountException)
        {
            return 10;
        }

        MethodInfo readToFill = typeof (System.Net.Http.HttpClient).Assembly
            .GetType ("System.Net.Http.SocksHelper", true)
            .GetMethod ("ReadToFillAsync", BindingFlags.NonPublic | BindingFlags.Static);

        if ((readToFill.MethodImplementationFlags & (MethodImplAttributes) 0x2000) == 0)
            return 2;

        try
        {
            readToFill.Invoke (null, new object[0]);
            return 3;
        }
        catch (TargetParameterCountException)
        {
        }
        catch (NotSupportedException)
        {
            return 4;
        }

        return 0;
    }
}
