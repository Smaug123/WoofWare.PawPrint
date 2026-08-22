using System;
using System.Reflection;

// `Assembly.GetCallingAssembly()` asks the same QCall as `GetExecutingAssembly` for the caller's
// *caller*, by handing it a `LookForMyCallersCaller` stack-crawl mark instead of a
// `LookForMyCaller` one.
//
// Called straight from the entry point there is no caller's caller to find: the walk runs out of
// managed frames. CoreCLR's crawl keeps whichever candidate it last recorded rather than giving up,
// so the answer degrades to the caller — this guest's own assembly. (That fallback is exactly why
// `GetCallingAssembly` is documented as not guaranteed to be right.) The cross-assembly file pins
// the case where the walk does find a caller's caller.
public class Program
{
    public static int Main (string[] args)
    {
        Assembly calling = Assembly.GetCallingAssembly ();

        if (calling == null)
            return 1;

        if (!ReferenceEquals (calling, typeof (Program).Assembly))
            return 2;

        return 0;
    }
}
