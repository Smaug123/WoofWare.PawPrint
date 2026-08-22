namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// <summary>
/// What a stack-crawling QCall answers when the crawl has to cross an assembly boundary.
/// </summary>
/// <remarks>
/// The single-assembly guests in <c>sourcesPure</c> cannot distinguish "the caller's assembly" from
/// "the entry assembly", so on their own they would be satisfied by a
/// <c>AssemblyNative_GetExecutingAssembly</c> that ignored the stack entirely and returned the entry
/// assembly. These cases put the call one assembly away from the entry point, where the two answers
/// differ.
///
/// Both rely on the callee keeping a frame under a JIT: <c>Assembly.GetExecutingAssembly</c> and
/// <c>GetCallingAssembly</c> carry <c>[DynamicSecurityMethod]</c>, which disables inlining of the
/// method that calls them, so the library's wrapper survives on the real runtime that is this
/// test's oracle.
/// </remarks>
[<TestFixture>]
module TestCrossAssemblyExecutingAssembly =

    [<Test>]
    let ``GetExecutingAssembly names the assembly of the method that called it`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "ExecutingAssemblyCross.Lib"
                        []
                        [
                            """
namespace ExecutingAssemblyCross;

using System.Reflection;

public static class Probe
{
    public static Assembly Executing()
    {
        return Assembly.GetExecutingAssembly();
    }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "ExecutingAssemblyCross.Entry"
                        [ "ExecutingAssemblyCross.Lib" ]
                        [
                            """
using System.Reflection;
using ExecutingAssemblyCross;

class Program
{
    static int Main(string[] argv)
    {
        Assembly fromLib = Probe.Executing();

        if (fromLib == null) return 1;

        // The crawl must answer the library, not this assembly: `Probe.Executing` is the frame
        // outside the one that declared the stack-crawl mark.
        if (!ReferenceEquals(fromLib, typeof(Probe).Assembly)) return 2;

        if (ReferenceEquals(fromLib, typeof(Program).Assembly)) return 3;

        // ... and this assembly is still what the crawl answers when *it* does the calling, so
        // the answer really does follow the caller rather than being fixed per run.
        if (!ReferenceEquals(Assembly.GetExecutingAssembly(), typeof(Program).Assembly)) return 4;

        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "ExecutingAssemblyCross.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``GetCallingAssembly steps one frame further out than GetExecutingAssembly`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CallingAssemblyCross.Lib"
                        []
                        [
                            """
namespace CallingAssemblyCross;

using System.Reflection;

public static class Probe
{
    public static Assembly Calling()
    {
        return Assembly.GetCallingAssembly();
    }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CallingAssemblyCross.Entry"
                        [ "CallingAssemblyCross.Lib" ]
                        [
                            """
using System.Reflection;
using CallingAssemblyCross;

class Program
{
    // Called directly from Main, with no frame in between: `GetCallingAssembly` is only reliable
    // one frame out, so an intermediate helper here would be at the mercy of the oracle's inliner.
    static int Main(string[] argv)
    {
        Assembly caller = Probe.Calling();

        if (caller == null) return 1;

        // The mark is `LookForMyCallersCaller`, so the answer skips `Probe.Calling` — the frame
        // `GetExecutingAssembly` would have named — and lands on this one.
        if (!ReferenceEquals(caller, typeof(Program).Assembly)) return 2;

        if (ReferenceEquals(caller, typeof(Probe).Assembly)) return 3;

        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CallingAssemblyCross.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest
