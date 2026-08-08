namespace WoofWare.PawPrint.Test

open System
open NUnit.Framework

/// Tests of the cross-assembly harness's *oracle*, rather than of any particular IL construct.
///
/// The harness ran its guests inside the test host's own process, which meant a guest could do
/// things to that process which the harness then could not report on: `Environment.Exit` took the
/// test runner down with it, and an escaped exception arrived as a `TargetInvocationException`
/// whose own message says nothing about what went wrong. Each test here is a guest doing one of
/// those things across an assembly boundary, so it also demonstrates that the sibling assembly
/// still resolves now that the guest is a child process rather than an `AssemblyLoadContext`.
[<TestFixture>]
module TestCrossAssemblyOracle =

    [<Test>]
    let ``a cross-assembly guest may call Environment.Exit`` () : unit =
        // The exit code is computed in the library, so a run that reports 7 has necessarily
        // resolved the sibling assembly: out of process there is no `Resolving` hook doing it, only
        // the host's rule that with no deps.json every dll in the app directory is trusted.
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CrossAssemblyOracle.ExitLib"
                        []
                        [
                            """
namespace CrossAssemblyOracle.ExitLib;

public static class Codes
{
    public static int Chosen => 7;
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyOracle.ExitEntry"
                        [ "CrossAssemblyOracle.ExitLib" ]
                        [
                            """
using System;
using CrossAssemblyOracle.ExitLib;

class Program
{
    static int Main(string[] args)
    {
        Environment.Exit(Codes.Chosen);
        // Unreachable. Returning a different code means Exit failed to terminate the process,
        // which would otherwise look like a passing test on the wrong path.
        return 1;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyOracle.ExitEntry"
            ExpectedReturnCode = 7
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``a cross-assembly guest's escaped exception is reported, naming the exception`` () : unit =
        // In-process this surfaced as a `TargetInvocationException` — "Exception has been thrown by
        // the target of an invocation" — which names neither the guest's exception type nor its
        // message. Out of process the payload is the runtime's own stderr report, so assert that
        // the type defined in the *library* reaches the failure message.
        let case =
            {
                Assemblies =
                    [
                        CrossAssemblySpec.library
                            "CrossAssemblyOracle.ThrowLib"
                            []
                            [
                                """
namespace CrossAssemblyOracle.ThrowLib;

using System;

public sealed class SiblingAssemblyFailure : Exception
{
    public SiblingAssemblyFailure(string message) : base(message)
    {
    }
}
"""
                            ]
                        CrossAssemblySpec.entryPoint
                            "CrossAssemblyOracle.ThrowEntry"
                            [ "CrossAssemblyOracle.ThrowLib" ]
                            [
                                """
using CrossAssemblyOracle.ThrowLib;

class Program
{
    static int Main(string[] args)
    {
        throw new SiblingAssemblyFailure("thrown across the assembly boundary");
    }
}
"""
                            ]
                    ]
                EntryAssemblyName = "CrossAssemblyOracle.ThrowEntry"
                // Not reached: the guest never returns. The harness fails on the real runtime's
                // report before any comparison happens.
                ExpectedReturnCode = 0
            }

        let e = Assert.Throws (fun () -> CrossAssemblyHarness.runTest case)

        // A `TargetInvocationException` from the old in-process path satisfies "something threw",
        // so the discrimination has to be on the message: only the runtime's own report names the
        // guest's exception type and text.
        if not (e.Message.Contains "SiblingAssemblyFailure") then
            failwith $"expected the failure to name the guest's exception type, got: %s{e.Message}"

        if not (e.Message.Contains "thrown across the assembly boundary") then
            failwith $"expected the failure to carry the guest's exception message, got: %s{e.Message}"

    [<Test>]
    let ``an expected return code a process cannot carry is rejected up front`` () : unit =
        // The oracle is a real process, so on Unix it reports 8 bits; PawPrint reads a full int32
        // off the evaluation stack. A test declaring 256 would fail with "expected 256, got 0",
        // which reads as an interpreter bug rather than as an unrepresentable expectation.
        let case =
            {
                Assemblies =
                    [
                        CrossAssemblySpec.entryPoint
                            "CrossAssemblyOracle.WideEntry"
                            []
                            [
                                """
class Program
{
    static int Main(string[] args) => 256;
}
"""
                            ]
                    ]
                EntryAssemblyName = "CrossAssemblyOracle.WideEntry"
                ExpectedReturnCode = 256
            }

        let e = Assert.Throws (fun () -> CrossAssemblyHarness.runTest case)

        if not (e.Message.Contains "256" && e.Message.Contains "0-255") then
            failwith $"expected a diagnostic naming the unrepresentable code and the range, got: %s{e.Message}"
