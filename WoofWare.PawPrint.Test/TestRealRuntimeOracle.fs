namespace WoofWare.PawPrint.Test

open System
open System.Threading.Tasks
open NUnit.Framework
open FsUnitTyped

/// Tests for the differential oracle itself.
///
/// Every `sourcesPure` comparison test trusts `RealRuntime.executeWithRealRuntime` to report what
/// real .NET would do with the guest. That trust is only worth anything if the oracle is itself
/// tested, so these cases pin the CLR's actual process-termination semantics, as specified by
/// `RunMain`/`RunMainInternal` in the pinned runtime's `src/coreclr/vm/assembly.cpp`:
///
///   * the entry point may return `void`, `int32` or `unsigned int32`, and nothing else;
///   * if it returns void, the process exit code is the *latched* exit code, which the guest can
///     set via `Environment.ExitCode` and which defaults to 0;
///   * if it returns an integer, that value is latched after the call returns, and therefore
///     overrides any `Environment.ExitCode` the guest assigned.
///
/// `async Task Main` is not a separate case: Roslyn compiles it to a synthetic `<Main>` entry point
/// returning `System.Void` which blocks on the user's task, so it goes down the void path.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRealRuntimeOracle =

    let private guest (body : string) : byte[] =
        Roslyn.compile
            [
                $"""
using System;
using System.Threading;
using System.Threading.Tasks;

public static class Program
{{
{body}
}}
"""
            ]

    [<Test>]
    let ``a void entry point exits with code zero`` () : unit =
        let image = guest """    public static void Main() { }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 0)

    [<Test>]
    let ``a void entry point exits with the latched exit code`` () : unit =
        let image = guest """    public static void Main() { Environment.ExitCode = 7; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 7)

    [<Test>]
    let ``an async void entry point exits with the latched exit code`` () : unit =
        // Roslyn synthesises a void `<Main>` around this, so it exercises the void path.
        let image =
            guest """    public static async Task Main() { await Task.Yield(); Environment.ExitCode = 5; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 5)

    [<Test>]
    let ``an integer return overrides the latched exit code`` () : unit =
        // RunMainInternal latches the return value *after* the call, so the return wins.
        let image =
            guest """    public static int Main() { Environment.ExitCode = 7; return 3; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 3)

    [<Test>]
    let ``an async integer entry point exits with its return value`` () : unit =
        let image =
            guest """    public static async Task<int> Main() { await Task.Yield(); return 4; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 4)

    [<Test>]
    let ``command line arguments reach the guest`` () : unit =
        let image =
            guest
                """    public static int Main(string[] args) { return args.Length == 2 && args[0] == "alpha" && args[1] == "beta" ? 9 : 1; }"""

        RealRuntime.executeWithRealRuntime [| "alpha" ; "beta" |] image
        |> shouldEqual (RealRuntimeResult.NormalExit 9)

    [<Test>]
    let ``an unhandled exception is not reported as a normal exit`` () : unit =
        let image =
            guest """    public static int Main() { throw new InvalidOperationException("boom"); }"""

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.UnhandledException _ -> ()
        | other -> failwith $"expected an unhandled exception to be reported as such, got %O{other}"

    [<Test>]
    let ``an unhandled exception report names the exception`` () : unit =
        let image =
            guest """    public static int Main() { throw new InvalidOperationException("boom"); }"""

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.UnhandledException payload ->
            // Callers put this straight into failure messages, so it has to identify the fault.
            let detail = string payload

            if not (detail.Contains "InvalidOperationException") then
                failwith $"expected the report to name the exception type, got: %s{detail}"

            if not (detail.Contains "boom") then
                failwith $"expected the report to carry the exception message, got: %s{detail}"
        | other -> failwith $"expected an unhandled exception, got %O{other}"

    [<Test>]
    let ``an exit code of 134 is not mistaken for a crash`` () : unit =
        // A guest that merely *returns* 134 is indistinguishable from an aborted one by exit code
        // alone: the CLR aborts on an unhandled exception, and the shell reports 128 + SIGABRT.
        let image = guest """    public static int Main() { return 134; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 134)

    [<Test>]
    let ``a guest writing the crash banner is not mistaken for a crash`` () : unit =
        let image =
            guest
                """    public static int Main() { Console.Error.WriteLine("Unhandled exception. System.Exception: not really"); return 0; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 0)

    [<Test>]
    let ``Environment Exit reports the requested code`` () : unit =
        let image =
            guest """    public static int Main() { Environment.Exit(5); return 0; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 5)

    [<Test>]
    let ``FailFast is distinguished from an unhandled exception`` () : unit =
        // Both abort the process with the same exit code, so only the runtime's stderr banner tells
        // them apart. PawPrint models them as distinct `RunOutcome`s, so the oracle must too.
        let image =
            guest """    public static int Main() { Environment.FailFast("nope"); return 0; }"""

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.FailFast report ->
            if not (report.Contains "nope") then
                failwith $"expected the FailFast report to carry the message, got: %s{report}"
        | other -> failwith $"expected FailFast to be reported as such, got %O{other}"

    [<Test>]
    let ``a guest that never terminates is killed and reported`` () : unit =
        // The bound exists so that a runaway guest fails diagnosably instead of hanging CI with no
        // output at all. Tested with a short limit; the production limit is far longer than any
        // real guest, so no passing test's outcome depends on timing.
        let image =
            guest """    public static void Main() { Thread.Sleep(Timeout.Infinite); }"""

        let e =
            Assert.Throws (fun () -> RealRuntime.executeWithTimeout (TimeSpan.FromSeconds 2.0) [||] image |> ignore)

        if not (e.Message.Contains "did not terminate") then
            failwith $"expected a timeout diagnostic naming the failure, got: %s{e.Message}"

    [<Test>]
    let ``a guest that outfills the pipe buffer still terminates`` () : unit =
        // Guest output is captured rather than inherited, so it has to be drained *while* the guest
        // runs. Draining only after waiting for exit deadlocks as soon as the guest writes more
        // than a pipe buffer, and the guest never gets to exit. 100 lines of 900 characters
        // comfortably exceeds the 64 KiB buffer Linux and macOS both use, on each of the two
        // streams; keep it at least that large or this stops testing anything.
        let image =
            guest
                """    public static int Main() { var chunk = new string('x', 900); for (int i = 0; i < 100; i++) { Console.WriteLine(chunk); Console.Error.WriteLine(chunk); } return 6; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 6)

    [<Test>]
    [<NonParallelizable>]
    let ``a guest cannot change the host's exit code`` () : unit =
        // `Environment.ExitCode` is a direct accessor for CoreCLR's single process-global
        // `LatchedExitCode` (vm/ceemain.cpp). An in-process oracle therefore shares that storage
        // with the guest, and a guest assignment silently becomes the *test host's* exit code.
        let before = Environment.ExitCode

        let image = guest """    public static void Main() { Environment.ExitCode = 7; }"""

        RealRuntime.executeWithRealRuntime [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 7)

        Environment.ExitCode |> shouldEqual before

    [<Test>]
    let ``concurrent guests do not observe each other's exit code`` () : unit =
        // The pure-case fixtures are `[<Parallelizable(ParallelScope.All)>]`, so oracle calls
        // genuinely overlap. Each guest holds its exit code across a sleep so that any shared
        // latch between them is near-certain to be observed by the wrong reader.
        let codes = [ 11 ; 22 ; 33 ; 44 ; 55 ; 66 ]

        let images =
            codes
            |> List.map (fun code ->
                code, guest $"    public static void Main() {{ Environment.ExitCode = %d{code}; Thread.Sleep(250); }}"
            )

        let results =
            images
            |> List.map (fun (code, image) -> Task.Run (fun () -> code, RealRuntime.executeWithRealRuntime [||] image))
            |> List.map (fun t -> t.Result)

        for expected, actual in results do
            actual |> shouldEqual (RealRuntimeResult.NormalExit expected)
