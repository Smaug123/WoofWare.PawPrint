namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// How a guest's run ends, as the kernel says it ended: each way a CoreCLR process can
/// end, under each flavour, carries the termination the configured kernel gives it, and
/// the App's exit code for it reads to a shell as that termination does.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter; see AGENTS.md for why that makes it
// `Explicit`, and CI selects it by category.
[<Category("Guest")>]
[<Explicit>]
module TestProcessTerminationWiring =

    /// Ends as `args[0]` says, with the status `args[1]` where it takes one.
    let private guest : string =
        """
using System;

class Program
{
    static int Main(string[] args)
    {
        switch (args[0])
        {
            case "return": return int.Parse(args[1]);
            case "exit": Environment.Exit(int.Parse(args[1])); return 1;
            case "failfast": Environment.FailFast("ending"); return 2;
            case "throw": throw new InvalidOperationException("ending");
            default: return 3;
        }
    }
}
"""

    let private run (platform : SimulatedUnixPlatform) (coreDumps : CoreDumps) (argv : string list) : RunOutcome =
        let description = $"""%O{platform} %O{coreDumps} %s{String.concat " " argv}"""

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "case", description ]

        use _loggerFactoryResource = loggerFactory
        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()
        use peImage = new MemoryStream (Roslyn.compile [ guest ])

        BoundedRun.run
            loggerFactory
            description
            None
            peImage
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel =
                            { KernelConfig.Default with
                                UnixPlatform = platform
                                CoreDumps = coreDumps
                            }
                        Argv = argv
                    }
            }
        |> ExpectRun.ended

    /// The App's Unix exit code for `outcome`, which must read to a shell as the
    /// kernel's termination does.
    let private unixExitCode (outcome : RunOutcome) : int =
        let code = AppExitCode.compute false outcome
        let state = RunOutcome.state outcome

        AppExitCode.checkConsistent
            (SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform)
            code
            (RunOutcome.termination outcome)
        |> shouldEqual (Ok ())

        code

    /// The status a parent's `waitid` would read, for an exit.
    let private waitidStatus (termination : ProcessTermination) : int =
        match termination with
        | ProcessTermination.Exited status -> ExitStatus.waitidStatus status
        | ProcessTermination.Signaled _ -> failwith $"expected an exit, got %O{termination}"

    [<TestCase("return", 256)>]
    [<TestCase("return", -1)>]
    [<TestCase("exit", 65543)>]
    let ``an exit keeps what the flavour keeps of the latched exit code`` (how : string, status : int) : unit =
        for platform, kept in
            [
                SimulatedUnixPlatform.linuxX64, status &&& 0xff
                SimulatedUnixPlatform.macOsArm64, status &&& 0xffffff
            ] do
            let outcome = run platform CoreDumps.Suppressed [ how ; string<int> status ]

            match how, outcome with
            | "return", RunOutcome.NormalExit (state, _, termination)
            | "exit", RunOutcome.ProcessExit (state, _, termination) ->
                state.LatchedExitCode |> shouldEqual status
                waitidStatus termination |> shouldEqual kept
                // The App passes the whole latched code on, as CoreCLR's host does.
                unixExitCode outcome |> shouldEqual status
            | _ ->
                failwith $"%s{how} %d{status} under %O{platform}: unexpected outcome %A{RunOutcome.termination outcome}"

    [<Test>]
    let ``a fail-fast ends the process by SIGABRT, dumping core as configured`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            for coreDumps in [ CoreDumps.Suppressed ; CoreDumps.Written ] do
                match run platform coreDumps [ "failfast" ] with
                | RunOutcome.Aborted (_, _, _, termination) as outcome ->
                    termination
                    |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, coreDumps = CoreDumps.Written))

                    unixExitCode outcome |> shouldEqual 134
                | outcome ->
                    failwith
                        $"fail-fast under %O{platform}: unexpected outcome ending by %O{RunOutcome.termination outcome}"

    [<Test>]
    let ``an escaping exception ends the process by SIGABRT, dumping core as configured`` () : unit =
        for platform, coreDumps in
            [
                SimulatedUnixPlatform.linuxX64, CoreDumps.Written
                SimulatedUnixPlatform.macOsArm64, CoreDumps.Suppressed
            ] do
            match run platform coreDumps [ "throw" ] with
            | RunOutcome.GuestUnhandledException (_, _, _, termination) as outcome ->
                termination
                |> shouldEqual (ProcessTermination.Signaled (Signal.SIGABRT, coreDumps = CoreDumps.Written))

                unixExitCode outcome |> shouldEqual 134
            | outcome ->
                failwith $"throw under %O{platform}: unexpected outcome ending by %O{RunOutcome.termination outcome}"
