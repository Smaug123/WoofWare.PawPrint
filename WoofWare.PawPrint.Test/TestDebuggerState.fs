namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDebuggerState =
    let private exitCodeOfRunOutcome (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.UndefinedValueObserved (_, _, observation) ->
            failwith $"guest used an undefined value: %O{observation}"
        | RunOutcome.NormalExit (state, _)
        | RunOutcome.ProcessExit (state, _) -> state.LatchedExitCode
        | RunOutcome.Aborted (_, _, fatal) ->
            let m = fatal.Message |> Option.defaultValue "<no message>"
            failwith $"PawPrint guest aborted (%O{fatal.Code}): %s{m}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"PawPrint guest was terminated by POSIX signal %O{signal}"
        | RunOutcome.GuestUnhandledException (finalState, _, exn) ->
            failwith
                $"PawPrint threw an unexpected guest exception:\n%s{UnhandledExceptionReport.describe finalState exn}"

    let private runSource (sourceFileName : string) (source : string) : RunOutcome =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceFileName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        try
            Program.run loggerFactory (Some sourceFileName) peImage (HostConfig.Default dotnetRuntimes)
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<Test>]
    let ``Detached debugger state is not attached`` () : unit =
        DebuggerState.isAttached DebuggerState.Detached |> shouldEqual false

    [<Test>]
    let ``Default debugger state reports no debugger attached`` () : unit =
        let source =
            """
using System.Diagnostics;

class Program
{
    static int Main(string[] args)
    {
        return Debugger.IsAttached ? 1 : 0;
    }
}
"""

        runSource "DebuggerIsAttached.cs" source
        |> exitCodeOfRunOutcome
        |> shouldEqual 0
