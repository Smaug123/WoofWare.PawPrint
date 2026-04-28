namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDebuggerState =
    let private exitCodeOfRunOutcome (outcome : RunOutcome) : int =
        let terminalState, terminatingThread =
            match outcome with
            | RunOutcome.NormalExit (state, thread)
            | RunOutcome.ProcessExit (state, thread) -> state, thread
            | RunOutcome.GuestUnhandledException (_, _, exn) ->
                failwith $"PawPrint threw an unexpected guest exception: %O{exn.ExceptionObject}"

        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
        | EvalStackValue.Int32 exitCode :: _ -> exitCode
        | [] -> failwith "expected program to return an int, but it returned void"
        | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"

    let private runSource (sourceFileName : string) (source : string) : RunOutcome =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceFileName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            Program.run loggerFactory (Some sourceFileName) peImage dotnetRuntimes (MockEnv.make ()) []
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
