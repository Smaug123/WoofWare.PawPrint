namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestHardwareIntrinsicsProfile =
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

    [<Test>]
    let ``Scalar-only profile disables every vector width`` () : unit =
        HardwareIntrinsicsProfile.ScalarOnly.Vector128 |> shouldEqual false
        HardwareIntrinsicsProfile.ScalarOnly.Vector256 |> shouldEqual false
        HardwareIntrinsicsProfile.ScalarOnly.Vector512 |> shouldEqual false

    [<Test>]
    let ``Default virtual hardware profile reports vector acceleration unavailable`` () : unit =
        let source =
            """
using System.Runtime.Intrinsics;

class Program
{
    static int Main(string[] args)
    {
        if (Vector128.IsHardwareAccelerated)
        {
            return 1;
        }

        if (Vector256.IsHardwareAccelerated)
        {
            return 2;
        }

        if (Vector512.IsHardwareAccelerated)
        {
            return 3;
        }

        return 0;
    }
}
"""

        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "HardwareIntrinsicsProfile.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            Program.run loggerFactory (Some "HardwareIntrinsicsProfile.cs") peImage dotnetRuntimes (MockEnv.make ()) []
            |> exitCodeOfRunOutcome
            |> shouldEqual 0
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()
