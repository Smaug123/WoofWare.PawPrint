namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

[<TestFixture>]
module TestNoOp =
    let assy = typeof<RunResult>.Assembly

    [<Test>]
    let ``Can run a no-op`` () : unit =
        let source = Assembly.getEmbeddedResourceAsString "NoOp.cs" assy
        let image = Roslyn.compile [ source ]
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimes = DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let terminalState, terminatingThread =
            Program.run loggerFactory peImage dotnetRuntimes []

        let exitCode =
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | [] -> failwith "expected program to return 1, but it returned void"
            | head :: _ ->
                match head with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith "TODO"

        exitCode |> shouldEqual 1
