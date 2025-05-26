namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test
open WoofWare.DotnetRuntimeLocator

[<TestFixture>]
module TestHelloWorld =
    let assy = typeof<RunResult>.Assembly

    [<Test ; Explicit "This test doesn't run yet">]
    let ``Can run Hello World`` () : unit =
        let source = Assembly.getEmbeddedResourceAsString "HelloWorld.cs" assy
        let image = Roslyn.compile [ source ]
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        let impls = NativeImpls.Mock ()

        try
            use peImage = new MemoryStream (image)

            let terminalState, terminatingThread =
                Program.run loggerFactory (Some "HelloWorld.cs") peImage dotnetRuntimes impls []

            let exitCode =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> failwith "expected program to return 1, but it returned void"
                | head :: _ ->
                    match head with
                    | EvalStackValue.Int32 i -> i
                    | _ -> failwith "TODO"

            exitCode |> shouldEqual 0
        with _ ->
            for m in messages () do
                Console.Error.WriteLine $"{m}"

            reraise ()
