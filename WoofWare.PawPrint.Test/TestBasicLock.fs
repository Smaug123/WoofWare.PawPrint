namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations
open WoofWare.PawPrint.Test
open WoofWare.DotnetRuntimeLocator

[<TestFixture>]
module TestBasicLock =
    let assy = typeof<RunResult>.Assembly

    [<Test ; Explicit "This test doesn't run yet">]
    let ``Can run BasicLock`` () : unit =
        let source = Assembly.getEmbeddedResourceAsString "BasicLock.cs" assy
        let image = Roslyn.compile [ source ]
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        let impls = NativeImpls.PassThru ()

        use peImage = new MemoryStream (image)

        try
            let terminalState, terminatingThread =
                Program.run loggerFactory (Some "BasicLock.cs") peImage dotnetRuntimes impls []

            let exitCode =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> failwith "expected program to return 1, but it returned void"
                | head :: _ ->
                    match head with
                    | EvalStackValue.Int32 i -> i
                    | _ -> failwith "TODO"

            exitCode |> shouldEqual 0
        finally
            let messages = messages ()

            for message in messages do
                System.Console.WriteLine $"%O{message}"
