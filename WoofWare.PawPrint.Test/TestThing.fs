namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

[<TestFixture>]
module TestThing =
    let assy = typeof<RunResult>.Assembly

    [<Test>]
    let ``Can run a no-op`` () : unit =
        let source = Assembly.getEmbeddedResourceAsString "NoOp.cs" assy
        let image = Roslyn.compile [ source ]
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimes =
            // TODO: work out which runtime it expects to use, parsing the runtimeconfig etc and using DotnetRuntimeLocator. For now we assume we're self-contained.
            // DotnetEnvironmentInfo.Get().Frameworks
            // |> Seq.map (fun fi -> Path.Combine (fi.Path, fi.Version.ToString ()))
            // |> ImmutableArray.CreateRange
            ImmutableArray.Create (FileInfo(assy.Location).Directory.FullName)

        use peImage = new MemoryStream (image)

        let terminalState, terminatingThread =
            Program.run loggerFactory peImage (ImmutableArray.CreateRange []) []

        let exitCode =
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | [] -> failwith "expected program to return 1, but it returned void"
            | head :: _ ->
                match head with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith "TODO"

        exitCode |> shouldEqual 1
