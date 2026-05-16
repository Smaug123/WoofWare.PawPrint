namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations
open WoofWare.PawPrint.Test

[<TestFixture>]
module TestRaces =
    let private assy = typeof<RunResult>.Assembly

    let private runSource (sourceName : string) : RunOutcome =
        let source = Assembly.getEmbeddedResourceAsString sourceName assy
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        Program.run loggerFactory (Some sourceName) peImage dotnetRuntimes (MockEnv.make ()) Map.empty []

    // The guest reads a shared `int` between starting a worker (which writes 1)
    // and joining it. Both 0 (read precedes worker's write) and 1 (read follows
    // worker's write) are legal outcomes; any other value would indicate a bug.
    // Once schedule fuzzing exists this should be strengthened to assert that
    // *both* outcomes are observable across schedules.
    [<Test>]
    let ``ReadWriteRace exits with a legal outcome`` () : unit =
        match runSource "ReadWriteRace.cs" with
        | RunOutcome.NormalExit (terminalState, terminatingThread)
        | RunOutcome.ProcessExit (terminalState, terminatingThread) ->
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 exitCode :: _ -> [ 0 ; 1 ] |> List.contains exitCode |> shouldEqual true
            | [] -> failwith "expected program to return a value, but it returned void"
            | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
        | RunOutcome.FailFast (_, _, message) ->
            let m = message |> Option.defaultValue "<no message>"
            failwith $"PawPrint guest called Environment.FailFast: %s{m}"
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
