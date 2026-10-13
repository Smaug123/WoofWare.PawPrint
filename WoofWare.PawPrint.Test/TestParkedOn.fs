namespace WoofWare.PawPrint.Test

open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PawPrint

/// `ParkedOn` skips a test on exactly the runtimes it names, and `GuestEnding` says whether the real
/// runtime ended a guest as a fixture expects.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestParkedOn =

    /// Every `EmulatedRuntime`, read off the union rather than from `EmulatedRuntime.supported`.
    let private everyRuntime : EmulatedRuntime list =
        FSharpType.GetUnionCases typeof<EmulatedRuntime>
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> EmulatedRuntime)
        |> List.ofArray

    /// Every non-empty sublist of `xs`, in `xs`'s order.
    let rec private nonEmptySublists (xs : 'a list) : 'a list list =
        match xs with
        | [] -> []
        | x :: rest ->
            let withoutX = nonEmptySublists rest
            [ x ] :: (withoutX |> List.map (fun sub -> x :: sub)) @ withoutX

    [<Test>]
    let ``A park applies exactly on the runtimes it names`` () : unit =
        for runtimes in nonEmptySublists everyRuntime do
            for runtime in everyRuntime do
                ParkedOn.applies runtimes runtime
                |> shouldEqual (List.contains runtime runtimes)

    [<Test>]
    let ``A park on no runtime is refused`` () : unit =
        for runtime in everyRuntime do
            let exn = Assert.Throws<exn> (fun () -> ParkedOn.applies [] runtime |> ignore<bool>)

            exn.Message |> shouldContainText "parked on no runtime"

    [<Test>]
    let ``A park on the runtime under test ignores the test, giving the reason`` () : unit =
        let exn =
            Assert.Throws<IgnoreException> (fun () ->
                ParkedOn.check [ FrameworkUnderTest.runtime () ] "the reason for this park"
            )

        exn.Message |> shouldContainText "the reason for this park"

        exn.Message
        |> shouldContainText (FrameworkUnderTest.name (FrameworkUnderTest.runtime ()))

    [<Test>]
    let ``A park with no reason is refused`` () : unit =
        for reason in [ "" ; " " ; "\n\t" ] do
            let exn =
                Assert.Throws<exn> (fun () -> ParkedOn.check [ FrameworkUnderTest.runtime () ] reason)

            exn.Message |> shouldContainText "without a reason"

    [<Test>]
    let ``A park on no runtime is refused by check too`` () : unit =
        let exn = Assert.Throws<exn> (fun () -> ParkedOn.check [] "a reason")

        exn.Message |> shouldContainText "parked on no runtime"

    [<Test>]
    let ``A case with no declared ending exits with 0`` () : unit =
        let cases : GuestCasesOnRuntime<string> =
            {
                Parked = Map.ofList [ "parked", "why" ]
                Endings = Map.ofList [ "exits", GuestEnding.ExitCode 3 ; "throws", GuestEnding.UnhandledException ]
            }

        cases.Ending "exits" |> shouldEqual (GuestEnding.ExitCode 3)
        cases.Ending "throws" |> shouldEqual GuestEnding.UnhandledException
        cases.Ending "parked" |> shouldEqual (GuestEnding.ExitCode 0)
        cases.Ending "undeclared" |> shouldEqual (GuestEnding.ExitCode 0)

    [<Test>]
    let ``The real runtime's ending is checked against the expected one`` () : unit =
        let results : RealRuntimeResult list =
            [
                RealRuntimeResult.NormalExit 0
                RealRuntimeResult.NormalExit 3
                RealRuntimeResult.UnhandledException "System.Exception: boom"
                RealRuntimeResult.Aborted (ObservedFatalError.FailFast, "Process terminated.")
                RealRuntimeResult.Aborted (ObservedFatalError.Other, "Fatal error.")
            ]

        let expectations : GuestEnding list =
            [
                GuestEnding.ExitCode 0
                GuestEnding.ExitCode 3
                GuestEnding.UnhandledException
            ]

        for expected in expectations do
            for result in results do
                let matches =
                    match expected, result with
                    | GuestEnding.ExitCode code, RealRuntimeResult.NormalExit exitCode -> code = exitCode
                    | GuestEnding.UnhandledException, RealRuntimeResult.UnhandledException _ -> true
                    | _ -> false

                match GuestEnding.checkReal "Case.cs" expected result, matches with
                | Ok (), true -> ()
                | Error message, false -> message |> shouldContainText "Case.cs"
                | outcome, _ ->
                    failwith $"expected %A{expected} against %A{result} to match: %b{matches}; got %A{outcome}"
