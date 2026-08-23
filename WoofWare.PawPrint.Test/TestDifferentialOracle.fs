namespace WoofWare.PawPrint.Test

open System
open System.Threading
open FsUnitTyped
open NUnit.Framework

/// `DifferentialOracle.alongsideInterpreted` overlaps a guest's two runs, so the two
/// properties that makes it safe are asserted here: the runs really do overlap, and the
/// oracle is always finished with before the call returns. The second matters most when
/// the interpreted run throws — the oracle owns a child process and a scratch directory
/// it deletes on its way out, and abandoning those would leak a process and a temporary
/// tree per failing test into the rest of the suite.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDifferentialOracle =

    /// Bound on a wait that is expected to be satisfied. Generous, because exceeding it
    /// means the test fails rather than hangs, and a loaded machine must not be able to
    /// reach it.
    let private generous = TimeSpan.FromSeconds 30.0

    [<Test>]
    let ``both answers come back`` () : unit =
        let oracle, interpreted =
            DifferentialOracle.alongsideInterpreted (fun () -> "oracle") (fun () -> 42)

        oracle |> shouldEqual "oracle"
        interpreted |> shouldEqual 42

    [<Test>]
    let ``the two runs overlap`` () : unit =
        // Each side waits for the other to have started, so neither can finish unless both
        // are in flight at once. An implementation that ran them one after the other
        // cannot pass: whichever it ran first would wait out `generous` and report false.
        use oracleStarted = new ManualResetEventSlim (false)
        use interpretedStarted = new ManualResetEventSlim (false)

        let oracle () : bool =
            oracleStarted.Set ()
            interpretedStarted.Wait generous

        let interpreted () : bool =
            interpretedStarted.Set ()
            oracleStarted.Wait generous

        let oracleSawInterpreted, interpretedSawOracle =
            DifferentialOracle.alongsideInterpreted oracle interpreted

        oracleSawInterpreted |> shouldEqual true
        interpretedSawOracle |> shouldEqual true

    [<Test>]
    let ``a throwing interpreted run still waits for the oracle`` () : unit =
        // Never set, so the oracle finishes only when its own bounded wait expires. That
        // is what makes this deterministic rather than a race: an implementation that
        // abandoned the oracle would reach the assertion below within microseconds of the
        // throw, while the oracle was still blocked, and see zero.
        use neverSet = new ManualResetEventSlim (false)
        use oracleStarted = new ManualResetEventSlim (false)
        let oracleFinished = ref 0

        let oracle () : unit =
            oracleStarted.Set ()
            neverSet.Wait (TimeSpan.FromSeconds 2.0) |> ignore<bool>
            oracleFinished.Value <- oracleFinished.Value + 1

        let interpreted () : int =
            // Thrown only once the oracle is definitely mid-flight, so there is always
            // something outstanding to abandon.
            oracleStarted.Wait generous |> ignore<bool>
            failwith "interpreted run failed"

        let exn =
            Assert.Throws<Exception> (fun () ->
                DifferentialOracle.alongsideInterpreted oracle interpreted |> ignore<unit * int>
            )

        // The interpreted run's failure is the one reported, not anything about the oracle.
        exn.Message |> shouldEqual "interpreted run failed"
        oracleFinished.Value |> shouldEqual 1

    [<Test>]
    let ``a throwing oracle surfaces its own exception`` () : unit =
        // Specifically not an AggregateException, which is what `Task.Result` would have
        // raised, and which would bury the oracle's own message a layer down.
        let exn =
            Assert.Throws<InvalidOperationException> (fun () ->
                DifferentialOracle.alongsideInterpreted
                    (fun () -> raise (InvalidOperationException "oracle failed"))
                    id
                |> ignore<unit * unit>
            )

        exn.Message |> shouldEqual "oracle failed"
