namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Covers the wiring between the sequence points an assembly carries and the trace log that
/// reports them. The resolution itself is unit-tested in `TestSequencePoints`; what can only be
/// seen by running a guest is that the *executing* method's own assembly is the one consulted.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTraceSourceLocations =

    let private assy = typeof<RunResult>.Assembly

    /// `Triple` occupies lines 3-7 and `Main` lines 8-11; both are the guest's own code, and so
    /// both should be attributed to File0.cs.
    let private source : string =
        String.concat
            "\n"
            [
                "public static class Sut" // 1
                "{" // 2
                "    public static int Triple(int x)" // 3
                "    {" // 4
                "        int y = x * 3;" // 5
                "        return y;" // 6
                "    }" // 7
                "    public static int Main()" // 8
                "    {" // 9
                "        return Triple(0) ;" // 10
                "    }" // 11
                "}" // 12
            ]

    /// One step of the trace log, as a structured consumer sees it: the extent of the executing
    /// method's body, and the source the step was attributed to if any.
    ///
    /// `MaxIlOpIndex` identifies a line as a step of the trace rather than any other message, and
    /// is retained because it distinguishes bodies that a line number cannot — see the synthetic
    /// startup frame below.
    type private TracedStep =
        {
            MaxIlOpIndex : int
            Source : (string * int) option
        }

    /// Harvests the distinct <c>TracedStep</c>s the trace log emits.
    let private harvestingLoggerFactory () : (unit -> TracedStep list) * ILoggerFactory =
        // Read the structured state rather than the formatted message, and keep a *set*: the
        // trace log fires once per interpreted IL instruction, so formatting each message — or
        // retaining one record per instruction — would dominate this test's time and memory.
        // Distinct steps are bounded by the sequence points of the methods actually executed.
        let seen = HashSet<TracedStep> ()

        let logger =
            { new ILogger with
                member _.BeginScope _state =
                    { new IDisposable with
                        member _.Dispose () = ()
                    }

                member _.IsEnabled (level : LogLevel) : bool = level >= LogLevel.Trace

                member _.Log (_level, _eventId, state, _ex, _formatter) =
                    match box state with
                    | :? IReadOnlyList<KeyValuePair<string, obj>> as pairs ->
                        let mutable maxIlOpIndex = None
                        let mutable file = None
                        let mutable line = None

                        for pair in pairs do
                            match pair.Key, pair.Value with
                            | "MaxIlOpIndex", (:? int as value) -> maxIlOpIndex <- Some value
                            | "SourceFile", (:? string as value) -> file <- Some value
                            | "SourceLine", (:? int as value) -> line <- Some value
                            | _ -> ()

                        match maxIlOpIndex with
                        | None -> ()
                        | Some maxIlOpIndex ->
                            let step =
                                {
                                    MaxIlOpIndex = maxIlOpIndex
                                    // The two holes are emitted together or not at all, so
                                    // anything else is the message template and the arguments
                                    // having drifted apart; say so rather than silently
                                    // reporting "no source".
                                    Source =
                                        match file, line with
                                        | Some file, Some line -> Some (file, line)
                                        | None, None -> None
                                        | Some file, None -> failwith $"trace step reported file %s{file}, no line"
                                        | None, Some line -> failwith $"trace step reported line %d{line}, no file"
                                }

                            lock seen (fun () -> seen.Add step |> ignore<bool>)
                    | _ -> ()
            }

        let getSeen () = lock seen (fun () -> List.ofSeq seen)

        getSeen,
        { new ILoggerFactory with
            member _.CreateLogger _categoryName = logger
            member _.AddProvider _provider = ()
            member _.Dispose () = ()
        }

    /// Run the guest under a trace-enabled harvesting logger.
    ///
    /// Shared by the tests below rather than run per test: interpreting a guest with the trace log
    /// on is by far the most expensive thing here, and both tests want the same run.
    let private tracedRun : Lazy<TracedStep list> =
        lazy
            (let image = Roslyn.compileWithSymbols [ source ]
             let seen, loggerFactory = harvestingLoggerFactory ()

             let dotnetRuntimes =
                 DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

             use peImage = new MemoryStream (image)

             let outcome =
                 Program.run loggerFactory (Some "TraceSourceLocations.cs") peImage (HostConfig.Default dotnetRuntimes)

             match outcome with
             | RunOutcome.NormalExit _ -> ()
             | other -> failwith $"expected the guest to exit normally, got %O{other}"

             seen ())

    [<Test>]
    let ``the trace log attributes guest instructions to guest source`` () : unit =
        let steps = tracedRun.Value

        // The framework assemblies ship no symbols, so almost every step of the run has no source
        // at all; that some step reports none is what tells us the unattributed path is live
        // rather than dead.
        steps |> List.exists (fun step -> step.Source.IsNone) |> shouldEqual true

        // The guest's own methods do carry symbols, and are the point of the exercise. Asserting
        // on specific *lines* rather than merely on the file keeps this from passing if the
        // offset-to-location lookup degenerated to "the first sequence point of the method".
        let guestLines =
            steps
            |> List.choose (fun step ->
                match step.Source with
                | Some ("File0.cs", line) -> Some line
                | Some _
                | None -> None
            )
            |> List.distinct
            |> List.sort

        guestLines |> List.isEmpty |> shouldEqual false
        // Statements of Triple (5, 6) and of Main (10), whichever else the compiler attributes.
        guestLines |> List.contains 5 |> shouldEqual true
        guestLines |> List.contains 6 |> shouldEqual true
        guestLines |> List.contains 10 |> shouldEqual true
        // Nothing outside the two methods the guest declares.
        guestLines |> List.filter (fun line -> line < 3 || line > 11) |> shouldEqual []

    /// `Program.buildStartupFrame` pumps class initialisers from a frame whose body is a lone
    /// `ret`; `SynthesisedMethod.EntryPointPlaceholder` denies that frame a handle to resolve
    /// against. A frame that kept `Main`'s metadata identity would resolve the `ret` against
    /// `Main`'s sequence points, and the trace would claim a guest instruction had run when
    /// none had.
    ///
    /// The tell is the body's extent rather than the line: the synthetic body has exactly one
    /// instruction, so `MaxIlOpIndex` is 0, whereas real `Main` is longer. A line-based assertion
    /// cannot see this at all — real `Main` reports its opening brace too, from the same offset.
    [<Test>]
    let ``the synthetic startup frame claims no guest source`` () : unit =
        let steps = tracedRun.Value

        // The bug this guards is only reachable if a one-instruction body is executed at all;
        // without this the test passes vacuously against an interpreter that never runs one.
        steps |> List.exists (fun step -> step.MaxIlOpIndex = 0) |> shouldEqual true

        let offenders =
            steps
            |> List.filter (fun step ->
                match step.MaxIlOpIndex, step.Source with
                | 0, Some ("File0.cs", _) -> true
                | _ -> false
            )

        match offenders with
        | [] -> ()
        | offenders -> failwith $"one-instruction synthetic bodies were attributed to guest source: %A{offenders}"
