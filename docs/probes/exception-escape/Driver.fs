namespace ExnSpike

open System.Diagnostics
open Microsoft.Extensions.Logging.Abstractions
open WoofWare.PawPrint

module Driver =

    let private pct (n : int) (d : int) =
        if d = 0 then 0.0 else 100.0 * float n / float d

    let private render (assy : DumpedAssembly) (s : Escape.EscapeSet) : string =
        let types =
            s.Types
            |> Set.toList
            |> List.map (Escape.display assy)
            |> List.sort
            |> String.concat ", "

        match s.Unknown, types with
        | true, "" -> "UNKNOWN"
        | true, t -> t + ", + UNKNOWN"
        | false, "" -> "(nothing)"
        | false, t -> t

    let private report (label : string) (a : Escape.Analysis) =
        let all = a.Escaping |> Seq.map (fun kv -> kv.Value) |> List.ofSeq
        let total = all.Length
        let unknown = all |> List.filter _.Unknown |> List.length

        let clean =
            all
            |> List.filter (fun s -> not s.Unknown && Set.isEmpty s.Types)
            |> List.length

        let exact =
            all
            |> List.filter (fun s -> not s.Unknown && not (Set.isEmpty s.Types))
            |> List.length

        printfn ""
        printfn "===== %s =====" label
        printfn "fixpoint rounds: %d" a.Rounds
        printfn "methods: %d" total
        printfn "  provably throws nothing:            %6d  %5.1f%%" clean (pct clean total)
        printfn "  exact non-empty escaping set:       %6d  %5.1f%%" exact (pct exact total)
        printfn "  Unknown (analysis envelope hit):    %6d  %5.1f%%" unknown (pct unknown total)

        let sizes =
            all
            |> List.filter (fun s -> not s.Unknown)
            |> List.map (fun s -> Set.count s.Types)

        if not sizes.IsEmpty then
            let sorted = List.sort sizes

            let at (p : float) =
                sorted.[min (sorted.Length - 1) (int (p * float sorted.Length))]

            printfn "  escaping-set size (exact answers): median %d, p90 %d, max %d" (at 0.5) (at 0.9) (List.max sorted)

        printfn "  incompleteness reasons (site counts):"

        for KeyValue (r, c) in a.Reasons |> Seq.sortByDescending (fun kv -> kv.Value) do
            printfn "    %-24O %8d" r c

    let private show (assy : DumpedAssembly) (a : Escape.Analysis) (needle : string) (n : int) =
        printfn ""
        printfn "-- sample answers matching %s --" needle
        let mutable shown = 0

        for KeyValue (h, name) in a.Names do
            if shown < n && name.Contains needle then
                match a.Escaping.TryGetValue h with
                | true, s ->
                    printfn "  %s" name
                    printfn "      %s" (render assy s)
                    shown <- shown + 1
                | _ -> ()

    /// What each fixture method's escaping set must be, derived from the instrument's stated
    /// envelope rather than from a previous run: a `newobj` of a CoreLib exception is a foreign
    /// callee, so every method that constructs one carries `UNKNOWN` alongside the type it throws.
    /// Anything else is a defect in the instrument or in this expectation, and the driver says so
    /// by exiting non-zero.
    let private fixtureExpectations : (string * string) list =
        [
            "Fixture.Cases::ThrowsDirectly", "System.InvalidOperationException, + UNKNOWN"
            // The clause absorbs the named type; the ctor's own opacity is not absorbed by a
            // clause that is not `catch (Exception)`.
            "Fixture.Cases::CaughtExactly", "UNKNOWN"
            // `InvalidOperationException` is foreign here, so its base chain is unreadable and the
            // instrument refuses to decide that `SystemException` covers it. This is the
            // cross-assembly wall, visible in one method.
            "Fixture.Cases::CaughtByBase", "System.InvalidOperationException, + UNKNOWN"
            "Fixture.Cases::UnrelatedCatch", "System.InvalidOperationException, + UNKNOWN"
            "Fixture.Cases::FinallyDoesNotCatch", "System.InvalidOperationException, + UNKNOWN"
            "Fixture.Cases::PropagatesOneHop", "System.InvalidOperationException, + UNKNOWN"
            "Fixture.Cases::TwoSources", "System.FormatException, System.InvalidOperationException, + UNKNOWN"
            // `catch (Exception)` absorbs everything, including what we could not name.
            "Fixture.Cases::CatchesBoth", "(nothing)"
            "Fixture.Cases::Leaf", "(nothing)"
            "Fixture.Cases::Recursive", "System.InvalidOperationException, + UNKNOWN"
            // The `throw;` is in the handler, which is outside the protected region.
            "Fixture.Cases::Rethrows", "UNKNOWN"
            "Fixture.Cases::ThrowsLocalDerived", "Fixture.Derived, + UNKNOWN"
            // `Derived`'s base chain *is* readable, being local, so the clause absorbs it.
            "Fixture.Cases::CaughtByLocalBase", "UNKNOWN"
        ]

    /// Expectations checked against the *sound* run instead, because what they are about is an
    /// opcode-raised fault, which the control run suppresses by construction.
    let private soundFixtureExpectations : (string * string) list =
        [
            // `Boom.M()` is a plain `call` whose declaring type's `.cctor` throws. The `.cctor` is
            // not the callee the edge names, so this is carried by `call`'s own entry or not at
            // all: with `call` classified as raising nothing, this method came back "(nothing)".
            //
            // Exact, with no `UNKNOWN`: everything on the path is local and resolvable — the call
            // is a MethodDef edge, and `M`'s own body is an `ldsfld` of a local static. So this is
            // also the one fixture method that shows the instrument giving a complete answer
            // rather than an answer plus an admission.
            "Fixture.CctorCases::CallsBoom", "System.TypeInitializationException"
            // The `catch` names a *locally declared* `System.NullReferenceException`, which is a
            // different type from the one the runtime throws, so it must not absorb the fault.
            //
            // The expected string does not name which of the two types is escaping — both display
            // the same — but it discriminates the bug exactly: an analysis that canonicalised
            // opcode faults by name would key this one to the local shadow, the clause would
            // absorb it, and this would read "(nothing)".
            "Fixture.ShadowCases::DereferencesNull", "System.NullReferenceException"
        ]

    /// Returns the number of mismatches.
    let private checkOne (assy : DumpedAssembly) (a : Escape.Analysis) (name : string) (expected : string) : int =
        let found =
            a.Names |> Seq.tryPick (fun kv -> if kv.Value = name then Some kv.Key else None)

        match found with
        | None ->
            printfn "  MISSING  %s" name
            1
        | Some h ->
            let actual = render assy a.Escaping.[h]

            if actual = expected then
                printfn "  ok       %-44s %s" name actual
                0
            else
                printfn "  MISMATCH %s" name
                printfn "             expected: %s" expected
                printfn "             actual:   %s" actual
                1

    let private checkFixture (assy : DumpedAssembly) (a : Escape.Analysis) : int =
        printfn ""
        printfn "===== fixture oracle ====="
        let mutable failures = 0

        for name, expected in fixtureExpectations do
            failures <- failures + checkOne assy a name expected

        let total = List.length fixtureExpectations

        if failures = 0 then
            printfn "  all %d expectations hold" total
        else
            printfn "  %d of %d expectations FAILED" failures total

        failures

    let private checkSoundFixture (assy : DumpedAssembly) (a : Escape.Analysis) : int =
        printfn ""
        printfn "===== fixture oracle, against the sound run ====="
        let mutable failures = 0

        for name, expected in soundFixtureExpectations do
            failures <- failures + checkOne assy a name expected

        let total = List.length soundFixtureExpectations

        if failures = 0 then
            printfn "  all %d expectations hold" total
        else
            printfn "  %d of %d expectations FAILED" failures total

        failures

    [<EntryPoint>]
    let main (argv : string[]) : int =
        let path = argv.[0]
        let lf = NullLoggerFactory.Instance
        let sw = Stopwatch.StartNew ()
        let assy = Assembly.readFile lf path
        printfn "read %s in %dms" path sw.ElapsedMilliseconds

        Census.run assy

        let sw2 = Stopwatch.StartNew ()

        let withImplicit =
            Escape.run
                { Escape.sound with
                    PruneSelfInitialisation = false
                }
                assy

        printfn ""
        printfn "escape analysis (implicit on) took %dms" sw2.ElapsedMilliseconds
        report "sound: opcode-raised exceptions included" withImplicit

        CctorCensus.run assy withImplicit

        // The same sound run, with one refinement: a `.cctor` touching its own type's members does
        // not pick up `TypeInitialization` from doing so. Reported as a separate run rather than
        // folded in, so the size of what the refinement buys is visible rather than assumed.
        let refined = Escape.run Escape.sound assy
        report "sound, with both type-initialisation prunes" refined
        CctorCensus.run assy refined

        // The mode a person would actually read: everything above, with resource exhaustion
        // hidden. Deliberately unsound, and reported as its own run so that nobody mistakes it for
        // the answer above.
        let practical =
            Escape.run
                { Escape.sound with
                    ExcludeKinds = [ FaultKind.ResourceExhaustion ]
                }
                assy

        report "practical: sound, minus resource exhaustion (UNSOUND by choice)" practical
        CctorCensus.run assy practical

        let sw3 = Stopwatch.StartNew ()

        let withoutImplicit =
            Escape.run
                { Escape.sound with
                    IncludeImplicit = false
                    PruneSelfInitialisation = false
                }
                assy

        printfn ""
        printfn "escape analysis (implicit off) took %dms" sw3.ElapsedMilliseconds
        report "control: opcode-raised exceptions suppressed" withoutImplicit

        let failures =
            // The oracle's expectations are written against the fixture, so only check when that
            // is what we were pointed at.
            if assy.ThisAssemblyDefinition.Name.Name = "Fixture" then
                checkFixture assy withoutImplicit + checkSoundFixture assy withImplicit
            else
                0

        if argv.Length > 1 then
            show assy withoutImplicit argv.[1] 40

        if failures > 0 then 1 else 0
