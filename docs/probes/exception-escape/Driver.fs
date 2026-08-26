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

    /// Returns the number of mismatches.
    let private checkFixture (assy : DumpedAssembly) (a : Escape.Analysis) : int =
        printfn ""
        printfn "===== fixture oracle ====="
        let mutable failures = 0

        for name, expected in fixtureExpectations do
            let found =
                a.Names |> Seq.tryPick (fun kv -> if kv.Value = name then Some kv.Key else None)

            match found with
            | None ->
                printfn "  MISSING  %s" name
                failures <- failures + 1
            | Some h ->
                let actual = render assy a.Escaping.[h]

                if actual = expected then
                    printfn "  ok       %-44s %s" name actual
                else
                    printfn "  MISMATCH %s" name
                    printfn "             expected: %s" expected
                    printfn "             actual:   %s" actual
                    failures <- failures + 1

        if failures = 0 then
            printfn "  all %d expectations hold" (List.length fixtureExpectations)
        else
            printfn "  %d of %d expectations FAILED" failures (List.length fixtureExpectations)

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
        let withImplicit = Escape.run true assy
        printfn ""
        printfn "escape analysis (implicit on) took %dms" sw2.ElapsedMilliseconds
        report "sound: opcode-raised exceptions included" withImplicit

        let sw3 = Stopwatch.StartNew ()
        let withoutImplicit = Escape.run false assy
        printfn ""
        printfn "escape analysis (implicit off) took %dms" sw3.ElapsedMilliseconds
        report "control: opcode-raised exceptions suppressed" withoutImplicit

        let failures =
            // The oracle's expectations are written against the fixture, so only check when that
            // is what we were pointed at.
            if assy.ThisAssemblyDefinition.Name.Name = "Fixture" then
                checkFixture assy withoutImplicit
            else
                0

        if argv.Length > 1 then
            show assy withoutImplicit argv.[1] 40

        if failures > 0 then 1 else 0
