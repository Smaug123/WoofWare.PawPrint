namespace WoofWare.Pawprint.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// The `Guest` fixtures' per-runtime parks and expected endings are consistent on every runtime:
/// what is parked everywhere is parked on each runtime, every parked case names a case that
/// exists, and the cases a fixture runs and the cases it parks do not overlap.
///
/// These read the fixtures' tables without running a guest, so they are not `Guest` tests.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGuestParking =

    /// Every `EmulatedRuntime`, read off the union rather than from `EmulatedRuntime.supported`.
    let private everyRuntime : EmulatedRuntime list =
        FSharpType.GetUnionCases typeof<EmulatedRuntime>
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> EmulatedRuntime)
        |> List.ofArray

    /// Every entry of `everywhere` is in `onRuntime`'s map, with the same value.
    let private assertFoldedIn (what : string) (everywhere : Map<string, 'v>) (onRuntime : Map<string, 'v>) : unit =
        let missing =
            everywhere
            |> Map.toList
            |> List.filter (fun (key, value) -> Map.tryFind key onRuntime <> Some value)
            |> List.map fst

        if not missing.IsEmpty then
            failwith $"%s{what}: entries declared for every runtime are missing or changed on this one: %A{missing}"

    let private assertReasonsNotBlank (what : string) (reasons : (string * string) list) : unit =
        let blank =
            reasons
            |> List.filter (fun (_, reason) -> String.IsNullOrWhiteSpace reason)
            |> List.map fst

        if not blank.IsEmpty then
            failwith $"%s{what}: parked without a reason: %A{blank}"

    [<Test>]
    let ``Pure cases parked or ending specially everywhere are so on every runtime`` () : unit =
        for runtime in everyRuntime do
            let cases = TestPureCases.onRuntime runtime
            assertFoldedIn $"%O{runtime} parks" TestPureCases.parkedEverywhere cases.Parked
            assertFoldedIn $"%O{runtime} endings" TestPureCases.endingsEverywhere cases.Endings
            assertReasonsNotBlank $"%O{runtime}" (Map.toList cases.Parked)

    [<Test>]
    let ``Every pure case a table names is a pure source`` () : unit =
        let named =
            [
                for runtime in everyRuntime do
                    let cases = TestPureCases.onRuntime runtime
                    yield! Map.keys cases.Parked
                    yield! Map.keys cases.Endings
                yield! Map.keys TestPureCases.seededCases
                yield! Map.keys TestPureCases.environmentCases
                yield! Map.keys TestPureCases.standardStreamsCases
            ]

        named
        |> List.filter (fun name -> not (TestPureCases.allPure.Contains name))
        |> List.distinct
        |> shouldEqual []

    [<Test>]
    let ``Each pure case is run by exactly one test, or parked, on every runtime`` () : unit =
        for runtime in everyRuntime do
            let sets = TestPureCases.runSetsOn runtime

            let assigned =
                [
                    "Simple", sets.Simple
                    "Seeded", sets.Seeded
                    "Environment", sets.Environment
                    "StandardStreams", sets.StandardStreams
                    "CustomExitCodes", sets.CustomExitCodes |> Map.keys |> Set.ofSeq
                    "UnhandledException", sets.UnhandledException
                    "Parked", sets.Parked
                ]

            let multiplyAssigned =
                assigned
                |> List.collect (fun (set, names) -> names |> Set.toList |> List.map (fun name -> name, set))
                |> List.groupBy fst
                |> List.filter (fun (_, sets) -> sets.Length > 1)
                |> List.map (fun (name, sets) -> name, List.map snd sets)

            if not multiplyAssigned.IsEmpty then
                failwith $"%O{runtime}: cases assigned to more than one test: %A{multiplyAssigned}"

            assigned |> List.map snd |> Set.unionMany |> shouldEqual TestPureCases.allPure

    /// What a generated case is, apart from whether it is parked.
    type private GeneratedKind =
        | Plain
        | Seeded
        | Environment
        | StandardStreams
        | Ending of GuestEnding

    /// The test `runSets` should assign a case to, stated directly.
    let private expectedRunSets
        (all : Set<string>)
        (kinds : Map<string, GeneratedKind>)
        (parked : Set<string>)
        : TestPureCases.PureRunSets
        =
        let runs = kinds |> Map.filter (fun name _ -> not (parked.Contains name))

        let ofKind (predicate : GeneratedKind -> bool) : Set<string> =
            runs |> Map.filter (fun _ kind -> predicate kind) |> Map.keys |> Set.ofSeq

        {
            Simple =
                all
                |> Set.filter (fun name -> not (parked.Contains name))
                |> Set.filter (fun name ->
                    match Map.tryFind name kinds with
                    | None
                    | Some GeneratedKind.Plain -> true
                    | Some _ -> false
                )
            Seeded = ofKind (fun kind -> kind = GeneratedKind.Seeded)
            Environment = ofKind (fun kind -> kind = GeneratedKind.Environment)
            StandardStreams = ofKind (fun kind -> kind = GeneratedKind.StandardStreams)
            CustomExitCodes =
                runs
                |> Map.toSeq
                |> Seq.choose (fun (name, kind) ->
                    match kind with
                    | GeneratedKind.Ending (GuestEnding.ExitCode code) -> Some (name, code)
                    | _ -> None
                )
                |> Map.ofSeq
            UnhandledException = ofKind (fun kind -> kind = GeneratedKind.Ending GuestEnding.UnhandledException)
            Parked = parked
        }

    [<Test>]
    let ``runSets assigns a parked case to Parked alone, and every other case by its kind`` () : unit =
        let names = List.init 12 (fun i -> $"Case%d{i}.cs")

        let genKind : Gen<GeneratedKind> =
            Gen.oneof
                [
                    Gen.constant GeneratedKind.Plain
                    Gen.constant GeneratedKind.Seeded
                    Gen.constant GeneratedKind.Environment
                    Gen.constant GeneratedKind.StandardStreams
                    Gen.constant (GeneratedKind.Ending GuestEnding.UnhandledException)
                    Gen.choose (0, 3) |> Gen.map (GuestEnding.ExitCode >> GeneratedKind.Ending)
                ]

        // Each name: whether it is a source at all, what kind of case it is, and whether it is
        // parked. A name that is not a source may still be named by a table.
        let genCase : Gen<bool * GeneratedKind * bool> =
            Gen.zip3 (Gen.elements [ true ; true ; true ; false ]) genKind (Gen.elements [ false ; false ; true ])

        let gen = Gen.listOfLength names.Length genCase |> Gen.map (List.zip names)

        let property (generated : (string * (bool * GeneratedKind * bool)) list) : bool =
            let all =
                generated
                |> List.filter (fun (_, (isSource, _, _)) -> isSource)
                |> List.map fst
                |> Set.ofList

            let kinds =
                generated |> List.map (fun (name, (_, kind, _)) -> name, kind) |> Map.ofList

            let parked =
                generated
                |> List.filter (fun (_, (_, _, isParked)) -> isParked)
                |> List.map fst
                |> Set.ofList

            let ofKind (predicate : GeneratedKind -> bool) : Set<string> =
                kinds |> Map.filter (fun _ kind -> predicate kind) |> Map.keys |> Set.ofSeq

            let cases : GuestCasesOnRuntime<string> =
                {
                    Parked = parked |> Seq.map (fun name -> name, "a reason") |> Map.ofSeq
                    Endings =
                        kinds
                        |> Map.toSeq
                        |> Seq.choose (fun (name, kind) ->
                            match kind with
                            | GeneratedKind.Ending ending -> Some (name, ending)
                            | _ -> None
                        )
                        |> Map.ofSeq
                }

            let actual =
                TestPureCases.runSets
                    all
                    (ofKind (fun kind -> kind = GeneratedKind.Seeded))
                    (ofKind (fun kind -> kind = GeneratedKind.Environment))
                    (ofKind (fun kind -> kind = GeneratedKind.StandardStreams))
                    cases

            actual = expectedRunSets all kinds parked

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``GuestTable.fold is the union of disjoint tables, and refuses tables that share a case`` () : unit =
        let property (everywhere : Map<int, string>) (here : Map<int, string>) : bool =
            let shared =
                Set.intersect (everywhere |> Map.keys |> Set.ofSeq) (here |> Map.keys |> Set.ofSeq)

            if shared.IsEmpty then
                let folded = GuestTable.fold "test" everywhere here

                Map.count folded = Map.count everywhere + Map.count here
                && Map.forall (fun case value -> Map.tryFind case folded = Some value) everywhere
                && Map.forall (fun case value -> Map.tryFind case folded = Some value) here
            else
                try
                    GuestTable.fold "test" everywhere here |> ignore<Map<int, string>>
                    false
                with e ->
                    e.Message.Contains "stated both for every runtime and for this one"

        // Keys from a small range, so that many pairs share a case.
        let genTable : Gen<Map<int, string>> =
            Gen.listOf (Gen.zip (Gen.choose (0, 5)) (Gen.elements [ "a" ; "b" ]))
            |> Gen.map Map.ofList

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 1000,
            Prop.forAll (Arb.fromGen (Gen.zip genTable genTable)) (fun (a, b) -> property a b)
        )

    [<Test>]
    let ``F# cases parked or ending specially everywhere are so on every runtime`` () : unit =
        for runtime in everyRuntime do
            let cases = TestFSharpPureCases.onRuntime runtime
            assertFoldedIn $"%O{runtime} parks" TestFSharpPureCases.parkedEverywhere cases.Parked
            assertFoldedIn $"%O{runtime} endings" TestFSharpPureCases.endingsEverywhere cases.Endings
            assertReasonsNotBlank $"%O{runtime}" (Map.toList cases.Parked)

    [<Test>]
    let ``Every F# case a table names is an F# case`` () : unit =
        let known = Set.ofList TestFSharpPureCases.testCases

        [
            for runtime in everyRuntime do
                let cases = TestFSharpPureCases.onRuntime runtime
                yield! Map.keys cases.Parked
                yield! Map.keys cases.Endings
        ]
        |> List.filter (fun name -> not (known.Contains name))
        |> List.distinct
        |> shouldEqual []

    [<Test>]
    let ``Impure cases parked everywhere are so on every runtime, and every registered case runs or is parked``
        ()
        : unit
        =
        for runtime in everyRuntime do
            let cases = TestImpureCases.onRuntime runtime

            let contains (case : EndToEndTestCase) (within : EndToEndTestCase list) : bool =
                within |> List.exists (fun other -> Object.ReferenceEquals (case, other))

            let parked = List.map fst cases.Parked

            for case, reason in TestImpureCases.parkedEverywhere do
                if
                    not (
                        cases.Parked
                        |> List.exists (fun (other, otherReason) ->
                            Object.ReferenceEquals (case, other) && otherReason = reason
                        )
                    )
                then
                    failwith
                        $"%O{runtime}: %s{case.FileName} is parked everywhere, but not parked, or parked differently, here"

            for case in TestImpureCases.registeredEverywhere do
                if not (contains case cases.Run || contains case parked) then
                    failwith
                        $"%O{runtime}: %s{case.FileName} is registered everywhere, but neither runs nor is parked here"

            for case in cases.Run do
                if contains case parked then
                    failwith $"%O{runtime}: %s{case.FileName} both runs and is parked"

            assertReasonsNotBlank
                $"%O{runtime}"
                (cases.Parked |> List.map (fun (case, reason) -> case.FileName, reason))

    [<Test>]
    let ``Every impure case names an impure source`` () : unit =
        let prefix = "WoofWare.PawPrint.Test.sourcesImpure."

        let sources =
            typeof<RunResult>.Assembly.GetManifestResourceNames ()
            |> Seq.filter (fun name -> name.StartsWith (prefix, StringComparison.Ordinal))
            |> Seq.map (fun name -> name.Substring prefix.Length)
            |> Set.ofSeq

        [
            for runtime in everyRuntime do
                let cases = TestImpureCases.onRuntime runtime
                yield! cases.Run
                yield! List.map fst cases.Parked
        ]
        |> List.map (fun case -> case.FileName)
        |> List.filter (fun name -> not (sources.Contains name))
        |> List.distinct
        |> shouldEqual []

    [<Test>]
    let ``The inline-array sweep and its carve-out split the combinations on every runtime`` () : unit =
        let all = Set.ofList TestInlineArrayLayout.allCases

        for runtime in everyRuntime do
            let swept = Set.ofList (TestInlineArrayLayout.casesOn runtime)
            let unrunnable = Set.ofList (TestInlineArrayLayout.unrunnableOn runtime)

            Set.difference unrunnable all |> shouldEqual Set.empty
            Set.intersect swept unrunnable |> shouldEqual Set.empty
            Set.union swept unrunnable |> shouldEqual all
