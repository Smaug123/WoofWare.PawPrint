namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open ApiSurface
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnionCaseOrder`, which `TestSurface` uses to hold each public union's committed cases to their
/// tags: its reading of an ApiSurface listing, against F#'s own reflection over the library, and
/// its comparison, against appends and against each kind of change that is not one.
[<TestFixture>]
module TestUnionCaseOrder =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    [<Test>]
    let ``Parsing the library's listing gives each public union's cases in tag order`` () : unit =
        let assembly = typeof<UnixError>.Assembly

        let parsed =
            ApiSurface.ofAssembly assembly |> ApiSurface.toString |> UnionCaseOrder.parse

        let reflected : Map<string, string list> =
            assembly.GetExportedTypes ()
            // `IsUnion` also holds of the class F# nests in a union for each case with fields, which
            // inherits from the union.
            |> Array.filter (fun (t : Type) ->
                FSharpType.IsUnion (t, BindingFlags.Public)
                && not (isNull t.BaseType || FSharpType.IsUnion (t.BaseType, BindingFlags.Public))
            )
            |> Array.map (fun (t : Type) ->
                let cases =
                    FSharpType.GetUnionCases (t, BindingFlags.Public)
                    |> Array.sortBy (fun (c : UnionCaseInfo) -> c.Tag)
                    |> Array.map (fun (c : UnionCaseInfo) -> c.Name)
                    |> Array.toList

                t.FullName, cases
            )
            |> Map.ofArray

        (Map.keys parsed |> Set.ofSeq) |> shouldEqual (Map.keys reflected |> Set.ofSeq)

        // Unions of each kind are present, so neither arm below is vacuous.
        parsed
        |> Map.exists (fun _ (u : RecordedUnion) ->
            match u with
            | RecordedUnion.Single _ -> true
            | RecordedUnion.Tagged _ -> false
        )
        |> shouldEqual true

        parsed
        |> Map.exists (fun _ (u : RecordedUnion) ->
            match u with
            | RecordedUnion.Single _ -> false
            | RecordedUnion.Tagged _ -> true
        )
        |> shouldEqual true

        for KeyValue (union : string, recorded : RecordedUnion) in parsed do
            match recorded, reflected.[union] with
            | RecordedUnion.Tagged cases, reflectedCases -> cases |> shouldEqual reflectedCases
            | RecordedUnion.Single constructorNames, [ only ] -> Set.contains only constructorNames |> shouldEqual true
            | RecordedUnion.Single _, reflectedCases ->
                failwith $"%s{union} parsed as one case, and has %A{reflectedCases}"

    /// Distinct case names: the first `count` of a shuffled pool, so that each test can mint names
    /// not already in use from the rest.
    let private distinctNames (prefix : string) (count : int) : Gen<string list> =
        Gen.shuffle [| for i in 0 .. count - 1 -> $"%s{prefix}%d{i}" |]
        |> Gen.map Array.toList

    /// Committed unions of two to six cases, the names of which are unique across all unions, plus a
    /// supply of further names that no union uses.
    let private committedGen : Gen<Map<string, string list> * string list> =
        gen {
            let! unionCount = Gen.choose (1, 5)
            let! sizes = Gen.listOfLength unionCount (Gen.choose (2, 6))
            let! names = distinctNames "C" (List.sum sizes + 20)

            let unions, fresh =
                ((Map.empty, names), List.indexed sizes)
                ||> List.fold (fun (acc : Map<string, string list>, pool : string list) (i : int, size : int) ->
                    Map.add $"U%d{i}" (List.take size pool) acc, List.skip size pool
                )

            return unions, fresh
        }

    let private tagged (unions : Map<string, string list>) : Map<string, RecordedUnion> =
        Map.map (fun _ (cases : string list) -> RecordedUnion.Tagged cases) unions

    /// Each union with up to three fresh cases appended, and a new union too.
    let private appended (unions : Map<string, string list>) (fresh : string list) : Gen<Map<string, string list>> =
        gen {
            let! extras = Gen.listOfLength unions.Count (Gen.choose (0, 3))

            let grown, rest =
                ((Map.empty, fresh), List.zip (Map.toList unions) extras)
                ||> List.fold (fun (acc, pool : string list) ((union : string, cases : string list), extra : int) ->
                    Map.add union (cases @ List.take extra pool) acc, List.skip extra pool
                )

            return Map.add "NewUnion" (List.take 2 rest) grown
        }

    [<Test>]
    let ``Appending cases to any unions, and adding unions, is no violation`` () : unit =
        let property =
            gen {
                let! committed, fresh = committedGen
                let! current = appended committed fresh
                return committed, current
            }
            |> Arb.fromGen
            |> fun arb ->
                Prop.forAll
                    arb
                    (fun (committed : Map<string, string list>, current : Map<string, string list>) ->
                        UnionCaseOrder.violations (tagged committed) (tagged current) |> shouldEqual []
                    )

        Check.One (config, property)

    /// A change to one union's cases that is not an append.
    type private Mutation =
        | InsertBefore of position : int
        | Swap of first : int * second : int
        | Remove of position : int
        | Reverse
        | DeleteUnion

    [<Test>]
    let ``Inserting a case before another, reordering, removing a case, or deleting the union, is a violation of that union alone``
        ()
        : unit
        =
        let property =
            gen {
                let! committed, fresh = committedGen
                let! others = appended committed fresh
                let! victim = Gen.elements (Map.keys committed |> List.ofSeq)
                let cases = committed.[victim]
                let n = cases.Length

                let! mutation =
                    Gen.oneof
                        [
                            Gen.choose (0, n - 1) |> Gen.map InsertBefore
                            gen {
                                let! i = Gen.choose (0, n - 2)
                                let! j = Gen.choose (i + 1, n - 1)
                                return Swap (i, j)
                            }
                            Gen.choose (0, n - 1) |> Gen.map Remove
                            Gen.constant Reverse
                            Gen.constant DeleteUnion
                        ]

                return committed, others, victim, mutation
            }
            |> Arb.fromGen
            |> fun arb ->
                Prop.forAll
                    arb
                    (fun (committed : Map<string, string list>, others, victim : string, mutation : Mutation) ->
                        let cases = committed.[victim]
                        let newCase = "NeverUsedBefore"

                        let mutated : RecordedUnion option =
                            match mutation with
                            | InsertBefore i -> Some (RecordedUnion.Tagged (List.insertAt i newCase cases))
                            | Swap (i, j) ->
                                let swapped =
                                    cases
                                    |> List.mapi (fun (k : int) (c : string) ->
                                        if k = i then cases.[j]
                                        elif k = j then cases.[i]
                                        else c
                                    )

                                Some (RecordedUnion.Tagged swapped)
                            | Remove i ->
                                match List.removeAt i cases with
                                | [ only ] -> Some (RecordedUnion.Single (Set.singleton only))
                                | remaining -> Some (RecordedUnion.Tagged remaining)
                            | Reverse -> Some (RecordedUnion.Tagged (List.rev cases))
                            | DeleteUnion -> None

                        let current =
                            match mutated with
                            | Some u -> Map.add victim u (tagged others)
                            | None -> Map.remove victim (tagged others)

                        let expected =
                            match mutated with
                            | None -> CaseOrderViolation.NoLongerAUnion victim
                            | Some (RecordedUnion.Single _) -> CaseOrderViolation.LostCases (victim, cases)
                            | Some (RecordedUnion.Tagged now) -> CaseOrderViolation.NotAnAppend (victim, cases, now)

                        UnionCaseOrder.violations (tagged committed) current |> shouldEqual [ expected ]
                    )

        Check.One (config, property)

    [<Test>]
    let ``A one-case union may gain cases after its case, and not before it`` () : unit =
        let property (before : int) (after : int) : unit =
            let before = abs before % 4
            let after = abs after % 4
            let earlier = [ for i in 1..before -> $"Earlier%d{i}" ]
            let later = [ for i in 1..after -> $"Later%d{i}" ]
            let committed = Map.ofList [ "U", RecordedUnion.Single (Set.singleton "Only") ]

            let current =
                match earlier @ [ "Only" ] @ later with
                | [ _ ] -> RecordedUnion.Single (Set.singleton "Only")
                | cases -> RecordedUnion.Tagged cases

            let expected =
                match earlier with
                | [] -> []
                | first :: _ -> [ CaseOrderViolation.InsertedBeforeOnlyCase ("U", first) ]

            UnionCaseOrder.violations committed (Map.ofList [ "U", current ])
            |> shouldEqual expected

        Check.One (config, property)
