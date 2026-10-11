namespace WoofWare.PosixKernel.Test

open ApiSurface
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `RecordLabels`, which `TestSurface` uses to stop a record newly reusing a committed record's
/// field label: its reading of an ApiSurface listing, against F#'s own reflection over the library,
/// and its comparison, against changes that reuse no label and against each way of reusing one.
[<TestFixture>]
module TestRecordLabels =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    [<Test>]
    let ``The library's listing lists every public record and each of its field labels`` () : unit =
        let assembly = typeof<UnixError>.Assembly
        let listing = ApiSurface.ofAssembly assembly |> ApiSurface.toString
        let records = TestSurface.publicRecords assembly

        // So the loop below is not vacuous, and the grandfathered collision the rule tolerates
        // is present.
        records.["WoofWare.PosixKernel.Ipv4Endpoint"]
        |> shouldEqual (Set.ofList [ "Address" ; "Port" ])

        records.["WoofWare.PosixKernel.Ipv6Endpoint"].Contains "Address"
        |> shouldEqual true

        for KeyValue (record : string, labels : Set<string>) in records do
            RecordLabels.listsType listing record |> shouldEqual true

            Set.difference labels (RecordLabels.listedProperties listing record)
            |> shouldEqual Set.empty

    let private labelPool : string list = [ for i in 0..7 -> $"L%d{i}" ]

    /// Committed records of one to four labels each, drawn from a small pool so that some records
    /// share labels, as committed records may.
    let private committedGen : Gen<Map<string, Set<string>>> =
        gen {
            let! count = Gen.choose (1, 5)

            let! labelSets = Gen.listOfLength count (Gen.nonEmptyListOf (Gen.elements labelPool) |> Gen.map Set.ofList)

            return
                labelSets
                |> List.mapi (fun (i : int) (s : Set<string>) -> $"R%d{i}", s)
                |> Map.ofList
        }

    /// `committed`, with fresh labels added to some records and a new record of fresh labels.
    let private grownFreshly (committed : Map<string, Set<string>>) : Gen<Map<string, Set<string>>> =
        gen {
            let! extras = Gen.listOfLength committed.Count (Gen.choose (0, 2))

            let grown =
                List.zip (Map.toList committed) extras
                |> List.map (fun ((record : string, labels : Set<string>), extra : int) ->
                    record, Set.union labels (Set.ofList [ for j in 1..extra -> $"Fresh%s{record}_%d{j}" ])
                )
                |> Map.ofList

            return Map.add "NewRecord" (Set.ofList [ "FreshNew1" ; "FreshNew2" ]) grown
        }

    [<Test>]
    let ``Committed collisions, fresh labels and new records of fresh labels are no collision`` () : unit =
        let property =
            gen {
                let! committed = committedGen
                let! current = grownFreshly committed
                return committed, current
            }
            |> Arb.fromGen
            |> fun arb ->
                Prop.forAll
                    arb
                    (fun (committed : Map<string, Set<string>>, current : Map<string, Set<string>>) ->
                        RecordLabels.collisions committed committed |> shouldEqual []
                        RecordLabels.collisions committed current |> shouldEqual []
                    )

        Check.One (config, property)

    [<Test>]
    let ``A new record, or a field added to a committed one, reusing another committed record's label is exactly that collision``
        ()
        : unit
        =
        let property =
            gen {
                let! committed = committedGen
                let! current = grownFreshly committed

                // A record of `current` paired with the labels other committed records have that
                // it did not have: the labels it could newly reuse.
                let candidates =
                    current
                    |> Map.toList
                    |> List.map (fun (record : string, _) ->
                        let had = Map.tryFind record committed |> Option.defaultValue Set.empty

                        let others =
                            committed
                            |> Map.toList
                            |> List.filter (fun (other : string, _) -> other <> record)
                            |> List.map snd
                            |> Set.unionMany

                        record, Set.difference others had
                    )
                    |> List.filter (fun (_, labels : Set<string>) -> not labels.IsEmpty)

                // "NewRecord" always qualifies: no committed record is it, and they have labels.
                let! record, reusable = Gen.elements candidates
                let! label = Gen.elements (Set.toList reusable)
                return committed, current, record, label
            }
            |> Arb.fromGen
            |> fun arb ->
                Prop.forAll
                    arb
                    (fun (committed : Map<string, Set<string>>, current, record : string, label : string) ->
                        let mutated = Map.add record (Set.add label current.[record]) current

                        let expected =
                            {
                                Record = record
                                IsNewRecord = not (committed.ContainsKey record)
                                Label = label
                                CommittedOwners =
                                    committed
                                    |> Map.toList
                                    |> List.filter (fun (other : string, labels : Set<string>) ->
                                        other <> record && labels.Contains label
                                    )
                                    |> List.map fst
                            }

                        RecordLabels.collisions committed mutated |> shouldEqual [ expected ]
                    )

        Check.One (config, property)
