namespace WoofWare.PosixKernel.Test

open System.Text.RegularExpressions

/// A field label that a public record has newly, and that some committed record already has.
type LabelCollision =
    {
        /// The record that newly has the label.
        Record : string
        /// Whether the record is new, rather than a committed record that gained the field.
        IsNewRecord : bool
        Label : string
        /// The other committed records that have the label, in order.
        CommittedOwners : string list
    }

/// Whether a public record newly has a field label that another committed record has. F# resolves
/// a label several records in scope share to the most recently declared of them, in a record
/// pattern, in a `x.Label` access on a value whose type is not yet inferred, and in a record
/// expression whose labels do not single out one record. So a new record, or a new field, that
/// reuses a label can change which record client code already compiled against the library means,
/// and that code may stop compiling.
[<RequireQualifiedAccess>]
module RecordLabels =

    /// Whether an ApiSurface listing (`ApiSurface.toString`'s text) has the type `typeName`.
    let listsType (listing : string) (typeName : string) : bool =
        listing.Split '\n'
        |> Array.exists (fun (line : string) ->
            line.StartsWith (typeName + " inherit ", System.StringComparison.Ordinal)
        )

    /// The names of the properties an ApiSurface listing (`ApiSurface.toString`'s text) gives the
    /// type `typeName`. A record's field labels are among them, as are any properties it declares
    /// as members: the listing does not tell the two apart, nor a record from a class.
    let listedProperties (listing : string) (typeName : string) : Set<string> =
        let property =
            Regex ("^" + Regex.Escape typeName + @"\.(?<name>[^ ]+) \[property\]:")

        listing.Split '\n'
        |> Array.choose (fun (line : string) ->
            let m = property.Match (line.TrimEnd '\r')
            if m.Success then Some m.Groups.["name"].Value else None
        )
        |> Set.ofArray

    /// Each label a record in `current` has and did not have in `committed` (every label, for a
    /// record `committed` lacks), which some other record in `committed` has. Labels that two
    /// committed records already share are not reported, nor labels two new records share.
    let collisions (committed : Map<string, Set<string>>) (current : Map<string, Set<string>>) : LabelCollision list =
        current
        |> Map.toList
        |> List.collect (fun (record : string, labels : Set<string>) ->
            let committedLabels, isNew =
                match Map.tryFind record committed with
                | Some existing -> existing, false
                | None -> Set.empty, true

            Set.difference labels committedLabels
            |> Set.toList
            |> List.choose (fun (label : string) ->
                let owners =
                    committed
                    |> Map.toList
                    |> List.filter (fun (other : string, otherLabels : Set<string>) ->
                        other <> record && Set.contains label otherLabels
                    )
                    |> List.map fst

                match owners with
                | [] -> None
                | _ ->
                    Some
                        {
                            Record = record
                            IsNewRecord = isNew
                            Label = label
                            CommittedOwners = owners
                        }
            )
        )

    let describe (collision : LabelCollision) : string =
        let what =
            if collision.IsNewRecord then
                "the new record"
            else
                "the field newly added to"

        let owners = String.concat ", " collision.CommittedOwners
        $"%s{collision.Label}, in %s{what} %s{collision.Record}, is already a label of %s{owners}"
