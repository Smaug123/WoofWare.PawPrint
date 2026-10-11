namespace WoofWare.PosixKernel.Test

open System.Text.RegularExpressions

/// A public union's cases, as an ApiSurface listing of an assembly records them.
[<RequireQualifiedAccess>]
type RecordedUnion =
    /// A union of two or more cases. F# numbers them in a nested `Tags` class, whose literal
    /// fields the listing prints with their values; these are the cases in tag order.
    | Tagged of cases : string list
    /// A union of one case, for which F# emits no `Tags` class; its case has tag 0. The listing
    /// names the case only through its constructor: a static method `New<Case>` when the case has
    /// fields, a static property `<Case>` when it has none. These are the names some constructor
    /// line of the type could belong to, so the case is among them.
    | Single of constructorNames : Set<string>

/// Why a union's cases in a compiled assembly are not its committed cases with others appended.
[<RequireQualifiedAccess>]
type CaseOrderViolation =
    /// The committed listing has the union, and the compiled assembly has no public union of that
    /// name.
    | NoLongerAUnion of union : string
    /// The committed union had several cases, and the compiled one has one.
    | LostCases of union : string * committed : string list
    /// The committed cases, in tag order, are not the first cases of the compiled union.
    | NotAnAppend of union : string * committed : string list * current : string list
    /// The committed union had one case, and the compiled union's tag 0 is a case it did not have.
    | InsertedBeforeOnlyCase of union : string * currentFirst : string
    /// The committed union had one case and so does the compiled one, and no constructor name of
    /// the committed union's is one of the compiled union's: its case was renamed or replaced.
    | ReplacedOnlyCase of union : string * committed : Set<string> * current : Set<string>

/// Whether each public union keeps every case it had at the same tag, comparing two ApiSurface
/// listings: that is, whether it gained cases only at its end.
/// An F# client compiled against the earlier listing may identify a case by its tag, which is its
/// position, so a case inserted before another, a reordering or a removal makes that client run
/// the wrong arm.
[<RequireQualifiedAccess>]
module UnionCaseOrder =

    let private unionHeader : Regex =
        Regex (@"^(?<union>\S+) inherit .* - union type with (?<count>\d+) cases$")

    let private tagField : Regex =
        Regex (@"^(?<union>\S+)\+Tags\.(?<case>\S+) \[static field\]: int = (?<tag>\d+)$")

    /// Every union an ApiSurface listing (`ApiSurface.toString`'s text) labels as one. Fails if the
    /// listing's `Tags` fields do not number each union's cases 0 to one fewer than its count.
    let parse (listing : string) : Map<string, RecordedUnion> =
        let lines =
            listing.Split '\n' |> Array.map (fun (line : string) -> line.TrimEnd '\r')

        let counts : Map<string, int> =
            lines
            |> Array.choose (fun (line : string) ->
                let m = unionHeader.Match line

                if m.Success then
                    Some (m.Groups.["union"].Value, int m.Groups.["count"].Value)
                else
                    None
            )
            |> Map.ofArray

        let tags : Map<string, (int * string) list> =
            lines
            |> Array.choose (fun (line : string) ->
                let m = tagField.Match line

                if m.Success then
                    Some (m.Groups.["union"].Value, (int m.Groups.["tag"].Value, m.Groups.["case"].Value))
                else
                    None
            )
            |> Array.groupBy fst
            |> Array.map (fun (union : string, entries : (string * (int * string))[]) ->
                union, entries |> Array.map snd |> Array.sortBy fst |> Array.toList
            )
            |> Map.ofArray

        for KeyValue (union : string, _) in tags do
            if not (counts.ContainsKey union) then
                failwith $"the listing has a Tags class for %s{union}, which it does not label as a union"

        let constructorNames (union : string) : Set<string> =
            let constructor =
                Regex (
                    "^"
                    + Regex.Escape union
                    + @"\.(?:New(?<case>\w+) \[static method\]|(?<case>\w+) \[static property\]):"
                )

            lines
            |> Array.choose (fun (line : string) ->
                let m = constructor.Match line
                if m.Success then Some m.Groups.["case"].Value else None
            )
            |> Set.ofArray

        counts
        |> Map.map (fun (union : string) (count : int) ->
            match count, Map.tryFind union tags with
            | 1, None -> RecordedUnion.Single (constructorNames union)
            | 1, Some _ -> failwith $"the listing has a Tags class for %s{union}, which has one case"
            | _, None -> failwith $"the listing has no Tags class for %s{union}, which has %d{count} cases"
            | _, Some numbered ->
                if List.map fst numbered <> [ 0 .. count - 1 ] then
                    failwith $"the listing numbers %s{union}'s %d{count} cases %A{numbered}, not 0 to %d{count - 1}"

                RecordedUnion.Tagged (List.map snd numbered)
        )

    /// The unions of `committed` whose cases `current` does not keep at the same tags. A union only
    /// in `current` is new, and a union may gain any cases at its end, since that changes no tag
    /// the union already had.
    let violations
        (committed : Map<string, RecordedUnion>)
        (current : Map<string, RecordedUnion>)
        : CaseOrderViolation list
        =
        committed
        |> Map.toList
        |> List.choose (fun (union : string, recorded : RecordedUnion) ->
            match recorded, Map.tryFind union current with
            | _, None -> Some (CaseOrderViolation.NoLongerAUnion union)
            | RecordedUnion.Single committedNames, Some (RecordedUnion.Single currentNames) ->
                if Set.isEmpty (Set.intersect committedNames currentNames) then
                    Some (CaseOrderViolation.ReplacedOnlyCase (union, committedNames, currentNames))
                else
                    None
            | RecordedUnion.Single constructorNames, Some (RecordedUnion.Tagged cases) ->
                match cases with
                | first :: _ when Set.contains first constructorNames -> None
                | first :: _ -> Some (CaseOrderViolation.InsertedBeforeOnlyCase (union, first))
                | [] -> failwith $"%s{union} is recorded as a union of several cases with no cases"
            | RecordedUnion.Tagged committedCases, Some (RecordedUnion.Single _) ->
                Some (CaseOrderViolation.LostCases (union, committedCases))
            | RecordedUnion.Tagged committedCases, Some (RecordedUnion.Tagged currentCases) ->
                if
                    currentCases.Length >= committedCases.Length
                    && List.take committedCases.Length currentCases = committedCases
                then
                    None
                else
                    Some (CaseOrderViolation.NotAnAppend (union, committedCases, currentCases))
        )

    let describe (violation : CaseOrderViolation) : string =
        let render (cases : string list) : string = String.concat ", " cases

        match violation with
        | CaseOrderViolation.NoLongerAUnion union -> $"%s{union} is no longer a public union"
        | CaseOrderViolation.LostCases (union, committed) ->
            $"%s{union} had the cases %s{render committed} in that order, and now has one case"
        | CaseOrderViolation.NotAnAppend (union, committed, current) ->
            $"%s{union} had the cases %s{render committed} in that order, and now has %s{render current}"
        | CaseOrderViolation.InsertedBeforeOnlyCase (union, currentFirst) ->
            $"%s{union} had one case, and now has %s{currentFirst}, which it did not have, at tag 0"
        | CaseOrderViolation.ReplacedOnlyCase (union, committed, current) ->
            $"%s{union} had one case, among %s{render (Set.toList committed)}, and now has one case, among %s{render (Set.toList current)}"
