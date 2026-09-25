namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Method bodies that a runtime writes itself rather than reads from an image.
[<RequireQualifiedAccess>]
module IlStub =

    /// A body of `instructions` at the offsets their encodings give them, with no locals and no
    /// exception regions.
    let ofInstructions (instructions : IlOp list) : MethodInstructions<TypeDefn> =
        let located =
            ((0, []), instructions)
            ||> List.fold (fun (offset, acc) op -> offset + IlOp.NumberOfBytes op, (op, offset) :: acc)
            |> snd
            |> List.rev

        {
            Instructions = located
            Locations = located |> List.map (fun (op, offset) -> offset, op) |> Map.ofList
            LocalsInit = false
            LocalVars = None
            ExceptionRegions = ImmutableArray.Empty
        }
