namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module List =
    let replaceWhere (f : 'a -> 'a option) (l : 'a list) : 'a list =
        ([], l)
        ||> List.fold (fun acc x ->
            match f x with
            | None -> x :: acc
            | Some y -> y :: acc
        )
        |> List.rev
