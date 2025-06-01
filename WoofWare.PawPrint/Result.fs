namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module internal Result =

    let allOkOrError<'a, 'b> (rs : Result<'a, 'b> list) : Result<'a list, 'a list * 'b list> =
        let rec go (oks : 'a list) (errs : 'b list) rs =
            match rs with
            | Ok ok :: rest -> go (ok :: oks) errs rest
            | Error e :: rest -> go oks (e :: errs) rest
            | [] ->
                let oks = List.rev oks

                if List.isEmpty errs then
                    Ok oks
                else
                    Error (oks, List.rev errs)

        go [] [] rs
