namespace WoofWare.PawPrint

/// Steps that thread a state and may fail: each returns the state it leaves behind with its result,
/// and a walk of them stops at the first failure, returning the state as it stood then.
[<RequireQualifiedAccess>]
module internal StateResult =

    /// Fold `step` over `items` in order, stopping at the first failure.
    let fold
        (step : 'state -> 'acc -> 'item -> 'state * Result<'acc, 'failure>)
        (state : 'state)
        (acc : 'acc)
        (items : 'item seq)
        : 'state * Result<'acc, 'failure>
        =
        let rec go (state : 'state) (acc : 'acc) (items : 'item list) : 'state * Result<'acc, 'failure> =
            match items with
            | [] -> state, Ok acc
            | item :: rest ->
                match step state acc item with
                | state, Ok acc -> go state acc rest
                | state, Error failure -> state, Error failure

        go state acc (List.ofSeq items)
