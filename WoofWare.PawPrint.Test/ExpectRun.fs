namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// For a test whose guest must not use a value nothing wrote.
[<RequireQualifiedAccess>]
module ExpectRun =

    /// How the process ended, failing the test if PawPrint instead stopped it at an undefined value.
    let ended (runEnd : RunEnd) : RunOutcome =
        match runEnd with
        | RunEnd.Ended outcome -> outcome
        | RunEnd.StoppedAtUndefinedValue (_, thread, observation) ->
            failwith $"PawPrint stopped the guest on %O{thread} at an undefined value: %O{observation}"
