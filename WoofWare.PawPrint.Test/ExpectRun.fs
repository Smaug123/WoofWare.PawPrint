namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// For a test whose guest must run until its process ends.
[<RequireQualifiedAccess>]
module ExpectRun =

    /// How the process ended.
    let ended (runEnd : RunEnd) : RunOutcome =
        match runEnd with
        | RunEnd.Ended outcome -> outcome
