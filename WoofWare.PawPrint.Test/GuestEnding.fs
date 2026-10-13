namespace WoofWare.PawPrint.Test

/// How a guest's run ends, as a fixture expects it of one case on one runtime: the real runtime and
/// PawPrint must both end this way.
[<RequireQualifiedAccess>]
type GuestEnding =
    /// The guest exits normally, with this exit code.
    | ExitCode of int
    /// The guest dies of an exception that nothing catches.
    | UnhandledException

/// What a `Guest` fixture expects of its cases, named by `'case`, on one runtime.
type GuestCasesOnRuntime<'case when 'case : comparison> =
    {
        /// The cases that PawPrint is not expected to run correctly on this runtime. Each maps to
        /// why it is parked, what would un-park it, and how it was verified to behave on real .NET.
        Parked : Map<'case, string>
        /// How each case ends that does not exit normally with code 0 on this runtime, parked or
        /// not. For a parked case this is only the real runtime's ending.
        Endings : Map<'case, GuestEnding>
    }

    /// How `case` is expected to end on this runtime.
    member this.Ending (case : 'case) : GuestEnding =
        this.Endings |> Map.tryFind case |> Option.defaultValue (GuestEnding.ExitCode 0)

[<RequireQualifiedAccess>]
module GuestEnding =

    /// Whether `result`, the real runtime's run of the guest `caseName`, ended as `expected`; the
    /// error is a description of how it did not.
    let checkReal (caseName : string) (expected : GuestEnding) (result : RealRuntimeResult) : Result<unit, string> =
        match expected, result with
        | GuestEnding.ExitCode expectedCode, RealRuntimeResult.NormalExit exitCode ->
            if exitCode = expectedCode then
                Ok ()
            else
                Error $"Real runtime exited with code %d{exitCode} for %s{caseName}, but %d{expectedCode} was expected"
        | GuestEnding.UnhandledException, RealRuntimeResult.UnhandledException _ -> Ok ()
        | GuestEnding.UnhandledException, RealRuntimeResult.NormalExit exitCode ->
            Error
                $"Real runtime exited with code %d{exitCode} for %s{caseName}, but an unhandled exception was expected"
        | GuestEnding.ExitCode expectedCode, RealRuntimeResult.UnhandledException report ->
            Error
                $"Real runtime terminated with an unhandled exception for %s{caseName}, but exit code %d{expectedCode} was expected:\n%s{report}"
        | _, RealRuntimeResult.Aborted (_code, report) ->
            Error $"Real runtime called Environment.FailFast for %s{caseName}:\n%s{report}"

    /// Fails unless `result`, the real runtime's run of the guest `caseName`, ended as `expected`.
    let assertReal (caseName : string) (expected : GuestEnding) (result : RealRuntimeResult) : unit =
        match checkReal caseName expected result with
        | Ok () -> ()
        | Error message -> failwith message
