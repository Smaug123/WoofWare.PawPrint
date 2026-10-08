namespace WoofWare.PosixKernel.Test

/// Boot-image and launch configuration a test means to be admitted.
[<RequireQualifiedAccess>]
module internal Configured =

    /// The value a setter returned, for a pipeline of setters a test expects
    /// every one of to admit its value; a refusal fails the test, described
    /// by `describe`, the refusal's own `describe`.
    let expectOk<'Value, 'Refusal> (describe : 'Refusal -> string) (result : Result<'Value, 'Refusal>) : 'Value =
        match result with
        | Ok value -> value
        | Error refusal ->
            failwith $"test bug: the kernel refused a value the test means it to admit: %s{describe refusal}"
