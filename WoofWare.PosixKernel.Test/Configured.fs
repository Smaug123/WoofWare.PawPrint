namespace WoofWare.PosixKernel.Test

/// Boot-image configuration a test means to be admitted.
[<RequireQualifiedAccess>]
module internal Configured =

    /// The image a setter returned, for a pipeline of setters a test expects
    /// every one of to admit its value; a refusal fails the test, described
    /// by `describe`, the refusal's own `describe`.
    let expectOk<'Image, 'Refusal> (describe : 'Refusal -> string) (result : Result<'Image, 'Refusal>) : 'Image =
        match result with
        | Ok image -> image
        | Error refusal ->
            failwith $"test bug: the setter refused a value the test means it to admit: %s{describe refusal}"
