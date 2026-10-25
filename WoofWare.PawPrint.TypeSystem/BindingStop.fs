namespace WoofWare.PawPrint

/// Where code that does not yet pass a binding failure on meets one: the run stops, with the
/// failure described. Each use marks a caller still to be converted to return the failure.
[<RequireQualifiedAccess>]
module internal BindingStop =

    /// What the binding bound; a failure stops the run.
    let bound (result : Result<'a, TypeBindingFailure>) : 'a =
        match result with
        | Ok bound -> bound
        | Error failure -> failwith (TypeBindingFailure.describe failure)
