namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// Reading what a Loader resolution bound, in a test that expects it to bind.
[<RequireQualifiedAccess>]
module BindingResult =

    /// The load context and what the resolution bound, failing the test where it bound nothing.
    let bound (assemblies : LoadedAssemblies, result : Result<'a, TypeBindingFailure>) : LoadedAssemblies * 'a =
        match result with
        | Ok bound -> assemblies, bound
        | Error failure -> failwith $"expected the resolution to bind, but: %s{TypeBindingFailure.describe failure}"
