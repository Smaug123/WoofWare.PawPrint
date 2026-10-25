namespace WoofWare.PawPrint.Test

open System.Reflection.Metadata
open WoofWare.PawPrint

/// Reading what a resolution bound, in a test that expects it to bind.
[<RequireQualifiedAccess>]
module BindingResult =

    /// What the resolution bound, failing the test where it bound nothing.
    let value (result : Result<'a, TypeBindingFailure>) : 'a =
        match result with
        | Ok bound -> bound
        | Error failure -> failwith $"expected the resolution to bind, but: %s{TypeBindingFailure.describe failure}"

    /// The state after the resolution and what it bound, failing the test where it bound nothing.
    let bound (state : 'state, result : Result<'a, TypeBindingFailure>) : 'state * 'a = state, value result

    /// What MemberRef row `m` of `assy` bound, failing the test where it bound nothing.
    let memberValue
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (result : Result<'a, MemberBindingFailure>)
        : 'a
        =
        match result with
        | Ok bound -> bound
        | Error failure ->
            failwith
                $"expected the member reference to bind, but: %s{MemberReferenceInstantiation.describeFailure assy m failure}"

    /// Fail the test where a walk that binds as it goes stopped at a failure.
    let completed (failure : TypeBindingFailure option) : unit =
        match failure with
        | None -> ()
        | Some failure -> failwith $"expected the walk to bind, but: %s{TypeBindingFailure.describe failure}"
