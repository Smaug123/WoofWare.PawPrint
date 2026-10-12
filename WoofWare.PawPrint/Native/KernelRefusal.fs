namespace WoofWare.PawPrint

/// What PawPrint does when `WoofWare.PosixKernel` will not answer a call it
/// made for the guest. The library throws `UnmodelledException` where its model
/// does not cover the call, and the BCL's own exception where the call is one
/// no client following its documentation makes (such as `ArgumentException`
/// for a boot setting no machine could have). Its message says what it knows
/// about why; PawPrint adds which entry point or knob asked, and what would
/// lift the refusal where that is PawPrint's to say.
[<RequireQualifiedAccess>]
module KernelRefusal =

    /// Fail as PawPrint fails for anything it does not model, with `context`
    /// before the library's own account and the library's exception inside.
    /// For a `with :? UnmodelledException as e ->` arm at the call it guards,
    /// so that which call may refuse stays visible where it is made.
    let fail<'a> (context : string) (e : exn) : 'a =
        raise (System.Exception ($"%s{context}: %s{e.Message}", e))

    /// `fail`, with `advice` (what would lift the refusal, which PawPrint
    /// derived from its own state rather than from the library's reason) after
    /// the library's account. Empty advice adds nothing.
    let failAdvising<'a> (context : string) (advice : string) (e : exn) : 'a =
        let advice = if advice = "" then "" else " " + advice
        raise (System.Exception ($"%s{context}: %s{e.Message}%s{advice}", e))

    /// The pieces of advice that apply, each a sentence, joined; empty where
    /// none does.
    let advising (pieces : string list) : string =
        pieces |> List.filter (fun piece -> piece <> "") |> String.concat " "
