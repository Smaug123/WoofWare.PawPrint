namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// Accessors onto the state carried by `PointerHashState.SequentialFirstTouch`.
///
/// These live in the test project rather than beside the type because they are
/// counter-scheme-specific: a keyed assignment rule has no counter and no memo
/// table, so there is nothing for them to return under such a case. The match
/// below is meant to fail to compile the moment a second case lands, because
/// these assertions would not describe it.
[<RequireQualifiedAccess>]
module PointerHashTestHelpers =
    /// Counter that will be spent on the next not-yet-seen canonical key.
    let nextCounter (counters : PointerHashState) : uint64 =
        match counters with
        | PointerHashState.SequentialFirstTouch (nextCounter, _) -> nextCounter

    /// Address bits assigned so far, keyed by canonical pointer identity. Tag bits
    /// are not in here; `materialiseHashBits` ORs those on per source.
    let assigned (counters : PointerHashState) : Map<CanonicalPointerKey, uint64> =
        match counters with
        | PointerHashState.SequentialFirstTouch (_, assigned) -> assigned

    /// How many distinct canonical keys have been assigned bits.
    let assignedCount (counters : PointerHashState) : int = (assigned counters).Count
