module UnionVirtualSlots

open System.Reflection

/// F# emits the structural equality and comparison members of a union as `Public, Final, Virtual,
/// HideBySig` with **no** NewSlot. Six of them match nothing on `Object`, so laying out this type's
/// vtable needs CoreCLR's rule that an unmatched non-NewSlot virtual is given a fresh slot
/// (`MethodTableBuilder::PlaceVirtualMethods`). Before that was implemented, reflecting over any F#
/// union at all failed outright.
///
/// `RuntimeType.PopulateMethods` asks `RuntimeMethodHandle.GetSlot` for each virtual and uses the
/// answer to index `overrides[slot]`, suppressing a base declaration something further down already
/// overrode. So a vtable with a slot too many or too few does not merely mis-number things: it
/// suppresses the wrong declaration, and the method list below comes back wrong. That is what makes
/// this observable from a guest, which cannot see slot numbers directly.
///
/// The counts are pinned rather than merely compared against each other, so that a compiler change
/// which stopped emitting these members is a failure rather than a silent weakening. The real
/// runtime checks them too -- it runs this same guest -- so a wrong expectation here fails the test
/// rather than being adopted as truth.
type Shape =
    | Point
    | Circle of radius : int
    | Rectangle of width : int * height : int

let main (_argv : string array) : int =
    let virtuals =
        typeof<Shape>.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        |> Array.filter (fun method -> method.IsVirtual)

    let named (name : string) : int =
        virtuals |> Array.filter (fun method -> method.Name = name) |> Array.length

    // The three that land on `Object`'s own slots, so they exercise the matching half of the rule
    // rather than the fallback.
    if named "Finalize" <> 1 then
        1
    elif named "ToString" <> 1 then
        2
    // `Equals(object)` matches `Object.Equals`; `Equals(Shape)` and `Equals(object, IEqualityComparer)`
    // match nothing and take fresh slots.
    elif named "Equals" <> 3 then
        3
    // `GetHashCode()` matches; `GetHashCode(IEqualityComparer)` does not.
    elif named "GetHashCode" <> 2 then
        4
    // None of the three `CompareTo` overloads matches anything on `Object`: all are fallbacks.
    elif named "CompareTo" <> 3 then
        5
    // No slot may go missing or spare: the five names above are the whole vtable, so a spurious
    // append shows up here even though its own name would be one of the five.
    elif virtuals.Length <> 10 then
        6
    else

    // Every one of them must report the union as its declaring type or `Object`; a slot bound to the
    // wrong occupant would show up as some third type.
    let wrongOwner =
        virtuals
        |> Array.filter (fun method -> method.DeclaringType <> typeof<Shape> && method.DeclaringType <> typeof<obj>)

    if wrongOwner.Length <> 0 then 7 else 0
