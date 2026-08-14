namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

[<RequireQualifiedAccess>]
module SynthesisedLayoutKind =
    /// The layout kind implied by a hand-built field list.
    ///
    /// `CliValueType.OfFields` takes the layout kind and the field shape as two independent
    /// records of the same fact, and rejects the combinations where they disagree — because for a
    /// type read from metadata they *are* independent, and disagreement means the metadata is
    /// malformed. A test that synthesises a type has no metadata behind it, so there is nothing
    /// for the field shape to disagree with: the fields are the whole specification, and a
    /// `FieldOffset` on each of them is exactly what makes the type explicitly laid out.
    ///
    /// Use this only where the field list is the specification. Where a test is *about* the layout
    /// kind — `TestStructLayout`, which lays one field list out under two different kinds and
    /// expects two different answers — pass the kind literally instead, or the test asserts
    /// nothing about the routing.
    let ofFieldShape (fields : CliField list) : TypeLayoutKind =
        if fields |> List.exists (fun field -> field.Offset.IsSome) then
            TypeLayoutKind.Explicit
        else
            TypeLayoutKind.Sequential

    /// `CliValueType.OfFields` for a synthesised type, taking the layout kind from the field shape
    /// as `ofFieldShape` describes. Same argument order as `OfFields` without its kind parameter,
    /// so a test that does not care which kind it gets can read as it did before the kind existed.
    let ofFields
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (declared : ConcreteTypeHandle)
        (layout : Layout)
        (charSet : System.Runtime.InteropServices.CharSet)
        (fields : CliField list)
        : CliValueType
        =
        CliValueType.OfFields bct allCt declared (ofFieldShape fields) layout charSet fields
