namespace WoofWare.PawPrint

/// The layout flags CoreCLR records for a sequential or explicit-layout struct, and whether
/// its instances contain GC pointers. These facts do not describe field offsets or alignment.
type internal LaidOutStructFacts =
    {
        /// Every instance field passes `IsFieldBlittable`, under the declaring type's charset.
        IsBlittable : bool
        /// Some nested struct has auto layout, directly or transitively.
        HasAutoLayoutField : bool
        /// The struct is CoreLib's Int128/UInt128, or contains one through laid-out structs.
        IsInt128OrHasInt128Fields : bool
        /// The struct is a CoreLib decimal floating-point type, or contains one through
        /// laid-out structs. These types and this flag exist in .NET 11.
        IsDecimalFloatingPointOrHasDecimalFloatingPointFields : bool
        /// An instance contains an object reference, directly or through a nested struct.
        ContainsGcPointers : bool
    }

/// CoreCLR's layout facts for a concrete struct, with the `MethodTable` accessors' layout
/// gating. Enums are excluded: a field of enum type must be normalized to its primitive.
[<RequireQualifiedAccess>]
type internal StructLayoutFacts =
    /// Auto layout retains GC-pointer information but has no layout-owned flags.
    | WithoutLayout of containsGcPointers : bool
    /// Sequential or explicit layout, including when GC pointers cause auto field placement.
    | WithLayout of LaidOutStructFacts

    /// `MethodTable::HasLayout`.
    member this.HasLayout : bool =
        match this with
        | StructLayoutFacts.WithoutLayout _ -> false
        | StructLayoutFacts.WithLayout _ -> true

    /// `MethodTable::IsBlittable`: false without layout.
    member this.IsBlittable : bool =
        match this with
        | StructLayoutFacts.WithoutLayout _ -> false
        | StructLayoutFacts.WithLayout facts -> facts.IsBlittable

    /// `MethodTable::IsAutoLayoutOrHasAutoLayoutField`: true without layout.
    member this.IsAutoLayoutOrHasAutoLayoutField : bool =
        match this with
        | StructLayoutFacts.WithoutLayout _ -> true
        | StructLayoutFacts.WithLayout facts -> facts.HasAutoLayoutField

    /// `MethodTable::IsInt128OrHasInt128Fields`: false without layout.
    member this.IsInt128OrHasInt128Fields : bool =
        match this with
        | StructLayoutFacts.WithoutLayout _ -> false
        | StructLayoutFacts.WithLayout facts -> facts.IsInt128OrHasInt128Fields

    /// `MethodTable::IsDecimalFloatingPointOrHasDecimalFloatingPointFields`: false without layout.
    member this.IsDecimalFloatingPointOrHasDecimalFloatingPointFields : bool =
        match this with
        | StructLayoutFacts.WithoutLayout _ -> false
        | StructLayoutFacts.WithLayout facts -> facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields

    /// `MethodTable::ContainsGCPointers`: object references, including those in nested structs.
    /// Unlike `GcReferences.containsReferences`, this excludes byrefs and TypedReference:
    /// their byrefs are reported by stack walking, not by a GC series in a heap object.
    member this.ContainsGcPointers : bool =
        match this with
        | StructLayoutFacts.WithoutLayout containsGcPointers -> containsGcPointers
        | StructLayoutFacts.WithLayout facts -> facts.ContainsGcPointers

/// Pure aggregation of CoreCLR's struct layout flags from already classified instance fields.
[<RequireQualifiedAccess>]
module internal StructLayoutFacts =

    /// The containing struct's special CoreLib identity, established by the caller.
    [<RequireQualifiedAccess>]
    type SelfKind =
        | Ordinary
        | Int128
        | DecimalFloatingPoint

    let private nestedFacts (element : FieldBlittability.Element<StructLayoutFacts>) : StructLayoutFacts option =
        match element with
        | FieldBlittability.Element.ValueType (_, facts) -> Some facts
        | FieldBlittability.Element.Boolean
        | FieldBlittability.Element.Char
        | FieldBlittability.Element.OneByte
        | FieldBlittability.Element.TwoBytes
        | FieldBlittability.Element.FourBytes
        | FieldBlittability.Element.EightBytes
        | FieldBlittability.Element.Single
        | FieldBlittability.Element.Double
        | FieldBlittability.Element.NativeInteger
        | FieldBlittability.Element.Void
        | FieldBlittability.Element.TypedReference
        | FieldBlittability.Element.Pointer
        | FieldBlittability.Element.FunctionPointer
        | FieldBlittability.Element.Byref
        | FieldBlittability.Element.Reference -> None

    /// Aggregate a concrete struct's instance fields. The caller validates layout and charset,
    /// normalizes enum and CoreLib primitive fields, and supplies completed nested-struct facts.
    /// `ansi` is the declaring charset as used by `FieldBlittability.isBlittable`.
    let ofStruct
        (layout : TypeLayoutKind)
        (ansi : bool)
        (selfKind : SelfKind)
        (fields : (FieldMarshalDescriptor option * FieldBlittability.Element<StructLayoutFacts>) list)
        : StructLayoutFacts
        =
        let nested = fields |> List.choose (snd >> nestedFacts)

        let containsGcPointers =
            fields
            |> List.exists (fun (_, element) -> element = FieldBlittability.Element.Reference)
            || nested |> List.exists (fun facts -> facts.ContainsGcPointers)

        // PlaceInstanceFields and GetNestedFieldFlags (methodtablebuilder.cpp, classlayoutinfo.cpp).
        // Child accessors gate their layout-owned flags before these flags reach the parent.
        match layout with
        | TypeLayoutKind.Auto -> StructLayoutFacts.WithoutLayout containsGcPointers
        | TypeLayoutKind.Sequential
        | TypeLayoutKind.Explicit ->
            StructLayoutFacts.WithLayout
                {
                    IsBlittable =
                        fields
                        |> List.forall (fun (descriptor, element) ->
                            element
                            |> FieldBlittability.mapValueType (fun facts -> facts.IsBlittable)
                            |> FieldBlittability.isBlittable ansi descriptor
                        )
                    HasAutoLayoutField = nested |> List.exists (fun facts -> facts.IsAutoLayoutOrHasAutoLayoutField)
                    IsInt128OrHasInt128Fields =
                        selfKind = SelfKind.Int128
                        || nested |> List.exists (fun facts -> facts.IsInt128OrHasInt128Fields)
                    IsDecimalFloatingPointOrHasDecimalFloatingPointFields =
                        selfKind = SelfKind.DecimalFloatingPoint
                        || nested
                           |> List.exists (fun facts -> facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields)
                    ContainsGcPointers = containsGcPointers
                }
