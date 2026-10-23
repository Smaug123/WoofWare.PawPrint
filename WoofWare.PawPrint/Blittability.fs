namespace WoofWare.PawPrint

/// Whether CoreCLR copies a value type's managed image byte for byte to marshal it
/// (`MethodTable::IsBlittable`), or a field's (`IsFieldBlittable`, fieldmarshaler.cpp), and whether
/// PawPrint can make that copy itself.
[<RequireQualifiedAccess>]
type Blittability =
    /// CoreCLR copies it byte for byte, and PawPrint holds it as bytes it can copy.
    | Blittable
    /// CoreCLR copies it byte for byte, but PawPrint does not: it holds a pointer, which PawPrint
    /// keeps as a cell with provenance and no byte image, or it is a value type without fields,
    /// which PawPrint holds as raw bytes and does not vouch for the marshalled size of.
    | BlittableUncopyable
    /// CoreCLR converts it to marshal it.
    | NotBlittable

[<RequireQualifiedAccess>]
module Blittability =

    /// Of the fields of one type, a type is blittable if each of them is, and PawPrint can copy it
    /// if it can copy each of them.
    let private ofAll (each : Blittability list) : Blittability =
        if List.contains Blittability.NotBlittable each then
            Blittability.NotBlittable
        elif List.contains Blittability.BlittableUncopyable each then
            Blittability.BlittableUncopyable
        else
            Blittability.Blittable

    /// Whether CoreCLR blits a field of a type it pairs with the native types `accepted`, under the
    /// field's `[MarshalAs]` `descriptor`. `IsFieldBlittable` reads only a blob's first byte, and an
    /// empty blob as `NATIVE_TYPE_DEFAULT`. A malformed blob is led by `ByValTStr`, `ByValArray` or
    /// `CustomMarshaler`, none of which such a type accepts.
    let private ofDescriptor
        (accepted : System.Runtime.InteropServices.UnmanagedType list)
        (descriptor : FieldMarshalDescriptor option)
        : Blittability
        =
        match descriptor with
        | None
        | Some FieldMarshalDescriptor.Empty -> Blittability.Blittable
        | Some (FieldMarshalDescriptor.Other unmanagedType) when List.contains unmanagedType accepted ->
            Blittability.Blittable
        | Some (FieldMarshalDescriptor.Other _)
        | Some (FieldMarshalDescriptor.ByValTStr _)
        | Some (FieldMarshalDescriptor.ByValArray _)
        | Some (FieldMarshalDescriptor.Malformed _) -> Blittability.NotBlittable

    /// A field's blittability, CoreCLR's `IsFieldBlittable` (fieldmarshaler.cpp:202). `charSet` is
    /// the declaring type's, `descriptor` the field's own `[MarshalAs]`, and `fieldType` its
    /// declared type, which tells a function pointer from the native integer that holds one.
    ///
    /// A field PawPrint's native layout calls illegal (`MarshalFieldNative.Illegal`) is never
    /// blittable: CoreCLR accepts a primitive under exactly the native types it pairs the
    /// primitive with (`CliValueType.PrimitiveFieldNativeTypes`), a value type under `Struct`
    /// alone, and a type with no layout of its own not at all.
    let rec ofField
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (charSet : System.Runtime.InteropServices.CharSet)
        (descriptor : FieldMarshalDescriptor option)
        (fieldType : ConcreteTypeHandle)
        (t : CliType)
        : Blittability
        =
        // `GetNestedFieldFlags` judges an enum field by its underlying element type
        // (classlayoutinfo.cpp:445), under the *containing* type's `CharSet`: an enum over `char`
        // is a `char` field, not a struct with a `CharSet` of its own.
        match CliValueType.TryEnumUnderlying concreteTypes assemblies corelib t with
        | Some (underlyingType, underlying) ->
            ofField concreteTypes assemblies corelib charSet descriptor underlyingType underlying
        | None ->

        match t with
        | CliType.Bool _
        | CliType.Char _ ->
            if
                CliValueType.TryBoolCharFieldMarshal charSet descriptor t = Some (Result.Ok BoolCharMarshal.Utf16Char)
            then
                Blittability.Blittable
            else
                Blittability.NotBlittable
        // A PawPrint pointer cell carries provenance with no byte rendering. CoreCLR blits an
        // unmanaged pointer or a function pointer under the native types it pairs each with, and
        // never a byref, whose `ELEMENT_TYPE_BYREF` takes `IsFieldBlittable`'s default arm.
        | CliType.RuntimePointer _ ->
            match fieldType with
            | ConcreteTypeHandle.Byref _ -> Blittability.NotBlittable
            | _ ->

            match CliValueType.PrimitiveFieldNativeTypes fieldType t with
            | Some accepted ->
                match ofDescriptor accepted descriptor with
                | Blittability.Blittable -> Blittability.BlittableUncopyable
                | other -> other
            | None -> failwith $"Blittability.ofField: a field of type %O{fieldType} holds the pointer cell %O{t}"
        | CliType.ObjectRef _ -> Blittability.NotBlittable
        | CliType.Numeric _
        | CliType.ValueType _ ->

        match CliValueType.PrimitiveFieldNativeTypes fieldType t with
        // `NativeInt` cells carry provenance under PawPrint (e.g. a pointer from
        // `Marshal.AllocHGlobal`, or `TypeHandlePtr` from `typeof(T).TypeHandle.Value`). CoreCLR
        // memmoves the integer-width bits regardless; PawPrint cannot, because
        // `CliNumericType.ToBytes` refuses to serialise provenance. `IntPtr`/`UIntPtr` are
        // accepted because neither caller flattens such a cell to bytes: the blittable arm
        // returns a null stub, so CoreLib's `SpanHelpers.Memmove` is intercepted and routed
        // through `CellAwareMemOps.copy`; and the stub path reads the source struct structurally
        // (`readSource`) and writes each field as a typed value, so a pointer cell survives into
        // the destination intact. Reading the destination back through a byte view
        // (`Marshal.ReadIntPtr`) is refused by `executeLdind` (#801).
        | Some accepted -> ofDescriptor accepted descriptor
        | None ->

        match t with
        | CliType.ValueType vt ->
            let isDefaultOrStruct =
                match descriptor with
                | None
                | Some FieldMarshalDescriptor.Empty
                | Some (FieldMarshalDescriptor.Other System.Runtime.InteropServices.UnmanagedType.Struct) -> true
                | Some (FieldMarshalDescriptor.Other _)
                | Some (FieldMarshalDescriptor.ByValTStr _)
                | Some (FieldMarshalDescriptor.ByValArray _)
                | Some (FieldMarshalDescriptor.Malformed _) -> false

            // DateTime is structurally a single `ulong _dateData` and would otherwise qualify as
            // strictly numeric, but CoreCLR's `MarshalInfo` (mlinfo.cpp:1747) special-cases
            // DateTime fields as `MARSHAL_TYPE_DATE`: 8 bytes of OADate, NOT the managed
            // `_dateData` byte image. The memmove fast path would silently emit the wrong bytes,
            // so it is not blittable — `tryComputePlan` picks it up as `OADate` instead.
            let isDateTime =
                CliValueType.IsHostKnownDateTime concreteTypes assemblies corelib vt

            // Decimal is structurally `{ int; uint; ulong }` and would otherwise recurse to true,
            // but CoreCLR's `IsFieldBlittable` rejects a Decimal field unconditionally
            // (fieldmarshaler.cpp:266): managed `System.Decimal`'s alignment need not match
            // native `DECIMAL`'s, so the enclosing struct's managed layout cannot stand in for its
            // native one. `tryComputePlan` copies the Decimal itself verbatim instead, at the
            // offset the native layout walk gives it.
            let isDecimal = CliValueType.IsHostKnownDecimal concreteTypes assemblies corelib vt

            // `MethodTable::IsBlittable` requires layout, which an auto-layout type lacks.
            let hasNoLayout =
                CliValueType.IsAutoLayoutHandle concreteTypes assemblies vt.Declared

            if not isDefaultOrStruct || isDateTime || isDecimal || hasNoLayout then
                Blittability.NotBlittable
            else
                ofValueType concreteTypes assemblies corelib vt
        | CliType.Numeric _ ->
            failwith
                $"unreachable: CliValueType.PrimitiveFieldNativeTypes classifies every numeric field, but %O{t} fell through"
        | CliType.Bool _
        | CliType.Char _
        | CliType.RuntimePointer _
        | CliType.ObjectRef _ -> failwith $"unreachable: %O{t} was classified above"

    /// The blittability of the value type `vt`, which has layout: its fields', each judged under `vt`'s
    /// own `CharSet`, since CoreCLR computes a nested struct's blittability once, for its own
    /// `MethodTable`. CoreCLR blits a value type without fields.
    and private ofValueType
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (vt : CliValueType)
        : Blittability
        =
        match vt._Storage with
        | CliValueTypeStorage.RawBytes _ -> Blittability.BlittableUncopyable
        | CliValueTypeStorage.Fields storage ->
            storage.Fields
            |> List.map (fun field ->
                ofField
                    concreteTypes
                    assemblies
                    corelib
                    vt.CharSet
                    field.MarshallingDescriptor
                    field.Type
                    field.Contents
            )
            |> ofAll

    /// The blittability of a value type that has layout, CoreCLR's `MethodTable::IsBlittable`,
    /// given a value of it. The host-known field-only rejections (Decimal) do not apply to the
    /// type itself.
    ///
    /// CoreLib's `System.Boolean` and `System.Char` are not blittable: CoreCLR marshals the first
    /// as a four-byte `BOOL` and the second as an ANSI byte. Throws on a reference or a pointer,
    /// which is not a value type.
    let ofValue
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (t : CliType)
        : Blittability
        =
        match t with
        | CliType.ValueType vt -> ofValueType concreteTypes assemblies corelib vt
        // A number's image is its own native image.
        | CliType.Numeric _ -> Blittability.Blittable
        | CliType.Bool _
        | CliType.Char _ -> Blittability.NotBlittable
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> failwith $"Blittability.ofValue: %O{t} is not a value of a value type"

    /// Whether a field is `Blittability.Blittable`. Shared by the blittable arm of
    /// `MarshalNative_TryGetStructMarshalStub` (through `isBlittableStruct`) and by
    /// `StructMarshalStub.tryComputePlan`, so that a field the QCall's fast path accepts is exactly
    /// a field the stub would copy verbatim.
    let isBlittableField
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (charSet : System.Runtime.InteropServices.CharSet)
        (descriptor : FieldMarshalDescriptor option)
        (fieldType : ConcreteTypeHandle)
        (t : CliType)
        : bool
        =
        ofField concreteTypes assemblies corelib charSet descriptor fieldType t = Blittability.Blittable

    /// Whether `t` is a value of a value type that is `Blittability.Blittable`, i.e. whether
    /// CoreCLR's `th.IsBlittable()` arm of `MarshalNative_TryGetStructMarshalStub` applies and the
    /// guest can memmove. A top-level DateTime is filtered earlier by the AutoLayout gate.
    let isBlittableStruct
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (t : CliType)
        : bool
        =
        match t with
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> false
        | CliType.ValueType _
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _ -> ofValue concreteTypes assemblies corelib t = Blittability.Blittable
