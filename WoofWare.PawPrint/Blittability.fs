namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module Blittability =

    /// Whether a field's managed byte image is also its native image, i.e. whether CoreCLR's
    /// `IsFieldBlittable` (fieldmarshaler.cpp:202) would accept it. `charSet` is the declaring
    /// type's, `descriptor` the field's own `[MarshalAs]`, and `fieldType` its declared type, which
    /// tells a function pointer from the native integer that holds one.
    ///
    /// A field PawPrint's native layout calls illegal (`MarshalFieldNative.Illegal`) is never
    /// blittable: CoreCLR accepts a primitive under exactly the native types it pairs the
    /// primitive with (`CliValueType.PrimitiveFieldNativeTypes`), a value type under `Struct`
    /// alone, and a type with no layout of its own not at all.
    ///
    /// Shared by the blittable arm of `MarshalNative_TryGetStructMarshalStub` (which needs the
    /// bare yes/no) and by `tryComputePlan` (which needs it per field). One recursion, so the two
    /// cannot drift: a field the QCall's fast path accepts is exactly a field the stub would
    /// copy verbatim.
    let rec isBlittableField
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (charSet : System.Runtime.InteropServices.CharSet)
        (descriptor : FieldMarshalDescriptor option)
        (fieldType : ConcreteTypeHandle)
        (t : CliType)
        : bool
        =
        // `GetNestedFieldFlags` judges an enum field by its underlying element type
        // (classlayoutinfo.cpp:445), under the *containing* type's `CharSet`: an enum over `char`
        // is a `char` field, not a struct with a `CharSet` of its own.
        match CliValueType.TryEnumUnderlying concreteTypes assemblies corelib t with
        | Some (underlyingType, underlying) ->
            isBlittableField concreteTypes assemblies corelib charSet descriptor underlyingType underlying
        | None ->

        match t with
        | CliType.Bool _
        | CliType.Char _ ->
            CliValueType.TryBoolCharFieldMarshal charSet descriptor t = Some (Result.Ok BoolCharMarshal.Utf16Char)
        // CoreCLR blits a pointer field. A PawPrint pointer cell carries provenance with no byte
        // rendering, so it is never called blittable here.
        | CliType.RuntimePointer _ -> false
        | CliType.ObjectRef _ -> false
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
        | Some accepted ->
            // `IsFieldBlittable` reads only a blob's first byte, and an empty blob as
            // `NATIVE_TYPE_DEFAULT`. A malformed blob is led by `ByValTStr`, `ByValArray` or
            // `CustomMarshaler`, none of which a primitive accepts.
            match descriptor with
            | None
            | Some FieldMarshalDescriptor.Empty -> true
            | Some (FieldMarshalDescriptor.Other unmanagedType) -> List.contains unmanagedType accepted
            | Some (FieldMarshalDescriptor.ByValTStr _)
            | Some (FieldMarshalDescriptor.ByValArray _)
            | Some (FieldMarshalDescriptor.Malformed _) -> false
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
                false
            else
                match vt._Storage with
                // RawBytes-backed value types are not the typical struct-with-fields shape;
                // conservatively reject so we don't quietly accept primitive wrappers whose
                // CoreCLR marshal size diverges from the byte image.
                | CliValueTypeStorage.RawBytes _ -> false
                | CliValueTypeStorage.Fields storage -> areFieldsBlittable concreteTypes assemblies corelib vt storage
        | CliType.Numeric _ ->
            failwith
                $"unreachable: CliValueType.PrimitiveFieldNativeTypes classifies every numeric field, but %O{t} fell through"
        | CliType.Bool _
        | CliType.Char _
        | CliType.RuntimePointer _
        | CliType.ObjectRef _ -> failwith $"unreachable: %O{t} was classified above"

    /// Whether every field of `vt` is blittable, each judged under `vt`'s own `CharSet`: CoreCLR
    /// computes a nested struct's blittability once, for its own `MethodTable`.
    and private areFieldsBlittable
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (vt : CliValueType)
        (storage : CliFieldBackedStorage)
        : bool
        =
        storage.Fields
        |> List.forall (fun field ->
            isBlittableField
                concreteTypes
                assemblies
                corelib
                vt.CharSet
                field.MarshallingDescriptor
                field.Type
                field.Contents
        )

    /// Whether the whole struct is blittable, i.e. whether CoreCLR's `th.IsBlittable()` arm of
    /// `MarshalNative_TryGetStructMarshalStub` applies and the guest can memmove.
    ///
    /// Walks the outer struct's fields via `isBlittableField`. The host-known field-only
    /// rejections (Decimal) do not apply to the outer type's own declared type; a top-level
    /// DateTime is filtered earlier by the AutoLayout gate.
    let isBlittableStruct
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (t : CliType)
        : bool
        =
        match t with
        | CliType.ValueType vt ->
            match vt._Storage with
            | CliValueTypeStorage.RawBytes _ -> false
            | CliValueTypeStorage.Fields storage -> areFieldsBlittable concreteTypes assemblies corelib vt storage
        // A top-level primitive (e.g. `Marshal.StructureToPtr<int>`). A number's image is its own
        // native image. A top-level `bool` or `char` has no containing type to take a `CharSet`
        // from, and is not modelled: calling it non-blittable sends it to `tryComputePlan`, which
        // refuses anything but a value type.
        | CliType.Numeric _ -> true
        | CliType.Bool _
        | CliType.Char _
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> false
