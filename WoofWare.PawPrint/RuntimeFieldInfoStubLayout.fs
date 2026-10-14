namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// The instance fields of a `System.RuntimeFieldInfoStub` whose `m_fieldHandle` is a
/// `RuntimeFieldHandleInternal`, in declaration order, together with the one field of that
/// `RuntimeFieldHandleInternal`.
type WrappedHandleStubFields =
    {
        /// `object m_keepalive`.
        Keepalive : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `object m_c`.
        C : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `object m_d`.
        D : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `int m_b`.
        B : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `object m_e`.
        E : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `object m_f`.
        F : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `RuntimeFieldHandleInternal m_fieldHandle`.
        FieldHandle : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr m_handle` on `RuntimeFieldHandleInternal`: the field-registry id itself.
        HandleInternalHandle : FieldInfo<GenericParamFromMetadata, TypeDefn>
    }

/// The instance fields of a `System.RuntimeFieldInfoStub` whose `m_fieldHandle` is a bare
/// `IntPtr`, in declaration order, together with the one field of the `RuntimeFieldHandleInternal`
/// that `IRuntimeFieldInfo.Value` wraps it in.
type BareHandleStubFields =
    {
        /// `object m_keepalive`.
        Keepalive : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr m_b`.
        B : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `object m_c`.
        C : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `object m_d`.
        D : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `object m_e`.
        E : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `object m_f`.
        F : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr m_fieldHandle`: the field-registry id itself.
        FieldHandle : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr m_handle` on `RuntimeFieldHandleInternal`.
        HandleInternalHandle : FieldInfo<GenericParamFromMetadata, TypeDefn>
    }

/// A layout of `System.RuntimeFieldInfoStub`, the `IRuntimeFieldInfo` behind every
/// `RuntimeFieldHandle` PawPrint mints, which PawPrint has checked against a real CoreLib and
/// knows how to write and read.
///
/// The cases are exactly the validated set. CoreLib pads the stub to mirror `RtFieldInfo`, so a
/// CoreLib that reorders or retypes those fields has changed what PawPrint must write, and a stub
/// of any other shape is refused rather than written field-by-field from whatever metadata says.
[<RequireQualifiedAccess>]
type RuntimeFieldInfoStubLayout =
    /// `object m_keepalive, m_c, m_d; int m_b; object m_e, m_f;
    /// RuntimeFieldHandleInternal m_fieldHandle`, where `RuntimeFieldHandleInternal` is
    /// `IntPtr m_handle`: as .NET 10's CoreCLR CoreLib declares it.
    | WrappedHandle of WrappedHandleStubFields
    /// `object m_keepalive; IntPtr m_b; object m_c, m_d, m_e, m_f; IntPtr m_fieldHandle`, with
    /// `RuntimeFieldHandleInternal` as for `WrappedHandle`: as .NET 11's CoreCLR CoreLib declares
    /// it.
    | BareHandle of BareHandleStubFields

[<RequireQualifiedAccess>]
module RuntimeFieldInfoStubLayout =

    let private isObject (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) (name : string) : bool =
        field.Name = name
        && field.Signature = TypeDefn.PrimitiveType PrimitiveType.Object

    let private instanceFields
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : FieldInfo<GenericParamFromMetadata, TypeDefn> list
        =
        ty.Fields |> List.filter (fun field -> not field.IsStatic)

    let private describeFields (corelib : DumpedAssembly) (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
        let scope = GenericScope.ofType ty

        instanceFields ty
        |> List.map (fun field -> $"%s{IlFormatting.renderTypeDefn corelib scope field.Signature} %s{field.Name}")
        |> String.concat "; "
        |> fun fields -> $"%s{ty.Namespace}.%s{ty.Name} {{ %s{fields} }}"

    /// The layouts `classify` recognises, for a refusal to name.
    let private describeKnown : string =
        "System.RuntimeFieldInfoStub { obj m_keepalive; obj m_c; obj m_d; int32 m_b; obj m_e; obj m_f; System.RuntimeFieldHandleInternal m_fieldHandle } or System.RuntimeFieldInfoStub { obj m_keepalive; intptr m_b; obj m_c; obj m_d; obj m_e; obj m_f; intptr m_fieldHandle }, each with System.RuntimeFieldHandleInternal { intptr m_handle }"

    /// The layout of `stub`, whose `IRuntimeFieldInfo.Value` is a `handleInternal`; or, naming
    /// the instance fields found, why it has none of them. `corelib` is the assembly both are
    /// read from. Only instance fields are compared, in declaration order, by name and type.
    let classify
        (corelib : DumpedAssembly)
        (stub : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (handleInternal : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : Result<RuntimeFieldInfoStubLayout, string>
        =
        let isIntPtr (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) (name : string) : bool =
            field.Name = name
            && field.Signature = TypeDefn.PrimitiveType PrimitiveType.IntPtr

        let isHandleInternal (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) : bool =
            field.Name = "m_fieldHandle"
            && field.Signature = TypeDefn.FromDefinition (handleInternal.Identity, SignatureTypeKind.ValueType)

        let recognised =
            match instanceFields handleInternal with
            | [ handle ] when isIntPtr handle "m_handle" ->
                match instanceFields stub with
                | [ keepalive ; c ; d ; b ; e ; f ; fieldHandle ] when
                    isObject keepalive "m_keepalive"
                    && isObject c "m_c"
                    && isObject d "m_d"
                    && b.Name = "m_b"
                    && b.Signature = TypeDefn.PrimitiveType PrimitiveType.Int32
                    && isObject e "m_e"
                    && isObject f "m_f"
                    && isHandleInternal fieldHandle
                    ->
                    ({
                        Keepalive = keepalive
                        C = c
                        D = d
                        B = b
                        E = e
                        F = f
                        FieldHandle = fieldHandle
                        HandleInternalHandle = handle
                    }
                    : WrappedHandleStubFields)
                    |> RuntimeFieldInfoStubLayout.WrappedHandle
                    |> Some
                | [ keepalive ; b ; c ; d ; e ; f ; fieldHandle ] when
                    isObject keepalive "m_keepalive"
                    && isIntPtr b "m_b"
                    && isObject c "m_c"
                    && isObject d "m_d"
                    && isObject e "m_e"
                    && isObject f "m_f"
                    && isIntPtr fieldHandle "m_fieldHandle"
                    ->
                    ({
                        Keepalive = keepalive
                        B = b
                        C = c
                        D = d
                        E = e
                        F = f
                        FieldHandle = fieldHandle
                        HandleInternalHandle = handle
                    }
                    : BareHandleStubFields)
                    |> RuntimeFieldInfoStubLayout.BareHandle
                    |> Some
                | _ -> None
            | _ -> None

        match recognised with
        | Some layout -> Ok layout
        | None ->
            Error
                $"CoreLib declares %s{describeFields corelib stub} with %s{describeFields corelib handleInternal}, which is not a RuntimeFieldInfoStub layout PawPrint knows how to write and read (it knows %s{describeKnown})"

    /// The layout of `baseClassTypes`' CoreLib's `RuntimeFieldInfoStub`, refusing an unrecognised
    /// one.
    let private require (baseClassTypes : BaseClassTypes<DumpedAssembly>) : RuntimeFieldInfoStubLayout =
        match
            classify
                baseClassTypes.Corelib
                baseClassTypes.RuntimeFieldInfoStub
                baseClassTypes.RuntimeFieldHandleInternal
        with
        | Ok layout -> layout
        | Error refusal -> failwith refusal

    /// A `RuntimeFieldHandleInternal` whose `m_handle`, the field `handle`, holds `contents`.
    let private handleInternalValue
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (handle : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (contents : CliType)
        : CliType
        =
        let handleInternalType =
            AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldHandleInternal

        FieldIdentity.cliField
            handleInternalType
            handle
            contents
            (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.IntPtr)
        |> List.singleton
        |> CliValueType.OfFields
            baseClassTypes
            allConcreteTypes
            handleInternalType
            (DeclaredTypeFacts.ofCorelibType baseClassTypes baseClassTypes.RuntimeFieldHandleInternal)
        |> CliType.ValueType

    /// The contents of a fresh `RuntimeFieldInfoStub` naming field-registry id `fieldHandleId`.
    /// Refuses a CoreLib whose stub has no recognised layout.
    let build
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (fieldHandleId : int64)
        : CliValueType
        =
        let layout = require baseClassTypes

        let handleOf (ty : TypeInfo<'a, 'b>) : ConcreteTypeHandle =
            AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes ty

        let stubType = handleOf baseClassTypes.RuntimeFieldInfoStub
        let objectType = handleOf baseClassTypes.Object

        let registryId =
            CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle fieldHandleId)

        let nullObject (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) : CliField =
            FieldIdentity.cliField stubType field (CliType.ObjectRef None) objectType

        // Listed in declaration order; the offsets come from the type's own (auto) layout.
        let fields =
            match layout with
            | RuntimeFieldInfoStubLayout.WrappedHandle fields ->
                [
                    nullObject fields.Keepalive
                    nullObject fields.C
                    nullObject fields.D
                    FieldIdentity.cliField
                        stubType
                        fields.B
                        (CliType.Numeric (CliNumericType.Int32 0))
                        (handleOf baseClassTypes.Int32)
                    nullObject fields.E
                    nullObject fields.F
                    FieldIdentity.cliField
                        stubType
                        fields.FieldHandle
                        (handleInternalValue baseClassTypes allConcreteTypes fields.HandleInternalHandle registryId)
                        (handleOf baseClassTypes.RuntimeFieldHandleInternal)
                ]
            | RuntimeFieldInfoStubLayout.BareHandle fields ->
                let intPtr = handleOf baseClassTypes.IntPtr

                [
                    nullObject fields.Keepalive
                    FieldIdentity.cliField
                        stubType
                        fields.B
                        (CliType.zeroOfPrimitive allConcreteTypes baseClassTypes PrimitiveType.IntPtr)
                        intPtr
                    nullObject fields.C
                    nullObject fields.D
                    nullObject fields.E
                    nullObject fields.F
                    FieldIdentity.cliField stubType fields.FieldHandle registryId intPtr
                ]

        fields
        |> CliValueType.OfFields
            baseClassTypes
            allConcreteTypes
            stubType
            (DeclaredTypeFacts.ofCorelibType baseClassTypes baseClassTypes.RuntimeFieldInfoStub)

    /// Whether an object of type `objectType` is a `RuntimeFieldInfoStub`.
    let isStub
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (objectType : ConcreteTypeHandle)
        : bool
        =
        AllConcreteTypes.findExistingNonGenericConcreteType
            allConcreteTypes
            baseClassTypes.RuntimeFieldInfoStub.Identity = Some objectType

    /// What `IRuntimeFieldInfo.Value` answers for the `RuntimeFieldInfoStub` with contents
    /// `contents` and type `objectType`: the `RuntimeFieldHandleInternal` it names, as a value of
    /// that type. Refuses a CoreLib whose stub has no recognised layout, an object which is not a
    /// stub, and a stub whose contents contradict its layout.
    let value
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (objectType : ConcreteTypeHandle)
        (contents : CliValueType)
        : CliType
        =
        let layout = require baseClassTypes

        if not (isStub baseClassTypes allConcreteTypes objectType) then
            failwith
                $"RuntimeFieldInfoStubLayout.value: the object's type %O{objectType} is not System.RuntimeFieldInfoStub"

        let handleOf (ty : TypeInfo<'a, 'b>) : ConcreteTypeHandle =
            AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes ty

        match layout with
        | RuntimeFieldInfoStubLayout.WrappedHandle fields ->
            match CliValueType.DereferenceFieldById (FieldIdentity.fieldId objectType fields.FieldHandle) contents with
            | CliType.ValueType handle as v when handle.Declared = handleOf baseClassTypes.RuntimeFieldHandleInternal ->
                v
            | other ->
                failwith
                    $"RuntimeFieldInfoStub.m_fieldHandle is declared a RuntimeFieldHandleInternal, but held %O{other}"
        | RuntimeFieldInfoStubLayout.BareHandle fields ->
            // `IRuntimeFieldInfo.Value` is `new RuntimeFieldHandleInternal(m_fieldHandle)`.
            let wrap (pointer : CliType) : CliType =
                handleInternalValue baseClassTypes allConcreteTypes fields.HandleInternalHandle pointer

            match CliValueType.DereferenceFieldById (FieldIdentity.fieldId objectType fields.FieldHandle) contents with
            | CliType.RuntimePointer _
            | CliType.Numeric (CliNumericType.NativeInt _) as pointer -> wrap pointer
            | CliType.ValueType handle as pointer when handle.Declared = handleOf baseClassTypes.IntPtr -> wrap pointer
            | other -> failwith $"RuntimeFieldInfoStub.m_fieldHandle is declared an IntPtr, but held %O{other}"
