namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// The instance fields of `System.Delegate` that every layout in `DelegateLayout` declares with
/// the same name, type and meaning: what a delegate is bound to, in the terms of CoreCLR's
/// delegate kinds table (comdelegate.cpp:2857-2867).
type DelegateBindingFields =
    {
        /// `object _target`: the bound first argument of a closed delegate, or the delegate
        /// itself for an open or multicast one.
        Target : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr _methodPtr`: the target of a closed delegate, the shuffle thunk of an open
        /// one, or the invoke stub of a multicast one.
        MethodPtr : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr _methodPtrAux`: zero for a closed delegate, and otherwise the target (or a
        /// virtual call stub over it) of an open one.
        MethodPtrAux : FieldInfo<GenericParamFromMetadata, TypeDefn>
    }

/// The fields in which a `DelegateLayout.InvocationListAndCount` delegate records what its
/// binding fields cannot: its invocation list, and the count or `MethodDesc*` beside it. Both are
/// declared on `System.MulticastDelegate`.
type InvocationListAndCountFields =
    {
        /// `object _invocationList`: a multicast delegate's `object[]` of element delegates, or
        /// the inner delegate of a wrapper delegate; otherwise null.
        InvocationList : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr _invocationCount`: how many of `InvocationList`'s elements a multicast
        /// delegate invokes; the `MethodDesc*` of an open virtual delegate's target; -1 for an
        /// unmanaged function pointer delegate; otherwise zero.
        InvocationCount : FieldInfo<GenericParamFromMetadata, TypeDefn>
    }

/// The fields in which a `DelegateLayout.HelperObjectAndExtraData` delegate records what its
/// binding fields cannot, both declared on `System.Delegate`, together with the field of the
/// `Delegate.Wrapper` that holds each element of a multicast delegate's invocation list.
type HelperObjectAndExtraDataFields =
    {
        /// `object _helperObject`: a multicast delegate's `Wrapper[]`; otherwise CoreLib's cache of
        /// `Delegate.Method`, a collectible delegate's loader allocator, or null.
        HelperObject : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr _extraData`: how many of a multicast delegate's elements it invokes; the
        /// `MethodDesc*` of an open virtual delegate's target; -1 for an unmanaged function pointer
        /// delegate; otherwise zero.
        ExtraData : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `Delegate Value` on `Delegate.Wrapper`: the delegate an element of the list names.
        WrapperValue : FieldInfo<GenericParamFromMetadata, TypeDefn>
    }

/// A layout of the instance fields of `System.Delegate` and `System.MulticastDelegate`, which
/// PawPrint has checked against a real CoreLib and knows how to write and read.
///
/// The cases are exactly the validated set. CoreLib's own managed code reads these fields, and
/// what each one holds for each kind of delegate is CoreCLR's contract with it, so a CoreLib that
/// renames, retypes, reorders or moves them has changed what PawPrint must write, and is refused
/// rather than written field-by-field from whatever its metadata says.
[<RequireQualifiedAccess>]
type DelegateLayout =
    /// `System.Delegate { object _target; object _methodBase; IntPtr _methodPtr;
    /// IntPtr _methodPtrAux }` and `System.MulticastDelegate { object _invocationList;
    /// IntPtr _invocationCount }`: as .NET 10's CoreCLR CoreLib declares them. `_methodBase` is
    /// CoreLib's own cache of `Delegate.Method`, which PawPrint neither writes nor reads.
    | InvocationListAndCount of binding : DelegateBindingFields * invocations : InvocationListAndCountFields
    /// `System.Delegate { object _helperObject; object _target; IntPtr _methodPtr;
    /// IntPtr _methodPtrAux; nint _extraData }`, with `System.MulticastDelegate { }` and the nested
    /// `System.Delegate.Wrapper { Delegate Value }`: as .NET 11's CoreCLR CoreLib declares them.
    | HelperObjectAndExtraData of binding : DelegateBindingFields * helper : HelperObjectAndExtraDataFields

[<RequireQualifiedAccess>]
module DelegateLayout =

    let private isNamed
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (name : string)
        (primitive : PrimitiveType)
        : bool
        =
        field.Name = name && field.Signature = TypeDefn.PrimitiveType primitive

    let private instanceFields
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : FieldInfo<GenericParamFromMetadata, TypeDefn> list
        =
        ty.Fields |> List.filter (fun field -> not field.IsStatic)

    let private describeFields (corelib : DumpedAssembly) (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
        let scope = GenericScope.ofType ty

        let body =
            instanceFields ty
            |> List.map (fun field -> $" %s{IlFormatting.renderTypeDefn corelib scope field.Signature} %s{field.Name}")
            |> String.concat ";"

        $"%s{ty.Namespace}.%s{ty.Name} {{%s{body} }}"

    /// Whether `ty`, a type `corelib` declares, derives directly from `System.ValueType`. A CoreLib
    /// names its own `ValueType` by definition; an image compiled against a framework names it by
    /// a reference, which binds to the same type.
    let private derivesFromValueType
        (corelib : DumpedAssembly)
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : bool
        =
        match ty.BaseType with
        | Some (BaseTypeInfo.TypeDef handle) ->
            let baseType = corelib.TypeDefs.[handle]
            baseType.Namespace = "System" && baseType.Name = "ValueType"
        | Some (BaseTypeInfo.TypeRef handle) ->
            let baseType = corelib.TypeRefs.[handle]
            baseType.Namespace = "System" && baseType.Name = "ValueType"
        | Some (BaseTypeInfo.TypeSpec _)
        | None -> false

    /// The layouts `classify` recognises, for a refusal to name.
    let private describeKnown : string =
        "System.Delegate { obj _target; obj _methodBase; intptr _methodPtr; intptr _methodPtrAux } with System.MulticastDelegate { obj _invocationList; intptr _invocationCount }, or System.Delegate { obj _helperObject; obj _target; intptr _methodPtr; intptr _methodPtrAux; intptr _extraData } with System.MulticastDelegate { } and System.Delegate.Wrapper { System.Delegate Value }"

    /// The layout of `delegateType` and `multicastDelegateType`, which are to be CoreLib's
    /// `System.Delegate` and `System.MulticastDelegate`; or, naming the instance fields found, why
    /// they have none of them. `corelib` is the assembly both are read from. Only instance fields
    /// are compared, in declaration order, by name and type.
    let classify
        (corelib : DumpedAssembly)
        (delegateType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (multicastDelegateType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : Result<DelegateLayout, string>
        =
        let refusal (wrapperProblem : string option) : string =
            let wrapperProblem =
                match wrapperProblem with
                | None -> ", which is"
                | Some problem -> $", but %s{problem}, so it is"

            $"CoreLib declares %s{describeFields corelib delegateType} with %s{describeFields corelib multicastDelegateType}%s{wrapperProblem} not a delegate layout PawPrint knows how to write and read (it knows %s{describeKnown})"

        let recognised : Result<DelegateLayout, string> =
            match instanceFields delegateType, instanceFields multicastDelegateType with
            | [ target ; methodBase ; methodPtr ; methodPtrAux ], [ invocationList ; invocationCount ] when
                isNamed target "_target" PrimitiveType.Object
                && isNamed methodBase "_methodBase" PrimitiveType.Object
                && isNamed methodPtr "_methodPtr" PrimitiveType.IntPtr
                && isNamed methodPtrAux "_methodPtrAux" PrimitiveType.IntPtr
                && isNamed invocationList "_invocationList" PrimitiveType.Object
                && isNamed invocationCount "_invocationCount" PrimitiveType.IntPtr
                ->
                let binding =
                    {
                        Target = target
                        MethodPtr = methodPtr
                        MethodPtrAux = methodPtrAux
                    }

                let invocations =
                    {
                        InvocationList = invocationList
                        InvocationCount = invocationCount
                    }

                DelegateLayout.InvocationListAndCount (binding, invocations) |> Ok
            | [ helperObject ; target ; methodPtr ; methodPtrAux ; extraData ], [] when
                isNamed helperObject "_helperObject" PrimitiveType.Object
                && isNamed target "_target" PrimitiveType.Object
                && isNamed methodPtr "_methodPtr" PrimitiveType.IntPtr
                && isNamed methodPtrAux "_methodPtrAux" PrimitiveType.IntPtr
                && isNamed extraData "_extraData" PrimitiveType.IntPtr
                ->
                let wrapperName = $"%s{delegateType.Namespace}.%s{delegateType.Name}.Wrapper"

                let wrappers =
                    corelib.TypeDefs.Values
                    |> Seq.filter (fun ty -> ty.Name = "Wrapper" && ty.DeclaringType = delegateType.TypeDefHandle)
                    |> Seq.toList

                match wrappers with
                | [] ->
                    Error (
                        refusal (Some $"%s{delegateType.Namespace}.%s{delegateType.Name} declares no nested Wrapper")
                    )
                | _ :: _ :: _ ->
                    Error (
                        refusal (
                            Some
                                $"%s{delegateType.Namespace}.%s{delegateType.Name} declares %i{wrappers.Length} nested types named Wrapper"
                        )
                    )
                | [ wrapper ] when not (derivesFromValueType corelib wrapper) ->
                    // The list's elements are read as values holding `Value`.
                    Error (refusal (Some $"%s{wrapperName} is not a value type"))
                | [ wrapper ] ->

                match instanceFields wrapper with
                | [ value ] when
                    value.Name = "Value"
                    && value.Signature = TypeDefn.FromDefinition (delegateType.Identity, SignatureTypeKind.Class)
                    ->
                    let binding =
                        {
                            Target = target
                            MethodPtr = methodPtr
                            MethodPtrAux = methodPtrAux
                        }

                    let helper =
                        {
                            HelperObject = helperObject
                            ExtraData = extraData
                            WrapperValue = value
                        }

                    DelegateLayout.HelperObjectAndExtraData (binding, helper) |> Ok
                | _ ->
                    let body =
                        describeFields corelib wrapper
                        |> fun described -> described.Substring (described.IndexOf '{')

                    Error (refusal (Some $"%s{wrapperName} is %s{body}"))
            | _ -> Error (refusal None)

        recognised

    /// The layout of `baseClassTypes`' CoreLib's `Delegate` and `MulticastDelegate`, refusing an
    /// unrecognised one. Every read and write of those types' fields goes through the layout this
    /// answers.
    let require (baseClassTypes : BaseClassTypes<DumpedAssembly>) : DelegateLayout =
        match classify baseClassTypes.Corelib baseClassTypes.DelegateType baseClassTypes.MulticastDelegateType with
        | Ok layout -> layout
        | Error refusal -> failwith refusal

    /// The binding fields of `layout`, which every layout declares alike.
    let binding (layout : DelegateLayout) : DelegateBindingFields =
        match layout with
        | DelegateLayout.InvocationListAndCount (binding, _) -> binding
        | DelegateLayout.HelperObjectAndExtraData (binding, _) -> binding

    /// The identity of `field`, one of the fields of a `DelegateLayout`, as it is stored in a
    /// delegate object: keyed by the type that declares it, which is `Delegate` or
    /// `MulticastDelegate` as the layout says, never the delegate's own type.
    let fieldId
        (allConcreteTypes : AllConcreteTypes)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        : FieldId
        =
        let declaringType =
            AllConcreteTypes.findExistingNonGenericConcreteType allConcreteTypes field.DeclaringType.Identity
            |> Option.defaultWith (fun () ->
                failwith
                    $"DelegateLayout.fieldId: %s{field.DeclaringType.Namespace}.%s{field.DeclaringType.Name}, which declares %s{field.Name}, is not registered in AllConcreteTypes"
            )

        FieldIdentity.fieldId declaringType field

/// The fields of .NET 11's `System.Delegate.BindToMethodDetails`, the struct through which the
/// delegate-binding QCalls report a binding for CoreLib's managed code to write into the delegate.
/// CoreCLR's native mirror of it is `BindToMethodDetails` (comdelegate.h:28).
type BindToMethodDetailsFields =
    {
        /// `int selfReferentialTarget`: a C `BOOL`, nonzero when the delegate's `_target` is to be
        /// the delegate itself, as it is for an open delegate.
        SelfReferentialTarget : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr methodPtr`: what `_methodPtr` is to hold.
        MethodPtr : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr methodPtrAux`: what `_methodPtrAux` is to hold. Written for an open delegate
        /// only; the interop stub zeroes the struct before the call.
        MethodPtrAux : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `IntPtr extraData`: what `_extraData` is to hold, the `MethodDesc*` of the method bound.
        ExtraData : FieldInfo<GenericParamFromMetadata, TypeDefn>
        /// `GCHandle loaderAllocatorGCHandle`: a handle to the bound method's loader allocator if it
        /// is collectible, which CoreLib then stores in `_helperObject`; otherwise unallocated.
        LoaderAllocatorGcHandle : FieldInfo<GenericParamFromMetadata, TypeDefn>
    }

[<RequireQualifiedAccess>]
module BindToMethodDetailsLayout =

    let private describeKnown : string =
        "System.Delegate.BindToMethodDetails { int32 selfReferentialTarget; intptr methodPtr; intptr methodPtrAux; intptr extraData; System.Runtime.InteropServices.GCHandle loaderAllocatorGCHandle }"

    /// The fields of `detailsType`, which is to be CoreLib's `System.Delegate.BindToMethodDetails`,
    /// nested in `delegateType`; or why it is not that struct. `corelib` is the assembly both are
    /// read from. Only instance fields are compared, in declaration order, by name and type, because
    /// the QCall writes them by their offsets in the native struct, which that order decides.
    let classify
        (corelib : DumpedAssembly)
        (delegateType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (detailsType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : Result<BindToMethodDetailsFields, string>
        =
        let isNamed
            (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
            (name : string)
            (primitive : PrimitiveType)
            : bool
            =
            field.Name = name && field.Signature = TypeDefn.PrimitiveType primitive

        let isGcHandle (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) : bool =
            match field.Signature with
            | TypeDefn.FromDefinition (identity, SignatureTypeKind.ValueType) when
                identity.AssemblyFullName = corelib.DefinitionFullName
                ->
                let ty = corelib.TypeDefs.[identity.TypeDefinition.Get]
                ty.Namespace = "System.Runtime.InteropServices" && ty.Name = "GCHandle"
            | _ -> false

        let instanceFields =
            detailsType.Fields |> List.filter (fun field -> not field.IsStatic)

        let refusal (problem : string) : string =
            $"%s{detailsType.Namespace}.%s{detailsType.Name} %s{problem}, so it is not the struct PawPrint knows the delegate-binding QCalls to write (it knows %s{describeKnown})"

        if detailsType.DeclaringType <> delegateType.TypeDefHandle then
            Error (refusal $"is not nested in %s{delegateType.Namespace}.%s{delegateType.Name}")
        else

        match instanceFields with
        | [ selfReferentialTarget ; methodPtr ; methodPtrAux ; extraData ; loaderAllocatorGcHandle ] when
            isNamed selfReferentialTarget "selfReferentialTarget" PrimitiveType.Int32
            && isNamed methodPtr "methodPtr" PrimitiveType.IntPtr
            && isNamed methodPtrAux "methodPtrAux" PrimitiveType.IntPtr
            && isNamed extraData "extraData" PrimitiveType.IntPtr
            && loaderAllocatorGcHandle.Name = "loaderAllocatorGCHandle"
            && isGcHandle loaderAllocatorGcHandle
            ->
            Ok
                {
                    SelfReferentialTarget = selfReferentialTarget
                    MethodPtr = methodPtr
                    MethodPtrAux = methodPtrAux
                    ExtraData = extraData
                    LoaderAllocatorGcHandle = loaderAllocatorGcHandle
                }
        | fields ->
            let described =
                fields
                |> List.map (fun field -> $"%s{field.Name} : %O{field.Signature}")
                |> String.concat "; "

            Error (refusal $"declares the instance fields {{ %s{described} }}")
