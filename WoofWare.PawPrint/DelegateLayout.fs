namespace WoofWare.PawPrint

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

    /// The layouts `classify` recognises, for a refusal to name.
    let private describeKnown : string =
        "System.Delegate { obj _target; obj _methodBase; intptr _methodPtr; intptr _methodPtrAux } with System.MulticastDelegate { obj _invocationList; intptr _invocationCount }"

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
        let recognised =
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

                DelegateLayout.InvocationListAndCount (binding, invocations) |> Some
            | _ -> None

        match recognised with
        | Some layout -> Ok layout
        | None ->
            Error
                $"CoreLib declares %s{describeFields corelib delegateType} with %s{describeFields corelib multicastDelegateType}, which is not a delegate layout PawPrint knows how to write and read (it knows %s{describeKnown})"

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
