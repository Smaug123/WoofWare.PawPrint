namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// PawPrint's stand-in for CoreCLR's `RuntimeFieldHandle::GetRVAFieldInfo`
/// (runtimehandles.cpp), which resolves a `FieldDesc*` to the address and size of the
/// static data the field's RVA points at. Two callers need exactly this: the
/// `RuntimeFieldHandle_GetRVAFieldInfo` QCall that CoreLib's reflection paths go through,
/// and the `RuntimeHelpers.InitializeArray` intrinsic, which on CoreCLR calls that same
/// QCall before copying. Keeping one definition means the intrinsic and the QCall cannot
/// disagree about how big an RVA field is.
[<RequireQualifiedAccess>]
module internal FieldRvaData =
    /// The declaring assembly and metadata row for a field handle. Fails loudly if the
    /// handle names an assembly or field row that is not loaded: a `FieldHandle` is only
    /// ever minted from a resolved field, so either would be an interpreter bug rather
    /// than anything the guest can provoke.
    let fieldForHandle
        (operation : string)
        (fieldHandle : FieldHandle)
        (state : IlMachineState)
        : DumpedAssembly * FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        let assemblyFullName = fieldHandle.GetAssemblyFullName ()

        let assembly =
            state.LoadedAssembly' assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

        let fieldDefinitionHandle = fieldHandle.GetFieldDefinitionHandle().Get

        let fieldInfo =
            match assembly.Fields.TryGetValue fieldDefinitionHandle with
            | true, fieldInfo -> fieldInfo
            | false, _ -> failwith $"%s{operation}: field %O{fieldDefinitionHandle} not found in %s{assemblyFullName}"

        assembly, fieldInfo

    /// `None` exactly when CoreCLR's `GetRVAFieldInfo` returns FALSE, i.e. when the field
    /// has no RVA. `Some` carries the byte range the RVA names, sized by the field's
    /// declared type — the same `pFD->LoadSize()` CoreCLR reports, and the bound callers
    /// must respect before reading.
    let tryGet
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (fieldHandle : FieldHandle)
        (state : IlMachineState)
        : IlMachineState * PeByteRangePointer option
        =
        let assembly, fieldInfo = fieldForHandle operation fieldHandle state

        // RVA fields live on non-generic declaring types — `[FieldOffset]`/RVA
        // initialisers cannot reference a generic typedef parameter. With the
        // canonical FieldHandle declaring type model, "non-generic" means
        // `Closed` (a generic declaring type would be `OpenGenericTypeDefinition`,
        // for which RVA layout is not even definable). Reject other shapes
        // loudly rather than silently fabricating empty generics.
        let typeGenerics =
            match fieldHandle.GetDeclaringTypeHandle () with
            | RuntimeTypeHandleTarget.Closed declaringTypeHandle ->
                match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
                | Some declaringType -> declaringType.Generics
                | None ->
                    failwith
                        $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so RVA field size cannot be computed"
            | other ->
                failwith
                    $"%s{operation}: RVA field's declaring type is %O{other}; expected a Closed concrete type. RVA fields cannot live on a generic typedef."

        IlMachineState.peByteRangeForFieldRva loggerFactory baseClassTypes assembly fieldInfo typeGenerics state
