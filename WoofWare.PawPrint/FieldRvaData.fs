namespace WoofWare.PawPrint

open System.Collections.Immutable
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

        // Sizing the field means concretising its signature, which needs the declaring type's
        // instantiation. CoreCLR's `GetRVAFieldInfo` asks for no such thing — it only tests
        // `FieldDesc::IsRVA` — and nothing forbids an RVA static on a generic type: the type
        // loader rejects only RVA-plus-thread-static and RVA-plus-GC-references
        // (methodtablebuilder.cpp:4129 and :4500). So an `OpenGenericTypeDefinition` declaring
        // type has to be sized, not rejected.
        //
        // It can be. An RVA static is one blob at a fixed spot in the image, resolved from the
        // module rather than from a per-instantiation static block (`FieldDesc::
        // GetStaticAddressHandle`, field.cpp:247), so every instantiation shares the same bytes
        // and the field's type cannot depend on the type arguments. That makes the empty
        // instantiation the right context to size it in — and the guard below turns an image
        // that violates the premise into a diagnosis rather than an index-out-of-range from
        // deep inside `TypeResolution`.
        let typeGenerics =
            match fieldHandle.GetDeclaringTypeHandle () with
            | RuntimeTypeHandleTarget.Closed declaringTypeHandle ->
                match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
                | Some declaringType -> declaringType.Generics
                | None ->
                    failwith
                        $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so RVA field size cannot be computed"
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> ImmutableArray.Empty
            | other ->
                failwith
                    $"%s{operation}: RVA field's declaring type is %O{other}; expected a concrete type or an open generic type definition. A field cannot be declared on a generic parameter."

        if
            IlMachineTypeResolution.containsUnboundGenericParameter
                typeGenerics
                ImmutableArray.Empty
                fieldInfo.Signature
        then
            failwith
                $"%s{operation}: RVA field %s{fieldInfo.Name} on %s{assembly.Name.Name} has a signature mentioning a generic parameter its declaring type supplies no argument for. An RVA static is shared by every instantiation, so its type cannot legally depend on the type arguments."

        IlMachineState.peByteRangeForFieldRva loggerFactory baseClassTypes assembly fieldInfo typeGenerics state
