namespace WoofWare.PawPrint

/// What the type handle behind an `ELEMENT_TYPE_INTERNAL` run in a `Reflection.Emit` signature is,
/// in the terms <see cref="DynamicSignatureDecoding" /> needs.
[<RequireQualifiedAccess>]
module InternalSignatureTypeResolution =

    /// <summary>
    /// The type <paramref name="source" /> names, which is the <c>RuntimeTypeHandle.Value</c>
    /// <c>SignatureHelper.InternalAddRuntimeType</c> copied into the blob.
    /// </summary>
    /// <remarks>
    /// Reads only what the handle already is: a guest holding the handle has already loaded the
    /// type, so this loads nothing and runs no class constructor. <c>SignatureHelper</c> writes a
    /// run only for a type with no generic parameters of its own or for the definition under a
    /// <c>GENERICINST</c>, so any other handle is refused rather than decoded.
    /// </remarks>
    let ofHandle
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (source : NativeIntSource)
        : InternalSignatureType
        =
        let definitionOf (identity : ResolvedTypeIdentity) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            let assembly =
                state.LoadedAssembly identity.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: an ELEMENT_TYPE_INTERNAL run names %O{identity}, whose assembly %s{identity.AssemblyFullName} is not loaded"
                )

            assembly.TypeDefs.[identity.TypeDefinition.Get]

        let kindOf (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>) =
            LoadedTypeInfo.signatureTypeKind baseClassTypes state._LoadedAssemblies definition

        match source with
        | NativeIntSource.TypeHandlePtr target ->
            match target with
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
                let concrete =
                    AllConcreteTypes.lookup handle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: an ELEMENT_TYPE_INTERNAL run names %O{handle}, which is not a registered concrete type"
                    )

                if not concrete.Generics.IsEmpty then
                    failwith
                        $"%s{operation}: an ELEMENT_TYPE_INTERNAL run names the constructed generic %O{concrete}; SignatureHelper spells one as GENERICINST over its definition, so this blob is not its output"

                let definition = definitionOf concrete.Identity

                if not definition.Generics.IsEmpty then
                    failwith
                        $"BUG: %s{operation}: %O{handle} is registered with no generic arguments, but its definition %O{concrete.Identity} has %d{definition.Generics.Length} generic parameter(s)"

                InternalSignatureType.NonGeneric (concrete.Identity, kindOf definition)
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                let definition = definitionOf identity

                InternalSignatureType.GenericDefinition (identity, kindOf definition, definition.Generics.Length)
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _)
            | RuntimeTypeHandleTarget.FunctionPointer _ ->
                failwith
                    $"TODO: %s{operation}: an ELEMENT_TYPE_INTERNAL run names the function pointer type %O{target}. SignatureHelper has no encoding for one and writes its handle instead, which CoreCLR accepts; PawPrint does not yet decode it"
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _)
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _)
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _
            | RuntimeTypeHandleTarget.OpenConstructed _
            | RuntimeTypeHandleTarget.Composite _
            | RuntimeTypeHandleTarget.DynamicMethodsClass _ ->
                failwith
                    $"%s{operation}: an ELEMENT_TYPE_INTERNAL run names %O{target}; SignatureHelper spells a byref, pointer, array or generic parameter structurally and never writes a run for one, so this blob is not its output"
        | other ->
            failwith
                $"%s{operation}: an ELEMENT_TYPE_INTERNAL run names %O{other}, which is not a type handle; RuntimeTypeHandle.Value is the only native int SignatureHelper copies into a blob"
