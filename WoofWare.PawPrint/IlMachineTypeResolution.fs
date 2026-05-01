namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
module IlMachineTypeResolution =
    /// <summary>
    /// Create a new IlMachineState which has loaded the given assembly.
    /// This involves reading assemblies from the disk and doing a complete parse of them, so it might be quite slow!
    ///
    /// This function doesn't do anything if the referenced assembly has already been loaded.
    /// </summary>
    /// <param name="loggerFactory">LoggerFactory into which to emit logs.</param>
    /// <param name="referencedInAssembly">The assembly which contains an AssemblyReference which causes us to want to load a new assembly.</param>
    /// <param name="r">The AssemblyReferenceHandle pointing at an assembly we want to load. *Important*: this is an AssemblyReferenceHandle from <c>referencedInAssembly</c>; in general, AssemblyReferenceHandles are only well-defined if you know what assembly they were defined in.</param>
    /// <param name="state">The immutable state to augment with the new assembly.</param>
    let loadAssembly
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * AssemblyName
        =
        let assemblies, dumped, assyName =
            TypeResolution.loadAssembly
                loggerFactory
                state.DotnetRuntimeDirs
                referencedInAssembly
                r
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        dumped,
        assyName

    let internal loader (loggerFactory : ILoggerFactory) (state : IlMachineState) : IAssemblyLoad =
        { new IAssemblyLoad with
            member _.LoadAssembly loaded assyName ref =
                let assemblies, targetAssy, _name =
                    TypeResolution.loadAssembly
                        loggerFactory
                        state.DotnetRuntimeDirs
                        loaded.[assyName.FullName]
                        ref
                        loaded

                assemblies, targetAssy
        }

    let concretizeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (declaringAssembly : AssemblyName)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (ty : TypeDefn)
        : IlMachineState * ConcreteTypeHandle
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let handle, ctx =
            TypeConcretization.concretizeType
                ctx
                (loader loggerFactory state)
                declaringAssembly
                typeGenerics
                methodGenerics
                ty

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, handle

    let internal resolveTopLevelTypeFromName
        (loggerFactory : ILoggerFactory)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTopLevelTypeFromName
                loggerFactory
                state.DotnetRuntimeDirs
                ns
                name
                genericArgs
                assy
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromExport
                loggerFactory
                state.DotnetRuntimeDirs
                fromAssembly
                ty
                genericArgs
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromRef
                loggerFactory
                state.DotnetRuntimeDirs
                referencedInAssembly
                target
                typeGenericArgs
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveType
        (loggerFactory : ILoggerFactory)
        (ty : TypeReferenceHandle)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveType loggerFactory state.DotnetRuntimeDirs ty genericArgs assy state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveTypeFromDefn
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromDefn
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                ty
                typeGenericArgs
                methodGenericArgs
                assy
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let resolveTypeFromSpec
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : TypeDefn ImmutableArray)
        (methodGenericArgs : TypeDefn ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromSpec
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                ty
                assy
                typeGenericArgs
                methodGenericArgs
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    /// Resolve a TypeSpecification using concrete type handles from execution context
    let resolveTypeFromSpecConcrete
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : ConcreteTypeHandle ImmutableArray)
        (methodGenericArgs : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let sign = assy.TypeSpecs.[ty].Signature

        // Convert ConcreteTypeHandle to TypeDefn
        let typeGenericArgsAsDefn =
            typeGenericArgs
            |> ImmutableArray.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )

        let methodGenericArgsAsDefn =
            methodGenericArgs
            |> ImmutableArray.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )

        resolveTypeFromDefn loggerFactory baseClassTypes sign typeGenericArgsAsDefn methodGenericArgsAsDefn assy state

    /// Resolve a TypeDefinition using concrete type handles from execution context
    let resolveTypeFromDefnConcrete
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefinitionHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : ConcreteTypeHandle ImmutableArray)
        (methodGenericArgs : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let typeDef = assy.TypeDefs.[ty]

        // Convert ConcreteTypeHandle to TypeDefn for the generics
        let typeGenericArgsAsDefn =
            typeGenericArgs
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let methodGenericArgsAsDefn =
            methodGenericArgs
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        // Map the type definition's generics using the provided type generic arguments
        let resolvedTypeDef =
            typeDef
            |> TypeInfo.mapGeneric (fun (param, _) ->
                if param.SequenceNumber < typeGenericArgsAsDefn.Length then
                    typeGenericArgsAsDefn.[param.SequenceNumber]
                else
                    failwithf "Generic type parameter %d out of range" param.SequenceNumber
            )

        state, assy, resolvedTypeDef

    let private isSequentialOpenGenericDefinitionArgs (args : ImmutableArray<TypeDefn>) : bool =
        args.Length > 0
        && (args
            |> Seq.mapi (fun i arg ->
                match arg with
                | TypeDefn.GenericTypeParameter index -> index = i
                | TypeDefn.GenericMethodParameter _
                | TypeDefn.PrimitiveType _
                | TypeDefn.Array _
                | TypeDefn.Pinned _
                | TypeDefn.Pointer _
                | TypeDefn.Byref _
                | TypeDefn.OneDimensionalArrayLowerBoundZero _
                | TypeDefn.Modified _
                | TypeDefn.FromReference _
                | TypeDefn.FromDefinition _
                | TypeDefn.GenericInstantiation _
                | TypeDefn.FunctionPointer _
                | TypeDefn.Void -> false
            )
            |> Seq.forall id)

    let rec private containsUnboundGenericParameter
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (ty : TypeDefn)
        : bool
        =
        let contains = containsUnboundGenericParameter typeGenerics methodGenerics

        match ty with
        | TypeDefn.GenericTypeParameter index -> index >= typeGenerics.Length
        | TypeDefn.GenericMethodParameter index -> index >= methodGenerics.Length
        | TypeDefn.Array (element, _)
        | TypeDefn.Pinned element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> contains element
        | TypeDefn.Modified (original, modifier, _) -> contains original || contains modifier
        | TypeDefn.GenericInstantiation (generic, args) -> contains generic || (args |> Seq.exists contains)
        | TypeDefn.FunctionPointer signature ->
            let returnContains =
                match signature.ReturnType with
                | MethodReturnType.Void -> false
                | MethodReturnType.Returns ret -> contains ret

            returnContains || (signature.ParameterTypes |> List.exists contains)
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.Void -> false

    let private tryResolveOpenGenericDefinitionTarget
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssembly : DumpedAssembly)
        (state : IlMachineState)
        (genericDef : TypeDefn)
        (args : ImmutableArray<TypeDefn>)
        : IlMachineState * RuntimeTypeHandleTarget option
        =
        if not (isSequentialOpenGenericDefinitionArgs args) then
            state, None
        else
            let state, identity, arity =
                match genericDef with
                | TypeDefn.FromDefinition (identity, _) ->
                    let assembly =
                        match state.LoadedAssembly identity.Assembly with
                        | Some assembly -> assembly
                        | None ->
                            failwithf
                                "Open generic type definition %s was not loaded while resolving ldtoken"
                                identity.AssemblyFullName

                    let typeDef = Assembly.resolveTypeIdentityDefinition assembly identity
                    state, identity, typeDef.Generics.Length
                | TypeDefn.FromReference _ ->
                    let state, _, resolved =
                        resolveTypeFromDefn
                            loggerFactory
                            baseClassTypes
                            genericDef
                            ImmutableArray<TypeDefn>.Empty
                            ImmutableArray<TypeDefn>.Empty
                            declaringAssembly
                            state

                    state, resolved.Identity, resolved.Generics.Length
                | TypeDefn.PrimitiveType _
                | TypeDefn.Array _
                | TypeDefn.Pinned _
                | TypeDefn.Pointer _
                | TypeDefn.Byref _
                | TypeDefn.OneDimensionalArrayLowerBoundZero _
                | TypeDefn.Modified _
                | TypeDefn.GenericInstantiation _
                | TypeDefn.FunctionPointer _
                | TypeDefn.GenericTypeParameter _
                | TypeDefn.GenericMethodParameter _
                | TypeDefn.Void -> failwith $"Unsupported open generic definition token shape: %O{genericDef}"

            if arity = args.Length then
                state, Some (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity)
            else
                failwithf
                    "Open generic type definition arity mismatch during ldtoken: definition has arity %i but token supplied %i argument placeholders"
                    arity
                    args.Length

    let runtimeTypeHandleTargetForTypeToken
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssembly : DumpedAssembly)
        (allowOpenGenericDefinition : bool)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (ty : TypeDefn)
        (state : IlMachineState)
        : IlMachineState * RuntimeTypeHandleTarget
        =
        let openGenericDefinitionTarget =
            if allowOpenGenericDefinition then
                match ty with
                | TypeDefn.GenericInstantiation (genericDef, args) ->
                    tryResolveOpenGenericDefinitionTarget
                        loggerFactory
                        baseClassTypes
                        declaringAssembly
                        state
                        genericDef
                        args
                | _ -> state, None
            else
                state, None

        match openGenericDefinitionTarget with
        | state, Some target -> state, target
        | state, None when containsUnboundGenericParameter typeGenerics methodGenerics ty ->
            match ty with
            | TypeDefn.GenericTypeParameter index ->
                failwith
                    $"TODO: ldtoken for unbound generic type parameter !%i{index} is not implemented. GenericParameter metadata tokens live in table 0x2A; add a RuntimeTypeHandleTarget generic-parameter case rather than returning a TypeDef token."
            | TypeDefn.GenericMethodParameter index ->
                failwith
                    $"TODO: ldtoken for unbound generic method parameter !!%i{index} is not implemented. GenericParameter metadata tokens live in table 0x2A; add a RuntimeTypeHandleTarget generic-parameter case rather than returning a TypeDef token."
            | TypeDefn.GenericInstantiation (genericDef, args) ->
                match
                    tryResolveOpenGenericDefinitionTarget
                        loggerFactory
                        baseClassTypes
                        declaringAssembly
                        state
                        genericDef
                        args
                with
                | state, Some target -> state, target
                | state, None ->
                    failwith
                        $"TODO: ldtoken for open constructed generic type is not implemented. Type token was %O{ty}"
            | _ ->
                failwith
                    $"TODO: ldtoken for type token with unbound generic parameters is not implemented. Type token was %O{ty}"
        | state, None ->
            let state, handle =
                concretizeType loggerFactory baseClassTypes state declaringAssembly.Name typeGenerics methodGenerics ty

            state, RuntimeTypeHandleTarget.Closed handle

    /// Get zero value for a type that's already been concretized
    let cliTypeZeroOfHandle
        (state : IlMachineState)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        : CliType * IlMachineState
        =
        let zero, updatedConcreteTypes =
            CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes handle

        let newState =
            { state with
                ConcreteTypes = updatedConcreteTypes
            }

        zero, newState

    /// Concretize a ConcreteType<TypeDefn> to get a ConcreteTypeHandle for static field access
    let concretizeFieldDeclaringType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringType : ConcreteType<TypeDefn>)
        (state : IlMachineState)
        : ConcreteTypeHandle * IlMachineState
        =
        // Create a concretization context from the current state
        let ctx : TypeConcretization.ConcretizationContext<_> =
            {
                ConcreteTypes = state.ConcreteTypes
                LoadedAssemblies = state._LoadedAssemblies
                BaseTypes = baseClassTypes
            }

        // Concretize each generic argument first
        let mutable currentCtx = ctx
        let genericHandles = ImmutableArray.CreateBuilder declaringType.Generics.Length

        for genericArg in declaringType.Generics do
            let handle, newCtx =
                TypeConcretization.concretizeType
                    currentCtx
                    (loader loggerFactory state)
                    declaringType.Assembly
                    ImmutableArray.Empty // No type generics in this context
                    ImmutableArray.Empty // No method generics in this context
                    genericArg

            currentCtx <- newCtx
            genericHandles.Add handle

        // Now we need to concretize the type definition itself
        // If it's a non-generic type, we can use concretizeTypeDefinition directly
        if declaringType.Generics.IsEmpty then
            let handle, currentCtx =
                TypeConcretization.concretizeTypeDefinition currentCtx declaringType.Identity

            let newState =
                { state with
                    ConcreteTypes = currentCtx.ConcreteTypes
                    _LoadedAssemblies = currentCtx.LoadedAssemblies
                }

            handle, newState
        else
            // For generic types, we need to check if this concrete instantiation already exists
            let genericHandles = genericHandles.ToImmutable ()

            match
                AllConcreteTypes.findExistingConcreteType currentCtx.ConcreteTypes declaringType.Identity genericHandles
            with
            | Some handle ->
                // Type already exists, just return it
                handle,
                { state with
                    ConcreteTypes = currentCtx.ConcreteTypes
                    _LoadedAssemblies = currentCtx.LoadedAssemblies
                }
            | None ->
                // Create the concrete type using mapGeneric to transform from TypeDefn to ConcreteTypeHandle
                let concreteTypeWithHandles =
                    declaringType |> ConcreteType.mapGeneric (fun i _ -> genericHandles.[i])

                // Add to the concrete types
                let handle, newConcreteTypes =
                    AllConcreteTypes.add concreteTypeWithHandles currentCtx.ConcreteTypes

                // Update the state with the new concrete types
                let newState =
                    { state with
                        ConcreteTypes = newConcreteTypes
                        _LoadedAssemblies = currentCtx.LoadedAssemblies
                    }

                handle, newState

    /// Get zero value for a TypeDefn, concretizing it first
    let cliTypeZeroOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assy : DumpedAssembly)
        (ty : TypeDefn)
        (typeGenerics : ConcreteTypeHandle ImmutableArray)
        (methodGenerics : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * CliType * ConcreteTypeHandle
        =

        // First concretize the type
        // Make sure the current assembly is included in the state for concretization
        let state =
            if state.LoadedAssembly assy.Name |> Option.isSome then
                state
            else
                state.WithLoadedAssembly assy.Name assy

        let state, handle =
            concretizeType loggerFactory baseClassTypes state assy.Name typeGenerics methodGenerics ty

        // Now get the zero value
        let zero, state = cliTypeZeroOfHandle state baseClassTypes handle
        state, zero, handle

    let ensureByteConcreteType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState * ConcreteType<ConcreteTypeHandle>
        =
        let byteTypeDefn =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Byte

        let state, byteHandle =
            concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                byteTypeDefn

        let byteType =
            AllConcreteTypes.lookup byteHandle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith "System.Byte was not present after concretization")

        state, byteType

    let peByteRangeForFieldRva
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assembly : DumpedAssembly)
        (field : FieldInfo<'typeGeneric, TypeDefn>)
        (typeGenerics : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        : IlMachineState * PeByteRangePointer option
        =
        match field.RelativeVirtualAddress with
        | None -> state, None
        | Some fieldRva ->
            let state, zero, _fieldType =
                cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    assembly
                    field.Signature
                    typeGenerics
                    ImmutableArray.Empty
                    state

            let data =
                {
                    AssemblyFullName = assembly.Name.FullName
                    Source = PeByteRangePointerSource.FieldRva (ComparableFieldDefinitionHandle.Make field.Handle)
                    RelativeVirtualAddress = fieldRva
                    Size = CliType.sizeOf zero
                }

            state, Some data

    let peByteRangePointer
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (peByteRange : PeByteRangePointer)
        (state : IlMachineState)
        : IlMachineState * ManagedPointerSource
        =
        let state, byteType = ensureByteConcreteType loggerFactory baseClassTypes state

        state,
        ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, [ ByrefProjection.ReinterpretAs byteType ])
