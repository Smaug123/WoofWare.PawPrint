namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type IlMachineState =
    {
        ConcreteTypes : AllConcreteTypes
        Logger : ILogger
        NextThreadId : int
        // CallStack : StackFrame list
        /// Multiple managed heaps are allowed, but we hopefully only need one.
        ManagedHeap : ManagedHeap
        ThreadState : Map<ThreadId, ThreadState>
        InternedStrings : ImmutableDictionary<StringToken, ManagedHeapAddress>
        /// Keyed by FullName. (Sometimes an assembly has a PublicKey when we read it from the disk, but we
        /// only have a reference to it by an AssemblyName without a PublicKey.)
        _LoadedAssemblies : ImmutableDictionary<string, DumpedAssembly>
        /// Tracks initialization state of types across assemblies
        TypeInitTable : TypeInitTable
        /// For each type, specialised to each set of generic args, a map of string field name to static value contained therein.
        _Statics : ImmutableDictionary<ConcreteTypeHandle, ImmutableDictionary<string, CliType>>
        DotnetRuntimeDirs : string ImmutableArray
        TypeHandles : TypeHandleRegistry
        FieldHandles : FieldHandleRegistry
    }

    member this.WithTypeBeginInit (thread : ThreadId) (ty : ConcreteTypeHandle) =
        let concreteType = AllConcreteTypes.lookup ty this.ConcreteTypes |> Option.get

        this.Logger.LogDebug (
            "Beginning initialisation of type {s_Assembly}.{TypeName}, handle {TypeDefinitionHandle}",
            concreteType.Assembly.FullName,
            this.LoadedAssembly(concreteType.Assembly).Value.TypeDefs.[concreteType.Definition.Get].Name,
            concreteType.Definition.Get.GetHashCode ()
        )

        let typeInitTable = this.TypeInitTable |> TypeInitTable.beginInitialising thread ty

        { this with
            TypeInitTable = typeInitTable
        }

    member this.WithTypeEndInit (thread : ThreadId) (ty : ConcreteTypeHandle) =
        let concreteType = AllConcreteTypes.lookup ty this.ConcreteTypes |> Option.get

        this.Logger.LogDebug (
            "Marking complete initialisation of type {s_Assembly}.{TypeName}, handle {TypeDefinitionHandle}",
            concreteType.Assembly.FullName,
            this.LoadedAssembly(concreteType.Assembly).Value.TypeDefs.[concreteType.Definition.Get].Name,
            concreteType.Definition.Get.GetHashCode ()
        )

        let typeInitTable = this.TypeInitTable |> TypeInitTable.markInitialised thread ty

        { this with
            TypeInitTable = typeInitTable
        }

    member this.WithLoadedAssembly (name : AssemblyName) (value : DumpedAssembly) =
        { this with
            _LoadedAssemblies = this._LoadedAssemblies.Add (name.FullName, value)
        }

    member this.LoadedAssembly' (fullName : string) : DumpedAssembly option =
        match this._LoadedAssemblies.TryGetValue fullName with
        | false, _ -> None
        | true, v -> Some v

    member this.LoadedAssembly (name : AssemblyName) : DumpedAssembly option = this.LoadedAssembly' name.FullName

    /// Returns also the original assembly name.
    member this.WithThreadSwitchedToAssembly (assy : AssemblyName) (thread : ThreadId) : IlMachineState * AssemblyName =
        let mutable existing = Unchecked.defaultof<AssemblyName>

        let newState =
            this.ThreadState
            |> Map.change
                thread
                (fun s ->
                    match s with
                    | None -> failwith $"expected thread {thread} to be in a state already; internal logic error"
                    | Some s ->
                        existing <- s.ActiveAssembly

                        { s with
                            ActiveAssembly = assy
                        }
                        |> Some
                )

        { this with
            ThreadState = newState
        },
        existing

    member this.ActiveAssembly (thread : ThreadId) =
        let active = this.ThreadState.[thread].ActiveAssembly

        match this.LoadedAssembly active with
        | Some v -> v
        | None ->
            let available = this._LoadedAssemblies.Keys |> String.concat " ; "

            failwith
                $"Somehow we believe the active assembly is {active}, but only had the following available: {available}"

(*
Type load algorithm, from II.10.5.3.3
1. At class load-time (hence prior to initialization time) store zero or null into all static fields of the
type.
2. If the type is initialized, you are done.
2.1. If the type is not yet initialized, try to take an initialization lock.
2.2. If successful, record this thread as responsible for initializing the type and proceed to step 2.3.
2.2.1. If not successful, see whether this thread or any thread waiting for this thread to complete already
holds the lock.
2.2.2. If so, return since blocking would create a deadlock. This thread will now see an incompletely
initialized state for the type, but no deadlock will arise.
2.2.3 If not, block until the type is initialized then return.
2.3 Initialize the base class type and then all interfaces implemented by this type.
2.4 Execute the type initialization code for this type.
2.5 Mark the type as initialized, release the initialization lock, awaken any threads waiting for this type
to be initialized, and return.
*)
type WhatWeDid =
    | Executed
    /// We didn't run what you wanted, because we have to do class initialisation first.
    | SuspendedForClassInit
    /// We can't proceed until this thread has finished the class initialisation work it's doing.
    | BlockedOnClassInit of threadBlockingUs : ThreadId

type ExecutionResult =
    | Terminated of IlMachineState * terminatingThread : ThreadId
    | Stepped of IlMachineState * WhatWeDid

type StateLoadResult =
    /// The type is loaded; you can proceed.
    | NothingToDo of IlMachineState
    /// We didn't manage to load the requested type, because that type itself requires first loading something.
    /// The state we give you is ready to load that something.
    | FirstLoadThis of IlMachineState

[<RequireQualifiedAccess>]
module IlMachineState =
    type private Dummy = class end

    let concretizeType
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
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let handle, ctx =
            TypeConcretization.concretizeType
                ctx
                (fun assyName ref ->
                    let currentAssy = state.LoadedAssembly assyName |> Option.get

                    let targetAssy =
                        currentAssy.AssemblyReferences.[ref].Name |> state.LoadedAssembly |> Option.get

                    state._LoadedAssemblies, targetAssy
                )
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
        let assemblyRef = referencedInAssembly.AssemblyReferences.[r]
        let assemblyName = assemblyRef.Name

        match state.LoadedAssembly assemblyName with
        | Some v -> state, v, assemblyName
        | None ->
            let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

            let assy =
                state.DotnetRuntimeDirs
                |> Seq.choose (fun dir ->
                    let file = Path.Combine (dir, assemblyName.Name + ".dll")

                    try
                        use f = File.OpenRead file
                        logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", file)
                        Assembly.read loggerFactory (Some file) f |> Some
                    with :? FileNotFoundException ->
                        None
                )
                |> Seq.toList

            match assy |> List.tryHead with
            | None -> failwith $"Could not find a readable DLL in any runtime dir with name %s{assemblyName.Name}.dll"
            | Some assy ->

            state.WithLoadedAssembly assemblyName assy, assy, assemblyName

    let rec internal resolveTypeFromName
        (loggerFactory : ILoggerFactory)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeFromName assy state._LoadedAssemblies ns name genericArgs with
        | TypeResolutionResult.Resolved (assy, typeDef) -> state, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let state, _, _ =
                loadAssembly
                    loggerFactory
                    state._LoadedAssemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    state

            resolveTypeFromName loggerFactory ns name genericArgs assy state

    and resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeFromExport fromAssembly state._LoadedAssemblies ty genericArgs with
        | TypeResolutionResult.Resolved (assy, typeDef) -> state, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let state, targetAssy, _ =
                loadAssembly
                    loggerFactory
                    state._LoadedAssemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    state

            resolveTypeFromName loggerFactory ty.Namespace ty.Name genericArgs targetAssy state

    and resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeRef state._LoadedAssemblies referencedInAssembly target typeGenericArgs with
        | TypeResolutionResult.Resolved (assy, typeDef) -> state, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let state, _, _ =
                loadAssembly
                    loggerFactory
                    state._LoadedAssemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    state

            resolveTypeFromRef loggerFactory referencedInAssembly target typeGenericArgs state

    and resolveType
        (loggerFactory : ILoggerFactory)
        (ty : TypeReferenceHandle)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let target = assy.TypeRefs.[ty]

        resolveTypeFromRef loggerFactory assy target genericArgs state

    let rec resolveTypeFromDefn
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, args) ->
            let args' = ImmutableArray.CreateBuilder ()

            let state =
                (state, args)
                ||> Seq.fold (fun state arg ->
                    let state, assy, resolvedArg =
                        resolveTypeFromDefn
                            loggerFactory
                            baseClassTypes
                            arg
                            typeGenericArgs
                            methodGenericArgs
                            assy
                            state

                    // If the resolved argument has generics, create a GenericInstantiation
                    // Otherwise, create a FromDefinition
                    let preservedArg =
                        let baseType =
                            resolvedArg.BaseType
                            |> DumpedAssembly.resolveBaseType baseClassTypes state._LoadedAssemblies assy.Name

                        let signatureTypeKind =
                            match baseType with
                            | ResolvedBaseType.Enum
                            | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                            | ResolvedBaseType.Object -> SignatureTypeKind.Class
                            | ResolvedBaseType.Delegate -> SignatureTypeKind.Class

                        if resolvedArg.Generics.IsEmpty then
                            TypeDefn.FromDefinition (
                                ComparableTypeDefinitionHandle.Make resolvedArg.TypeDefHandle,
                                assy.Name.FullName,
                                signatureTypeKind
                            )
                        else
                            // Preserve the generic instantiation
                            let genericDef =
                                TypeDefn.FromDefinition (
                                    ComparableTypeDefinitionHandle.Make resolvedArg.TypeDefHandle,
                                    assy.Name.FullName,
                                    signatureTypeKind
                                )

                            TypeDefn.GenericInstantiation (genericDef, resolvedArg.Generics)

                    args'.Add preservedArg

                    state
                )

            let args' = args'.ToImmutable ()
            resolveTypeFromDefn loggerFactory baseClassTypes generic args' methodGenericArgs assy state
        | TypeDefn.FromDefinition (defn, assy, _typeKind) ->
            let assy = state._LoadedAssemblies.[assy]

            let defn =
                assy.TypeDefs.[defn.Get]
                |> TypeInfo.mapGeneric (fun (param, _) -> typeGenericArgs.[param.SequenceNumber])

            state, assy, defn
        | TypeDefn.FromReference (ref, _typeKind) ->
            let state, assy, ty =
                resolveTypeFromRef loggerFactory assy ref typeGenericArgs state

            state, assy, ty
        | TypeDefn.PrimitiveType prim ->
            let ty =
                match prim with
                | PrimitiveType.Boolean -> baseClassTypes.Boolean
                | PrimitiveType.Char -> baseClassTypes.Char
                | PrimitiveType.SByte -> baseClassTypes.SByte
                | PrimitiveType.Byte -> baseClassTypes.Byte
                | PrimitiveType.Int16 -> baseClassTypes.Int16
                | PrimitiveType.UInt16 -> baseClassTypes.UInt16
                | PrimitiveType.Int32 -> baseClassTypes.Int32
                | PrimitiveType.UInt32 -> baseClassTypes.UInt32
                | PrimitiveType.Int64 -> baseClassTypes.Int64
                | PrimitiveType.UInt64 -> baseClassTypes.UInt64
                | PrimitiveType.Single -> baseClassTypes.Single
                | PrimitiveType.Double -> baseClassTypes.Double
                | PrimitiveType.String -> baseClassTypes.String
                | PrimitiveType.TypedReference -> failwith "todo"
                | PrimitiveType.IntPtr -> baseClassTypes.IntPtr
                | PrimitiveType.UIntPtr -> baseClassTypes.UIntPtr
                | PrimitiveType.Object -> baseClassTypes.Object
                |> TypeInfo.mapGeneric (fun _ -> failwith "none of these types are generic")

            state, baseClassTypes.Corelib, ty
        | TypeDefn.GenericTypeParameter param ->
            let arg = typeGenericArgs.[param]
            // TODO: this assembly is probably wrong?
            resolveTypeFromDefn loggerFactory baseClassTypes arg typeGenericArgs methodGenericArgs assy state
        | TypeDefn.GenericMethodParameter param ->
            let arg = methodGenericArgs.[param]
            // TODO: this assembly is probably wrong?
            resolveTypeFromDefn loggerFactory baseClassTypes arg typeGenericArgs methodGenericArgs assy state
        | s -> failwith $"TODO: resolveTypeFromDefn unimplemented for {s}"

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
        let sign = assy.TypeSpecs.[ty].Signature
        resolveTypeFromDefn loggerFactory baseClassTypes sign typeGenericArgs methodGenericArgs assy state

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

    /// Helper to get ConcreteTypeHandle from ConcreteType<ConcreteTypeHandle> during migration
    let getConcreteTypeHandle
        (ct : ConcreteType<ConcreteTypeHandle>)
        (state : IlMachineState)
        : ConcreteTypeHandle option
        =
        AllConcreteTypes.lookup' ct state.ConcreteTypes

    /// Concretize a ConcreteType<TypeDefn> to get a ConcreteTypeHandle for static field access
    let concretizeFieldDeclaringType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<'corelib>)
        (declaringType : ConcreteType<TypeDefn>)
        (state : IlMachineState)
        : ConcreteTypeHandle * IlMachineState
        =
        // Create a concretization context from the current state
        let ctx : TypeConcretization.ConcretizationContext<'corelib> =
            {
                InProgress = ImmutableDictionary.Empty
                ConcreteTypes = state.ConcreteTypes
                LoadedAssemblies = state._LoadedAssemblies
                BaseTypes = baseClassTypes
            }

        // Helper function to get assembly from reference
        let loadAssembly
            (currentAssembly : AssemblyName)
            (assyRef : AssemblyReferenceHandle)
            : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly
            =
            let assyToLoad =
                match state.LoadedAssembly currentAssembly with
                | Some assy -> assy
                | None -> failwithf "Assembly %s not loaded" currentAssembly.FullName

            let referencedAssy = assyToLoad.AssemblyReferences.[assyRef]

            match state.LoadedAssembly referencedAssy.Name with
            | Some assy -> state._LoadedAssemblies, assy
            | None ->
                // Need to load the assembly
                let newState, loadedAssy, _ = loadAssembly loggerFactory assyToLoad assyRef state
                newState._LoadedAssemblies, loadedAssy

        // Concretize each generic argument first
        let mutable currentCtx = ctx
        let genericHandles = ImmutableArray.CreateBuilder declaringType.Generics.Length

        for genericArg in declaringType.Generics do
            let handle, newCtx =
                TypeConcretization.concretizeType
                    currentCtx
                    loadAssembly
                    declaringType.Assembly
                    ImmutableArray.Empty // No type generics in this context
                    ImmutableArray.Empty // No method generics in this context
                    genericArg

            currentCtx <- newCtx
            genericHandles.Add handle

        // Now we need to concretize the type definition itself
        // If it's a non-generic type, we can use concretizeTypeDefinition directly
        if declaringType.Generics.IsEmpty then
            let handle, newCtx =
                TypeConcretization.concretizeTypeDefinition currentCtx declaringType.Assembly declaringType.Definition

            let newState =
                { state with
                    ConcreteTypes = newCtx.ConcreteTypes
                }

            handle, newState
        else
            // For generic types, we need to check if this concrete instantiation already exists
            let key =
                (declaringType.Assembly, declaringType.Namespace, declaringType.Name, genericHandles.ToImmutable ())

            match AllConcreteTypes.findExistingConcreteType currentCtx.ConcreteTypes key with
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
        : IlMachineState * CliType
        =

        // First concretize the type
        // Make sure the current assembly is included in the state for concretization
        let state =
            if state.LoadedAssembly assy.Name |> Option.isSome then
                state
            else
                state.WithLoadedAssembly assy.Name assy

        let state, handle =
            concretizeType baseClassTypes state assy.Name typeGenerics methodGenerics ty

        // Now get the zero value
        let zero, state = cliTypeZeroOfHandle state baseClassTypes handle
        state, zero

    let pushToEvalStack' (o : EvalStackValue) (thread : ThreadId) (state : IlMachineState) =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack' o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let pushToEvalStack (o : CliType) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let peekEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue option =
        ThreadState.peekEvalStack state.ThreadState.[thread]

    let popEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue * IlMachineState =
        let ret, popped = ThreadState.popFromEvalStack state.ThreadState.[thread]

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add thread popped
            }

        ret, state

    let advanceProgramCounter (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.advanceProgramCounter |> Some
                    )
        }

    let setArrayValue
        (arrayAllocation : ManagedHeapAddress)
        (v : CliType)
        (index : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let heap = ManagedHeap.setArrayValue arrayAllocation index v state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let getArrayValue (arrayAllocation : ManagedHeapAddress) (index : int) (state : IlMachineState) : CliType =
        ManagedHeap.getArrayValue arrayAllocation index state.ManagedHeap

    /// There might be no stack frame to return to, so you might get None.
    let returnStackFrame
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState option
        =
        let threadStateAtEndOfMethod = state.ThreadState.[currentThread]

        match threadStateAtEndOfMethod.MethodState.ReturnState with
        | None -> None
        | Some returnState ->

        let state =
            match returnState.WasInitialisingType with
            | None -> state
            | Some finishedInitialising -> state.WithTypeEndInit currentThread finishedInitialising

        // Return to previous stack frame
        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add
                        currentThread
                        { threadStateAtEndOfMethod with
                            ActiveMethodState = returnState.JumpTo
                            ActiveAssembly =
                                threadStateAtEndOfMethod.MethodStates.[returnState.JumpTo].ExecutingMethod.DeclaringType
                                    .Assembly
                        }
            }

        match returnState.WasConstructingObj with
        | Some constructing ->
            // Assumption: a constructor can't also return a value.
            // If we were constructing a reference type, we push a reference to it.
            // Otherwise, extract the now-complete object from the heap and push it to the stack directly.
            let constructed = state.ManagedHeap.NonArrayObjects.[constructing]

            let resolvedBaseType =
                DumpedAssembly.resolveBaseType
                    baseClassTypes
                    state._LoadedAssemblies
                    constructed.Type.Assembly
                    constructed.Type.BaseType

            match resolvedBaseType with
            | ResolvedBaseType.Delegate
            | ResolvedBaseType.Object -> state |> pushToEvalStack (CliType.ofManagedObject constructing) currentThread
            | ResolvedBaseType.ValueType ->
                let vt =
                    {
                        CliValueType.Fields = constructed.Fields
                    }

                state
                // TODO: ordering of fields probably important
                |> pushToEvalStack (CliType.ValueType vt) currentThread
            | ResolvedBaseType.Enum -> failwith "TODO"
        | None ->
            match threadStateAtEndOfMethod.MethodState.EvaluationStack.Values with
            | [] ->
                // no return value
                state
            | [ retVal ] ->
                let retType =
                    threadStateAtEndOfMethod.MethodState.ExecutingMethod.Signature.ReturnType

                match retType with
                // TODO: Claude, don't worry about this one for now, I need to think harder about what's going on here.
                // | TypeDefn.Void -> state
                | retType ->
                    // TODO: generics
                    let zero, state = cliTypeZeroOfHandle state baseClassTypes retType

                    let toPush = EvalStackValue.toCliTypeCoerced zero retVal

                    state |> pushToEvalStack toPush currentThread
            | _ ->
                failwith
                    "Unexpected interpretation result has a local evaluation stack with more than one element on RET"

        |> Some

    let concretizeMethodWithTypeGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
        (methodGenerics : TypeDefn ImmutableArray option)
        (callingAssembly : AssemblyName)
        (currentExecutingMethodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =

        // Concretize method generics if any
        let state, concretizedMethodGenerics =
            match methodGenerics with
            | None -> state, ImmutableArray.Empty
            | Some generics ->
                let handles = ImmutableArray.CreateBuilder ()
                let mutable state = state

                for i = 0 to generics.Length - 1 do
                    let state2, handle =
                        concretizeType
                            baseClassTypes
                            state
                            callingAssembly
                            typeGenerics
                            currentExecutingMethodGenerics
                            generics.[i]

                    state <- state2
                    handles.Add handle

                state, handles.ToImmutable ()

        // Now concretize the entire method
        let concretizedMethod, newConcreteTypes, newAssemblies =
            Concretization.concretizeMethod
                state.ConcreteTypes
                (fun assyName ref ->
                    match state.LoadedAssembly assyName with
                    | Some currentAssy ->
                        let targetAssyRef = currentAssy.AssemblyReferences.[ref]

                        match state.LoadedAssembly targetAssyRef.Name with
                        | Some _ ->
                            // Assembly already loaded, return existing state
                            state._LoadedAssemblies, state._LoadedAssemblies.[targetAssyRef.Name.FullName]
                        | None ->
                            // Need to load the assembly
                            let newState, loadedAssy, _ = loadAssembly loggerFactory currentAssy ref state
                            newState._LoadedAssemblies, loadedAssy
                    | None ->
                        failwithf "Current assembly %s not loaded when trying to resolve reference" assyName.FullName
                )
                state._LoadedAssemblies
                baseClassTypes
                methodToCall
                typeGenerics
                concretizedMethodGenerics

        let state =
            { state with
                ConcreteTypes = newConcreteTypes
                _LoadedAssemblies = newAssemblies
            }

        // Get the handle for the declaring type
        let declaringTypeHandle =
            match AllConcreteTypes.lookup' concretizedMethod.DeclaringType state.ConcreteTypes with
            | Some handle -> handle
            | None -> failwith "Concretized method's declaring type not found in ConcreteTypes"

        state, concretizedMethod, declaringTypeHandle

    let concretizeMethodForExecution
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
        (methodGenerics : TypeDefn ImmutableArray option)
        (typeArgsFromMetadata : TypeDefn ImmutableArray option)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =
        // Use type generics from metadata if available, otherwise fall back to current execution context
        let typeGenerics =
            match typeArgsFromMetadata with
            | Some args when not args.IsEmpty ->
                // We have concrete type arguments from the IL metadata
                // Need to concretize them to ConcreteTypeHandle first
                let handles = ImmutableArray.CreateBuilder args.Length
                let mutable state = state

                for i = 0 to args.Length - 1 do
                    let ctx =
                        {
                            TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                            TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                            TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                            TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
                        }

                    let handle, newCtx =
                        TypeConcretization.concretizeType
                            ctx
                            (fun assyName ref ->
                                let currentAssy = state.LoadedAssembly assyName |> Option.get

                                let targetAssy =
                                    currentAssy.AssemblyReferences.[ref].Name |> state.LoadedAssembly |> Option.get

                                state._LoadedAssemblies, targetAssy
                            )
                            (state.ActiveAssembly thread).Name
                            ImmutableArray.Empty // No type generics for the concretization context
                            ImmutableArray.Empty // No method generics for the concretization context
                            args.[i]

                    handles.Add handle

                    state <-
                        { state with
                            ConcreteTypes = newCtx.ConcreteTypes
                        }

                handles.ToImmutable (), state
            | _ ->
                // Fall back to current execution context
                let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod
                currentMethod.DeclaringType.Generics, state

        let typeGenerics, state = typeGenerics

        let callingAssembly = (state.ActiveAssembly thread).Name
        let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod

        concretizeMethodWithTypeGenerics
            loggerFactory
            baseClassTypes
            typeGenerics
            methodToCall
            methodGenerics
            callingAssembly
            currentMethod.Generics
            state

    // Add to IlMachineState module
    let concretizeFieldForExecution
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (field : WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle * ImmutableArray<ConcreteTypeHandle>
        =
        // Get type and method generics from current execution context
        let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod

        let contextTypeGenerics = currentMethod.DeclaringType.Generics

        let contextMethodGenerics = currentMethod.Generics |> ImmutableArray.CreateRange

        // Create a concretization context
        let ctx =
            {
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        // Create a TypeDefn for the field's declaring type
        let declaringTypeDefn =
            if field.DeclaringType.Generics.IsEmpty then
                // Non-generic type - determine the SignatureTypeKind
                let assy = state._LoadedAssemblies.[field.DeclaringType.Assembly.FullName]
                let typeDef = assy.TypeDefs.[field.DeclaringType.Definition.Get]

                let baseType =
                    typeDef.BaseType
                    |> DumpedAssembly.resolveBaseType baseClassTypes state._LoadedAssemblies assy.Name

                let signatureTypeKind =
                    match baseType with
                    | ResolvedBaseType.Enum
                    | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                    | ResolvedBaseType.Object
                    | ResolvedBaseType.Delegate -> SignatureTypeKind.Class

                TypeDefn.FromDefinition (
                    field.DeclaringType.Definition,
                    field.DeclaringType.Assembly.FullName,
                    signatureTypeKind
                )
            else
                // Generic type - the field's declaring type already has the generic arguments
                let assy = state._LoadedAssemblies.[field.DeclaringType.Assembly.FullName]
                let typeDef = assy.TypeDefs.[field.DeclaringType.Definition.Get]

                let baseTypeResolved =
                    typeDef.BaseType
                    |> DumpedAssembly.resolveBaseType baseClassTypes state._LoadedAssemblies assy.Name

                let signatureTypeKind =
                    match baseTypeResolved with
                    | ResolvedBaseType.Enum
                    | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                    | ResolvedBaseType.Object -> SignatureTypeKind.Class
                    | ResolvedBaseType.Delegate -> failwith "TODO: delegate"

                let baseType =
                    TypeDefn.FromDefinition (
                        field.DeclaringType.Definition,
                        field.DeclaringType.Assembly.FullName,
                        signatureTypeKind
                    )

                // Use the actual type arguments from the field's declaring type
                // These should already be correctly instantiated (e.g., GenericMethodParameter 0 for Array.Empty<T>)
                let genericArgs = field.DeclaringType.Generics

                TypeDefn.GenericInstantiation (baseType, genericArgs)

        // Concretize the declaring type
        let declaringHandle, newCtx =
            TypeConcretization.concretizeType
                ctx
                (fun assyName ref ->
                    let currentAssy = state.LoadedAssembly assyName |> Option.get

                    let targetAssy =
                        currentAssy.AssemblyReferences.[ref].Name |> state.LoadedAssembly |> Option.get

                    state._LoadedAssemblies, targetAssy
                )
                field.DeclaringType.Assembly
                contextTypeGenerics
                contextMethodGenerics
                declaringTypeDefn

        let state =
            { state with
                ConcreteTypes = newCtx.ConcreteTypes
                _LoadedAssemblies = newCtx.LoadedAssemblies
            }

        // Get the concretized type's generics
        let concretizedType =
            AllConcreteTypes.lookup declaringHandle state.ConcreteTypes |> Option.get

        let typeGenerics = concretizedType.Generics

        state, declaringHandle, typeGenerics

    let initial
        (lf : ILoggerFactory)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (entryAssembly : DumpedAssembly)
        : IlMachineState
        =
        let assyName = entryAssembly.ThisAssemblyDefinition.Name
        let logger = lf.CreateLogger "IlMachineState"

        let state =
            {
                ConcreteTypes = AllConcreteTypes.Empty
                Logger = logger
                NextThreadId = 0
                // CallStack = []
                ManagedHeap = ManagedHeap.empty
                ThreadState = Map.empty
                InternedStrings = ImmutableDictionary.Empty
                _LoadedAssemblies = ImmutableDictionary.Empty
                _Statics = ImmutableDictionary.Empty
                TypeInitTable = ImmutableDictionary.Empty
                DotnetRuntimeDirs = dotnetRuntimeDirs
                TypeHandles = TypeHandleRegistry.empty ()
                FieldHandles = FieldHandleRegistry.empty ()
            }

        state.WithLoadedAssembly assyName entryAssembly

    let addThread
        (newThreadState : MethodState)
        (newThreadAssy : AssemblyName)
        (state : IlMachineState)
        : IlMachineState * ThreadId
        =
        let thread = ThreadId state.NextThreadId

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState =
                    state.ThreadState
                    |> Map.add thread (ThreadState.New newThreadAssy newThreadState)
            }

        newState, thread

    let allocateArray
        (zeroOfType : unit -> CliType)
        (len : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let initialisation =
            (fun _ -> zeroOfType ()) |> Seq.init len |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                Length = len
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let allocateStringData (len : int) (state : IlMachineState) : int * IlMachineState =
        let addr, heap = state.ManagedHeap |> ManagedHeap.allocateString len

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    let setStringData (addr : int) (contents : string) (state : IlMachineState) : IlMachineState =
        let heap = ManagedHeap.setStringData addr contents state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let allocateManagedObject<'generic, 'field>
        (typeInfo : WoofWare.PawPrint.TypeInfo<'generic, 'field>)
        (fields : CliField list)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let o =
            {
                Fields = fields
                Type = TypeInfoCrate.make typeInfo
                SyncBlock = SyncBlock.Free
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateNonArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let popFromStackToLocalVariable
        (thread : ThreadId)
        (localVariableIndex : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStackToVariable
                localVariableIndex
                threadState.MethodStates.[threadState.ActiveMethodState]

        { state with
            ThreadState =
                state.ThreadState
                |> Map.add
                    thread
                    { threadState with
                        MethodStates = threadState.MethodStates.SetItem (threadState.ActiveMethodState, methodState)
                    }
        }

    let jumpProgramCounter (thread : ThreadId) (bytes : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.jumpProgramCounter bytes |> Some
                    )
        }

    let loadArgument (thread : ThreadId) (index : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some state -> state |> ThreadState.loadArgument index |> Some
                    )
        }

    let resolveMember
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (genericMethodTypeArgs : ImmutableArray<ConcreteTypeHandle>)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        // TODO: do we need to initialise the parent class here?
        let mem = assy.Members.[m]

        let memberName : string = assy.Strings mem.Name

        let executing = state.ThreadState.[currentThread].MethodState.ExecutingMethod
        // Create synthetic TypeDefn generics based on the arity of the concrete generics
        let typeGenerics =
            executing.DeclaringType.Generics
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let state, assy, targetType, extractedTypeArgs =
            match mem.Parent with
            | MetadataToken.TypeReference parent ->
                // TODO: generics here?
                let state, assy, targetType =
                    resolveType loggerFactory parent ImmutableArray.Empty assy state

                state, assy, targetType, ImmutableArray.Empty // No type args from TypeReference
            | MetadataToken.TypeSpecification parent ->
                let methodGenerics =
                    executing.Generics
                    |> Seq.map (fun handle ->
                        Concretization.concreteHandleToTypeDefn
                            baseClassTypes
                            handle
                            state.ConcreteTypes
                            state._LoadedAssemblies
                    )
                    |> ImmutableArray.CreateRange

                let state, assy, targetType =
                    resolveTypeFromSpec loggerFactory baseClassTypes parent assy typeGenerics methodGenerics state

                // Extract type arguments from the resolved type
                let extractedTypeArgs = targetType.Generics

                state, assy, targetType, extractedTypeArgs
            | parent -> failwith $"Unexpected: {parent}"

        let state, concreteExtractedTypeArgs =
            ((state, ImmutableArray.CreateBuilder ()), extractedTypeArgs)
            ||> Seq.fold (fun (state, acc) ty ->
                // TODO: generics?
                let state, t =
                    concretizeType
                        baseClassTypes
                        state
                        targetType.Assembly
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty

                acc.Add t
                state, acc
            )
            |> Tuple.rmap (fun x -> x.ToImmutable ())

        match mem.Signature with
        | MemberSignature.Field fieldSig ->
            // Concretize the field signature from the member reference
            let state, concreteFieldSig =
                concretizeType
                    baseClassTypes
                    state
                    (state.ActiveAssembly(currentThread).Name)
                    concreteExtractedTypeArgs
                    ImmutableArray.Empty
                    fieldSig

            // Find matching fields by comparing concretized signatures
            let state, availableFields =
                ((state, []), targetType.Fields)
                ||> List.fold (fun (state, acc) fi ->
                    if fi.Name <> memberName then
                        state, acc
                    else
                        // Concretize the field's signature for comparison
                        let state, fieldSigConcrete =
                            concretizeType
                                baseClassTypes
                                state
                                assy.Name
                                concreteExtractedTypeArgs
                                ImmutableArray.Empty
                                fi.Signature

                        if fieldSigConcrete = concreteFieldSig then
                            state, fi :: acc
                        else
                            state, acc
                )

            let field =
                match availableFields with
                | [] ->
                    failwith
                        $"Could not find field member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
                | [ x ] -> x |> FieldInfo.mapTypeGenerics (fun index _ -> targetType.Generics.[index])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for {targetType.Namespace}.{targetType.Name}'s field {memberName}!"

            state, assy.Name, Choice2Of2 field, extractedTypeArgs

        | MemberSignature.Method memberSig ->
            let availableMethods =
                targetType.Methods |> List.filter (fun mi -> mi.Name = memberName)

            let state, memberSig =
                memberSig
                |> TypeMethodSignature.map
                    state
                    (fun state ty ->
                        concretizeType
                            baseClassTypes
                            state
                            (state.ActiveAssembly(currentThread).Name)
                            concreteExtractedTypeArgs
                            genericMethodTypeArgs
                            ty
                    )

            let state, availableMethods =
                ((state, []), availableMethods)
                ||> List.fold (fun (state, acc) meth ->
                    let state, methSig =
                        meth.Signature
                        |> TypeMethodSignature.map
                            state
                            (fun state ty ->
                                concretizeType
                                    baseClassTypes
                                    state
                                    assy.Name
                                    concreteExtractedTypeArgs
                                    genericMethodTypeArgs
                                    ty
                            )

                    if methSig = memberSig then
                        state, meth :: acc
                    else
                        state, acc
                )

            let method =
                match availableMethods with
                | [] ->
                    failwith
                        $"Could not find member {memberName} with the right signature {memberSig} on {targetType.Namespace}.{targetType.Name}"
                | [ x ] ->
                    x
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> targetType.Generics.[par.SequenceNumber])
                | _ ->
                    failwith
                        $"Multiple overloads matching signature for call to {targetType.Namespace}.{targetType.Name}'s {memberName}!"

            state, assy.Name, Choice1Of2 method, extractedTypeArgs

    let getLocalVariable (thread : ThreadId) (stackFrame : int) (varIndex : uint16) (state : IlMachineState) : CliType =
        state.ThreadState.[thread].MethodStates.[stackFrame].LocalVariables.[int<uint16> varIndex]

    let setLocalVariable
        (thread : ThreadId)
        (stackFrame : int)
        (varIndex : uint16)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun existing ->
                        match existing with
                        | None -> failwith "tried to set variable in nonactive thread"
                        | Some existing -> existing |> ThreadState.setLocalVariable stackFrame varIndex value |> Some
                    )
        }

    let setSyncBlock
        (addr : ManagedHeapAddress)
        (syncBlockValue : SyncBlock)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ManagedHeap = state.ManagedHeap |> ManagedHeap.setSyncBlock addr syncBlockValue
        }

    let getSyncBlock (addr : ManagedHeapAddress) (state : IlMachineState) : SyncBlock =
        state.ManagedHeap |> ManagedHeap.getSyncBlock addr

    let executeDelegateConstructor (instruction : MethodState) (state : IlMachineState) : IlMachineState =
        // We've been called with arguments already popped from the stack into local arguments.
        let constructing = instruction.Arguments.[0]
        let targetObj = instruction.Arguments.[1]
        let methodPtr = instruction.Arguments.[2]

        let targetObj =
            match targetObj with
            | CliType.RuntimePointer (CliRuntimePointer.Managed (CliRuntimePointerSource.Heap target))
            | CliType.ObjectRef (Some target) -> Some target
            | CliType.ObjectRef None
            | CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null) -> None
            | _ -> failwith $"Unexpected target type for delegate: {targetObj}"

        let constructing =
            match constructing with
            | CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null)
            | CliType.ObjectRef None -> failwith "unexpectedly constructing the null delegate"
            | CliType.RuntimePointer (CliRuntimePointer.Managed (CliRuntimePointerSource.Heap target))
            | CliType.ObjectRef (Some target) -> target
            | _ -> failwith $"Unexpectedly not constructing a managed object: {constructing}"

        let heapObj =
            match state.ManagedHeap.NonArrayObjects.TryGetValue constructing with
            | true, obj -> obj
            | false, _ -> failwith $"Delegate object {constructing} not found on heap"

        // Standard delegate fields in .NET are _target and _methodPtr
        // Update the fields with the target object and method pointer
        let updatedFields =
            // Let's not consider field ordering for reference types like delegates.
            // Nobody's going to be marshalling a reference type anyway, I hope.
            {
                Name = "_target"
                Contents = CliType.ObjectRef targetObj
                Offset = None
            }
            :: {
                   Name = "_methodPtr"
                   Contents = methodPtr
                   Offset = None
               }
            :: heapObj.Fields

        let updatedObj =
            { heapObj with
                Fields = updatedFields
            }

        let updatedHeap =
            { state.ManagedHeap with
                NonArrayObjects = state.ManagedHeap.NonArrayObjects |> Map.add constructing updatedObj
            }

        { state with
            ManagedHeap = updatedHeap
        }

    /// Returns the type handle and an allocated System.RuntimeType.
    let getOrAllocateType<'corelib>
        (baseClassTypes : BaseClassTypes<'corelib>)
        (defn : ConcreteTypeHandle)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let result, reg, state =
            TypeHandleRegistry.getOrAllocate
                state
                (fun fields state -> allocateManagedObject baseClassTypes.RuntimeType fields state)
                defn
                state.TypeHandles

        let state =
            { state with
                TypeHandles = reg
            }

        result, state

    /// Returns a System.RuntimeFieldHandle.
    let getOrAllocateField<'corelib>
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<'corelib>)
        (declaringAssy : AssemblyName)
        (fieldHandle : FieldDefinitionHandle)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let field = state.LoadedAssembly(declaringAssy).Value.Fields.[fieldHandle]

        // For LdToken, we need to convert GenericParamFromMetadata to TypeDefn
        // When we don't have generic context, we use the generic type parameters directly
        let declaringTypeWithGenerics =
            field.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringType, state =
            concretizeFieldDeclaringType loggerFactory baseClassTypes declaringTypeWithGenerics state

        let result, reg, state =
            FieldHandleRegistry.getOrAllocate
                baseClassTypes
                state
                (fun fields state -> allocateManagedObject baseClassTypes.RuntimeType fields state)
                declaringAssy
                declaringType
                fieldHandle
                state.FieldHandles

        let state =
            { state with
                FieldHandles = reg
            }

        result, state

    let setStatic
        (ty : ConcreteTypeHandle)
        (field : string)
        (value : CliType)
        (this : IlMachineState)
        : IlMachineState
        =
        let statics =
            match this._Statics.TryGetValue ty with
            | false, _ -> this._Statics.Add (ty, ImmutableDictionary.Create().Add (field, value))
            | true, v -> this._Statics.SetItem (ty, v.SetItem (field, value))

        { this with
            _Statics = statics
        }

    let getStatic (ty : ConcreteTypeHandle) (field : string) (this : IlMachineState) : CliType option =
        match this._Statics.TryGetValue ty with
        | false, _ -> None
        | true, v ->
            match v.TryGetValue field with
            | false, _ -> None
            | true, v -> Some v

    let rec dereferencePointer (state : IlMachineState) (src : ManagedPointerSource) : CliType =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NRE"
        | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
            state.ThreadState.[sourceThread].MethodStates.[methodFrame].LocalVariables.[int<uint16> whichVar]
        | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) ->
            state.ThreadState.[sourceThread].MethodStates.[methodFrame].Arguments.[int<uint16> whichVar]
        | ManagedPointerSource.Heap addr -> failwith "todo"
        | ManagedPointerSource.ArrayIndex (arr, index) -> getArrayValue arr index state
        | ManagedPointerSource.Field (addr, name) ->
            let obj = dereferencePointer state addr

            match obj with
            | CliType.ValueType vt -> vt |> CliValueType.DereferenceField name
            | v -> failwith $"could not find field {name} on object {v}"
