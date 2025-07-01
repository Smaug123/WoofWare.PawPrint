namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type ConcreteTypeHandle =
    | Concrete of int
    | Byref of ConcreteTypeHandle
    | Pointer of ConcreteTypeHandle

type AllConcreteTypes =
    {
        Mapping : Map<int, ConcreteType<ConcreteTypeHandle>>
        NextHandle : int
    }

    static member Empty =
        {
            Mapping = Map.empty
            NextHandle = 0
        }

[<RequireQualifiedAccess>]
module AllConcreteTypes =
    let lookup (cth : ConcreteTypeHandle) (this : AllConcreteTypes) : ConcreteType<ConcreteTypeHandle> option =
        match cth with
        | ConcreteTypeHandle.Concrete id -> this.Mapping |> Map.tryFind id
        | ConcreteTypeHandle.Byref _ -> None // Byref types are not stored in the mapping
        | ConcreteTypeHandle.Pointer _ -> None // Pointer types are not stored in the mapping

    let lookup' (ct : ConcreteType<ConcreteTypeHandle>) (this : AllConcreteTypes) : ConcreteTypeHandle option =
        this.Mapping
        |> Map.tryPick (fun id existingCt ->
            if
                existingCt._AssemblyName = ct._AssemblyName
                && existingCt._Namespace = ct._Namespace
                && existingCt._Name = ct._Name
                && existingCt._Definition = ct._Definition
                && existingCt._Generics = ct._Generics
            then
                Some (ConcreteTypeHandle.Concrete id)
            else
                None
        )

    let findExistingConcreteType
        (concreteTypes : AllConcreteTypes)
        (asm : AssemblyName, ns : string, name : string, generics : ConcreteTypeHandle list as key)
        : ConcreteTypeHandle option
        =

        concreteTypes.Mapping
        |> Map.tryPick (fun id ct ->
            if
                ct.Assembly.FullName = asm.FullName
                && ct.Namespace = ns
                && ct.Name = name
                && ct.Generics = generics
            then
                Some (ConcreteTypeHandle.Concrete id)
            else
                None
        )

    /// `source` is AssemblyName * Namespace * Name
    let add (ct : ConcreteType<ConcreteTypeHandle>) (this : AllConcreteTypes) : ConcreteTypeHandle * AllConcreteTypes =
        let id = this.NextHandle
        let toRet = ConcreteTypeHandle.Concrete id

        let newState =
            {
                NextHandle = this.NextHandle + 1
                Mapping = this.Mapping |> Map.add id ct
            }

        toRet, newState

[<RequireQualifiedAccess>]
module TypeConcretization =

    type ConcretizationContext =
        {
            /// Types currently being processed (to detect cycles)
            InProgress : ImmutableDictionary<AssemblyName * TypeDefn, ConcreteTypeHandle>
            /// All concrete types created so far
            ConcreteTypes : AllConcreteTypes
            /// For resolving type references
            LoadedAssemblies : ImmutableDictionary<string, DumpedAssembly>
            BaseTypes : BaseClassTypes<DumpedAssembly>
        }

    // Helper function to find existing primitive types
    let private findExistingPrimitiveType
        (concreteTypes : AllConcreteTypes)
        (key : AssemblyName * string * string)
        : ConcreteTypeHandle option
        =
        concreteTypes.Mapping
        |> Map.tryPick (fun id ct ->
            if
                ct._AssemblyName = (let (asm, _, _) = key in asm)
                && ct._Namespace = (let (_, ns, _) = key in ns)
                && ct._Name = (let (_, _, name) = key in name)
                && ct._Generics.IsEmpty
            then
                Some (ConcreteTypeHandle.Concrete id)
            else
                None
        )

    let private concretizePrimitive
        (ctx : ConcretizationContext)
        (prim : PrimitiveType)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // Get the TypeInfo for this primitive from BaseClassTypes
        let typeInfo =
            match prim with
            | PrimitiveType.Boolean -> ctx.BaseTypes.Boolean
            | PrimitiveType.Char -> ctx.BaseTypes.Char
            | PrimitiveType.SByte -> ctx.BaseTypes.SByte
            | PrimitiveType.Byte -> ctx.BaseTypes.Byte
            | PrimitiveType.Int16 -> ctx.BaseTypes.Int16
            | PrimitiveType.UInt16 -> ctx.BaseTypes.UInt16
            | PrimitiveType.Int32 -> ctx.BaseTypes.Int32
            | PrimitiveType.UInt32 -> ctx.BaseTypes.UInt32
            | PrimitiveType.Int64 -> ctx.BaseTypes.Int64
            | PrimitiveType.UInt64 -> ctx.BaseTypes.UInt64
            | PrimitiveType.Single -> ctx.BaseTypes.Single
            | PrimitiveType.Double -> ctx.BaseTypes.Double
            | PrimitiveType.String -> ctx.BaseTypes.String
            | PrimitiveType.Object -> ctx.BaseTypes.Object
            | PrimitiveType.TypedReference -> ctx.BaseTypes.TypedReference
            | PrimitiveType.IntPtr -> ctx.BaseTypes.IntPtr
            | PrimitiveType.UIntPtr -> ctx.BaseTypes.UIntPtr

        // Check if we've already concretized this primitive type
        let key = (typeInfo.Assembly, typeInfo.Namespace, typeInfo.Name)

        match findExistingPrimitiveType ctx.ConcreteTypes key with
        | Some handle -> handle, ctx
        | None ->
            // Create the concrete type (primitives have no generic arguments)
            let concreteType =
                {
                    _AssemblyName = typeInfo.Assembly
                    _Definition = ComparableTypeDefinitionHandle.Make typeInfo.TypeDefHandle
                    _Namespace = typeInfo.Namespace
                    _Name = typeInfo.Name
                    _Generics = [] // Primitives have no generic parameters
                }

            // Add to the concrete types
            let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

            let newCtx =
                { ctx with
                    ConcreteTypes = newConcreteTypes
                }

            handle, newCtx

    let private concretizeArray
        (ctx : ConcretizationContext)
        (elementHandle : ConcreteTypeHandle)
        (shape : 'a)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // Arrays are System.Array<T> where T is the element type
        let arrayTypeInfo = ctx.BaseTypes.Array

        // Check if we've already concretized this array type
        let key =
            (arrayTypeInfo.Assembly, arrayTypeInfo.Namespace, arrayTypeInfo.Name, [ elementHandle ])

        match AllConcreteTypes.findExistingConcreteType ctx.ConcreteTypes key with
        | Some handle -> handle, ctx
        | None ->
            // Create the concrete array type
            let concreteType =
                {
                    _AssemblyName = arrayTypeInfo.Assembly
                    _Definition = ComparableTypeDefinitionHandle.Make arrayTypeInfo.TypeDefHandle
                    _Namespace = arrayTypeInfo.Namespace
                    _Name = arrayTypeInfo.Name
                    _Generics = [ elementHandle ] // Array<T> has one generic parameter
                }

            // Add to the concrete types
            let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

            let newCtx =
                { ctx with
                    ConcreteTypes = newConcreteTypes
                }

            handle, newCtx

    let private concretizeOneDimArray
        (ctx : ConcretizationContext)
        (elementHandle : ConcreteTypeHandle)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // One-dimensional arrays with lower bound 0 are also System.Array<T>
        // They just have different IL instructions for access
        let arrayTypeInfo = ctx.BaseTypes.Array

        // Check if we've already concretized this array type
        let key =
            (arrayTypeInfo.Assembly, arrayTypeInfo.Namespace, arrayTypeInfo.Name, [ elementHandle ])

        match AllConcreteTypes.findExistingConcreteType ctx.ConcreteTypes key with
        | Some handle -> handle, ctx
        | None ->
            // Create the concrete array type
            let concreteType =
                {
                    _AssemblyName = arrayTypeInfo.Assembly
                    _Definition = ComparableTypeDefinitionHandle.Make arrayTypeInfo.TypeDefHandle
                    _Namespace = arrayTypeInfo.Namespace
                    _Name = arrayTypeInfo.Name
                    _Generics = [ elementHandle ] // Array<T> has one generic parameter
                }

            // Add to the concrete types
            let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

            let newCtx =
                { ctx with
                    ConcreteTypes = newConcreteTypes
                }

            handle, newCtx

    let concretizeTypeDefinition
        (ctx : ConcretizationContext)
        (assemblyName : AssemblyName)
        (typeDefHandle : ComparableTypeDefinitionHandle)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // Look up the type definition in the assembly
        let assembly =
            match ctx.LoadedAssemblies.TryGetValue assemblyName.FullName with
            | false, _ -> failwithf "Cannot concretize type definition - assembly %s not loaded" assemblyName.FullName
            | true, assy -> assy

        let typeInfo = assembly.TypeDefs.[typeDefHandle.Get]

        // Check if this type has generic parameters
        if not typeInfo.Generics.IsEmpty then
            failwithf
                "Cannot concretize open generic type %s.%s - it has %d generic parameters"
                typeInfo.Namespace
                typeInfo.Name
                typeInfo.Generics.Length

        // Check if we've already concretized this type
        let key = (assemblyName, typeInfo.Namespace, typeInfo.Name, [])

        match AllConcreteTypes.findExistingConcreteType ctx.ConcreteTypes key with
        | Some handle -> handle, ctx
        | None ->
            // Create the concrete type (no generic arguments since it's not generic)
            let concreteType =
                {
                    _AssemblyName = assemblyName
                    _Definition = typeDefHandle
                    _Namespace = typeInfo.Namespace
                    _Name = typeInfo.Name
                    _Generics = [] // No generic parameters
                }

            // Add to the concrete types
            let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

            let newCtx =
                { ctx with
                    ConcreteTypes = newConcreteTypes
                }

            handle, newCtx

    let private concretizeTypeReference
        (loadAssembly :
            AssemblyName -> AssemblyReferenceHandle -> ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly)
        (ctx : ConcretizationContext)
        (currentAssembly : AssemblyName)
        (typeRef : TypeRef)
        : ConcreteTypeHandle * ConcretizationContext
        =
        // The currentAssembly is the assembly context where this TypeRef is being used
        // We need to resolve it from that assembly's perspective
        let currentAssy =
            match ctx.LoadedAssemblies.TryGetValue currentAssembly.FullName with
            | false, _ -> failwithf "Current assembly %s not loaded" currentAssembly.FullName
            | true, assy -> assy

        // Use the proper type resolution that handles type forwarding
        let resolutionResult =
            Assembly.resolveTypeRef ctx.LoadedAssemblies currentAssy typeRef ImmutableArray.Empty

        match resolutionResult with
        | TypeResolutionResult.Resolved (targetAssy, typeInfo) ->
            // Check if this type has generic parameters
            if not typeInfo.Generics.IsEmpty then
                failwithf
                    "Cannot concretize type reference to open generic type %s.%s - it has %d generic parameters"
                    typeInfo.Namespace
                    typeInfo.Name
                    typeInfo.Generics.Length

            // Create or find the concrete type
            concretizeTypeDefinition
                ctx
                targetAssy.Name
                (ComparableTypeDefinitionHandle.Make typeInfo.TypeDefHandle)

        | TypeResolutionResult.FirstLoadAssy assemblyRef ->
            // Need to load the assembly
            match typeRef.ResolutionScope with
            | TypeRefResolutionScope.Assembly assyRef ->
                let newAssemblies, loadedAssy = loadAssembly currentAssembly assyRef

                let newCtx =
                    { ctx with
                        LoadedAssemblies = newAssemblies
                    }

                // Now try to resolve again with the loaded assembly
                let resolutionResult2 =
                    Assembly.resolveTypeRef newCtx.LoadedAssemblies currentAssy typeRef ImmutableArray.Empty

                match resolutionResult2 with
                | TypeResolutionResult.Resolved (targetAssy, typeInfo) ->
                    // Check if this type has generic parameters
                    if not typeInfo.Generics.IsEmpty then
                        failwithf
                            "Cannot concretize type reference to open generic type %s.%s - it has %d generic parameters"
                            typeInfo.Namespace
                            typeInfo.Name
                            typeInfo.Generics.Length

                    // Create or find the concrete type
                    concretizeTypeDefinition
                        newCtx
                        targetAssy.Name
                        (ComparableTypeDefinitionHandle.Make typeInfo.TypeDefHandle)

                | TypeResolutionResult.FirstLoadAssy _ ->
                    failwithf "Failed to resolve type %s.%s after loading assembly" typeRef.Namespace typeRef.Name

            | _ -> failwith "Unexpected resolution scope"

    /// Concretize a type in a specific generic context
    let rec concretizeType
        (ctx : ConcretizationContext)
        (loadAssembly :
            AssemblyName -> AssemblyReferenceHandle -> (ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly))
        (assembly : AssemblyName)
        (typeGenerics : ConcreteTypeHandle ImmutableArray)
        (methodGenerics : ConcreteTypeHandle ImmutableArray)
        (typeDefn : TypeDefn)
        : ConcreteTypeHandle * ConcretizationContext
        =

        let key = (assembly, typeDefn)

        // Check if we're already processing this type (cycle detection)
        match ctx.InProgress.TryGetValue key with
        | true, handle -> handle, ctx
        | false, _ ->

        match typeDefn with
        | TypeDefn.PrimitiveType prim -> concretizePrimitive ctx prim

        | TypeDefn.Array (elementType, shape) ->
            let elementHandle, ctx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

            concretizeArray ctx elementHandle shape

        | TypeDefn.OneDimensionalArrayLowerBoundZero elementType ->
            let elementHandle, ctx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

            concretizeOneDimArray ctx elementHandle

        | TypeDefn.GenericTypeParameter index ->
            if index < typeGenerics.Length then
                typeGenerics.[index], ctx
            else
                failwithf "Generic type parameter %d out of range" index

        | TypeDefn.GenericMethodParameter index ->
            if index < methodGenerics.Length then
                methodGenerics.[index], ctx
            else
                failwithf "Generic method parameter %d out of range" index

        | TypeDefn.GenericInstantiation (genericDef, args) ->
            concretizeGenericInstantiation ctx loadAssembly assembly typeGenerics methodGenerics genericDef args

        | TypeDefn.FromDefinition (typeDefHandle, targetAssembly, _) ->
            concretizeTypeDefinition ctx (AssemblyName targetAssembly) typeDefHandle

        | TypeDefn.FromReference (typeRef, _) -> concretizeTypeReference loadAssembly ctx assembly typeRef

        | TypeDefn.Byref elementType ->
            // Byref types are managed references to other types
            // First concretize the element type
            let elementHandle, ctx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

            // Return a Byref constructor wrapping the element type
            ConcreteTypeHandle.Byref elementHandle, ctx

        | TypeDefn.Pointer elementType ->
            // Pointer types are unmanaged pointers to other types
            // First concretize the element type
            let elementHandle, ctx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

            // Return a Pointer constructor wrapping the element type
            ConcreteTypeHandle.Pointer elementHandle, ctx

        | TypeDefn.Void ->
            // Void isn't a real runtime type, but we assign it a concretization entry anyway
            // Use System.Void from the base class types
            let voidTypeInfo = ctx.BaseTypes.Void
            let key = (voidTypeInfo.Assembly, voidTypeInfo.Namespace, voidTypeInfo.Name, [])

            match AllConcreteTypes.findExistingConcreteType ctx.ConcreteTypes key with
            | Some handle -> handle, ctx
            | None ->
                let concreteType =
                    {
                        _AssemblyName = voidTypeInfo.Assembly
                        _Definition = ComparableTypeDefinitionHandle.Make voidTypeInfo.TypeDefHandle
                        _Namespace = voidTypeInfo.Namespace
                        _Name = voidTypeInfo.Name
                        _Generics = [] // Void has no generic parameters
                    }

                let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

                let newCtx =
                    { ctx with
                        ConcreteTypes = newConcreteTypes
                    }

                handle, newCtx

        | _ -> failwithf "TODO: Concretization of %A not implemented" typeDefn

    and private concretizeGenericInstantiation
        (ctx : ConcretizationContext)
        (loadAssembly :
            AssemblyName -> AssemblyReferenceHandle -> (ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly))
        (assembly : AssemblyName)
        (typeGenerics : ConcreteTypeHandle ImmutableArray)
        (methodGenerics : ConcreteTypeHandle ImmutableArray)
        (genericDef : TypeDefn)
        (args : ImmutableArray<TypeDefn>)
        : ConcreteTypeHandle * ConcretizationContext
        =
        // First, concretize all type arguments
        let argHandles, ctxAfterArgs =
            args
            |> Seq.fold
                (fun (handles, ctx) arg ->
                    let handle, ctx =
                        concretizeType ctx loadAssembly assembly typeGenerics methodGenerics arg

                    handle :: handles, ctx
                )
                ([], ctx)

        let argHandles = argHandles |> List.rev

        // Get the base type definition
        let baseAssembly, baseTypeDefHandle, baseNamespace, baseName, ctxAfterArgs =
            match genericDef with
            | FromDefinition (handle, assy, _) ->
                // Look up the type definition to get namespace and name
                let currentAssy = ctxAfterArgs.LoadedAssemblies.[AssemblyName(assy).FullName]
                let typeDef = currentAssy.TypeDefs.[handle.Get]
                AssemblyName assy, handle, typeDef.Namespace, typeDef.Name, ctxAfterArgs
            | FromReference (typeRef, _) ->
                // For a type reference, we need to find where the type is defined
                // We're looking for the generic type definition, not an instantiation
                let currentAssy = ctxAfterArgs.LoadedAssemblies.[assembly.FullName]

                // Helper to find the type definition without instantiating generics
                let rec findTypeDefinition (assy : DumpedAssembly) (ns : string) (name : string) =
                    // First check if it's defined in this assembly
                    match assy.TypeDef ns name with
                    | Some typeDef -> Some (assy, typeDef)
                    | None ->
                        // Check if it's exported/forwarded
                        match assy.ExportedType (Some ns) name with
                        | Some export ->
                            match export.Data with
                            | NonForwarded _ -> None // Shouldn't happen
                            | ForwardsTo assyRef ->
                                let forwardedAssy = assy.AssemblyReferences.[assyRef]

                                match ctxAfterArgs.LoadedAssemblies.TryGetValue forwardedAssy.Name.FullName with
                                | true, targetAssy -> findTypeDefinition targetAssy ns name
                                | false, _ -> None // Assembly not loaded yet
                        | None -> None

                // First try to resolve without loading new assemblies
                match typeRef.ResolutionScope with
                | TypeRefResolutionScope.Assembly assyRef ->
                    let targetAssyRef = currentAssy.AssemblyReferences.[assyRef]
                    let targetAssyName = targetAssyRef.Name

                    match ctxAfterArgs.LoadedAssemblies.TryGetValue targetAssyName.FullName with
                    | true, targetAssy ->
                        // Try to find the type
                        match findTypeDefinition targetAssy typeRef.Namespace typeRef.Name with
                        | Some (foundAssy, typeDef) ->
                            foundAssy.Name,
                            ComparableTypeDefinitionHandle.Make typeDef.TypeDefHandle,
                            typeDef.Namespace,
                            typeDef.Name,
                            ctxAfterArgs
                        | None ->
                            failwithf
                                "Type %s.%s not found in assembly %s or its forwards"
                                typeRef.Namespace
                                typeRef.Name
                                targetAssyName.FullName

                    | false, _ ->
                        // Need to load the assembly
                        let newAssemblies, loadedAssy = loadAssembly assembly assyRef

                        let ctxWithNewAssy =
                            { ctxAfterArgs with
                                LoadedAssemblies = newAssemblies
                            }

                        // Now try to find the type in the loaded assembly
                        match findTypeDefinition loadedAssy typeRef.Namespace typeRef.Name with
                        | Some (foundAssy, typeDef) ->
                            foundAssy.Name,
                            ComparableTypeDefinitionHandle.Make typeDef.TypeDefHandle,
                            typeDef.Namespace,
                            typeDef.Name,
                            ctxWithNewAssy
                        | None ->
                            failwithf
                                "Type %s.%s not found in loaded assembly %s or its forwards"
                                typeRef.Namespace
                                typeRef.Name
                                loadedAssy.Name.FullName

                | _ -> failwith "TODO: handle other resolution scopes for type refs in generic instantiation"
            | _ -> failwithf "Generic instantiation of %A not supported" genericDef

        // Check if this exact generic instantiation already exists
        let key = (baseAssembly, baseNamespace, baseName, argHandles)

        match AllConcreteTypes.findExistingConcreteType ctxAfterArgs.ConcreteTypes key with
        | Some existingHandle ->
            // Type already exists, return it
            existingHandle, ctxAfterArgs
        | None ->
            // Need to handle cycles: check if we're already processing this type
            let typeDefnKey = (assembly, GenericInstantiation (genericDef, args))

            match ctxAfterArgs.InProgress.TryGetValue typeDefnKey with
            | true, handle ->
                // We're in a cycle, return the in-progress handle
                handle, ctxAfterArgs
            | false, _ ->
                // Pre-allocate a handle for this type to handle cycles
                let tempId = ctxAfterArgs.ConcreteTypes.NextHandle
                let tempHandle = ConcreteTypeHandle.Concrete tempId

                // Create the concrete type
                let concreteType =
                    {
                        _AssemblyName = baseAssembly
                        _Definition = baseTypeDefHandle
                        _Namespace = baseNamespace
                        _Name = baseName
                        _Generics = argHandles
                    }

                // Add to the concrete types and mark as in progress
                let newCtx =
                    { ctxAfterArgs with
                        ConcreteTypes =
                            { ctxAfterArgs.ConcreteTypes with
                                NextHandle = ctxAfterArgs.ConcreteTypes.NextHandle + 1
                                Mapping = ctxAfterArgs.ConcreteTypes.Mapping |> Map.add tempId concreteType
                            }
                        InProgress = ctxAfterArgs.InProgress.SetItem (typeDefnKey, tempHandle)
                    }

                // Remove from in-progress when done
                let finalCtx =
                    { newCtx with
                        InProgress = newCtx.InProgress.Remove typeDefnKey
                    }

                tempHandle, finalCtx

/// High-level API for concretizing types
[<RequireQualifiedAccess>]
module Concretization =

    /// Helper to concretize an array of types
    let private concretizeTypeArray
        (ctx : TypeConcretization.ConcretizationContext)
        (loadAssembly :
            AssemblyName -> AssemblyReferenceHandle -> (ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly))
        (assembly : AssemblyName)
        (typeArgs : ConcreteTypeHandle ImmutableArray)
        (methodArgs : ConcreteTypeHandle ImmutableArray)
        (types : ImmutableArray<TypeDefn>)
        : ImmutableArray<ConcreteTypeHandle> * TypeConcretization.ConcretizationContext
        =

        let handles = ImmutableArray.CreateBuilder (types.Length)
        let mutable ctx = ctx

        for i = 0 to types.Length - 1 do
            let handle, newCtx =
                TypeConcretization.concretizeType ctx loadAssembly assembly typeArgs methodArgs types.[i]

            handles.Add handle
            ctx <- newCtx

        handles.ToImmutable (), ctx

    /// Helper to concretize a method signature
    let private concretizeMethodSignature
        (ctx : TypeConcretization.ConcretizationContext)
        (loadAssembly :
            AssemblyName -> AssemblyReferenceHandle -> (ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly))
        (assembly : AssemblyName)
        (typeArgs : ConcreteTypeHandle ImmutableArray)
        (methodArgs : ConcreteTypeHandle ImmutableArray)
        (signature : TypeMethodSignature<TypeDefn>)
        : TypeMethodSignature<ConcreteTypeHandle> * TypeConcretization.ConcretizationContext
        =

        // Concretize return type
        let returnHandle, ctx =
            TypeConcretization.concretizeType ctx loadAssembly assembly typeArgs methodArgs signature.ReturnType

        // Concretize parameter types
        let paramHandles = ResizeArray<ConcreteTypeHandle> ()
        let mutable ctx = ctx

        for paramType in signature.ParameterTypes do
            let handle, newCtx =
                TypeConcretization.concretizeType ctx loadAssembly assembly typeArgs methodArgs paramType

            paramHandles.Add (handle)
            ctx <- newCtx

        let newSignature =
            {
                Header = signature.Header
                ReturnType = returnHandle
                ParameterTypes = paramHandles |> Seq.toList
                GenericParameterCount = signature.GenericParameterCount
                RequiredParameterCount = signature.RequiredParameterCount
            }

        newSignature, ctx

    /// Helper to ensure base type assembly is loaded
    let rec private ensureBaseTypeAssembliesLoaded
        (loadAssembly : AssemblyName -> AssemblyReferenceHandle -> (ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly))
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (assyName : AssemblyName)
        (baseTypeInfo : BaseTypeInfo option)
        : ImmutableDictionary<string, DumpedAssembly>
        =
        match baseTypeInfo with
        | None -> assemblies
        | Some (BaseTypeInfo.TypeRef r) ->
            let assy = assemblies.[assyName.FullName]
            let typeRef = assy.TypeRefs.[r]
            match typeRef.ResolutionScope with
            | TypeRefResolutionScope.Assembly assyRef ->
                let targetAssyRef = assy.AssemblyReferences.[assyRef]
                match assemblies.TryGetValue targetAssyRef.Name.FullName with
                | true, _ -> assemblies
                | false, _ ->
                    // Need to load the assembly - pass the assembly that contains the reference
                    let newAssemblies, _ = loadAssembly assy.Name assyRef
                    newAssemblies
            | _ -> assemblies
        | Some (BaseTypeInfo.TypeDef _)
        | Some (BaseTypeInfo.ForeignAssemblyType _)
        | Some (BaseTypeInfo.TypeSpec _) -> assemblies

    /// Concretize a method's signature and body
    let concretizeMethod
        (ctx : AllConcreteTypes)
        (loadAssembly :
            AssemblyName -> AssemblyReferenceHandle -> (ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly))
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (baseTypes : BaseClassTypes<DumpedAssembly>)
        (method : WoofWare.PawPrint.MethodInfo<TypeDefn, WoofWare.PawPrint.GenericParameter, TypeDefn>)
        (typeArgs : ConcreteTypeHandle ImmutableArray)
        (methodArgs : ConcreteTypeHandle ImmutableArray)
        : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          AllConcreteTypes *
          ImmutableDictionary<string, DumpedAssembly>
        =

        // Ensure base type assemblies are loaded for the declaring type
        let assemblies =
            let assy = assemblies.[method.DeclaringType._AssemblyName.FullName]
            let typeDef = assy.TypeDefs.[method.DeclaringType._Definition.Get]
            ensureBaseTypeAssembliesLoaded loadAssembly assemblies assy.Name typeDef.BaseType

        let concCtx =
            {
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = ctx
                TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseTypes
            }

        // First, we need to create a TypeDefn for the declaring type with its generics instantiated
        let declaringTypeDefn =
            if method.DeclaringType._Generics.IsEmpty then
                // Non-generic type - determine the SignatureTypeKind
                let assy = concCtx.LoadedAssemblies.[method.DeclaringType._AssemblyName.FullName]
                let arg = assy.TypeDefs.[method.DeclaringType._Definition.Get]

                let baseType =
                    arg.BaseType
                    |> DumpedAssembly.resolveBaseType baseTypes concCtx.LoadedAssemblies assy.Name

                let signatureTypeKind =
                    match baseType with
                    | ResolvedBaseType.Enum
                    | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                    | ResolvedBaseType.Object
                    | ResolvedBaseType.Delegate -> SignatureTypeKind.Class

                TypeDefn.FromDefinition (
                    method.DeclaringType._Definition,
                    method.DeclaringType._AssemblyName.FullName,
                    signatureTypeKind
                )
            else
                // Generic type - create a GenericInstantiation
                let assy = concCtx.LoadedAssemblies.[method.DeclaringType._AssemblyName.FullName]
                let arg = assy.TypeDefs.[method.DeclaringType._Definition.Get]

                let baseTypeResolved =
                    arg.BaseType
                    |> DumpedAssembly.resolveBaseType baseTypes concCtx.LoadedAssemblies assy.Name

                let signatureTypeKind =
                    match baseTypeResolved with
                    | ResolvedBaseType.Enum
                    | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                    | ResolvedBaseType.Object
                    | ResolvedBaseType.Delegate -> SignatureTypeKind.Class

                let baseType =
                    TypeDefn.FromDefinition (
                        method.DeclaringType._Definition,
                        method.DeclaringType._AssemblyName.FullName,
                        signatureTypeKind
                    )

                let genericArgsLength = method.DeclaringType.Generics.Length

                if genericArgsLength > typeArgs.Length then
                    failwithf
                        "Method declaring type expects %d generic arguments but only %d provided"
                        genericArgsLength
                        typeArgs.Length

                let genericArgs =
                    typeArgs.Slice (0, genericArgsLength)
                    |> Seq.mapi (fun i _ -> TypeDefn.GenericTypeParameter i)
                    |> ImmutableArray.CreateRange

                TypeDefn.GenericInstantiation (baseType, genericArgs)

        // Concretize the declaring type
        let declaringHandle, concCtx =
            TypeConcretization.concretizeType
                concCtx
                loadAssembly
                method.DeclaringType._AssemblyName
                typeArgs
                methodArgs
                declaringTypeDefn

        // Look up the concretized declaring type
        let concretizedDeclaringType =
            AllConcreteTypes.lookup declaringHandle concCtx.ConcreteTypes |> Option.get

        // Concretize signature
        let signature, concCtx =
            concretizeMethodSignature
                concCtx
                loadAssembly
                method.DeclaringType._AssemblyName
                typeArgs
                methodArgs
                method.Signature

        // Concretize local variables
        let instructions, concCtx2 =
            match method.Instructions with
            | None -> None, concCtx
            | Some instr ->
                let locals, updatedCtx =
                    match instr.LocalVars with
                    | None -> None, concCtx
                    | Some vars ->
                        let handles, ctx =
                            concretizeTypeArray
                                concCtx
                                loadAssembly
                                method.DeclaringType._AssemblyName
                                typeArgs
                                methodArgs
                                vars

                        Some handles, ctx

                Some (MethodInstructions.setLocalVars locals instr), updatedCtx

        // Map generics to handles
        let genericHandles =
            method.Generics
            |> Seq.mapi (fun i _ -> methodArgs.[i])
            |> ImmutableArray.CreateRange

        let concretizedMethod : MethodInfo<_, _, ConcreteTypeHandle> =
            {
                DeclaringType = concretizedDeclaringType
                Handle = method.Handle
                Name = method.Name
                Instructions = instructions
                Parameters = method.Parameters
                Generics = genericHandles
                Signature = signature
                RawSignature = method.RawSignature
                CustomAttributes = method.CustomAttributes
                MethodAttributes = method.MethodAttributes
                ImplAttributes = method.ImplAttributes
                IsStatic = method.IsStatic
            }

        concretizedMethod, concCtx2.ConcreteTypes, concCtx2.LoadedAssemblies

    let rec concreteHandleToTypeDefn
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : TypeDefn
        =
        match handle with
        | ConcreteTypeHandle.Byref elementHandle ->
            let elementType =
                concreteHandleToTypeDefn baseClassTypes elementHandle concreteTypes assemblies

            TypeDefn.Byref elementType
        | ConcreteTypeHandle.Pointer elementHandle ->
            let elementType =
                concreteHandleToTypeDefn baseClassTypes elementHandle concreteTypes assemblies

            TypeDefn.Pointer elementType
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle concreteTypes with
            | None -> failwith "Logic error: handle not found"
            | Some concreteType ->

            // Determine SignatureTypeKind
            let assy = assemblies.[concreteType.Assembly.FullName]
            let typeDef = assy.TypeDefs.[concreteType.Definition.Get]
            // Determine SignatureTypeKind from base type
            let baseType =
                typeDef.BaseType
                |> DumpedAssembly.resolveBaseType baseClassTypes assemblies assy.Name

            let signatureTypeKind =
                match baseType with
                | ResolvedBaseType.Enum
                | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                | ResolvedBaseType.Object
                | ResolvedBaseType.Delegate -> SignatureTypeKind.Class

            if concreteType.Generics.IsEmpty then
                TypeDefn.FromDefinition (concreteType.Definition, concreteType.Assembly.FullName, signatureTypeKind)
            else
                // Recursively convert generic arguments
                let genericArgs =
                    concreteType.Generics
                    |> List.map (fun h -> concreteHandleToTypeDefn baseClassTypes h concreteTypes assemblies)
                    |> ImmutableArray.CreateRange

                let baseDef =
                    TypeDefn.FromDefinition (concreteType.Definition, concreteType.Assembly.FullName, signatureTypeKind)

                TypeDefn.GenericInstantiation (baseDef, genericArgs)
